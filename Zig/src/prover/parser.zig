// ==========================================
// parser.zig
// テストパーサー (TestParser.scala移植)
// Pratt parser (演算子優先度上昇法) + UTF-8トークナイザ
// ==========================================

const std = @import("std");
const Allocator = std.mem.Allocator;
const expr_mod = @import("expr.zig");
const syms = @import("symbols.zig");
const Expr = expr_mod.Expr;
const sym = expr_mod.sym;
const var_ = expr_mod.var_;
const app1 = expr_mod.app1;
const app2 = expr_mod.app2;

pub const ParseError = error{ UnexpectedToken, UnexpectedEnd, InvalidSyntax, OutOfMemory };

pub const Token = union(enum) {
    symbol: []const u8,
    ident: []const u8,
    lparen: void,
    rparen: void,
    comma: void,
    dot: void,
    eof: void,
};

/// UTF-8対応トークナイザ
pub const Tokenizer = struct {
    input: []const u8,
    pos: usize,

    pub fn init(input: []const u8) Tokenizer {
        return .{ .input = input, .pos = 0 };
    }

    pub fn next(self: *Tokenizer) Token {
        self.skipWhitespace();
        if (self.pos >= self.input.len) return .eof;

        const c = self.input[self.pos];

        // 単文字トークン
        switch (c) {
            '(' => { self.pos += 1; return .lparen; },
            ')' => { self.pos += 1; return .rparen; },
            ',' => { self.pos += 1; return .comma; },
            '.' => { self.pos += 1; return .dot; },
            else => {},
        }

        // UTF-8マルチバイト演算子 (∧, ∨, →, ⊸, ⊗, etc.)
        if (c >= 0x80) {
            const start = self.pos;
            // UTF-8のコードポイントを1文字分読み進める
            const len = std.unicode.utf8ByteSequenceLength(c) catch 1;
            self.pos += len;
            // 連続する非ASCII文字がある場合（例: ∧ᶠ）
            while (self.pos < self.input.len and self.input[self.pos] >= 0x80) {
                const next_len = std.unicode.utf8ByteSequenceLength(self.input[self.pos]) catch break;
                // subscript/superscript modifiers
                if (self.input[self.pos] == 0xe1 or // ᶠ等のmodifier
                    (self.pos + next_len <= self.input.len and isModifier(self.input[self.pos..self.pos + next_len])))
                {
                    self.pos += next_len;
                } else break;
            }
            return .{ .symbol = self.input[start..self.pos] };
        }

        // 特殊ASCII演算子
        if (c == '=' or c == '!' or c == '?' or c == '*' or c == '+' or c == '^' or c == '&') {
            self.pos += 1;
            return .{ .symbol = self.input[self.pos - 1 .. self.pos] };
        }

        // 2文字演算子
        if (self.pos + 1 < self.input.len) {
            const two = self.input[self.pos .. self.pos + 2];
            if (std.mem.eql(u8, two, "->") or std.mem.eql(u8, two, ":=")) {
                self.pos += 2;
                return .{ .symbol = two };
            }
        }

        // 識別子 (英数字+アンダースコア)
        if (std.ascii.isAlphabetic(c) or c == '_') {
            const start = self.pos;
            while (self.pos < self.input.len and (std.ascii.isAlphanumeric(self.input[self.pos]) or self.input[self.pos] == '_')) {
                self.pos += 1;
            }
            return .{ .ident = self.input[start..self.pos] };
        }

        // 数字
        if (std.ascii.isDigit(c)) {
            const start = self.pos;
            while (self.pos < self.input.len and std.ascii.isDigit(self.input[self.pos])) {
                self.pos += 1;
            }
            return .{ .ident = self.input[start..self.pos] };
        }

        // 不明文字: 1文字トークンとして返す
        self.pos += 1;
        return .{ .symbol = self.input[self.pos - 1 .. self.pos] };
    }

    fn skipWhitespace(self: *Tokenizer) void {
        while (self.pos < self.input.len and (self.input[self.pos] == ' ' or self.input[self.pos] == '\t' or self.input[self.pos] == '\n' or self.input[self.pos] == '\r')) {
            self.pos += 1;
        }
    }

    fn isModifier(bytes: []const u8) bool {
        // ᶠ (U+1DA0) = E1 B6 A0 等のmodifier letters
        if (bytes.len >= 3 and bytes[0] == 0xe1) return true;
        return false;
    }
};

/// Pratt Parser
pub const Parser = struct {
    tokenizer: Tokenizer,
    current: Token,
    arena: Allocator,

    pub fn init(input: []const u8, arena: Allocator) Parser {
        var p = Parser{
            .tokenizer = Tokenizer.init(input),
            .current = .eof,
            .arena = arena,
        };
        p.current = p.tokenizer.next();
        return p;
    }

    fn advance(self: *Parser) void {
        self.current = self.tokenizer.next();
    }

    /// 式をパース
    pub fn parseExpr(self: *Parser) ParseError!*const Expr {
        return self.parseBinary(0);
    }

    fn parseBinary(self: *Parser, min_prec: u8) ParseError!*const Expr {
        var left = try self.parseUnary();

        while (true) {
            const prec = self.currentPrecedence();
            if (prec < min_prec) break;

            const op = switch (self.current) {
                .symbol => |s| s,
                else => break,
            };

            // 二項演算子か確認
            if (!isBinaryOp(op)) break;

            self.advance();
            const right = try self.parseBinary(prec + 1);
            const op_sym = try sym(self.arena, try self.arena.dupe(u8, op));
            left = try app2(self.arena, op_sym, left, right);
        }

        return left;
    }

    fn parseUnary(self: *Parser) ParseError!*const Expr {
        switch (self.current) {
            .symbol => |s| {
                // 単項演算子 (¬, □, ◇, G, F, X, !, S)
                if (isUnaryOp(s)) {
                    self.advance();
                    const operand = try self.parseUnary();
                    const op_sym = try sym(self.arena, try self.arena.dupe(u8, s));
                    return app1(self.arena, op_sym, operand);
                }
                return self.parsePrimary();
            },
            else => return self.parsePrimary(),
        }
    }

    fn parsePrimary(self: *Parser) ParseError!*const Expr {
        switch (self.current) {
            .lparen => {
                self.advance();
                const e = try self.parseExpr();
                if (self.current != .rparen) return ParseError.UnexpectedToken;
                self.advance();
                return e;
            },
            .ident => |name| {
                const owned = try self.arena.dupe(u8, name);
                self.advance();

                // 関数適用: f(a, b, ...)
                if (self.current == .lparen) {
                    self.advance();
                    var args: std.ArrayList(*const Expr) = .{};
                    if (self.current != .rparen) {
                        try args.append(self.arena, try self.parseExpr());
                        while (self.current == .comma) {
                            self.advance();
                            try args.append(self.arena, try self.parseExpr());
                        }
                    }
                    if (self.current != .rparen) return ParseError.UnexpectedToken;
                    self.advance();

                    const head = try sym(self.arena, owned);
                    if (args.items.len == 0) return head;
                    return expr_mod.app(self.arena, head, args.items);
                }

                // 小文字で始まる = 変数、大文字/数字 = シンボル
                if (owned.len > 0 and std.ascii.isLower(owned[0])) {
                    return var_(self.arena, owned);
                }
                return sym(self.arena, owned);
            },
            .symbol => |s| {
                const owned = try self.arena.dupe(u8, s);
                self.advance();

                // シンボルが後に引数を取る場合
                if (self.current == .lparen) {
                    self.advance();
                    var args: std.ArrayList(*const Expr) = .{};
                    if (self.current != .rparen) {
                        try args.append(self.arena, try self.parseExpr());
                        while (self.current == .comma) {
                            self.advance();
                            try args.append(self.arena, try self.parseExpr());
                        }
                    }
                    if (self.current != .rparen) return ParseError.UnexpectedToken;
                    self.advance();

                    const head = try sym(self.arena, owned);
                    if (args.items.len == 0) return head;
                    return expr_mod.app(self.arena, head, args.items);
                }

                return sym(self.arena, owned);
            },
            .eof => return ParseError.UnexpectedEnd,
            else => return ParseError.UnexpectedToken,
        }
    }

    fn currentPrecedence(self: *Parser) u8 {
        return switch (self.current) {
            .symbol => |s| getPrec(s),
            else => 0,
        };
    }

    fn getPrec(op: []const u8) u8 {
        if (std.mem.eql(u8, op, "∨") or std.mem.eql(u8, op, "+")) return 2;
        if (std.mem.eql(u8, op, "∧") or std.mem.eql(u8, op, "×") or std.mem.eql(u8, op, "*") or std.mem.eql(u8, op, "&")) return 3;
        if (std.mem.eql(u8, op, "→") or std.mem.eql(u8, op, "⊸") or std.mem.eql(u8, op, "->")) return 1;
        if (std.mem.eql(u8, op, "=")) return 4;
        if (std.mem.eql(u8, op, "⊗")) return 3;
        return 0;
    }

    fn isBinaryOp(op: []const u8) bool {
        return getPrec(op) > 0;
    }

    fn isUnaryOp(op: []const u8) bool {
        return std.mem.eql(u8, op, "¬") or std.mem.eql(u8, op, "□") or std.mem.eql(u8, op, "◇") or std.mem.eql(u8, op, "!");
    }
};

/// 文字列をパースしてExprに変換
pub fn parse(input: []const u8, arena: Allocator) ParseError!*const Expr {
    var parser = Parser.init(input, arena);
    return parser.parseExpr();
}

// テスト
test "parse simple symbol" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const e = try parse("A", arena);
    try std.testing.expect(e.* == .sym);
    try std.testing.expectEqualStrings("A", e.sym);
}

test "parse implication" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const e = try parse("A → B", arena);
    try std.testing.expect(e.* == .app);
    try std.testing.expectEqualStrings("→", e.app.head.sym);
}

test "parse function application" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const e = try parse("f(A, B)", arena);
    try std.testing.expect(e.* == .app);
    try std.testing.expect(e.app.args.len == 2);
}

test "tokenizer handles UTF-8" {
    var tok = Tokenizer.init("A ∧ B → C");
    const t1 = tok.next();
    try std.testing.expect(t1 == .ident);
    const t2 = tok.next();
    try std.testing.expect(t2 == .symbol);
    try std.testing.expectEqualStrings("∧", t2.symbol);
}
