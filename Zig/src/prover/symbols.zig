// ==========================================
// symbols.zig
// 論理演算子の定義 (LogicSymbols.scala移植)
// ==========================================

const std = @import("std");

// 命題論理
pub const And = "∧";
pub const Or = "∨";
pub const Implies = "→";
pub const Not = "¬";
pub const Forall = "∀";
pub const Exists = "∃";
pub const Lambda = "λ";
pub const Eq = "=";
pub const True = "⊤";
pub const False = "⊥";

// 圏論的な別名
pub const Product = "×";
pub const Coproduct = "+";
pub const Exp = "^";
pub const Terminal = "1";
pub const Initial = "0";

// 圏論的演算子
pub const Compose = "∘";
pub const Id = "id";
pub const Pair = "pair";
pub const Proj1 = "pi1";
pub const Proj2 = "pi2";
pub const Case = "case";
pub const Inl = "inl";
pub const Inr = "inr";

// 自然数
pub const Zero = "0";
pub const Succ = "S";

// モーダル論理
pub const Box = "□";
pub const Diamond = "◇";
pub const Knowledge = "K";
pub const Obligation = "O";

// 線形論理
pub const LImplies = "⊸";
pub const Bang = "!";
pub const Question = "?";
pub const Tensor = "⊗";
pub const LPlus = "⊕";
pub const LOne = "1";
pub const LZero = "0";
pub const LTop = "⊤";
pub const LWith = "&";

// 時相論理
pub const Globally = "G";
pub const Finally = "F";
pub const Next = "X";
pub const Until = "U";

// 分離論理
pub const SepAnd = "*";
pub const PointsTo = "↦";

// HoTT
pub const Path = "path";
pub const PathP = "pathP";
pub const Univ = "Univalence";
pub const Type = "Type";
pub const Refl = "refl";
pub const Transport = "transport";
pub const Concat = "concat";
pub const Cube = "cube";
pub const Comp = "comp";
pub const Fill = "fill";
pub const I0 = "I0";
pub const I1 = "I1";
pub const HComp = "hcomp";
pub const FaceAnd = "∧ᶠ";
pub const FaceOr = "∨ᶠ";
pub const FaceNeg = "¬ᶠ";

// Hoare Logic
pub const Triple = "triple";
pub const Assign = ":=";
pub const Seq = ";";
pub const If = "if";
pub const While = "while";
pub const Skip = "skip";

// 表記揺れ
pub const ImpliesAlt1 = "⊃";
pub const ImpliesAlt2 = "⇒";

/// インターン化されたシンボル (ポインタ等値で比較可能)
pub const InternedString = struct {
    ptr: [*]const u8,
    len: usize,

    pub fn slice(self: InternedString) []const u8 {
        return self.ptr[0..self.len];
    }

    pub fn eql(a: InternedString, b: InternedString) bool {
        return a.ptr == b.ptr and a.len == b.len;
    }
};

/// 文字列プール: 文字列をインターン化してポインタ等値比較を可能にする
pub const StringPool = struct {
    map: std.StringHashMap(InternedString),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) StringPool {
        return .{
            .map = std.StringHashMap(InternedString).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *StringPool) void {
        var iter = self.map.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.map.deinit();
    }

    pub fn intern(self: *StringPool, str: []const u8) !InternedString {
        if (self.map.get(str)) |existing| {
            return existing;
        }
        const owned = try self.allocator.dupe(u8, str);
        const interned = InternedString{ .ptr = owned.ptr, .len = owned.len };
        try self.map.put(owned, interned);
        return interned;
    }
};

/// 全定数シンボルをインターン化した構造体
pub const Symbols = struct {
    and_: InternedString,
    or_: InternedString,
    implies: InternedString,
    not_: InternedString,
    forall: InternedString,
    exists: InternedString,
    lambda: InternedString,
    eq: InternedString,
    true_: InternedString,
    false_: InternedString,
    compose: InternedString,
    id_: InternedString,
    pair: InternedString,
    proj1: InternedString,
    proj2: InternedString,
    case_: InternedString,
    inl: InternedString,
    inr: InternedString,
    zero: InternedString,
    succ: InternedString,
    box: InternedString,
    diamond: InternedString,
    limplies: InternedString,
    bang: InternedString,
    tensor: InternedString,
    globally: InternedString,
    finally_: InternedString,
    next: InternedString,
    until: InternedString,
    path: InternedString,
    refl: InternedString,
    transport: InternedString,
    type_: InternedString,
    triple: InternedString,
    skip: InternedString,

    pub fn init(pool: *StringPool) !Symbols {
        return .{
            .and_ = try pool.intern(And),
            .or_ = try pool.intern(Or),
            .implies = try pool.intern(Implies),
            .not_ = try pool.intern(Not),
            .forall = try pool.intern(Forall),
            .exists = try pool.intern(Exists),
            .lambda = try pool.intern(Lambda),
            .eq = try pool.intern(Eq),
            .true_ = try pool.intern(True),
            .false_ = try pool.intern(False),
            .compose = try pool.intern(Compose),
            .id_ = try pool.intern(Id),
            .pair = try pool.intern(Pair),
            .proj1 = try pool.intern(Proj1),
            .proj2 = try pool.intern(Proj2),
            .case_ = try pool.intern(Case),
            .inl = try pool.intern(Inl),
            .inr = try pool.intern(Inr),
            .zero = try pool.intern(Zero),
            .succ = try pool.intern(Succ),
            .box = try pool.intern(Box),
            .diamond = try pool.intern(Diamond),
            .limplies = try pool.intern(LImplies),
            .bang = try pool.intern(Bang),
            .tensor = try pool.intern(Tensor),
            .globally = try pool.intern(Globally),
            .finally_ = try pool.intern(Finally),
            .next = try pool.intern(Next),
            .until = try pool.intern(Until),
            .path = try pool.intern(Path),
            .refl = try pool.intern(Refl),
            .transport = try pool.intern(Transport),
            .type_ = try pool.intern(Type),
            .triple = try pool.intern(Triple),
            .skip = try pool.intern(Skip),
        };
    }
};

test "StringPool intern and pointer equality" {
    var pool = StringPool.init(std.testing.allocator);
    defer pool.deinit();

    const a = try pool.intern("hello");
    const b = try pool.intern("hello");
    const c = try pool.intern("world");

    try std.testing.expect(InternedString.eql(a, b));
    try std.testing.expect(!InternedString.eql(a, c));
}

test "Symbols initialization" {
    var pool = StringPool.init(std.testing.allocator);
    defer pool.deinit();

    const syms = try Symbols.init(&pool);
    try std.testing.expect(InternedString.eql(syms.and_, try pool.intern("∧")));
    try std.testing.expect(InternedString.eql(syms.lambda, try pool.intern("λ")));
}
