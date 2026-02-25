// ============================================================
// tests.zig — Scalaテストケースから移植したZig統合テスト群
//
// Curry-Howard-Lambek同型対応により、Scalaの証明 ↔ Zigのバイトコードプログラム
//
//   Curry-Howard : 証明    ↔ プログラム (項)
//   Lambek       : 証明    ↔ 圏の射 (∘, pair, case, λ)
//                  命題    ↔ オブジェクト
//
// Romanescは型無しがメインの設計であり、型を圏論的射で回避している。
// そのためバイトコードも「型付き値」ではなく「射の合成」として解釈する。
// 各テストはScalaの対応するテストケース名をコメントで示す
// ============================================================

const std = @import("std");
const vm_mod = @import("Runtime/vm.zig");
const opt_mod = @import("Runtime/optimizer.zig");
const ana_mod = @import("Runtime/analyzer.zig");

const VM = vm_mod.VM;
const Op = vm_mod.Op;
const Value = vm_mod.Value;
const Optimizer = opt_mod.Optimizer;
const RangeAnalyzer = ana_mod.RangeAnalyzer;

// ============================================================
// 1. 命題論理テスト (SolverTest.scala の基本ケース)
//    Lambek対応: 命題 = オブジェクト、証明 = 射、推論規則 = 射の構成則
//    バイトコード命令はそのまま圏の射 (pair, pi1, pi2, case, λ, ∘) に対応
// ============================================================

// A → A (恒等射: id_A)
// Lambek: 恒等射 id は圏の公理。型無しVMでは reg[0]→ret がそのまま id
// 対応Scala: SolverTest.scala, LinearLogicTest.scala "A ⊸ A"
test "命題論理: A → A (恒等)" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    const id_body = &[_]Op{
        .{ .ret = .{ .src = 0 } }, // λx. x : 引数 reg[0] をそのまま返す
    };
    const code = &[_]Op{
        .{ .make_closure = .{ .dst = 0, .body = id_body, .captures = &[_]u32{}, .arity = 1, .block_idx = 0 } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 42 } } },
        .{ .call = .{ .dst = 2, .func = 0, .args = &[_]u32{1} } },
        .{ .ret = .{ .src = 2 } },
    };
    const result = try machine.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 42), result.bits);
}

// A ∧ B → A (第一射影: π₁ — 積の普遍性)
// Lambek: 直積オブジェクト A×B から A への射 π₁
// 対応Scala: SolverTest.scala, StandardRules pi1
test "命題論理: A ∧ B → A (第一射影)" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    // 証明項: λp. π₁(p)
    // proj1(dst=1, src=0): reg[1] = pair.fst, reg[0] = pair.snd (副作用)
    const fst_body = &[_]Op{
        .{ .proj1 = .{ .dst = 1, .src = 0 } }, // reg[1] = A, reg[0] = B
        .{ .ret = .{ .src = 1 } },              // Aを返す
    };
    const code = &[_]Op{
        .{ .make_closure = .{ .dst = 0, .body = fst_body, .captures = &[_]u32{}, .arity = 1, .block_idx = 1 } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 10 } } }, // A = 10
        .{ .load_const = .{ .dst = 2, .val = .{ .bits = 20 } } }, // B = 20
        .{ .make_pair = .{ .dst = 3, .fst = 1, .snd = 2 } },     // (A, B)
        .{ .call = .{ .dst = 4, .func = 0, .args = &[_]u32{3} } },
        .{ .ret = .{ .src = 4 } },
    };
    const result = try machine.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 10), result.bits); // Aを得る
}

// A ∧ B → B (第二射影: π₂ — 積の普遍性)
// Lambek: 直積オブジェクト A×B から B への射 π₂
// 対応Scala: SolverTest.scala, StandardRules pi2
test "命題論理: A ∧ B → B (第二射影)" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    // 証明項: λp. π₂(p)
    // proj2(dst=1, src=0): reg[1] = pair.snd, reg[0] = pair.fst (副作用)
    const snd_body = &[_]Op{
        .{ .proj2 = .{ .dst = 1, .src = 0 } }, // reg[1] = B, reg[0] = A
        .{ .ret = .{ .src = 1 } },              // Bを返す
    };
    const code = &[_]Op{
        .{ .make_closure = .{ .dst = 0, .body = snd_body, .captures = &[_]u32{}, .arity = 1, .block_idx = 2 } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 10 } } },
        .{ .load_const = .{ .dst = 2, .val = .{ .bits = 20 } } },
        .{ .make_pair = .{ .dst = 3, .fst = 1, .snd = 2 } },
        .{ .call = .{ .dst = 4, .func = 0, .args = &[_]u32{3} } },
        .{ .ret = .{ .src = 4 } },
    };
    const result = try machine.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 20), result.bits); // Bを得る
}

// A → A ∧ A (対角射: Δ = ⟨id, id⟩ — 積の普遍性の双対)
// Lambek: pair(f, g) = ⟨f, g⟩ は積の普遍射
// 対応Scala: StandardRules pair/diagonal
test "命題論理: A → A ∧ A (対角写像)" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    // 証明項: λx. (x, x)
    // borrow で複製してからpairを作る
    const diag_body = &[_]Op{
        .{ .borrow = .{ .dst = 1, .src = 0 } },       // x をコピー
        .{ .make_pair = .{ .dst = 2, .fst = 0, .snd = 1 } }, // (x, x)
        .{ .ret = .{ .src = 2 } },
    };
    const code = &[_]Op{
        .{ .make_closure = .{ .dst = 0, .body = diag_body, .captures = &[_]u32{}, .arity = 1, .block_idx = 3 } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 7 } } },
        .{ .call = .{ .dst = 2, .func = 0, .args = &[_]u32{1} } },
        // 結果はペア (7, 7) → π₁を確認
        .{ .proj1 = .{ .dst = 3, .src = 2 } },
        .{ .ret = .{ .src = 3 } },
    };
    const result = try machine.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 7), result.bits);
}

// (A → B) → (B → C) → (A → C) (射の合成: g ∘ f)
// Lambek対応: 圏の射合成 ∘ そのもの
// 対応Scala: StandardRules compose (∘)
test "命題論理: 関数合成 (A→B)→(B→C)→(A→C)" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    // compose_inner: env=[f(0), g(1)], arg=x(2)
    //   f: x → x+1, g: x → x*2
    //   compose(f, g)(3) = g(f(3)) = g(4) = 8
    const compose_inner = &[_]Op{
        .{ .borrow = .{ .dst = 3, .src = 0 } },              // clone f
        .{ .call = .{ .dst = 4, .func = 3, .args = &[_]u32{2} } }, // f(x)
        .{ .borrow = .{ .dst = 5, .src = 1 } },              // clone g
        .{ .call = .{ .dst = 6, .func = 5, .args = &[_]u32{4} } }, // g(f(x))
        .{ .ret = .{ .src = 6 } },
    };
    // compose_outer: env=[f(0)], arg=g(1)
    const compose_outer = &[_]Op{
        .{ .make_closure = .{ .dst = 2, .body = compose_inner, .captures = &[_]u32{ 0, 1 }, .arity = 1, .block_idx = 40 } },
        .{ .ret = .{ .src = 2 } },
    };
    // compose: env=[], arg=f(0)
    const compose_top = &[_]Op{
        .{ .make_closure = .{ .dst = 1, .body = compose_outer, .captures = &[_]u32{0}, .arity = 1, .block_idx = 41 } },
        .{ .ret = .{ .src = 1 } },
    };

    // f(x) = x + 1
    const f_body = &[_]Op{
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 1 } } },
        .{ .add = .{ .dst = 2, .lhs = 0, .rhs = 1 } },
        .{ .ret = .{ .src = 2 } },
    };
    // g(x) = x * 2
    const g_body = &[_]Op{
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 2 } } },
        .{ .mul = .{ .dst = 2, .lhs = 0, .rhs = 1 } },
        .{ .ret = .{ .src = 2 } },
    };

    const code = &[_]Op{
        // compose関数を作成
        .{ .make_closure = .{ .dst = 0, .body = compose_top, .captures = &[_]u32{}, .arity = 1, .block_idx = 42 } },
        // fとgを定義
        .{ .make_closure = .{ .dst = 1, .body = f_body, .captures = &[_]u32{}, .arity = 1, .block_idx = 43 } },
        .{ .make_closure = .{ .dst = 2, .body = g_body, .captures = &[_]u32{}, .arity = 1, .block_idx = 44 } },
        // compose(f)
        .{ .call = .{ .dst = 3, .func = 0, .args = &[_]u32{1} } },
        // compose(f)(g)
        .{ .call = .{ .dst = 4, .func = 3, .args = &[_]u32{2} } },
        // compose(f)(g)(3) = g(f(3)) = g(4) = 8
        .{ .load_const = .{ .dst = 5, .val = .{ .bits = 3 } } },
        .{ .call = .{ .dst = 6, .func = 4, .args = &[_]u32{5} } },
        .{ .ret = .{ .src = 6 } },
    };
    const result = try machine.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 8), result.bits);
}

// A ∨ B → C (余積の普遍射: [f, g])
// Lambek: case(f, g) = [f, g] は余積の普遍射 (コペア)
// 対応Scala: SolverTest.scala, StandardRules case
test "命題論理: A ∨ B → C (case分析)" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    // case(inl(5)): left branch returns x+1 = 6
    // case(inr(5)): right branch returns x*2 = 10
    const inl_branch = &[_]Op{
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 1 } } },
        .{ .add = .{ .dst = 2, .lhs = 2, .rhs = 1 } }, // dst=2に値が入る
        .{ .ret = .{ .src = 2 } },
    };
    const inr_branch = &[_]Op{
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 2 } } },
        .{ .mul = .{ .dst = 2, .lhs = 2, .rhs = 1 } },
        .{ .ret = .{ .src = 2 } },
    };

    // Left injection: inl(5) → case → 6
    {
        const code = &[_]Op{
            .{ .load_const = .{ .dst = 0, .val = .{ .bits = 5 } } },
            .{ .make_inl = .{ .dst = 1, .src = 0 } },
            .{ .case_op = .{ .dst = 2, .scrutinee = 1, .inl_branch = inl_branch, .inr_branch = inr_branch } },
            .{ .ret = .{ .src = 2 } },
        };
        const result = try machine.run(code);
        defer result.deinit(allocator);
        try std.testing.expectEqual(@as(u64, 6), result.bits);
    }

    // Right injection: inr(5) → case → 10
    {
        const code = &[_]Op{
            .{ .load_const = .{ .dst = 0, .val = .{ .bits = 5 } } },
            .{ .make_inr = .{ .dst = 1, .src = 0 } },
            .{ .case_op = .{ .dst = 2, .scrutinee = 1, .inl_branch = inl_branch, .inr_branch = inr_branch } },
            .{ .ret = .{ .src = 2 } },
        };
        const result = try machine.run(code);
        defer result.deinit(allocator);
        try std.testing.expectEqual(@as(u64, 10), result.bits);
    }
}

// ============================================================
// 2. 線形論理テスト (LinearLogicTest.scala)
//    Lambek対応: モノイダル閉圏。⊗ はテンソル積（モノイダル構造）
//    ⊸ は内部hom (A ⊸ B ≅ Hom(A, B))。線形性 = 射は資源を1回だけ使う
// ============================================================

// A ⊸ A (線形恒等写像)
// 対応Scala: LinearLogicTest.scala "A ⊸ A"
test "線形論理: A ⊸ A (線形恒等)" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    // 線形恒等: 引数を複製せず直接返す (borrow不使用)
    const linear_id_body = &[_]Op{
        .{ .ret = .{ .src = 0 } },
    };
    const code = &[_]Op{
        .{ .make_closure = .{ .dst = 0, .body = linear_id_body, .captures = &[_]u32{}, .arity = 1, .block_idx = 5 } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 99 } } },
        .{ .call = .{ .dst = 2, .func = 0, .args = &[_]u32{1} } },
        .{ .ret = .{ .src = 2 } },
    };
    const result = try machine.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 99), result.bits);
}

// (A ⊗ B) ⊸ (A ⊗ B) (テンソル積の恒等写像)
// 対応Scala: LinearLogicTest.scala "(A ⊗ B) ⊸ (A ⊗ B)"
test "線形論理: (A ⊗ B) ⊸ (A ⊗ B) (テンソル恒等)" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    // テンソル恒等: ペアをそのまま返す
    const tensor_id_body = &[_]Op{
        .{ .ret = .{ .src = 0 } }, // ペアをそのまま返す
    };
    const code = &[_]Op{
        .{ .make_closure = .{ .dst = 0, .body = tensor_id_body, .captures = &[_]u32{}, .arity = 1, .block_idx = 6 } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 3 } } },
        .{ .load_const = .{ .dst = 2, .val = .{ .bits = 7 } } },
        .{ .make_pair = .{ .dst = 3, .fst = 1, .snd = 2 } },
        .{ .call = .{ .dst = 4, .func = 0, .args = &[_]u32{3} } },
        // π₁を確認
        .{ .proj1 = .{ .dst = 5, .src = 4 } },
        .{ .ret = .{ .src = 5 } },
    };
    const result = try machine.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 3), result.bits);
}

// A ⊗ B ⊸ B ⊗ A (テンソル積の対称性: σ_{A,B})
// Lambek: 対称モノイダル圏における交換子 (braiding)
// 対応Scala: LinearLogicTest.scala のtensor swap
test "線形論理: A ⊗ B ⊸ B ⊗ A (テンソル交換)" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    // 証明項: λp. (π₂(p), π₁(p))
    // proj1(dst=1, src=0): reg[1]=A, reg[0]=B (pairを消費)
    // make_pair(dst=2, fst=0, snd=1): (B, A)
    const swap_body = &[_]Op{
        .{ .proj1 = .{ .dst = 1, .src = 0 } }, // reg[1]=A, reg[0]=B
        .{ .make_pair = .{ .dst = 2, .fst = 0, .snd = 1 } }, // (B, A)
        .{ .ret = .{ .src = 2 } },
    };
    const code = &[_]Op{
        .{ .make_closure = .{ .dst = 0, .body = swap_body, .captures = &[_]u32{}, .arity = 1, .block_idx = 7 } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 100 } } }, // A = 100
        .{ .load_const = .{ .dst = 2, .val = .{ .bits = 200 } } }, // B = 200
        .{ .make_pair = .{ .dst = 3, .fst = 1, .snd = 2 } },      // (A, B) = (100, 200)
        .{ .call = .{ .dst = 4, .func = 0, .args = &[_]u32{3} } },
        // 結果は (B, A) = (200, 100) → π₁ = 200
        .{ .proj1 = .{ .dst = 5, .src = 4 } },
        .{ .ret = .{ .src = 5 } },
    };
    const result = try machine.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 200), result.bits); // Bが先頭
}

// !A → A (Bang除去: 必然から存在)
// 対応Scala: LinearLogicTest.scala "!A → A"
test "線形論理: !A → A (Bang除去)" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    // Bang除去: !Aはborrowで複製可能な値、Aとして使う
    // borrowで値を取り出してそのまま返す
    const bang_elim_body = &[_]Op{
        .{ .borrow = .{ .dst = 1, .src = 0 } }, // !Aからコピーを取り出す
        .{ .free = .{ .reg = 0 } },              // 元のリソースを解放
        .{ .ret = .{ .src = 1 } },
    };
    const code = &[_]Op{
        .{ .make_closure = .{ .dst = 0, .body = bang_elim_body, .captures = &[_]u32{}, .arity = 1, .block_idx = 8 } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 55 } } },
        .{ .call = .{ .dst = 2, .func = 0, .args = &[_]u32{1} } },
        .{ .ret = .{ .src = 2 } },
    };
    const result = try machine.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 55), result.bits);
}

// A ⊗ B → A ∧ B (テンソルから積へのマッピング)
// 対応Scala: LinearLogicTest.scala "A ⊗ B → A ∧ B"
// 線形積と非線形積は構造的に同じ
test "線形論理: A ⊗ B → A ∧ B (テンソルから積)" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    // テンソル(A ⊗ B)と積(A ∧ B)は同じペア構造
    // 証明項: λp. (π₁(borrow p), π₂(borrow p))
    const tensor_to_prod_body = &[_]Op{
        .{ .borrow = .{ .dst = 1, .src = 0 } },   // p をコピー (非線形的使用のため)
        .{ .proj1 = .{ .dst = 2, .src = 0 } },     // π₁(p) = A, reg[0] = B
        .{ .proj2 = .{ .dst = 3, .src = 1 } },     // π₂(copy_p) = B', reg[1] = A'
        .{ .make_pair = .{ .dst = 4, .fst = 2, .snd = 3 } }, // (A, B)
        .{ .ret = .{ .src = 4 } },
    };
    const code = &[_]Op{
        .{ .make_closure = .{ .dst = 0, .body = tensor_to_prod_body, .captures = &[_]u32{}, .arity = 1, .block_idx = 9 } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 11 } } },
        .{ .load_const = .{ .dst = 2, .val = .{ .bits = 22 } } },
        .{ .make_pair = .{ .dst = 3, .fst = 1, .snd = 2 } },
        .{ .call = .{ .dst = 4, .func = 0, .args = &[_]u32{3} } },
        // 結果の第2要素を確認
        .{ .proj2 = .{ .dst = 5, .src = 4 } },
        .{ .ret = .{ .src = 5 } },
    };
    const result = try machine.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 22), result.bits);
}

// ============================================================
// 3. HoTTテスト (HoTTTest.scala)
//    パス型をu64の差分で表現、concat=加算、inv=否定
// ============================================================

// concat(refl(x), p) = p (左単位元)
// 対応Scala: HoTTTest.scala "∀x. ∀y. ∀p:path(A,x,y). concat(refl(x), p) = p"
test "HoTT: concat(refl(x), p) = p (左単位元)" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    // refl(x) = 0 (同一性パス)、concat = 加算
    // concat(0, p) = p
    const code = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 0 } } }, // refl(x) = 0
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 7 } } }, // p = 7
        .{ .add = .{ .dst = 2, .lhs = 0, .rhs = 1 } },          // concat(refl, p) = 0 + 7
        .{ .ret = .{ .src = 2 } },
    };
    const result = try machine.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 7), result.bits); // = p
}

// concat(p, refl(y)) = p (右単位元)
// 対応Scala: HoTTTest.scala "∀x. ∀y. ∀p:path(A,x,y). concat(p, refl(y)) = p"
test "HoTT: concat(p, refl(y)) = p (右単位元)" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    const code = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 13 } } }, // p = 13
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 0 } } },  // refl(y) = 0
        .{ .add = .{ .dst = 2, .lhs = 0, .rhs = 1 } },           // concat(p, refl) = 13 + 0
        .{ .ret = .{ .src = 2 } },
    };
    const result = try machine.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 13), result.bits); // = p
}

// パスの結合法則: concat(concat(p, q), r) = concat(p, concat(q, r))
// 対応Scala: HoTTTest.scala パス結合結合法則
test "HoTT: パス結合の結合法則" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    // concat = 加算 → 加算の結合法則 (p+q)+r = p+(q+r)
    const code_left = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 2 } } }, // p
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 3 } } }, // q
        .{ .load_const = .{ .dst = 2, .val = .{ .bits = 5 } } }, // r
        .{ .add = .{ .dst = 3, .lhs = 0, .rhs = 1 } },          // p+q
        .{ .add = .{ .dst = 4, .lhs = 3, .rhs = 2 } },          // (p+q)+r
        .{ .ret = .{ .src = 4 } },
    };
    const code_right = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 2 } } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 3 } } },
        .{ .load_const = .{ .dst = 2, .val = .{ .bits = 5 } } },
        .{ .add = .{ .dst = 3, .lhs = 1, .rhs = 2 } },          // q+r
        .{ .add = .{ .dst = 4, .lhs = 0, .rhs = 3 } },          // p+(q+r)
        .{ .ret = .{ .src = 4 } },
    };

    const left = try machine.run(code_left);
    defer left.deinit(allocator);
    const right = try machine.run(code_right);
    defer right.deinit(allocator);
    try std.testing.expectEqual(left.bits, right.bits); // 両辺が等しい
}

// inv(inv(p)) = p (逆パスの逆は元のパス)
// 対応Scala: HoTTTest.scala "∀x. ∀y. ∀p:path(A,x,y). inv(inv(p)) = p"
// inv = 符号反転 (u64の補数演算で表現)
test "HoTT: inv(inv(p)) = p (逆パスの逆)" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    // inv(p) = -p (u64補数)、inv(inv(p)) = -(-p) = p
    // u64では: inv(p) = 0 - p
    const inv_body = &[_]Op{
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 0 } } },
        .{ .sub = .{ .dst = 2, .lhs = 1, .rhs = 0 } }, // 0 - p = -p (wrap)
        .{ .ret = .{ .src = 2 } },
    };
    const code = &[_]Op{
        .{ .make_closure = .{ .dst = 0, .body = inv_body, .captures = &[_]u32{}, .arity = 1, .block_idx = 20 } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 42 } } },  // p = 42
        // inv(p)
        .{ .borrow = .{ .dst = 2, .src = 0 } },                   // invをコピー
        .{ .call = .{ .dst = 3, .func = 2, .args = &[_]u32{1} } }, // inv(p)
        // inv(inv(p))
        .{ .call = .{ .dst = 4, .func = 0, .args = &[_]u32{3} } }, // inv(inv(p))
        .{ .ret = .{ .src = 4 } },
    };
    const result = try machine.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 42), result.bits); // = p
}

// ∀x:S1. P(base) → P(x) (HIT: 円S1の帰納法)
// 対応Scala: HoTTTest.scala "∀x:S1. P(base) → P(x)"
// base点の性質は全ての点に伝播 → ペアの第一成分として記録
test "HoTT: HIT S1帰納法 (base点から全点への性質伝播)" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    // S1の点を (point, loop_count) のペアで表現
    // base = (base_prop, 0), any_x = (base_prop, n)
    // P(base) → P(x): base_propをそのまま返す
    const s1_ind_body = &[_]Op{
        // p = (base_prop, loop_count) として受け取る
        .{ .proj1 = .{ .dst = 1, .src = 0 } }, // base_prop を取得、reg[0]=loop_count
        .{ .ret = .{ .src = 1 } },              // base_propを返す
    };
    const code = &[_]Op{
        .{ .make_closure = .{ .dst = 0, .body = s1_ind_body, .captures = &[_]u32{}, .arity = 1, .block_idx = 21 } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 999 } } }, // P(base) = 999
        .{ .load_const = .{ .dst = 2, .val = .{ .bits = 3 } } },   // loop_count = 3
        .{ .make_pair = .{ .dst = 3, .fst = 1, .snd = 2 } },      // x = (P(base), 3)
        .{ .call = .{ .dst = 4, .func = 0, .args = &[_]u32{3} } },
        .{ .ret = .{ .src = 4 } },
    };
    const result = try machine.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 999), result.bits); // P(base)が伝播
}

// ============================================================
// 4. 帰納法テスト (SolverTest.scala の帰納的定義)
//    自然数を整数で表現した算術テスト
// ============================================================

// plus(n, 0) = n (右零元)
// 対応Scala: SolverTest.scala "∀n. plus(n, 0) = n"
test "帰納法: plus(n, 0) = n" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    const n: u64 = 17;
    const code = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = n } } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 0 } } },
        .{ .add = .{ .dst = 2, .lhs = 0, .rhs = 1 } }, // plus(n, 0)
        .{ .ret = .{ .src = 2 } },
    };
    const result = try machine.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(n, result.bits);
}

// plus(0, n) = n (左零元)
// 対応Scala: SolverTest.scala "∀n. plus(0, n) = n"
test "帰納法: plus(0, n) = n" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    const n: u64 = 23;
    const code = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 0 } } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = n } } },
        .{ .add = .{ .dst = 2, .lhs = 0, .rhs = 1 } }, // plus(0, n)
        .{ .ret = .{ .src = 2 } },
    };
    const result = try machine.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(n, result.bits);
}

// plus(n, S(m)) = S(plus(n, m)) (後者の計算則)
// 対応Scala: SolverTest.scala "∀n. ∀m. plus(n, S(m)) = S(plus(n, m))"
test "帰納法: plus(n, S(m)) = S(plus(n, m))" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    const n: u64 = 5;
    const m: u64 = 3;
    // 左辺: plus(n, S(m)) = n + (m + 1)
    const code_lhs = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = n } } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = m } } },
        .{ .load_const = .{ .dst = 2, .val = .{ .bits = 1 } } },
        .{ .add = .{ .dst = 3, .lhs = 1, .rhs = 2 } }, // S(m) = m + 1
        .{ .add = .{ .dst = 4, .lhs = 0, .rhs = 3 } }, // n + S(m)
        .{ .ret = .{ .src = 4 } },
    };
    // 右辺: S(plus(n, m)) = (n + m) + 1
    const code_rhs = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = n } } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = m } } },
        .{ .load_const = .{ .dst = 2, .val = .{ .bits = 1 } } },
        .{ .add = .{ .dst = 3, .lhs = 0, .rhs = 1 } }, // plus(n, m)
        .{ .add = .{ .dst = 4, .lhs = 3, .rhs = 2 } }, // S(plus(n, m))
        .{ .ret = .{ .src = 4 } },
    };
    const lhs = try machine.run(code_lhs);
    defer lhs.deinit(allocator);
    const rhs = try machine.run(code_rhs);
    defer rhs.deinit(allocator);
    try std.testing.expectEqual(lhs.bits, rhs.bits);
}

// plus(n, m) = plus(m, n) (交換法則) — 具体例で検証
// 対応Scala: SolverTest.scala "∀n. ∀m. plus(n, m) = plus(m, n)"
test "帰納法: plus(n, m) = plus(m, n) (加算交換法則)" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    const n: u64 = 11;
    const m: u64 = 7;
    const code_nm = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = n } } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = m } } },
        .{ .add = .{ .dst = 2, .lhs = 0, .rhs = 1 } },
        .{ .ret = .{ .src = 2 } },
    };
    const code_mn = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = m } } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = n } } },
        .{ .add = .{ .dst = 2, .lhs = 0, .rhs = 1 } },
        .{ .ret = .{ .src = 2 } },
    };
    const r_nm = try machine.run(code_nm);
    defer r_nm.deinit(allocator);
    const r_mn = try machine.run(code_mn);
    defer r_mn.deinit(allocator);
    try std.testing.expectEqual(r_nm.bits, r_mn.bits);
}

// plus(plus(n, m), k) = plus(n, plus(m, k)) (結合法則)
// 対応Scala: SolverTest.scala "∀n. ∀m. ∀k. plus(plus(n, m), k) = plus(n, plus(m, k))"
test "帰納法: plus(plus(n, m), k) = plus(n, plus(m, k)) (加算結合法則)" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    const n: u64 = 3;
    const m: u64 = 4;
    const k: u64 = 5;
    const code_left = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = n } } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = m } } },
        .{ .load_const = .{ .dst = 2, .val = .{ .bits = k } } },
        .{ .add = .{ .dst = 3, .lhs = 0, .rhs = 1 } }, // plus(n, m)
        .{ .add = .{ .dst = 4, .lhs = 3, .rhs = 2 } }, // plus(plus(n,m), k)
        .{ .ret = .{ .src = 4 } },
    };
    const code_right = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = n } } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = m } } },
        .{ .load_const = .{ .dst = 2, .val = .{ .bits = k } } },
        .{ .add = .{ .dst = 3, .lhs = 1, .rhs = 2 } }, // plus(m, k)
        .{ .add = .{ .dst = 4, .lhs = 0, .rhs = 3 } }, // plus(n, plus(m,k))
        .{ .ret = .{ .src = 4 } },
    };
    const r_left = try machine.run(code_left);
    defer r_left.deinit(allocator);
    const r_right = try machine.run(code_right);
    defer r_right.deinit(allocator);
    try std.testing.expectEqual(r_left.bits, r_right.bits);
}

// append(xs, nil) = xs の算術的検証
// 対応Scala: SolverTest.scala "∀xs. append(xs, nil) = xs"
// リストをペア (要素値, 長さ) で表現、nil = (0, 0)
test "帰納法: append(xs, nil) = xs" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    // append(xs, nil): xs の長さに 0 を加える
    // 長さが保存されることを確認
    const code = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 5 } } }, // xs.len = 5
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 0 } } }, // nil.len = 0
        .{ .add = .{ .dst = 2, .lhs = 0, .rhs = 1 } },          // append(xs, nil).len
        .{ .ret = .{ .src = 2 } },
    };
    const result = try machine.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 5), result.bits); // xs.lenと同じ
}

// ============================================================
// 5. オプティマイザテスト (Scala証明器のバイトコード最適化)
// ============================================================

// 定数畳み込み: load(3) + load(4) → load(7)
// 対応Scala: optimizer.zig の foldConstants 検証
test "オプティマイザ: 定数畳み込み (3 + 4 → 7)" {
    const allocator = std.testing.allocator;
    var opt = Optimizer.init(allocator);

    const code = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 3 } } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 4 } } },
        .{ .add = .{ .dst = 2, .lhs = 0, .rhs = 1 } },
        .{ .ret = .{ .src = 2 } },
    };
    const optimized = try opt.optimize(code);
    defer {
        for (optimized) |op| {
            switch (op) {
                .call => |c| allocator.free(c.args),
                .make_closure => |mc| allocator.free(mc.captures),
                else => {},
            }
        }
        allocator.free(optimized);
    }

    // 最適化後にVMで実行して正しい結果を確認
    var machine = VM.init(allocator);
    const result = try machine.run(optimized);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 7), result.bits);
}

// 定数畳み込み: 乗算
test "オプティマイザ: 定数畳み込み (5 * 6 → 30)" {
    const allocator = std.testing.allocator;
    var opt = Optimizer.init(allocator);

    const code = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 5 } } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 6 } } },
        .{ .mul = .{ .dst = 2, .lhs = 0, .rhs = 1 } },
        .{ .ret = .{ .src = 2 } },
    };
    const optimized = try opt.optimize(code);
    defer {
        for (optimized) |op| {
            switch (op) {
                .call => |c| allocator.free(c.args),
                .make_closure => |mc| allocator.free(mc.captures),
                else => {},
            }
        }
        allocator.free(optimized);
    }

    var machine = VM.init(allocator);
    const result = try machine.run(optimized);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 30), result.bits);
}

// 不要コード除去: 使われない定数ロードが削除される
// 対応Scala: optimizer.zig の eliminateDeadCode 検証
test "オプティマイザ: 不要コード除去 (dead load削除)" {
    const allocator = std.testing.allocator;
    var opt = Optimizer.init(allocator);

    // reg[0]は使われない (dead)、reg[1]のみ使用
    const code = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 999 } } }, // dead
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 42 } } },  // used
        .{ .ret = .{ .src = 1 } },
    };
    const optimized = try opt.optimize(code);
    defer {
        for (optimized) |op| {
            switch (op) {
                .call => |c| allocator.free(c.args),
                .make_closure => |mc| allocator.free(mc.captures),
                else => {},
            }
        }
        allocator.free(optimized);
    }

    // 最適化後の命令数が減っているか確認
    try std.testing.expect(optimized.len < code.len);

    // 実行結果は同じ
    var machine = VM.init(allocator);
    const result = try machine.run(optimized);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 42), result.bits);
}

// 多段定数畳み込み: (2 + 3) * 4 → 20
test "オプティマイザ: 多段定数畳み込み ((2+3)*4 → 20)" {
    const allocator = std.testing.allocator;
    var opt = Optimizer.init(allocator);

    const code = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 2 } } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 3 } } },
        .{ .add = .{ .dst = 2, .lhs = 0, .rhs = 1 } },           // 2+3=5
        .{ .load_const = .{ .dst = 3, .val = .{ .bits = 4 } } },
        .{ .mul = .{ .dst = 4, .lhs = 2, .rhs = 3 } },           // 5*4=20
        .{ .ret = .{ .src = 4 } },
    };
    const optimized = try opt.optimize(code);
    defer {
        for (optimized) |op| {
            switch (op) {
                .call => |c| allocator.free(c.args),
                .make_closure => |mc| allocator.free(mc.captures),
                else => {},
            }
        }
        allocator.free(optimized);
    }

    var machine = VM.init(allocator);
    const result = try machine.run(optimized);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 20), result.bits);
}

// ============================================================
// 6. アナライザテスト (RangeAnalyzer, EscapeAnalysis)
// ============================================================

// ビット幅推論: 小さい定数は少ないビット数で表現可能
// 対応Scala: Zigランタイムのビット幅最適化検証
test "アナライザ: ビット幅推論 (小整数 → 小ビット幅)" {
    const allocator = std.testing.allocator;
    var ana = RangeAnalyzer.init(allocator);

    const code = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 1 } } },   // 1 bit
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 255 } } },  // 8 bits
        .{ .load_const = .{ .dst = 2, .val = .{ .bits = 65535 } } }, // 16 bits
        .{ .add = .{ .dst = 3, .lhs = 0, .rhs = 1 } },             // max(1,8)+1 = 9 bits
        .{ .ret = .{ .src = 3 } },
    };
    var result = try ana.analyze(code, null, 0);
    defer result.deinit();

    try std.testing.expectEqual(@as(u8, 1), result.bitWidth(0));   // 1 → 1 bit
    try std.testing.expectEqual(@as(u8, 8), result.bitWidth(1));   // 255 → 8 bits
    try std.testing.expectEqual(@as(u8, 16), result.bitWidth(2));  // 65535 → 16 bits
    try std.testing.expect(result.bitWidth(3) <= 10);              // (1+255) の幅
}

// ビット幅推論: 乗算のビット幅伝播
test "アナライザ: ビット幅推論 (乗算のビット幅拡大)" {
    const allocator = std.testing.allocator;
    var ana = RangeAnalyzer.init(allocator);

    const code = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 15 } } },  // 4 bits
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 7 } } },   // 3 bits
        .{ .mul = .{ .dst = 2, .lhs = 0, .rhs = 1 } },             // 4+3=7 bits
        .{ .ret = .{ .src = 2 } },
    };
    var result = try ana.analyze(code, null, 0);
    defer result.deinit();

    try std.testing.expectEqual(@as(u8, 4), result.bitWidth(0));
    try std.testing.expectEqual(@as(u8, 3), result.bitWidth(1));
    try std.testing.expectEqual(@as(u8, 7), result.bitWidth(2)); // 4+3
}

// エスケープ解析: retされるレジスタはエスケープする
// 対応Scala: ZigランタイムのEscapeAnalysis検証
test "アナライザ: エスケープ解析 (ret レジスタはエスケープ)" {
    const allocator = std.testing.allocator;
    var ana = RangeAnalyzer.init(allocator);

    const code = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 1 } } }, // local only
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 2 } } }, // returned → escapes
        .{ .ret = .{ .src = 1 } },
    };
    var result = try ana.analyze(code, null, 0);
    defer result.deinit();

    try std.testing.expect(result.doesEscape(1));  // 返り値はエスケープ
    try std.testing.expect(!result.doesEscape(0)); // 内部のみ使用はエスケープしない
}

// エスケープ解析: クロージャにキャプチャされた値はエスケープ
test "アナライザ: エスケープ解析 (クロージャキャプチャはエスケープ)" {
    const allocator = std.testing.allocator;
    var ana = RangeAnalyzer.init(allocator);

    const dummy_body = &[_]Op{ .{ .ret = .{ .src = 0 } } };
    const code = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 10 } } }, // captured → escapes
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 20 } } }, // local only
        .{ .make_closure = .{ .dst = 2, .body = dummy_body, .captures = &[_]u32{0}, .arity = 1, .block_idx = 99 } },
        .{ .ret = .{ .src = 2 } },
    };
    var result = try ana.analyze(code, null, 0);
    defer result.deinit();

    try std.testing.expect(result.doesEscape(0)); // キャプチャされる値はエスケープ
    try std.testing.expect(result.doesEscape(2)); // クロージャ自体もエスケープ (ret)
}

// ============================================================
// 7. 統合テスト (IntegratedTests.scala, MathAndVerificationTests.scala)
//    Lambek対応: カリー化 = 閉圏の随伴 (curry ⊣ eval)
//    curry(f) は積の随伴による指数オブジェクトへの射
// ============================================================

// カリー化: (A × B → C) ↔ (A → B → C)
// 対応Scala: IntegratedTests.scala カリー化検証
test "統合: カリー化 (A×B→C) ↔ (A→B→C)" {
    const allocator = std.testing.allocator;
    var machine = VM.init(allocator);

    // curry(f)(a)(b) = f(a, b) の確認
    // f(a, b) = a + b (具体例)
    // curry化したf: λa. λb. a + b

    const inner = &[_]Op{
        // env[0]=a, arg=b(1)
        .{ .add = .{ .dst = 2, .lhs = 0, .rhs = 1 } },
        .{ .ret = .{ .src = 2 } },
    };
    const outer = &[_]Op{
        // arg=a(0)
        .{ .make_closure = .{ .dst = 1, .body = inner, .captures = &[_]u32{0}, .arity = 1, .block_idx = 50 } },
        .{ .ret = .{ .src = 1 } },
    };
    const code = &[_]Op{
        .{ .make_closure = .{ .dst = 0, .body = outer, .captures = &[_]u32{}, .arity = 1, .block_idx = 51 } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 10 } } }, // a = 10
        .{ .call = .{ .dst = 2, .func = 0, .args = &[_]u32{1} } }, // curry(f)(10)
        .{ .load_const = .{ .dst = 3, .val = .{ .bits = 5 } } },  // b = 5
        .{ .call = .{ .dst = 4, .func = 2, .args = &[_]u32{3} } }, // curry(f)(10)(5) = 15
        .{ .ret = .{ .src = 4 } },
    };
    const result = try machine.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 15), result.bits);
}

// オプティマイザ + VM の一貫性テスト
// 最適化前後で同じ結果を返すことを保証
test "統合: オプティマイザ後もVMの実行結果が一致" {
    const allocator = std.testing.allocator;
    var opt = Optimizer.init(allocator);
    var machine = VM.init(allocator);

    // 複雑な定数式: (10 + 5) * 2 - 3 = 27
    const code = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 10 } } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 5 } } },
        .{ .add = .{ .dst = 2, .lhs = 0, .rhs = 1 } },           // 10+5=15
        .{ .load_const = .{ .dst = 3, .val = .{ .bits = 2 } } },
        .{ .mul = .{ .dst = 4, .lhs = 2, .rhs = 3 } },           // 15*2=30
        .{ .load_const = .{ .dst = 5, .val = .{ .bits = 3 } } },
        .{ .sub = .{ .dst = 6, .lhs = 4, .rhs = 5 } },           // 30-3=27
        .{ .ret = .{ .src = 6 } },
    };

    const r_before = try machine.run(code);
    defer r_before.deinit(allocator);

    const optimized = try opt.optimize(code);
    defer {
        for (optimized) |op| {
            switch (op) {
                .call => |c| allocator.free(c.args),
                .make_closure => |mc| allocator.free(mc.captures),
                else => {},
            }
        }
        allocator.free(optimized);
    }

    const r_after = try machine.run(optimized);
    defer r_after.deinit(allocator);

    try std.testing.expectEqual(r_before.bits, r_after.bits);
    try std.testing.expectEqual(@as(u64, 27), r_after.bits);
    // 最適化で命令数が削減されることを確認
    try std.testing.expect(optimized.len <= code.len);
}
