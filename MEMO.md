# 開発メモ: 構造的書き換えエンジン (Structural Rewriting Engine)

## 背景
初期のパーサー実装では、再帰下降法によるプレフィックス記法のパースのみをサポートしていた。しかし、`x = 1 + 2` のような自然な中置記法（Mixfix）をサポートするため、トークン列を動的に書き換えるエンジンが必要となった。

## アーキテクチャの進化

### 1. トークンベースの書き換え (初期案)
トークン列をフラットなリストとして扱い、パターンマッチングで置換する方式。
- **課題**: `[L] = [R]` のようなマクロにおいて、`R` がどこまでを指すかの境界が曖昧になり、無限ループや誤キャプチャが発生した。

### 2. 構造的書き換え (現在の実装)
トークン列を論理的な「ユニット（Unit）」に分割してからマッチングを行う方式。
- **ユニットの定義**: 
    - 括弧 `()` や `{}` で囲まれたバランスの取れたグループ
    - 演算子の項数（Arity）に基づいたプレフィックス適用
    - 単一の識別子や数値
- **利点**:
    - **境界の明確化**: マクロの引数が一つの「完成した式」に限定されるため、隣接する演算子を誤って飲み込むことがない。
    - **無限ループの防止**: キーワードがすでにプレフィックス位置にある場合の適用スキップロジックと組み合わせることで、安定した書き換え（固定点への到達）が可能になった。

## 評価器の非決定性
Python版およびScala版の両方で、評価結果を `List[(Value, Env)]` として返す非決定的評価を実装した。
- `or` 演算子による分岐の探索。
- `unify`（単一化）による制約の解消と環境へのバインディング。

## 実装のパリティ
Python版でのプロトタイピングとデバッグを経て、洗練されたロジックをScala版へ逆輸入（Backport）した。
- Python: `RewritingEngine`, `match_one_expression`
- Scala: `RewritingEngine`, `matchOneExpression`

### Implicit Bit-Width Inference (Z3-based)

The Romanesco runtime now employs a fully implicit, Z3-based bit-width inference system. Users no longer need to declare `BitVec` types or bit-widths manually.

#### Key Features:
- **Zero-Syntax Control**: Arithmetic operators (`+`, `-`, `*`, `/`) automatically trigger bit-width inference when applied to integer literals or variables.
- **Safety Guaranteed (Bit-growth)**:
  - `x + y`: Result width is `max(width(x), width(y)) + 1` to prevent overflow.
  - `x * y`: Result width is `width(x) + width(y)`.
- **Global Optimization**: Uses `z3.Optimize` to find the minimum global bit-widths that satisfy all constraints across the entire program.
- **Unification**: Operands of binary operations are unified to the same bit-width where possible, mimicking hardware signal alignment.
- **Implicit Casting**: Assignments (`=`) automatically cast values to the inferred width of the target variable.

#### Examples:
```romanesco
= x 15      // Inferred as 4-bit
= y 1       // Unified to 4-bit (due to addition with x)
= z + x y   // Inferred as 5-bit (max(4,4)+1)
z           // Result: 16 (Safe from overflow)
```
