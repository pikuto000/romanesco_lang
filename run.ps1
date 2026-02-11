chcp 65001
$action = $args[0]

if ($action -eq "repl") {
    sbt "runMain romanesco.Solver.runRepl"
}
else {
    sbt `
        "runMain romanesco.Solver.SolverTests.CycleTest" `
        "runMain romanesco.Solver.SolverTests.CycleTest2" `
        "runMain romanesco.Solver.SolverTests.DebugAssoc" `
        "runMain romanesco.Solver.SolverTests.DebugLinear" `
        "runMain romanesco.Solver.SolverTests.HoTTCubeTest" `
        "runMain romanesco.Solver.SolverTests.HoTTTest" `
        "runMain romanesco.Solver.SolverTests.LinearResourceTest" `
        "runMain romanesco.Solver.SolverTests.SeparationLogicTest" `
        "runMain romanesco.Solver.SolverTests.TemporalLogicTest" `
        "runMain romanesco.Solver.SolverTests.TypeLevelTest" `
        "runMain romanesco.Solver.testGeneralizedUnification" `
        "runMain romanesco.Solver.testLinearLogic" `
        "runMain romanesco.Solver.testMapFusion" `
        "runMain romanesco.Solver.testModalLogic" `
        "runMain romanesco.Solver.testPatternUnification" `
        "runMain romanesco.Solver.testTactics" `
        "runMain romanesco.Solver.testTreeInduction" `
        "runMain romanesco.Solver.SolverTests.RecursiveInductionTest" `
        "runMain romanesco.testSomeCases"
}
