chcp 65001
$action = $args[0]

if ($action -eq "repl") {
    sbt "runMain romanesco.Solver.runRepl"
}
else {
    sbt `
        "runMain romanesco.Solver.testTactics" `
        "runMain romanesco.testSomeCases" `
        "runMain romanesco.Solver.SolverTests.HoTTTest" `
        "runMain romanesco.Solver.SolverTests.HoTTCubeTest" `
        "runMain romanesco.Solver.SolverTests.TypeLevelTest" `
        "runMain romanesco.Solver.SolverTests.LinearResourceTest" `
        "runMain romanesco.Solver.SolverTests.TemporalLogicTest" `
        "runMain romanesco.Solver.SolverTests.SeparationLogicTest"
}
