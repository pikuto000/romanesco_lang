chcp 65001
$action = $args[0]

if ($action -eq "repl") {
    sbt "runMain romanesco.Solver.runRepl"
} else {
    sbt "runMain romanesco.Solver.testTactics" "runMain romanesco.testSomeCases"
}
