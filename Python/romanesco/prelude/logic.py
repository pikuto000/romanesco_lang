from ..syntax.ast import Atom

bindings = {
    "and": Atom("and"), 
    "or": Atom("or"), 
    "seq": Atom("seq"), 
    "=": Atom("="),
    "lambda": Atom("lambda"),
    "true": Atom("true"), 
    "false": Atom("false")
}
