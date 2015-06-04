class YarpenList(object):
    def __init__(self, exprs):
        self.expressions = exprs

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        return self.expressions == other.expressions

    def __repr__(self):
        return self.expressions.__repr__()


class YarpenQuoted(object):
    def __init__(self, datum):
        self.datum = datum

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        return self.datum == other.datum

    def __repr__(self):
        return self.datum.__repr__()


class YarpenNumber(object):
    def __init__(self, number):
        self.number = number

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        return self.number == other.number

    def __repr__(self):
        return str(self.number)


class YarpenSymbol(object):
    def __init__(self, sym):
        self.symbol = sym

    def __eq__(self, other):
        #return (isinstance(other, YarpenFreeVarRef) or isinstance(other, YarpenSymbol)) and other.symbol == self.symbol
        if not isinstance(other, self.__class__):
            return False
        return self.symbol == other.symbol

    def __repr__(self):
        return "VAR " + self.symbol 

    def __hash__(self):
        return hash(self.symbol)


class YarpenBoolean(object):
    def __init__(self, bool):
        self.bool = bool

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        return self.bool == other.bool

    def __repr__(self):
        if self.bool:
            return '#t'
        else:
            return '#f'


class YarpenClosure(object):
    def __init__(self, body, free_variables, parameters):
        self.body = body
        self.free_variables = free_variables
        self.parameters = parameters

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        return (self.body == other.body
                and self.free_variables == other.free_variables
                and self.parameters == other.parameters)

    def __repr__(self):
        return "Closure: (%s) (%s) (%s)" % (self.body, self.free_variables,
                                            self.parameters)


class YarpenFreeVarRef(object):
    def __init__(self, free_var):
        self.symbol = free_var

    def __eq__(self, other):
        #return (isinstance(other, YarpenFreeVarRef) or isinstance(other, YarpenSymbol)) and other.symbol == self.symbol
        if not isinstance(other, self.__class__):
            return False
        return self.symbol == other.symbol

    def __repr__(self):
        return "FreeVarReference %s" % self.symbol

    def __hash__(self):
        return hash(self.symbol)


def is_number(expr):
    return isinstance(expr, YarpenNumber)


def is_boolean(expr):
    return isinstance(expr, YarpenBoolean)


def is_lambda(expr):
    return is_tagged_list(expr, YarpenSymbol("lambda"))


def lambda_args(expr):
    return expr.expressions[1].expressions


def lambda_body(expr):
    return expr.expressions[2]


def make_lambda(args, body):
    return YarpenList([YarpenSymbol("lambda"), YarpenList(args), body])


def is_application(expression):
    return isinstance(expression, YarpenList)


def is_variable(expr):
    return type(expr) == YarpenSymbol


def is_free_var_reference(expr):
    return isinstance(expr, YarpenFreeVarRef)

def is_if(expr):
    return is_tagged_list(expr, YarpenSymbol("if"))


def make_if(cond, cons, alter):
    return YarpenList([YarpenSymbol("if"), cond, cons, alter])


def is_let(expr):
    return is_tagged_list(expr, YarpenSymbol("let"))


def let_bindings(expr):
    assert(type(expr.expressions[1]) == YarpenList)
    return expr.expressions[1].expressions


def let_body(expr):
    return expr.expressions[2]


def if_condition(expr):
        return expr.expressions[1]


def if_conseq(expr):
        return expr.expressions[2]


def if_alternative(expr):
        return expr.expressions[3]


def is_closure(expr):
    return isinstance(expr, YarpenClosure)


def is_tagged_list(expr, tag):
    return (isinstance(expr, YarpenList) and len(expr.expressions) > 0
            and expr.expressions[0] == tag)