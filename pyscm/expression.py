class PyScmList(object):
    def __init__(self, exprs):
        self.expressions = exprs

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        return self.expressions == other.expressions

    def __repr__(self):
        return self.expressions.__repr__()


class PyScmQuoted(object):
    def __init__(self, datum):
        self.datum = datum

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        return self.datum == other.datum

    def __repr__(self):
        return self.datum.__repr__()


class PyScmNumber(object):
    def __init__(self, number):
        self.number = number

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        return self.number == other.number

    def __repr__(self):
        return str(self.number)


class PyScmSymbol(object):
    def __init__(self, sym):
        self.symbol = sym

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        return self.symbol == other.symbol

    def __repr__(self):
        return self.symbol


class PyScmBoolean(object):
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


class PyScmClosure(object):
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


class PyScmFreeVarRef(object):
    def __init__(self, free_var):
        self.symbol = free_var

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        return self.symbol == other.symbol

    def __repr__(self):
        return "FreeVarReference %s" % self.symbol


def is_number(expr):
    return isinstance(expr, PyScmNumber)


def is_boolean(expr):
    return isinstance(expr, PyScmBoolean)


def is_lambda(expr):
    return is_tagged_list(expr, PyScmSymbol("lambda"))


def lambda_args(expr):
    return expr.expressions[1].expressions


def lambda_body(expr):
    return expr.expressions[2]


def make_lambda(args, body):
    return PyScmList([PyScmSymbol("lambda"), PyScmList(args), body])


def is_application(expression):
    return isinstance(expression, PyScmList)


def is_variable(expr):
    return type(expr) == PyScmSymbol


def is_if(expr):
    return is_tagged_list(expr, PyScmSymbol("if"))


def make_if(cond, cons, alter):
    return PyScmList([PyScmSymbol("if"), cond, cons, alter])


def is_let(expr):
    return is_tagged_list(expr, PyScmSymbol("let"))


def let_bindings(expr):
    assert(type(expr.expressions[1]) == PyScmList)
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
    return is_tagged_list(expr, PyScmSymbol("closure"))


def is_tagged_list(expr, tag):
    return (isinstance(expr, PyScmList) and len(expr.expressions) > 0
            and expr.expressions[0] == tag)
