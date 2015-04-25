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
        self.free_var = free_var

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        return self.free_var == other.free_var

    def __repr__(self):
        return "FreeVarReference %s" % self.free_var
