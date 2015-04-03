class PyScmList(object):
    def __init__(self, exprs):
        self.expressions = exprs

    def __repr__(self):
        return self.expressions.__repr__()


class PyScmQuoted(object):
    def __init__(self, datum):
        self.datum = datum

    def __repr__(self):
        return self.datum.__repr__()


class PyScmNumber(object):
    def __init__(self, number):
        self.number = number

    def __repr__(self):
        return str(self.number)


class PyScmSymbol(object):
    def __init__(self, sym):
        self.symbol = sym

    def __eq__(self, other):
        return self.symbol == other.symbol

    def __repr__(self):
        return self.symbol


class PyScmBoolean(object):
    def __init__(self, bool):
        self.bool = bool

    def __repr__(self):
        if self.bool:
            return '#t'
        else:
            return '#f'
