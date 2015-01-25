class PyScmList(object):
    def __init__(self, exprs):
        self.expressions = exprs


class PyScmQuoted(object):
    def __init__(self, datum):
        self.datum = datum


class PyScmNumber(object):
    def __init__(self, number):
        self.number = number


class PyScmSymbol(object):
    def __init__(self, sym):
        self.symbol = sym


class PyScmBoolean(object):
    def __init__(self, bool):
        self.bool = bool
