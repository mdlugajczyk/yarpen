class NoTokens:
    pass


class LParen:
    def __init__(self, _):
        pass


class RParen:
    def __init__(self, _):
        pass


class Quote:
    def __init__(self, _):
        pass


class Hash:
    def __init__(self, _):
        pass


class Symbol:
    def __init__(self, sym):
        self.symbol = sym


class Number:
    def __init__(self, number):
        self.number = number


class Boolean:
    def __init__(self, bool):
        self.bool = bool
