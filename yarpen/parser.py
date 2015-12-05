
from .expression import YarpenList, YarpenNumber, YarpenQuoted, YarpenBoolean
from .expression import YarpenSymbol, YarpenCharacter
from .tokens import LParen, RParen, Quote, Boolean, Symbol, Number
from .tokens import NoTokens, Character
from .tokenizer import Tokenizer


class Parser:
    def __init__(self, source):
        self._tokenizer = Tokenizer(source)

    def parse(self):
        exprs = []
        while not isinstance(self._tokenizer.peek(), NoTokens):
            exprs.append(self._parse_expression())
        return exprs

    def _parse_expression(self):
        if self._is_list():
            return self._parse_list()
        elif self._is_number():
            return self._parse_number()
        elif self._is_symbol():
            return self._parse_symbol()
        elif self._is_boolean():
            return self._parse_boolean()
        elif self._is_quote():
            return self._parse_quoted()
        elif self._is_character():
            return self._parse_character()
        else:
            raise Exception("Unknow token %s" % self._get_token())

    def _parse_quoted(self):
        self._consume_quote()  # consume '
        exp = self._parse_expression()
        return YarpenQuoted(exp)

    def _parse_boolean(self):
        b = self._consume_bool().bool
        res = False
        if b == "#t":
            res = True
        return YarpenBoolean(res)

    def _parse_number(self):
        token = self._consume_number().number
        number = 0
        try:
            number = int(token)
        except ValueError:
            number = float(token)
        return YarpenNumber(number)

    def _parse_symbol(self):
        return YarpenSymbol(self._consume_symbol().symbol)

    def _parse_list(self):
        self._consume_lparen()
        exprs = []
        while not self._is_list_end():
            exprs.append(self._parse_expression())
        self._consume_rparen()
        return YarpenList(exprs)

    def _parse_character(self):
        c = self._consume_current_token(Character).character[2:]
        if c == 'newline':
            c = "\n"
        elif c == 'space':
            c = ' '
        return YarpenCharacter(c)

    def _get_token(self):
        return self._tokenizer.peek()

    def _consume_lparen(self):
        return self._consume_current_token(LParen)

    def _consume_rparen(self):
        return self._consume_current_token(RParen)

    def _consume_symbol(self):
        return self._consume_current_token(Symbol)

    def _consume_number(self):
        return self._consume_current_token(Number)

    def _consume_quote(self):
        return self._consume_current_token(Quote)

    def _consume_bool(self):
        return self._consume_current_token(Boolean)

    def _consume_current_token(self, token_type):
        token = self._tokenizer.peek()
        assert(isinstance(token, token_type))
        self._tokenizer.next()
        return token

    def _is_boolean(self):
        return self._is_token_type(Boolean)

    def _is_number(self):
        return self._is_token_type(Number)

    def _is_list(self):
        return self._is_token_type(LParen)

    def _is_symbol(self):
        return self._is_token_type(Symbol)

    def _is_quote(self):
        return self._is_token_type(Quote)

    def _is_list_end(self):
        return self._is_token_type(RParen)

    def _is_character(self):
        return self._is_token_type(Character)

    def _is_token_type(self, token_type):
        return isinstance(self._get_token(), token_type)
