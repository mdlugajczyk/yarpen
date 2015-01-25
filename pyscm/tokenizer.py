from tokens import LParen, RParen, Quote, Boolean, Hash, Symbol, Number
from tokens import NoTokens
import re


class Tokenizer(object):
    def __init__(self, source):
        # as per R5RS
        extended_alphabetic_character = r"""!|\$|%|&|\*|\+|-|\.|/|:
|<|=|>|\?|@|\^|_|~"""
        rules = [("\(", LParen),
                 ("\)", RParen),
                 ("'", Quote),
                 ("-??((\d+\.\d*)|(\d+.e-??\d+)|\d)+", Number),
                 ("#t|#f", Boolean),
                 ('#', Hash),
                 ("({0}|[a-zA-Z]|\d)+".format(extended_alphabetic_character),
                  Symbol)]
        self._rules = [(re.compile(regex), name) for regex, name in rules]
        self._source = source.strip()
        self._position = 0
        self._whitespace_regex = re.compile("\s+")
        self._current_token = NoTokens()
        self.next()

    def peek(self):
        return self._current_token

    def next(self):
        if self._has_input():
            return self._next_token()
        return NoTokens()

    def _next_token(self):
        self._skip_whitespace()
        for regex, name in self._rules:
            result = regex.match(self._source[self._position:])
            if result:
                token = self._consume_token(result)
                self._current_token = name(token)
                return self._current_token
        raise Exception("Error in input stream at position %s"
                        % self._position)

    def _consume_token(self, match_result):
        end = self._position + match_result.end()
        token = self._source[self._position:end]
        self._position += match_result.end()
        return token

    def _skip_whitespace(self):
        result = self._whitespace_regex.match(self._source[self._position:])
        if result:
            self._consume_token(result)

    def _has_input(self):
        return self._position < len(self._source)
