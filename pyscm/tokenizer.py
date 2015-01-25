import re


class Tokenizer(object):
    def __init__(self, source):
        extended_alphabetic_character = r"!|\$|%|&|\*|\+|-|\.|/|:|<|=|>|\?|@|\^|_|~"  # as per R5RS
        rules = [("\(", "LPAREN"),
                 ("\)", "RPAREN"),
                 ("'", "QUOTE"),
                 ("-??((\d+\.\d*)|(\d+.e-??\d+)|\d)+", 'NUMBER'),
                 ("#t|#f", 'BOOLEAN'),
                 ('#', 'HASH'),
                 ("({0}|[a-zA-Z]|\d)+".format(extended_alphabetic_character), 'SYMBOL')]
        self._rules = [(re.compile(regex), name) for regex, name in rules]
        self._source = source.strip()
        self._position = 0
        self._whitespace_regex = re.compile("\s+")

    def tokenize(self):
        tokens = []
        while self._has_input():
            tokens.append(self._next_token())
        return tokens

    def _next_token(self):
        self._skip_whitespace()
        for regex, name in self._rules:
            result = regex.match(self._source[self._position:])
            if result:
                token = self._consume_token(result)
                return token, name
        raise Exception("Error in input stream at position %s" % self._position)

    def _consume_token(self, match_result):
        token = self._source[self._position:self._position + match_result.end()]
        self._position += match_result.end()
        return token

    def _skip_whitespace(self):
        result = self._whitespace_regex.match(self._source[self._position:])
        if result:
            self._consume_token(result)

    def _has_input(self):
        return self._position < len(self._source)
