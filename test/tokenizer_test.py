from pyscm.tokenizer import Tokenizer

__author__ = 'Marcin Dlugajczyk'

import unittest


class TokenizerTest(unittest.TestCase):

    def test_tokenizes_empty_input(self):
        self.assertEqual(Tokenizer("   ").tokenize(), [])

    def test_tokenizes_symbols(self):
        self.assertEqual(Tokenizer(" -asdf*   foo-!@$bar").tokenize(), [("-asdf*", "SYMBOL"),
                                                                             ("foo-!@$bar", "SYMBOL")])

    def test_tokenizes_parenthesis(self):
        self.assertEqual(Tokenizer(" ( foo (bar   )   fnord )").tokenize(),
                         [("(", "LPAREN"), ("foo", "SYMBOL"), ("(", "LPAREN"),
                          ("bar", "SYMBOL"), (")", "RPAREN"), ("fnord", "SYMBOL"), (")", "RPAREN")])

    def test_tokenizes_numbers(self):
        self.assertEqual(Tokenizer("-10  3.459 10e-3 10e34").tokenize(),
                         [("-10", "NUMBER"), ("3.459", "NUMBER"), ("10e-3", "NUMBER"), ("10e34", "NUMBER")])

    def test_tokenizes_hash_symbol(self):
        self.assertEqual(Tokenizer("foo #(bar))").tokenize(),
                         [("foo", "SYMBOL"), ("#", "HASH"), ("(", "LPAREN"),
                          ("bar", "SYMBOL"), (")", "RPAREN"), (")", "RPAREN")])

    def test_tokenizes_quote(self):
        self.assertEqual(Tokenizer("foo 'bar fnord").tokenize(),
                         [("foo", "SYMBOL"), ("'", "QUOTE"), ("bar", "SYMBOL"), ("fnord", "SYMBOL")])
        
if __name__ == '__main__':
    unittest.main()
