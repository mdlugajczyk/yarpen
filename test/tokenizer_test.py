from pyscm.tokenizer import Tokenizer
import unittest


class TokenizerTest(unittest.TestCase):

    def test_tokenizes_empty_input(self):
        t = Tokenizer("   ")
        self.assertEqual(t.peek(), None)

    def test_tokenizes_symbols(self):
        t = Tokenizer(" -asdf*   foo-!@$bar")
        self.assertEqual(t.peek(), ("-asdf*", "SYMBOL"))
        self.assertEqual(t.next(), ("foo-!@$bar", "SYMBOL"))

    def test_tokenizes_parenthesis(self):
        t = Tokenizer(" ( foo (bar   )   fnord )")
        self.assertEqual(t.peek(), ("(", "LPAREN"))
        self.assertEqual(t.next(), ("foo", "SYMBOL"))
        self.assertEqual(t.next(), ("(", "LPAREN"))
        self.assertEqual(t.next(), ("bar", "SYMBOL"))
        self.assertEqual(t.next(), (")", "RPAREN"))
        self.assertEqual(t.next(), ("fnord", "SYMBOL"))
        self.assertEqual(t.next(), (")", "RPAREN"))

    def test_tokenizes_numbers(self):
        t = Tokenizer("-10  3.459 10e-3 10e34")
        self.assertEqual(t.peek(), ("-10", "NUMBER"))
        self.assertEqual(t.next(), ("3.459", "NUMBER"))
        self.assertEqual(t.next(), ("10e-3", "NUMBER"))
        self.assertEqual(t.next(), ("10e34", "NUMBER"))

    def test_tokenizes_hash_symbol(self):
        t = Tokenizer("foo #(bar))")
        self.assertEqual(t.peek(), ("foo", "SYMBOL"))
        self.assertEqual(t.next(), ("#", "HASH"))
        self.assertEqual(t.next(), ("(", "LPAREN"))
        self.assertEqual(t.next(), ("bar", "SYMBOL"))
        self.assertEqual(t.next(), (")", "RPAREN"))
        self.assertEqual(t.next(), (")", "RPAREN"))

    def test_tokenizes_quote(self):
        t = Tokenizer("foo 'bar fnord")
        self.assertEqual(t.peek(), ("foo", "SYMBOL"))
        self.assertEqual(t.next(), ("'", "QUOTE"))
        self.assertEqual(t.next(), ("bar", "SYMBOL"))
        self.assertEqual(t.next(), ("fnord", "SYMBOL"))
