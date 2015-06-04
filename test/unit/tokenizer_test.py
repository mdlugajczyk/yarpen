from yarpen.tokens import LParen, RParen, Quote, Hash, NoTokens
from yarpen.tokenizer import Tokenizer
import unittest


class TokenizerTest(unittest.TestCase):

    def test_tokenizes_empty_input(self):
        t = Tokenizer("   ")
        self.assertTrue(isinstance(t.peek(), NoTokens))
        self.assertTrue(isinstance(t.next(), NoTokens))

    def test_tokenizes_symbols(self):
        t = Tokenizer(" -asdf*   foo-!@$bar")
        self.assertEqual(t.peek().symbol, "-asdf*")
        self.assertEqual(t.next().symbol, "foo-!@$bar")
        self.assertTrue(isinstance(t.next(), NoTokens))

    def test_tokenizes_parenthesis(self):
        t = Tokenizer(" ( foo (bar   )   fnord )")
        self.assertTrue(isinstance(t.peek(), LParen))
        self.assertEqual(t.next().symbol, "foo")
        self.assertTrue(isinstance(t.next(), LParen))
        self.assertEqual(t.next().symbol, "bar")
        self.assertTrue(isinstance(t.next(), RParen))
        self.assertEqual(t.next().symbol, "fnord")
        self.assertTrue(isinstance(t.next(), RParen))
        self.assertTrue(isinstance(t.next(), NoTokens))

    def test_tokenizes_numbers(self):
        t = Tokenizer("-10  3.459 10e-3 10e34")
        self.assertEqual(t.peek().number, "-10")
        self.assertEqual(t.next().number, "3.459")
        self.assertEqual(t.next().number, "10e-3")
        self.assertEqual(t.next().number, "10e34")

    def test_tokenizes_hash_symbol(self):
        t = Tokenizer("foo #(bar))")
        self.assertEqual(t.peek().symbol, "foo")
        self.assertTrue(isinstance(t.next(), Hash))
        self.assertTrue(isinstance(t.next(), LParen))
        self.assertEqual(t.next().symbol, "bar")
        self.assertTrue(isinstance(t.next(), RParen))
        self.assertTrue(isinstance(t.next(), RParen))

    def test_tokenizes_quote(self):
        t = Tokenizer("foo 'bar fnord")
        self.assertEqual(t.peek().symbol, "foo")
        self.assertTrue(isinstance(t.next(), Quote))
        self.assertEqual(t.next().symbol, "bar")
        self.assertEqual(t.next().symbol, "fnord")
