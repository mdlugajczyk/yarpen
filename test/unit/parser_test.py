from yarpen.parser import Parser
import unittest


class ParserTest(unittest.TestCase):
    def test_parse_no_input(self):
        p = Parser("")
        self.assertEqual(p.parse(), [])

    def test_parse_symbol(self):
        p = Parser("foo-bar")
        self.assertEqual(p.parse()[0].symbol, "foo-bar")

    def test_parse_number(self):
        p = Parser("123.34")
        self.assertEqual(p.parse()[0].number, 123.34)

    def test_parse_list(self):
        p = Parser("(foo bar quux (fnord 123))")
        [result] = p.parse()
        exprs = result.expressions
        self.assertEqual(len(exprs), 4)
        self.assertEqual(exprs[0].symbol, "foo")
        self.assertEqual(exprs[1].symbol, "bar")
        self.assertEqual(exprs[2].symbol, "quux")
        self.assertEqual(exprs[3].expressions[0].symbol, "fnord")
        self.assertEqual(exprs[3].expressions[1].number, 123)

    def test_parse_boolean(self):
        p = Parser("#t")
        [exp] = p.parse()
        self.assertEqual(exp.bool, True)

    def test_parse_quoted_symbol(self):
        p = Parser("'foo")
        [exp] = p.parse()
        self.assertEqual(exp.datum.symbol, "foo")

    def test_parse_quoted_list(self):
        p = Parser("'(foo bar baz)")
        [exp] = p.parse()
        self.assertEqual(len(exp.datum.expressions), 3)
        self.assertEqual(exp.datum.expressions[0].symbol, "foo")
        self.assertEqual(exp.datum.expressions[1].symbol, "bar")
        self.assertEqual(exp.datum.expressions[2].symbol, "baz")

    def test_parse_multiple_expressions(self):
        p = Parser("'foo bar (baz)")
        [exp1, exp2, exp3] = p.parse()
        self.assertEqual(exp1.datum.symbol, "foo")
        self.assertEqual(exp2.symbol, "bar")
        self.assertEqual(exp3.expressions[0].symbol, "baz")

if __name__ == '__main__':
    unittest.main()
