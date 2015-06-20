from __future__ import absolute_import
from yarpen.closure_conversion import ClosureConverter
from yarpen.expression import YarpenSymbol, YarpenNumber, YarpenBoolean
from yarpen.expression import YarpenClosure, YarpenFreeVarRef, YarpenList
from yarpen.expression import make_begin
from yarpen.parser import Parser
from unittest import TestCase


class ClosureConversionTest(TestCase):

    def setUp(self):
        self.converter = ClosureConverter([YarpenSymbol("fx+")])

    def test_free_variables_for_symbol(self):
        sym = YarpenSymbol("foo")
        free = self.converter.free_variables(sym)
        self.assertEqual(free, [sym])

    def test_free_variables_in_lambda_without_args(self):
        expr = Parser("(lambda () x)").parse()[0]
        free = self.converter.free_variables(expr)
        self.assertEqual(free, [YarpenSymbol("x")])

    def test_free_variables_in_lambda_with_args(self):
        expr = Parser("(lambda (y) x)").parse()[0]
        free = self.converter.free_variables(expr)
        self.assertEqual(free, [YarpenSymbol("x")])

    def test_no_free_variables_in_lambda(self):
        expr = Parser("(lambda (x) x)").parse()[0]
        free = self.converter.free_variables(expr)
        self.assertEqual(free, [])

    def test_free_variables_if_predicate(self):
        expr = Parser("(if x y z)").parse()[0]
        free = self.converter.free_variables(expr)
        self.assertEqual(free, [YarpenSymbol("x"),
                                YarpenSymbol("y"),
                                YarpenSymbol("z")])

    def test_free_no_variables_if(self):
        expr = Parser("(if #t 2 3)").parse()[0]
        free = self.converter.free_variables(expr)
        self.assertEqual(free, [])

    def test_free_variables_number(self):
        self.assertEqual(self.converter.free_variables(YarpenNumber(3)), [])

    def test_free_variables_application(self):
        expr = Parser("(x y z)").parse()[0]
        free = self.converter.free_variables(expr)
        self.assertEqual(free, [YarpenSymbol("x"),
                                YarpenSymbol("y"),
                                YarpenSymbol("z")])

    def test_free_variables_globally_defined_symbols(self):
        expr = Parser("(fx+ x 3)").parse()[0]
        free = self.converter.free_variables(expr)
        self.assertEqual(free, [YarpenSymbol("x")])

    def test_closure_convert_number(self):
        num = YarpenNumber(3)
        self.assertEqual(num, self.converter.closure_convert(num))

    def test_closure_convert_boolean(self):
        bool = YarpenBoolean(True)
        self.assertEqual(bool, self.converter.closure_convert(bool))

    def test_closure_convert_symbol(self):
        sym = YarpenSymbol("foo")
        self.assertEqual(sym, self.converter.closure_convert(sym))

    def test_closure_convert_lambda_without_variables(self):
        lambda_exp = Parser("(lambda () 3)").parse()[0]
        self.assertEqual(self.converter.closure_convert(lambda_exp),
                         YarpenClosure(make_begin([YarpenNumber(3)]), [], []))

    def test_closure_convert_lambda_without_free_variables(self):
        lambda_exp = Parser("(lambda (x) x)").parse()[0]
        self.assertEqual(self.converter.closure_convert(lambda_exp),
                         YarpenClosure(make_begin([YarpenSymbol("x")]), [],
                                       [YarpenSymbol("x")]))

    def test_closure_convert_lambda_with_variables(self):
        lambda_exp = Parser("(lambda (x) (f x))").parse()[0]
        closure_converted_body = YarpenList([YarpenFreeVarRef("f"),
                                            YarpenSymbol("x")])
        self.assertEqual(self.converter.closure_convert(lambda_exp),
                         YarpenClosure(make_begin([closure_converted_body]),
                                       [YarpenSymbol("f")],
                                       [YarpenSymbol("x")]))

    def test_closure_convert_nested_lambda(self):
        exp = "(lambda (y) (lambda (x) (f x ((lambda (f) f) y))))"
        lambda_exp = Parser(exp).parse()[0]
        most_inner_closure = YarpenClosure(make_begin([YarpenSymbol("f")]),
                                           [],
                                           [YarpenSymbol("f")])
        inner_closure_body = make_begin([YarpenList([YarpenFreeVarRef("f"),
                                        YarpenSymbol("x"),
                                        YarpenList([most_inner_closure,
                                                   YarpenFreeVarRef("y")])])])
        inner_closure = YarpenClosure(inner_closure_body,
                                      [YarpenSymbol("f"), YarpenSymbol("y")],
                                      [YarpenSymbol("x")])
        outer_closure = YarpenClosure(make_begin([inner_closure]),
                                      [YarpenSymbol("f")],
                                      [YarpenSymbol("y")])
        self.assertEqual(self.converter.closure_convert(lambda_exp),
                         outer_closure)
