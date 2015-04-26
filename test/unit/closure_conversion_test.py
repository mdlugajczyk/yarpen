from pyscm.closure_conversion import free_variables, closure_convert
from pyscm.expression import PyScmSymbol, PyScmNumber, PyScmBoolean
from pyscm.expression import PyScmClosure, PyScmFreeVarRef, PyScmList
from pyscm.parser import Parser
from unittest import TestCase


class ClosureConversionTest(TestCase):

    def test_free_variables_for_symbol(self):
        sym = PyScmSymbol("foo")
        free = free_variables(sym)
        self.assertEqual(free, [sym])

    def test_free_variables_in_lambda_without_args(self):
        expr = Parser("(lambda () x)").parse()[0]
        free = free_variables(expr)
        self.assertEqual(free, [PyScmSymbol("x")])

    def test_free_variables_in_lambda_with_args(self):
        expr = Parser("(lambda (y) x)").parse()[0]
        free = free_variables(expr)
        self.assertEqual(free, [PyScmSymbol("x")])

    def test_no_free_variables_in_lambda(self):
        expr = Parser("(lambda (x) x)").parse()[0]
        free = free_variables(expr)
        self.assertEqual(free, [])

    def test_free_variables_if_predicate(self):
        expr = Parser("(if x y z)").parse()[0]
        free = free_variables(expr)
        self.assertEqual(free, [PyScmSymbol("x"),
                                PyScmSymbol("y"),
                                PyScmSymbol("z")])

    def test_free_no_variables_if(self):
        expr = Parser("(if #t 2 3)").parse()[0]
        free = free_variables(expr)
        self.assertEqual(free, [])

    def test_free_variables_number(self):
        self.assertEqual(free_variables(PyScmNumber(3)), [])

    def test_free_variables_application(self):
        expr = Parser("(x y z)").parse()[0]
        free = free_variables(expr)
        self.assertEqual(free, [PyScmSymbol("x"),
                                PyScmSymbol("y"),
                                PyScmSymbol("z")])

    def test_closure_convert_number(self):
        num = PyScmNumber(3)
        self.assertEqual(num, closure_convert(num))

    def test_closure_convert_boolean(self):
        bool = PyScmBoolean(True)
        self.assertEqual(bool, closure_convert(bool))

    def test_closure_convert_symbol(self):
        sym = PyScmSymbol("foo")
        self.assertEqual(sym, closure_convert(sym))

    def test_closure_convert_lambda_without_variables(self):
        lambda_exp = Parser("(lambda () 3)").parse()[0]
        self.assertEqual(closure_convert(lambda_exp),
                         PyScmClosure(PyScmNumber(3), [], []))

    def test_closure_convert_lambda_without_free_variables(self):
        lambda_exp = Parser("(lambda (x) x)").parse()[0]
        self.assertEqual(closure_convert(lambda_exp),
                         PyScmClosure(PyScmSymbol("x"), [],
                                      [PyScmSymbol("x")]))

    def test_closure_convert_lambda_with_free_variables(self):
        lambda_exp = Parser("(lambda (x) (f x))").parse()[0]
        closure_converted_body = PyScmList([PyScmFreeVarRef("f"),
                                            PyScmSymbol("x")])
        self.assertEqual(closure_convert(lambda_exp),
                         PyScmClosure(closure_converted_body,
                                      [PyScmSymbol("f")],
                                      [PyScmSymbol("x")]))

    def test_closure_convert_nested_lambda(self):
        exp = "(lambda (y) (lambda (x) (f x ((lambda (f) f) y))))"
        lambda_exp = Parser(exp).parse()[0]
        most_inner_closure = PyScmClosure(PyScmSymbol("f"),
                                          [],
                                          [PyScmSymbol("f")])
        inner_closure_body = PyScmList([PyScmFreeVarRef("f"),
                                        PyScmSymbol("x"),
                                        PyScmList([most_inner_closure,
                                                   PyScmFreeVarRef("y")])])
        inner_closure = PyScmClosure(inner_closure_body,
                                     [PyScmSymbol("f"), PyScmSymbol("y")],
                                     [PyScmSymbol("x")])
        outer_closure = PyScmClosure(inner_closure,
                                     [PyScmSymbol("f")],
                                     [PyScmSymbol("y")])
        self.assertEqual(closure_convert(lambda_exp),
                         outer_closure)
