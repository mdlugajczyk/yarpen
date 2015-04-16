from pyscm.compiler import Compiler
from pyscm.expression import PyScmSymbol, PyScmNumber
from pyscm.parser import Parser
from unittest import TestCase


class CompilerTest(TestCase):

    def setUp(self):
        self.compiler = Compiler("")

    def test_free_variables_for_symbol(self):
        sym = PyScmSymbol("foo")
        free = self.compiler.free_variables(sym)
        self.assertEqual(free, [sym])

    def test_free_variables_in_lambda_without_args(self):
        expr = Parser("(lambda () x)").parse()[0]
        free = self.compiler.free_variables(expr)
        self.assertEqual(free, [PyScmSymbol("x")])

    def test_free_variables_in_lambda_with_args(self):
        expr = Parser("(lambda (y) x)").parse()[0]
        free = self.compiler.free_variables(expr)
        self.assertEqual(free, [PyScmSymbol("x")])

    def test_no_free_variables_in_lambda(self):
        expr = Parser("(lambda (x y) x y)").parse()[0]
        free = self.compiler.free_variables(expr)
        self.assertEqual(free, [])

    def test_free_variables_if_predicate(self):
        expr = Parser("(if x y z)").parse()[0]
        free = self.compiler.free_variables(expr)
        self.assertEqual(free, [PyScmSymbol("x"),
                                PyScmSymbol("y"),
                                PyScmSymbol("z")])

    def test_free_no_variables_if(self):
        expr = Parser("(if #t 2 3)").parse()[0]
        free = self.compiler.free_variables(expr)
        self.assertEqual(free, [])

    def test_free_variables_number(self):
        self.assertEqual(self.compiler.free_variables(PyScmNumber(3)), [])

    def test_free_variables_application(self):
        expr = Parser("(x y z)").parse()[0]
        free = self.compiler.free_variables(expr)
        self.assertEqual(free, [PyScmSymbol("x"),
                                PyScmSymbol("y"),
                                PyScmSymbol("z")])
