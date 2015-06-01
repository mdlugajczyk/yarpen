from unittest import TestCase
from pyscm.parser import Parser
from pyscm.expression import PyScmList, PyScmNumber, PyScmSymbol, make_lambda
from pyscm.desugar import desugar


class DesugarTest(TestCase):

    def test_desugar_empty_let(self):
        expr = Parser("(let () 3)").parse()[0]
        desugared_exp = desugar(expr)
        self.assertEqual(desugared_exp,
                         PyScmList([make_lambda([], PyScmNumber(3))]))

    def test_desugar_let_with_bindings(self):
        expr = Parser("(let ((x 3) (y 4)) (+ x y))").parse()[0]
        desugared_exp = desugar(expr)
        self.assertEqual(desugared_exp,
                         PyScmList([make_lambda([PyScmSymbol("x"),
                                                 PyScmSymbol("y")],
                                                PyScmList([PyScmSymbol("+"),
                                                           PyScmSymbol("x"),
                                                           PyScmSymbol("y")
                                                           ])),
                                    PyScmNumber(3), PyScmNumber(4)]))
