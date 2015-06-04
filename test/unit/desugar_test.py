from unittest import TestCase
from yarpen.parser import Parser
from yarpen.expression import YarpenList, YarpenNumber, YarpenSymbol
from yarpen.expression import make_lambda
from yarpen.desugar import desugar


class DesugarTest(TestCase):

    def test_desugar_empty_let(self):
        expr = Parser("(let () 3)").parse()[0]
        desugared_exp = desugar(expr)
        self.assertEqual(desugared_exp,
                         YarpenList([make_lambda([], YarpenNumber(3))]))

    def test_desugar_let_with_bindings(self):
        expr = Parser("(let ((x 3) (y 4)) (+ x y))").parse()[0]
        desugared_exp = desugar(expr)
        self.assertEqual(desugared_exp,
                         YarpenList([make_lambda([YarpenSymbol("x"),
                                                 YarpenSymbol("y")],
                                                 YarpenList([YarpenSymbol("+"),
                                                             YarpenSymbol("x"),
                                                             YarpenSymbol("y")
                                                             ])),
                                     YarpenNumber(3), YarpenNumber(4)]))
