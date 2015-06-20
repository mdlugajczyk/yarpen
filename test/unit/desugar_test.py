from __future__ import absolute_import
from unittest import TestCase
from yarpen.parser import Parser
from yarpen.expression import YarpenList, YarpenNumber, YarpenSymbol
from yarpen.expression import make_lambda, make_begin
from yarpen.desugar import desugar


class DesugarTest(TestCase):

    def test_desugar_empty_let(self):
        expr = Parser("(let () 3)").parse()[0]
        desugared_exp = desugar(expr)
        self.assertEqual(desugared_exp,
                         YarpenList([make_lambda([],
                                                 make_begin([YarpenNumber(3)]))]))

    def test_desugar_let_with_bindings(self):
        expr = Parser("(let ((x 3) (y 4)) (+ x y))").parse()[0]
        desugared_exp = desugar(expr)
        lambda_body = make_begin([YarpenList([YarpenSymbol("+"),
                                             YarpenSymbol("x"),
                                              YarpenSymbol("y")])])
        expected = YarpenList([make_lambda([YarpenSymbol("x"),
                                            YarpenSymbol("y")],
                                           lambda_body),
                               YarpenNumber(3), YarpenNumber(4)])
        self.assertEqual(desugared_exp, expected)

    def test_desugar_let_wtih_multiple_exprs_in_body(self):
        expr = Parser("(let () 1 2)").parse()[0]
        desugared_exp = desugar(expr)
        expected = YarpenList([make_lambda([],
                                           YarpenList([YarpenSymbol("begin"),
                                                       YarpenNumber(1),
                                                       YarpenNumber(2)]))])
        self.assertEqual(desugared_exp, expected)
