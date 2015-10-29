from __future__ import absolute_import
from unittest import TestCase
from yarpen.parser import Parser
from yarpen.desugar import desugar
import unittest


class DesugarTest(TestCase):

    def test_desugar_empty_let(self):
        expr = Parser("(let () 3)").parse()[0]
        desugared_exp = desugar(expr)
        self.assertEqual(desugared_exp,
                         Parser("((lambda () (begin 3)))").parse()[0])

    def test_desugar_let_with_bindings(self):
        expr = Parser("(let ((x 3) (y 4)) (+ x y))").parse()[0]
        desugared_exp = desugar(expr)
        expected = Parser("((lambda (x y) (begin (+ x y))) 3 4)").parse()[0]
        self.assertEqual(desugared_exp, expected)

    def test_desugar_nested_let(self):
        expr = Parser("(let ((x 1)) (let ((y 2)) (+ x y)))").parse()[0]
        desugared_expr = desugar(expr)
        expected = Parser("((lambda (x) (begin ((lambda (y) (begin (+ x y))) 2))) 1)").parse()[0]
        self.assertEqual(desugared_expr, expected)

    def test_desugar_let_wtih_multiple_exprs_in_body(self):
        expr = Parser("(let () 1 2)").parse()[0]
        desugared_exp = desugar(expr)
        expected = Parser("((lambda () (begin 1 2)))").parse()[0]
        self.assertEqual(desugared_exp, expected)

    def test_desugar_let_start(self):
        expr = Parser("(let* ((x 1) (y (+ x 1))) y)").parse()[0]
        desugared_exp = desugar(expr)
        expected = desugar(Parser("(let ((x 1)) (let ((y (+ x 1))) y))").parse()[0])
        self.assertEqual(desugared_exp, expected)

    def test_desugar_letrec(self):
        expr = Parser("(letrec ((x 1) (y 2)) (+ x y))").parse()[0]
        expected = Parser("((lambda (x y) (begin (set! x 1) (set! y 2) (+ x y))) 0 0)").parse()[0]
        desugared = desugar(expr)
        self.assertEqual(desugared, expected)

if __name__ == '__main__':
    unittest.main()
