from __future__ import absolute_import

from unittest import TestCase
from yarpen.parser import Parser

from yarpen.assignment_elimination import AssignmentElimination
from yarpen.expression import YarpenBoxedValue, YarpenList, YarpenNumber, \
    YarpenSymbol, make_assignment, make_begin, make_if, make_lambda


class AssignmentEliminationTest(TestCase):

    def setUp(self):
        self.converter = AssignmentElimination()

    def expression_is_unmodified(self, expr):
        parsed_expr = Parser(expr).parse()[0]
        self.assertEqual(self.converter.transform(parsed_expr), parsed_expr)
    
    def lambda_without_assignment_test(self):
        self.expression_is_unmodified("(lambda (x) x)")

    def constant_test(self):
        self.expression_is_unmodified("13")

    def variable_test(self):
        self.expression_is_unmodified("x")

    def assignment_test(self):
        expr = Parser("(set! foo 3)").parse()[0]
        self.assertEqual(self.converter.transform(expr),
                         make_assignment(YarpenBoxedValue("foo"),
                                         YarpenNumber(3)))

    def lambda_with_mutable_var_test(self):
        expr = Parser("(lambda(x y) (set! x y))").parse()[0]
        expected = make_lambda([YarpenBoxedValue("x"), YarpenSymbol("y")],
                               make_assignment(YarpenBoxedValue("x"),
                                                YarpenSymbol("y")))
        self.assertEqual(self.converter.transform(expr), expected)

    def set_with_mutation_in_value_test(self):
        expr = Parser("(set! foo (set! x 3))").parse()[0]
        self.assertEqual(self.converter.transform(expr),
                         make_assignment(YarpenBoxedValue("foo"),
                                         make_assignment(YarpenBoxedValue("x"),
                                                         YarpenNumber(3))))

    def if_test(self):
        expr = Parser("(if (set! x 3) (set! y 4) (set! z 5))").parse()[0]
        self.assertEqual(self.converter.transform(expr),
                         make_if(make_assignment(YarpenBoxedValue("x"), YarpenNumber(3)),
                                 make_assignment(YarpenBoxedValue("y"), YarpenNumber(4)),
                                 make_assignment(YarpenBoxedValue("z"), YarpenNumber(5))))

    def application_test(self):
        expr = Parser("(foo (begin (set! x 3) x))").parse()[0]
        self.assertEqual(self.converter.transform(expr),
                         YarpenList([YarpenSymbol("foo"),
                                     make_begin([
                                         make_assignment(YarpenBoxedValue("x"),
                                                         YarpenNumber(3)),
                                         YarpenBoxedValue("x")])]))
                                     
