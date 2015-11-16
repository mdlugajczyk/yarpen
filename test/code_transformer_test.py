from unittest import TestCase
from yarpen.parser import Parser

from yarpen.code_transformer import CodeTransformer
from yarpen.expression import YarpenBoxedValue, YarpenClosure, YarpenFreeVarRef, \
    YarpenList, YarpenNumber, YarpenSymbol, make_assignment, make_begin


class CodeTransformerTest(TestCase):

    def runs_all_test(self):
        expr = Parser("(let () (set! x 3))").parse()[0]
        expected = YarpenList([YarpenClosure(make_begin([make_assignment(YarpenBoxedValue(YarpenFreeVarRef("x")),
                                                                         YarpenNumber(3))]),
                                             [YarpenSymbol("x")], [])])
        self.assertEqual(CodeTransformer([]).transform(expr),
                         expected)
