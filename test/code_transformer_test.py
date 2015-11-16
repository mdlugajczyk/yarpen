from unittest import TestCase
from yarpen.parser import Parser
from yarpen.code_transformer import CodeTransformer

from yarpen.expression import YarpenBoxedValue, YarpenClosure, YarpenFreeVarRef, \
    YarpenNumber, YarpenSymbol, make_assignment


class CodeTransformerTest(TestCase):

    def runs_all_transformations_test(self):
        expr = Parser("(lambda () (set! x 3))").parse()[0]
        expected = YarpenClosure(make_assignment(YarpenBoxedValue(YarpenFreeVarRef("x")),
                                                 YarpenNumber(3)),
                                 [YarpenSymbol("x")], [])
        self.assertEqual(CodeTransformer().transform(expr),
                         expected)
