from .desugar import Desugarer

from yarpen.assignment_elimination import AssignmentElimination
from yarpen.closure_conversion import ClosureConverter


class CodeTransformer(object):

    def __init__(self, global_variables):
        self._transformations = []
        self._register_transformation(Desugarer())
        self.global_variables = global_variables
        self._register_transformation(AssignmentElimination())
        self._register_transformation(ClosureConverter(global_variables))

    def transform(self, expr):
        for t in self._transformations:
            expr = t.transform(expr)
        return expr

    def _register_transformation(self, transformation):
        self._transformations.append(transformation)
