
from .expression import YarpenFreeVarRef, YarpenList, YarpenClosure
from .expression import is_number, is_boolean, is_lambda, lambda_args, is_if
from .expression import lambda_body, is_application, is_variable, if_condition
from .expression import if_conseq, if_alternative, make_lambda
from .expression import is_begin, make_begin, begin_expressions


class ClosureConverter(object):

    def __init__(self, global_variables=[]):
        self.global_variables = global_variables

    def closure_convert(self, exp):
        if is_number(exp) or is_boolean(exp) or is_variable(exp):
            return exp
        elif is_lambda(exp):
            return self.closure_convert_lambda(exp)
        elif is_begin(exp):
            return make_begin([self.closure_convert(e)
                               for e in begin_expressions(exp)])
        elif is_application(exp):
            res = YarpenList([self.closure_convert(e)
                              for e in exp.expressions])
            return res
        else:
            return exp

    def closure_convert_lambda(self, exp):
        free_vars = self.free_variables(exp)
        converted_body = self.closure_convert(lambda_body(exp))
        return YarpenClosure(self.substitute(converted_body, free_vars),
                             free_vars,
                             lambda_args(exp))

    def substitute(self, exp, free_vars):
        if is_number(exp) or is_boolean(exp):
            return exp
        elif is_variable(exp):
            if exp in free_vars:
                return YarpenFreeVarRef(exp.symbol)
            else:
                return exp
        elif is_lambda(exp):
            lambda_body_exp = self.substitute(lambda_body(exp),
                                              sub(free_vars,
                                                  lambda_args(exp)))
            return make_lambda(lambda_args(exp), lambda_body_exp)
        elif is_begin(exp):
            return make_begin([self.substitute(e, free_vars)
                               for e in begin_expressions(exp)])
        elif is_application(exp):
            return YarpenList([self.substitute(e, free_vars)
                              for e in exp.expressions])
        else:
            return exp

    def free_variables(self, expr):
        if is_variable(expr):
            if expr in self.global_variables:
                return []
            else:
                return [expr]
        elif is_if(expr):
            return (self.free_variables(if_condition(expr))
                    + self.free_variables(if_conseq(expr))
                    + self.free_variables(if_alternative(expr)))
        elif is_lambda(expr):
            return sub(self.free_variables(lambda_body(expr)),
                       lambda_args(expr))
        elif is_begin(expr):
            fv = [self.free_variables(exp) for exp in begin_expressions(expr)]
            return [x for y in fv for x in y]
        elif is_application(expr):
            fv = [self.free_variables(exp) for exp in expr.expressions]
            return [x for y in fv for x in y]
        else:
            return []


def sub(a, b):
    # Return a - b
    return [x for x in a if x not in b]
