from .expression import YarpenBoxedValue, YarpenClosure, YarpenFreeVarRef, \
    YarpenList, assignment_value, assignment_variable, begin_expressions, \
    if_alternative, if_condition, if_conseq, is_application, is_assignment, \
    is_begin, is_boolean, is_boxed_value, is_if, is_lambda, is_number, \
    is_variable, lambda_args, lambda_body, make_begin, make_lambda

from yarpen.expression import make_assignment


class ClosureConverter(object):

    def __init__(self, global_variables=[]):
        self.global_variables = global_variables

    def transform(self, exp):
        if is_number(exp) or is_boolean(exp) or is_variable(exp):
            return exp
        elif is_lambda(exp):
            return self.closure_convert_lambda(exp)
        elif is_begin(exp):
            return make_begin([self.transform(e)
                               for e in begin_expressions(exp)])
        elif is_application(exp):
            res = YarpenList([self.transform(e)
                              for e in exp.expressions])
            return res
        elif is_boxed_value(exp):
            return YarpenBoxedValue(self.transform(exp.boxed_value))
        else:
            return exp

    def closure_convert_lambda(self, exp):
        free_vars = self.free_variables(exp)
        converted_body = self.transform(lambda_body(exp))
        return YarpenClosure(self.substitute(converted_body, free_vars),
                             free_vars,
                             lambda_args(exp))

    def substitute(self, exp, free_vars):
        if is_number(exp) or is_boolean(exp):
            return exp
        elif is_boxed_value(exp):
            return YarpenBoxedValue(self.substitute(exp.boxed_value, free_vars))
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
        elif is_assignment(exp):
            return make_assignment(self.substitute(assignment_variable(exp), free_vars),
                                   self.substitute(assignment_value(exp), free_vars))
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
        elif is_boxed_value(expr):
            return self.free_variables(expr.boxed_value)
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
        elif is_assignment(expr):
            return self.free_variables(assignment_variable(expr)) + self.free_variables(assignment_value(expr))
        elif is_application(expr):
            fv = [self.free_variables(exp) for exp in expr.expressions]
            return [x for y in fv for x in y]
        else:
            return []


def sub(free_vars, args):
    # Return a - b
    result = []
    for f in free_vars:
        if f in args or YarpenBoxedValue(f) in args:
            continue
        result.append(f)
    return result
