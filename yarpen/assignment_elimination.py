from yarpen.expression import YarpenBoxedValue, YarpenList, assignment_value, \
    assignment_variable, begin_expressions, if_alternative, if_condition, \
    if_conseq, is_application, is_assignment, is_begin, is_if, is_lambda, \
    is_number, is_variable, lambda_args, lambda_body, make_assignment, \
    make_begin, make_if, make_lambda


class AssignmentElimination(object):
    """
    Eliminates mutable variables by transforming them into a `YarpenBoxedValue`.

    Operates on a desugared syntax.
    """

    def __init__(self):
        self._mutable_variables = []

    def transform(self, exp):
        self._find_mutable_variables(exp)
        return self._wrap_mutable_variables(exp)

    def _wrap_mutable_variables(self, exp):
        if is_variable(exp):
            return self._maybe_wrap_variable(exp)
        elif is_number(exp):
            return exp
        elif is_assignment(exp):
            return make_assignment(YarpenBoxedValue(assignment_variable(exp).symbol),
                                   self.transform(assignment_value(exp)))
        elif is_lambda(exp):
            return make_lambda(self._maybe_wrap_formals(lambda_args(exp)),
                               self.transform(lambda_body(exp)))
        elif is_if(exp):
            return make_if(self.transform(if_condition(exp)),
                           self.transform(if_conseq(exp)),
                           self.transform(if_alternative(exp)))
        elif is_begin(exp):
            return make_begin(self._wrap_expressions(begin_expressions(exp)))
        elif is_application(exp):
            return YarpenList(self._wrap_expressions(exp.expressions))
        else:
            return exp

    def _find_mutable_variables(self, exp):
        if is_number(exp) or is_variable(exp):
            return
        elif is_assignment(exp):
            self._mutable_variables.append(assignment_variable(exp))
            self._find_mutable_variables(assignment_variable(exp))
        elif is_lambda(exp):
            self._find_mutable_variables(lambda_body(exp))
        elif is_if(exp):
            self._find_mutable_variables(if_condition(exp))
            self._find_mutable_variables(if_conseq(exp))
            self._find_mutable_variables(if_alternative(exp))
        elif is_begin(exp):
            for e in begin_expressions(exp):
                self._find_mutable_variables(e)
        elif is_application(exp):
            for e in exp.expressions:
                self._find_mutable_variables(e)

    def _maybe_wrap_variable(self, exp):
        if self._is_mutable(exp):
            return YarpenBoxedValue(exp.symbol)
        else:
            return exp

    def _wrap_expressions(self, expressions):
        wrapped_expressions = []
        for e in expressions:
            wrapped_expressions.append(self.transform(e))
        return wrapped_expressions

    def _maybe_wrap_formals(self, lambda_formals):
        transformed_formals = []
        for formal in lambda_formals:
            if self._is_mutable(formal):
                transformed_formals.append(YarpenBoxedValue(formal.symbol))
            else:
                transformed_formals.append(formal)
        return transformed_formals

    def _is_mutable(self, exp):
        return exp in self._mutable_variables
