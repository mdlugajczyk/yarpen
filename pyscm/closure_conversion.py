from expression import PyScmFreeVarRef, PyScmList, PyScmSymbol, PyScmClosure
from expression import is_number, is_boolean, is_lambda, lambda_args, is_if
from expression import lambda_body, is_application, is_variable, if_condition
from expression import if_conseq, if_alternative


def closure_convert(exp):
    if is_number(exp) or is_boolean(exp) or is_variable(exp):
        return exp
    elif is_lambda(exp):
        return closure_convert_lambda(exp)
    elif is_application(exp):
        return PyScmList([closure_convert(e)
                          for e in exp.expressions])
    else:
        return exp


def closure_convert_lambda(exp):
    free_vars = free_variables(exp)
    converted_body = closure_convert(lambda_body(exp))
    return PyScmClosure(substitute(converted_body, free_vars),
                        free_vars,
                        lambda_args(exp))


def substitute(exp, free_vars):
    if is_number(exp) or is_boolean(exp):
        return exp
    elif is_variable(exp):
        if exp in free_vars:
            return PyScmFreeVarRef(exp.symbol)
        else:
            return exp
    elif is_lambda(exp):
        lambda_body_exp = substitute(lambda_body(exp),
                                     sub(free_vars,
                                         lambda_args(exp)))
        return PyScmList([PyScmSymbol("lambda"),
                          PyScmList(lambda_args(exp)),
                          lambda_body_exp])
    elif is_application(exp):
        return PyScmList([substitute(e, free_vars)
                          for e in exp.expressions])
    else:
        return exp


def free_variables(expr):
    if is_variable(expr):
        return [expr]
    elif is_if(expr):
        return (free_variables(if_condition(expr))
                + free_variables(if_conseq(expr))
                + free_variables(if_alternative(expr)))
    elif is_lambda(expr):
        return sub(free_variables(lambda_body(expr)),
                   lambda_args(expr))
    elif is_application(expr):
        fv = [free_variables(exp) for exp in expr.expressions]
        return [x for y in fv for x in y]
    else:
        return []


def sub(a, b):
    # Return a - b
    return [x for x in a if x not in b]
