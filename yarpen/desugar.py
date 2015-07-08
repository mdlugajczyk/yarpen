
from .expression import is_let, let_body, let_bindings, YarpenList, make_lambda
from .expression import is_if, is_lambda, is_application, make_if, lambda_body
from .expression import if_condition, if_conseq, if_alternative, lambda_args


def desugar(exp):
    if is_let(exp):
        return desugar_let(exp)
    if is_if(exp):
        return make_if(desugar(if_condition(exp)),
                       desugar(if_conseq(exp)),
                       desugar(if_alternative(exp)))
    elif is_lambda(exp):
        return make_lambda(lambda_args(exp),
                           desugar(lambda_body(exp)))
    elif is_application(exp):
        return YarpenList([desugar(e) for e in exp.expressions])
    else:
        return exp


def desugar_let(exp):
    """ Let expression in the form of
(let ((b1 exp1)
      (b2 exp2)
      ...)
    body)

can be transformed into an application of lambda expression:

((lambda (b1 b2 ...) body) exp1 exp2 ...)
    """
    bindings = let_bindings(exp)
    args = [b.expressions[0] for b in bindings]
    args_values = [desugar(b.expressions[1]) for b in bindings]
    body = desugar(let_body(exp))
    new_lambda = make_lambda(args, body)
    return YarpenList([new_lambda] + args_values)
