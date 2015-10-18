from .expression import is_let, let_body, let_bindings, YarpenList, make_lambda
from .expression import is_if, is_lambda, is_application, make_if, lambda_body
from .expression import if_condition, if_conseq, if_alternative, lambda_args
from .expression import is_let_star, make_let, make_begin, is_letrec
from .expression import make_assignment, YarpenNumber, YarpenSymbol


def desugar(exp):
    if is_letrec(exp):
        return desugar_letrec(exp)
    if is_let_star(exp):
        return desugar_let_star(exp)
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

def desugar_letrec(exp):
    bindings = let_bindings(exp)
    sets = [make_assignment(b.expressions[0], b.expressions[1]) for b in bindings]
    dummy_bindings = [YarpenList([b.expressions[0], YarpenNumber(0)]) for b in bindings]
    return desugar_let(YarpenList([YarpenSymbol("let"),
                                   YarpenList(dummy_bindings)]+
                                  sets + let_body(exp)))

def desugar_let_star(exp):
    return desugar_let(transform_let_star_to_let(let_bindings(exp), let_body(exp)))

def transform_let_star_to_let(bindings, body):
    if not bindings:
        return make_let(bindings, body)
    if len(bindings) == 1:
        return make_let(bindings[0], body)
    return make_let(bindings[0], transform_let_star_to_let(bindings[1:], body))


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
    body = make_begin([desugar(e) for e in let_body(exp)])
    new_lambda = make_lambda(args, body)
    return YarpenList([new_lambda] + args_values)
