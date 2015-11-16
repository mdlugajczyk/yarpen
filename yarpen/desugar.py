from .expression import is_let, let_body, let_bindings, YarpenList, make_lambda
from .expression import is_if, is_lambda, is_application, make_if, lambda_body
from .expression import if_condition, if_conseq, if_alternative, lambda_args
from .expression import is_let_star, make_let, make_begin, is_letrec
from .expression import make_assignment, YarpenNumber, YarpenSymbol


class Desugarer(object):
    """ Removes the syntatic sugar by transforming expressions
    into equivalent, more primitive constructs. 

    Currently supported synatix sugar: let, letrec, let*
    """

    def transform(self, exp):
        if is_letrec(exp):
            return self._desugar_letrec(exp)
        if is_let_star(exp):
            return self._desugar_let_star(exp)
        if is_let(exp):
            return self._desugar_let(exp)
        if is_if(exp):
            return make_if(self.transform(if_condition(exp)),
                           self.transform(if_conseq(exp)),
                           self.transform(if_alternative(exp)))
        elif is_lambda(exp):
            return make_lambda(lambda_args(exp),
                               self.transform(lambda_body(exp)))
        elif is_application(exp):
            return YarpenList([self.transform(e) for e in exp.expressions])
        else:
            return exp

    def _desugar_letrec(self, exp):
        bindings = let_bindings(exp)
        sets = [make_assignment(b.expressions[0], b.expressions[1]) for b in bindings]
        dummy_bindings = [YarpenList([b.expressions[0], YarpenNumber(0)]) for b in bindings]
        return self._desugar_let(YarpenList([YarpenSymbol("let"),
                                             YarpenList(dummy_bindings)]+
                                            sets + let_body(exp)))

    def _desugar_let_star(self, exp):
        return self._desugar_let(self._transform_let_star_to_let(let_bindings(exp), let_body(exp)))

    def _transform_let_star_to_let(self, bindings, body):
        if not bindings:
            return make_let(bindings, body)
        if len(bindings) == 1:
            return make_let(bindings[0], body)
        return make_let(bindings[0], self._transform_let_star_to_let(bindings[1:], body))


    def _desugar_let(self, exp):
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
        args_values = [self.transform(b.expressions[1]) for b in bindings]
        body = make_begin([self.transform(e) for e in let_body(exp)])
        new_lambda = make_lambda(args, body)
        return YarpenList([new_lambda] + args_values)
