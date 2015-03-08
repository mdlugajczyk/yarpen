from parser import Parser
from emitter import Emitter
from expression import PyScmNumber, PyScmBoolean


class Compiler(object):
    def __init__(self, source):
        self.parser = Parser(source)
        self.emitter = Emitter()

    def compile(self):
        self.exprs = self.parser.parse()
        self.emitter.entry_point_preamble("pyscm_start")
        self.compile_exprs()
        self.emitter.emit_ret()
        return self.emitter.emit()

    def compile_exprs(self):
        for expr in self.exprs:
            self.compile_expr(expr)

    def compile_expr(self, expr):
        if is_number(expr):
            self.compile_number(expr)
        elif is_boolean(expr):
            self.compile_boolean(expr)
        else:
            raise Exception("Unknow expression %s", expr)

    def compile_number(self, num):
        repr = num.number << 2
        self.emitter.emit_constant(repr, "rax")

    def compile_boolean(self, b):
        if b.bool:
            self.emitter.emit_constant(0x2F, "rax")
        else:
            self.emitter.emit_constant(0x6F, "rax")


def is_number(expr):
    return isinstance(expr, PyScmNumber)


def is_boolean(expr):
    return isinstance(expr, PyScmBoolean)
