from parser import Parser
from emitter import Emitter
from expression import PyScmNumber, PyScmBoolean, PyScmList, PyScmSymbol


class Compiler(object):
    BOOL_BIT = 6
    INT_MASK = 0x03
    INT_TAG = 0x00
    INT_SHIFT = 2
    BOOL_FALSE = 0x2F
    BOOL_TRUE = 0x6F

    def __init__(self, source):
        self.parser = Parser(source)
        self.emitter = Emitter()
        self.primitive_functions = [PyScmSymbol("integer?")]

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
        elif self.is_primitive_function(expr):
            self.compile_primitive_function(expr)
        else:
            raise Exception("Unknow expression %s", expr)

    def compile_number(self, num):
        repr = num.number << Compiler.INT_SHIFT
        self.emitter.emit_constant(repr, "rax")

    def compile_boolean(self, b):
        if b.bool:
            self.emitter.emit_constant(Compiler.BOOL_TRUE, "rax")
        else:
            self.emitter.emit_constant(Compiler.BOOL_FALSE, "rax")

    def compile_primitive_function(self, expr):
        prim = expr.expressions[0].symbol
        if prim == "integer?":
            assert(len(expr.expressions) == 2)
            self.compile_expr(expr.expressions[1])
            self.emitter.emit_stmt("    and $%d, %%rax" % Compiler.INT_MASK)
            self.emitter.emit_stmt("    cmp $%d, %%rax" % Compiler.INT_TAG)
            self.emitter.emit_stmt("    sete %al")
            self.emitter.emit_stmt('    movzbq %al, %rax')
            self.emitter.emit_stmt("    shl $%d, %%rax" % Compiler.BOOL_BIT)
            self.emitter.emit_stmt("    or $%d, %%rax" % Compiler.BOOL_FALSE)
        else:
            raise Exception("Unknow primitive function %s", prim)

    def is_primitive_function(self, expr):
        return any(is_tagged_list(expr, prim)
                   for prim in self.primitive_functions)


def is_number(expr):
    return isinstance(expr, PyScmNumber)


def is_boolean(expr):
    return isinstance(expr, PyScmBoolean)


def is_tagged_list(expr, tag):
    return (isinstance(expr, PyScmList) and len(expr.expressions) > 0
            and expr.expressions[0] == tag)
