class Emitter(object):
    def __init__(self):
        self.code = ""

    def entry_point_preamble(self, function_name):
        self.code += ".text\n"
        self.function_header(function_name)

    def function_header(self, function_name):
        self.code += """.globl %s
.type %s @function
%s:
""" % (function_name, function_name, function_name)

    def emit(self):
        return self.code + "\n"

    def binary_instruction(self, inst, arg1, arg2):
        self.emit_stmt('{0} {1}, {2}'.format(inst, arg1, arg2))

    def unary_instruction(self, inst, arg):
        self.emit_stmt('{0} {1}'.format(inst, arg))

    def emit_stmt(self, stmt):
        self.code += "\t%s\n" % stmt

    def label(self, label):
        self.code += label + ":\n"

    def and_inst(self, constant, register):
        self.binary_instruction('and', constant, register)

    def cmp(self, constant, register):
        self.binary_instruction('cmp', constant, register)

    def sete(self, register):
        self.unary_instruction('sete', register)

    def movzbq(self, src, dst):
        self.binary_instruction('movzbq', src, dst)

    def shl(self, shift, register):
        self.binary_instruction('shl', shift, register)

    def or_inst(self, const, register):
        self.binary_instruction('or', const, register)

    def shr(self, shift, register):
        self.binary_instruction('shr', shift, register)

    def jmp(self, label):
        self.unary_instruction('jmp', label)

    def ret(self):
        self.emit_stmt('ret')

    def add(self, src, dst):
        self.binary_instruction('addq', src, dst)

    def sub(self, src, dst):
        self.binary_instruction('subq', src, dst)

    def mul(self, reg):
        self.unary_instruction('mulq', reg)

    def mov(self, src, dst):
        self.binary_instruction('movq', src, dst)

    def jump_equal(self, label):
        self.unary_instruction('je', label)

    def call(self, label):
        self.unary_instruction('call', label)

    def lea(self, label, register):
        self.binary_instruction('lea', label, register)
