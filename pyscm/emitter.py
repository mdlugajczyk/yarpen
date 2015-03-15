class Emitter(object):
    def __init__(self):
        self.code = ""

    def entry_point_preamble(self, function_name):
        self.code += """.text
.globl %s
.type %s @function
pyscm_start:
""" % (function_name, function_name)

    def emit(self):
        return self.code + "\n"

    def emit_ret(self):
        self.code += "    ret\n"

    def emit_constant(self, const, dst):
        self.code += "movq $%s, %%%s\n" % (const, dst)

    def emit_stmt(self, stmt):
        self.code += "%s\n" % stmt

    def save_on_stack(self, stack_index):
        self.emit_stmt("    movq %%rax, %d(%%rsp)" % stack_index)

    def load_from_stack(self, stack_index):
        self.emit_stmt("    movq  %d(%%rsp), %%rax" % stack_index)