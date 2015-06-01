from parser import Parser
from emitter import Emitter
from expression import PyScmSymbol, PyScmFreeVarRef
from expression import is_number, is_boolean, is_closure, is_free_var_reference
from expression import is_application, is_variable, is_tagged_list
from expression import if_condition, if_conseq, is_if, if_alternative
from environment import Environment
from desugar import desugar
from closure_conversion import ClosureConverter


class Compiler(object):
    BOOL_BIT = 6
    INT_MASK = 0x03
    INT_TAG = 0x00
    INT_SHIFT = 2
    BOOL_FALSE = 0x2F
    BOOL_TRUE = 0x6F
    WORDSIZE = 8

    def __init__(self, source):
        self.label_generator = LabelGenerator()
        self.parser = Parser(source)
        self.emitter = Emitter()
        self.primitive_functions = {"integer?": self.compile_prim_integer_p,
                                    "fx+": self.compile_prim_add,
                                    "fx-": self.compile_prim_sub,
                                    "fx*": self.compile_prim_mul,
                                    "zero?": self.compile_prim_zero_p}

    def compile(self):
        exprs = self.parser.parse()
        desugared_exprs = [desugar(exp) for exp in exprs]
        global_variables = [PyScmSymbol(fn) for fn in self.primitive_functions]
        closure_converter = ClosureConverter(global_variables)
        closure_converted = [closure_converter.closure_convert(exp)
                             for exp in desugared_exprs]
        self.emitter.entry_point_preamble("pyscm_start")
        self.compile_exprs(closure_converted)
        self.emitter.emit_ret()
        return self.emitter.emit()

    def compile_exprs(self, exprs):
        env = Environment()
        for expr in exprs:
            self.compile_expr(expr, env, -Compiler.WORDSIZE)

    def compile_expr(self, expr, env, stack_index):
        if is_number(expr):
            self.compile_number(expr)
        elif is_boolean(expr):
            self.compile_boolean(expr)
        elif is_variable(expr) or is_free_var_reference(expr):
            self.compile_variable_reference(expr, env, stack_index)
        elif is_if(expr):
            self.compile_if(expr, env, stack_index)
        elif self.is_primitive_function(expr):
            self.compile_primitive_function(expr, env, stack_index)
        elif is_closure(expr):
            self.compile_closure(expr, env, stack_index)
        elif is_application(expr):
            return self.compile_application(expr, env, stack_index)
        else:
            raise Exception("Unknow expression %s", expr)

    def compile_number(self, num):
        self.emitter.emit_constant(self.int_repr(num.number), "rax")

    def compile_boolean(self, b):
        if b.bool:
            self.emitter.emit_constant(Compiler.BOOL_TRUE, "rax")
        else:
            self.emitter.emit_constant(Compiler.BOOL_FALSE, "rax")

    def compile_primitive_function(self, expr, env, stack_index):
        prim = expr.expressions[0].symbol
        compile_fun = self.primitive_functions[prim]
        compile_fun(expr, env, stack_index)

    def is_primitive_function(self, expr):
        return any(is_tagged_list(expr, PyScmSymbol(prim))
                   for prim in self.primitive_functions)

    def compile_prim_integer_p(self, expr, env, stack_index):
        assert(len(expr.expressions) == 2)
        self.compile_expr(expr.expressions[1], env, stack_index)
        self.emitter.emit_stmt("    and $%d, %%rax" % Compiler.INT_MASK)
        self.emitter.emit_stmt("    cmp $%d, %%rax" % Compiler.INT_TAG)
        self.emitter.emit_stmt("    sete %al")
        self.emitter.emit_stmt('    movzbq %al, %rax')
        self.emitter.emit_stmt("    shl $%d, %%rax" % Compiler.BOOL_BIT)
        self.emitter.emit_stmt("    or $%d, %%rax" % Compiler.BOOL_FALSE)

    def compile_prim_add(self, expr, env, stack_index):
        assert(len(expr.expressions) == 3)
        self.compile_expr(expr.expressions[1], env, stack_index)
        self.emitter.save_on_stack(stack_index)
        self.compile_expr(expr.expressions[2], env,
                          stack_index - Compiler.WORDSIZE)
        self.emitter.emit_stmt("   addq %s(%%rsp), %%rax" % stack_index)

    def compile_prim_sub(self, expr, env, stack_index):
        assert(len(expr.expressions) == 3)
        self.compile_expr(expr.expressions[1], env, stack_index)
        self.emitter.save_on_stack(stack_index)
        self.compile_expr(expr.expressions[2], env,
                          stack_index - Compiler.WORDSIZE)
        self.emitter.emit_stmt("   subq %%rax, %s(%%rsp)" % stack_index)
        self.emitter.load_from_stack(stack_index)

    def compile_prim_mul(self, expr, env, stack_index):
        assert(len(expr.expressions) == 3)
        self.compile_expr(expr.expressions[1], env, stack_index)
        self.emitter.save_on_stack(stack_index)
        self.compile_expr(expr.expressions[2], env,
                          stack_index - Compiler.WORDSIZE)
        self.emitter.emit_stmt("   shr $%d, %%rax" % Compiler.INT_SHIFT)
        self.emitter.emit_stmt("   mulq %d(%%rsp)" % stack_index)

    def compile_prim_zero_p(self, expr, env, stack_index):
        assert(len(expr.expressions) == 2)
        self.compile_expr(expr.expressions[1], env, stack_index)
        self.emitter.emit_stmt("    cmp $%d, %%rax" %
                               self.int_repr(0))
        self.emitter.emit_stmt("    sete %al")
        self.emitter.emit_stmt('    movzbq %al, %rax')
        self.emitter.emit_stmt("    shl $%d, %%rax" % Compiler.BOOL_BIT)
        self.emitter.emit_stmt("    or $%d, %%rax" % Compiler.BOOL_FALSE)

    def int_repr(self, integer):
        return integer << Compiler.INT_SHIFT

    def compile_variable_reference(self, expr, env, stack_index):
        if isinstance(expr, PyScmSymbol):
            self.emitter.load_from_stack(env.get_var(expr))
        else:
            index = env.get_var(expr) * Compiler.WORDSIZE
            self.emitter.emit_stmt('    movq {0}(%rbx), %rax'.format(index))

    def compile_if(self, expr, env, stack_index):
        cond_false_label = self.label_generator.unique_label("false_branch")
        if_end_label = self.label_generator.unique_label("if_end")
        self.compile_expr(if_condition(expr), env, stack_index)
        self.emitter.emit_stmt("    cmp $%d, %%rax" % Compiler.BOOL_FALSE)
        self.emitter.emit_stmt("    je %s" % cond_false_label)
        self.compile_expr(if_conseq(expr), env, stack_index)
        self.emitter.emit_stmt("    jmp %s" % if_end_label)
        self.emitter.emit_label(cond_false_label)
        self.compile_expr(if_alternative(expr), env, stack_index)
        self.emitter.emit_label(if_end_label)

    def alloc_memory(self, stack_index, size):
        self.emitter.adjust_base(stack_index + Compiler.WORDSIZE)
        self.emitter.emit_stmt("    movq $%d, %%rdi" % size)
        self.emitter.emit_stmt("    call pyscm_alloc")
        self.emitter.adjust_base(- (stack_index + Compiler.WORDSIZE))

    def alloc_closure(self, stack_index, size_free_variables, label):
        """ We need to allocate a closure structure holding:
        (a) number of free variables, (b) addreses of label indicating
        start of closure's body
        (c) list of free variables
        """
        self.alloc_memory(stack_index,
                          (2 + size_free_variables) * Compiler.WORDSIZE)
        self.emitter.save_on_stack(stack_index)
        stack_index -= Compiler.WORDSIZE

        self.emitter.emit_stmt('   movq ${0}, (%rax)'.
                               format(size_free_variables))
        self.emitter.emit_stmt("   lea %s, %%rdi" % label)
        self.emitter.emit_stmt('   movq %rdi, {0}(%rax)'.
                               format(Compiler.WORDSIZE))

    def compile_closure(self, expr, env, stack_index):
        args = expr.parameters
        body = expr.body
        closure_env, si = self.extend_env_for_closure(args, env,
                                                      -Compiler.WORDSIZE)
        closure_label = self.label_generator.unique_label("closure")
        closure_end = self.label_generator.unique_label("closure_end")

        self.alloc_closure(stack_index, len(expr.free_variables),
                           closure_label)
        self.emitter.save_on_stack(stack_index)
        closure_stack_index = stack_index
        stack_index -= Compiler.WORDSIZE
        offset = 2
        self.emitter.emit_stmt('   movq %rax, %rdx')
        for fv in expr.free_variables:
            self.compile_variable_reference(fv, env, stack_index)
            self.emitter.emit_stmt('    movq %rax, {0}(%rdx)'.
                                   format(offset * Compiler.WORDSIZE))
            closure_env = closure_env.extend(PyScmFreeVarRef(fv.symbol),
                                             offset)
            offset += 1

        self.emitter.emit_stmt("    jmp %s" % closure_end)
        self.emitter.function_header(closure_label)
        self.compile_expr(body, closure_env, si)
        self.emitter.emit_stmt("    ret")
        self.emitter.emit_label(closure_end)
        self.emitter.load_from_stack(closure_stack_index)

    def extend_env_for_closure(self, closure_args, env, stack_index):
        extended_env = env
        for arg in closure_args:
            extended_env = extended_env.extend(arg, stack_index)
            stack_index -= Compiler.WORDSIZE
        return extended_env, stack_index

    def compile_application(self, expr, env, stack_index):
        function = expr.expressions[0]
        args = expr.expressions[1:]
        self.compile_expr(function, env, stack_index)
        self.emitter.save_on_stack(stack_index)
        closure_stack_index = stack_index
        stack_index -= Compiler.WORDSIZE
        self.emitter.emit_stmt('    movq {0}(%rax), %rax'.
                               format(Compiler.WORDSIZE))
        self.emitter.save_on_stack(stack_index)
        function_stack_index = stack_index
        stack_index -= Compiler.WORDSIZE
        self.emit_application_arguments(args, env, stack_index)

        self.emitter.emit_stmt('    movq %rbx, %rax')
        self.emitter.save_on_stack(stack_index)
        env_stack_index = stack_index
        self.emitter.load_from_stack(closure_stack_index)
        self.emitter.emit_stmt('    movq %rax, %rbx')
        self.emitter.load_from_stack(function_stack_index)
        self.emitter.adjust_base(stack_index + Compiler.WORDSIZE)
        self.emitter.emit_stmt('    call *%rax')
        self.emitter.adjust_base(- (stack_index + Compiler.WORDSIZE))
        stack_index -= Compiler.WORDSIZE
        self.emitter.save_on_stack(stack_index)
        self.emitter.load_from_stack(env_stack_index)
        self.emitter.emit_stmt('    movq %rax, %rbx')
        self.emitter.load_from_stack(stack_index)

    def emit_application_arguments(self, args, env, stack_index):
        for arg in args:
            stack_index -= Compiler.WORDSIZE
            self.compile_expr(arg, env, stack_index)
            self.emitter.save_on_stack(stack_index)


class LabelGenerator:
    def __init__(self):
        self.counter = 0

    def unique_label(self, prefix):
        label = prefix + ("__%d" % self.counter)
        self.counter += 1
        return label
