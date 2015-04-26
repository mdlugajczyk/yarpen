from parser import Parser
from emitter import Emitter
from expression import PyScmSymbol
from expression import is_number, is_boolean, is_lambda
from expression import lambda_body, is_application, is_variable, lambda_args
from expression import is_tagged_list, if_condition, if_conseq, is_let, is_if
from expression import if_alternative, let_body, let_bindings
from environment import Environment
from closure_conversion import closure_convert


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
        closure_converted = closure_convert(exprs)
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
        elif is_variable(expr):
            self.compile_variable_reference(expr, env, stack_index)
        elif is_if(expr):
            self.compile_if(expr, env, stack_index)
        elif self.is_primitive_function(expr):
            self.compile_primitive_function(expr, env, stack_index)
        elif is_lambda(expr):
            self.compile_lambda(expr, env, stack_index)
        elif is_let(expr):
            self.compile_let(expr, env, stack_index)
        elif is_application(expr):
            return self.compile_application(expr, env, stack_index)
        else:
            raise Exception("Unknow expression %s", expr)

    def compile_number(self, num):
        self.emitter.emit_constant(self.int_representation(num.number), "rax")

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
                               self.int_representation(0))
        self.emitter.emit_stmt("    sete %al")
        self.emitter.emit_stmt('    movzbq %al, %rax')
        self.emitter.emit_stmt("    shl $%d, %%rax" % Compiler.BOOL_BIT)
        self.emitter.emit_stmt("    or $%d, %%rax" % Compiler.BOOL_FALSE)

    def int_representation(self, integer):
        return integer << Compiler.INT_SHIFT

    def compile_let(self, expr, env, stack_index):
        si = stack_index
        extended_env = env
        for binding in let_bindings(expr):
            var = binding.expressions[0]
            val = binding.expressions[1]
            self.compile_expr(val, env, si)
            self.emitter.save_on_stack(si)
            extended_env = extended_env.extend(var.symbol, si)
            si -= Compiler.WORDSIZE
        self.compile_expr(let_body(expr), extended_env, si)

    def compile_variable_reference(self, expr, env, stack_index):
        self.emitter.load_from_stack(env.get_var(expr.symbol))

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

    def compile_lambda(self, expr, env, stack_index):
        args = lambda_args(expr)
        body = lambda_body(expr)
        lambda_env, si = self.extend_env_for_lambda(args, env,
                                                    -Compiler.WORDSIZE)
        lambda_label = self.label_generator.unique_label("lambda")
        lambda_end = self.label_generator.unique_label("lambda_end")
        self.emitter.emit_stmt("    jmp %s" % lambda_end)
        self.emitter.function_header(lambda_label)
        self.compile_expr(body, lambda_env, si)
        self.emitter.emit_stmt("    ret")
        self.emitter.emit_label(lambda_end)
        self.emitter.emit_stmt("   lea %s, %%rax" % lambda_label)

    def extend_env_for_lambda(self, lambda_args, env, stack_index):
        extended_env = env
        for arg in lambda_args:
            extended_env = extended_env.extend(arg.symbol, stack_index)
            stack_index -= Compiler.WORDSIZE
        return extended_env, stack_index

    def compile_application(self, expr, env, stack_index):
        function = expr.expressions[0]
        args = expr.expressions[1:]
        self.compile_expr(function, env, stack_index)
        self.emitter.save_on_stack(stack_index)
        self.emit_application_arguments(args, env, stack_index)
        self.emitter.load_from_stack(stack_index)
        self.emitter.adjust_base(stack_index + Compiler.WORDSIZE)
        self.emitter.emit_stmt('    call *%rax')
        self.emitter.adjust_base(- (stack_index + Compiler.WORDSIZE))

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
