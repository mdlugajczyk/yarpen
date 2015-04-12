from parser import Parser
from emitter import Emitter
from expression import PyScmNumber, PyScmBoolean, PyScmList, PyScmSymbol
from environment import Environment


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
        self.exprs = self.parser.parse()
        self.emitter.entry_point_preamble("pyscm_start")
        self.compile_exprs()
        self.emitter.emit_ret()
        return self.emitter.emit()

    def compile_exprs(self):
        env = Environment()
        for expr in self.exprs:
            self.compile_expr(expr, env, -Compiler.WORDSIZE)

    def compile_expr(self, expr, env, stack_index):
        if is_number(expr):
            self.compile_number(expr)
        elif is_boolean(expr):
            self.compile_boolean(expr)
        elif self.is_variable(expr):
            self.compile_variable_reference(expr, env, stack_index)
        elif self.is_if(expr):
            self.compile_if(expr, env, stack_index)
        elif self.is_primitive_function(expr):
            self.compile_primitive_function(expr, env, stack_index)
        elif self.is_lambda(expr):
            self.compile_lambda(expr, env, stack_index)
        elif self.is_let(expr):
            self.compile_let(expr, env, stack_index)
        elif self.is_application(expr):
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

    def is_let(self, expr):
        return is_tagged_list(expr, PyScmSymbol("let"))

    def let_bindings(self, expr):
        assert(type(expr.expressions[1]) == PyScmList)
        return expr.expressions[1].expressions

    def let_body(self, expr):
        return expr.expressions[2]

    def compile_let(self, expr, env, stack_index):
        si = stack_index
        extended_env = env
        for binding in self.let_bindings(expr):
            var = binding.expressions[0]
            val = binding.expressions[1]
            self.compile_expr(val, env, si)
            self.emitter.save_on_stack(si)
            extended_env = extended_env.extend(var.symbol, si)
            si -= Compiler.WORDSIZE
        self.compile_expr(self.let_body(expr), extended_env, si)

    def is_variable(self, expr):
        return type(expr) == PyScmSymbol

    def compile_variable_reference(self, expr, env, stack_index):
        self.emitter.load_from_stack(env.get_var(expr.symbol))

    def is_if(self, expr):
        return is_tagged_list(expr, PyScmSymbol("if"))

    def if_condition(self, expr):
        return expr.expressions[1]

    def if_conseq(self, expr):
        return expr.expressions[2]

    def if_alternative(self, expr):
        return expr.expressions[3]

    def compile_if(self, expr, env, stack_index):
        cond_false_label = self.label_generator.unique_label("false_branch")
        if_end_label = self.label_generator.unique_label("if_end")
        self.compile_expr(self.if_condition(expr), env, stack_index)
        self.emitter.emit_stmt("    cmp $%d, %%rax" % Compiler.BOOL_FALSE)
        self.emitter.emit_stmt("    je %s" % cond_false_label)
        self.compile_expr(self.if_conseq(expr), env, stack_index)
        self.emitter.emit_stmt("    jmp %s" % if_end_label)
        self.emitter.emit_label(cond_false_label)
        self.compile_expr(self.if_alternative(expr), env, stack_index)
        self.emitter.emit_label(if_end_label)

    def is_lambda(self, expr):
        return is_tagged_list(expr, PyScmSymbol("lambda"))

    def lambda_args(self, expr):
        return expr.expressions[1].expressions

    def lambda_body(self, expr):
        return expr.expressions[2]

    def compile_lambda(self, expr, env, stack_index):
        args = self.lambda_args(expr)
        body = self.lambda_body(expr)
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

    def is_application(self, expression):
        return isinstance(expression, PyScmList)

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


def is_number(expr):
    return isinstance(expr, PyScmNumber)


def is_boolean(expr):
    return isinstance(expr, PyScmBoolean)


def is_tagged_list(expr, tag):
    return (isinstance(expr, PyScmList) and len(expr.expressions) > 0
            and expr.expressions[0] == tag)
