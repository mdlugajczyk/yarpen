from .code_transformer import CodeTransformer
from .emitter import Emitter
from .environment import Environment
from .expression import YarpenFreeVarRef, YarpenSymbol, assignment_value, \
    assignment_variable, begin_expressions, if_alternative, if_condition, \
    if_conseq, is_application, is_assignment, is_begin, is_boolean, is_closure, \
    is_free_var_reference, is_if, is_number, is_tagged_list, is_variable
from .parser import Parser
from .registers import AL, RAX, RBX, RDI, RDX, RSP, dereference, \
    immediate_const, offset

from yarpen.expression import is_boxed_value


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
        global_variables = [YarpenSymbol(fn) for fn
                            in self.primitive_functions]
        self._code_transformer = CodeTransformer(global_variables)

    def compile(self):
        exprs = self._get_transformed_source()
        self.emitter.entry_point_preamble("pyscm_start")
        self.compile_exprs(exprs, Environment(),
                           -Compiler.WORDSIZE, True)
        self.emitter.ret()
        return self.emitter.emit()

    def compile_exprs(self, exprs, env, stack_index, tail_position):
        for expr in exprs[:-1]:
            self.compile_expr(expr, env, stack_index, False)
        self.compile_expr(exprs[-1], env, stack_index, tail_position)

    def compile_expr(self, expr, env, stack_index, tail_position):
        if is_number(expr):
            self.compile_number(expr)
        elif is_boolean(expr):
            self.compile_boolean(expr)
        elif is_variable(expr) or is_free_var_reference(expr) or is_boxed_value(expr):
            self.compile_variable_reference(expr, env, stack_index)
        elif is_if(expr):
            self.compile_if(expr, env, stack_index, tail_position)
        elif self.is_primitive_function(expr):
            self.compile_primitive_function(expr, env, stack_index)
        elif is_closure(expr):
            self.compile_closure(expr, env, stack_index, True)
        elif is_begin(expr):
            self.compile_begin(expr, env, stack_index, tail_position)
        elif is_assignment(expr):
            self.compile_assignment(expr, env, stack_index)
        elif is_application(expr):
            return self.compile_application(expr, env, stack_index, tail_position)
        else:
            raise Exception("Unknow expression %s %s", expr, type(expr))

        if ((is_number(expr) or is_boolean(expr)  or is_variable(expr)
             or is_free_var_reference(expr)) and tail_position):
            self.emitter.ret()

    def compile_number(self, num):
        self.emitter.mov(immediate_const(self.int_repr(num.number)), RAX)

    def compile_boolean(self, b):
        if b.bool:
            self.emitter.mov(immediate_const(Compiler.BOOL_TRUE), RAX)
        else:
            self.emitter.mov(immediate_const(Compiler.BOOL_FALSE), RAX)

    def compile_primitive_function(self, expr, env, stack_index):
        prim = expr.expressions[0].symbol
        compile_fun = self.primitive_functions[prim]
        compile_fun(expr, env, stack_index)

    def is_primitive_function(self, expr):
        return any(is_tagged_list(expr, YarpenSymbol(prim))
                   for prim in self.primitive_functions)

    def compile_prim_integer_p(self, expr, env, stack_index):
        assert(len(expr.expressions) == 2)
        self.compile_expr(expr.expressions[1], env, stack_index, None)
        self.emitter.and_inst(immediate_const(Compiler.INT_MASK), RAX)
        self.emitter.cmp(immediate_const(Compiler.INT_TAG), RAX)
        self.emitter.sete(AL)
        self.emitter.movzbq(AL, RAX)
        self.emitter.shl(immediate_const(Compiler.BOOL_BIT), RAX)
        self.emitter.or_inst(immediate_const(Compiler.BOOL_FALSE), RAX)

    def compile_prim_add(self, expr, env, stack_index):
        assert(len(expr.expressions) == 3)
        self.compile_expr(expr.expressions[1], env, stack_index, None)
        self.save_on_stack(stack_index)
        self.compile_expr(expr.expressions[2], env,
                          stack_index - Compiler.WORDSIZE, None)
        self.emitter.add(offset(RSP, stack_index), RAX)

    def compile_prim_sub(self, expr, env, stack_index):
        assert(len(expr.expressions) == 3)
        self.compile_expr(expr.expressions[1], env, stack_index, None)
        self.save_on_stack(stack_index)
        self.compile_expr(expr.expressions[2], env,
                          stack_index - Compiler.WORDSIZE, None)
        self.emitter.sub(RAX, offset(RSP, stack_index))
        self.load_from_stack(stack_index)

    def compile_prim_mul(self, expr, env, stack_index):
        assert(len(expr.expressions) == 3)
        self.compile_expr(expr.expressions[1], env, stack_index, None)
        self.save_on_stack(stack_index)
        self.compile_expr(expr.expressions[2], env,
                          stack_index - Compiler.WORDSIZE, None)
        self.emitter.shr(immediate_const(Compiler.INT_SHIFT), RAX)
        self.emitter.mul(offset(RSP, stack_index))

    def compile_prim_zero_p(self, expr, env, stack_index):
        assert(len(expr.expressions) == 2)
        self.compile_expr(expr.expressions[1], env, stack_index, None)
        self.emitter.cmp(immediate_const(self.int_repr(0)), RAX)
        self.emitter.sete(AL)
        self.emitter.movzbq(AL, RAX)
        self.emitter.shl(immediate_const(Compiler.BOOL_BIT), RAX)
        self.emitter.or_inst(immediate_const(Compiler.BOOL_FALSE), RAX)

    def int_repr(self, integer):
        return integer << Compiler.INT_SHIFT

    def compile_variable_reference(self, expr, env, stack_index):
        unboxed_variable = expr
        if is_boxed_value(expr):
            unboxed_variable = expr.boxed_value
        variable_index = env.get_var(unboxed_variable)
        if is_variable(unboxed_variable):
            self.emitter.comment("Loading bound variable: " + str(expr))
            self.load_from_stack(variable_index)
        else:
            self.emitter.comment("Loading free variable: " + str(unboxed_variable))
            self.emitter.mov(offset(RBX, variable_index), RAX)
        if is_boxed_value(expr):
            self.emitter.comment("Loading boxed variable")
            self.emitter.mov(offset(RAX,0), RAX)

    def compile_if(self, expr, env, stack_index, tail_position):
        cond_false_label = self.label_generator.unique_label("false_branch")
        if_end_label = self.label_generator.unique_label("if_end")
        self.compile_expr(if_condition(expr), env, stack_index, False)
        self.emitter.cmp(immediate_const(Compiler.BOOL_FALSE), RAX)
        self.emitter.jump_equal(cond_false_label)
        self.compile_expr(if_conseq(expr), env, stack_index, tail_position)
        self.emitter.jmp(if_end_label)
        self.emitter.label(cond_false_label)
        self.compile_expr(if_alternative(expr), env,
                          stack_index, tail_position)
        self.emitter.label(if_end_label)

    def compile_begin(self, expr, env, stack_index, tail_position):
        self.compile_exprs(begin_expressions(expr), env,
                           stack_index, tail_position)

    def compile_assignment(self, expr, env, stack_index):
        variable_index = env.get_var(assignment_variable(expr))
        self.emitter.comment('Assignment to variable: {0} at index: {1} in env: {2}'.format(str(expr), variable_index, env))
        self.compile_expr(assignment_value(expr), env, stack_index, None)
        self.emitter.comment("Done with assignment to " + str(assignment_variable(expr)))
        variable = assignment_variable(expr)
        if is_boxed_value(variable):
            self.assign_to_boxed_variable(variable.boxed_value, env, stack_index)
        elif is_variable(variable):
            self.emitter.comment("Saving bound variable: " + str(assignment_variable(expr)))
            self.save_on_stack(variable_index)
        else:
            self.emitter.comment("Saving free variable: " + str(assignment_variable(expr)))
            self.emitter.mov(RAX, offset(RBX, variable_index))

    def assign_to_boxed_variable(self, var, env, stack_index):
        self.save_on_stack(stack_index)
        var_index = env.get_var(var)
        if is_variable(var):
            self.load_from_stack(var_index)
            self.emitter.mov(RAX, RDX)
        else:
            self.emitter.mov(offset(RBX, var_index), RDX)
        self.load_from_stack(stack_index)
        self.emitter.mov(RAX, offset(RDX,0))
            
    def alloc_memory(self, stack_index, size):
        self.adjust_base(stack_index + Compiler.WORDSIZE)
        self.emitter.mov(immediate_const(size), RDI)
        self.emitter.call("pyscm_alloc")
        self.adjust_base(- (stack_index + Compiler.WORDSIZE))

    def alloc_closure(self, stack_index, size_free_variables, label):
        """ We need to allocate a closure structure holding:
        (a) number of free variables, (b) addreses of label indicating
        start of closure's body
        (c) list of free variables
        """
        self.alloc_memory(stack_index,
                          (2 + size_free_variables) * Compiler.WORDSIZE)
        self.save_on_stack(stack_index)
        stack_index -= Compiler.WORDSIZE

        self.emitter.mov(immediate_const(size_free_variables),
                         offset(RAX, 0))
        self.emitter.lea(label, RDI)
        self.emitter.mov(RDI, offset(RAX, Compiler.WORDSIZE))

    def compile_closure(self, expr, env, stack_index, tail_position):
        self.emitter.comment("Compiling closure: " + str(expr) + " in env " + str(env))
        args = expr.parameters
        body = expr.body
        closure_env, si = self.extend_env_for_closure(args, Environment(),
                                                      -Compiler.WORDSIZE)
        closure_label = self.label_generator.unique_label("closure")
        closure_end = self.label_generator.unique_label("closure_end")

        self.emitter.comment("Allocating closure.")
        self.alloc_closure(stack_index, len(expr.free_variables),
                           closure_label)
        self.emitter.comment("Done allocating closure. Saving it on stack.")
        self.save_on_stack(stack_index)
        closure_stack_index = stack_index
        stack_index -= Compiler.WORDSIZE
        var_offset = 2
        self.emitter.mov(RAX, RDX)
        for fv in expr.free_variables:
            try:
                env.get_var(fv)
            except KeyError:
                fv = YarpenFreeVarRef(fv.symbol)
            self.emitter.comment("Compiling free variable: " + str(fv))
            self.compile_variable_reference(fv, env, stack_index)
            index = var_offset * Compiler.WORDSIZE
            self.emitter.mov(RAX, offset(RDX, index))
            closure_env = closure_env.extend(YarpenFreeVarRef(fv.symbol),
                                             index)
            var_offset += 1

        self.emitter.jmp(closure_end)
        self.emitter.function_header(closure_label)
        self.emitter.comment("Allocating boxed values")
        self.allocate_boxed_parameters(args, closure_env, stack_index)
        self.emitter.comment("Compiling closure body: " + closure_label + " " + str(expr))
        self.compile_expr(body, closure_env, si, tail_position)
        self.emitter.ret()
        self.emitter.label(closure_end)
        self.load_from_stack(closure_stack_index)

    def allocate_boxed_parameters(self, parameters, env, stack_index):
        # TODO: could be optimized if we allocated an array for all boxed
        # variables instead of calling malloc per each var.
        for arg in filter(lambda a: is_boxed_value(a), parameters):
            self.emitter.comment("Allocating boxed variable: %s" % str(arg))
            arg_index = env.get_var(arg)
            self.alloc_memory(stack_index, Compiler.WORDSIZE)
            self.emitter.mov(RAX, RDX)
            self.load_from_stack(arg_index)
            self.emitter.mov(RAX, offset(RDX, 0))
            self.emitter.mov(RDX, RAX)
            self.save_on_stack(arg_index)

    def extend_env_for_closure(self, closure_args, env, stack_index):
        extended_env = env
        for arg in closure_args:
            extended_env = extended_env.extend(arg, stack_index)
            stack_index -= Compiler.WORDSIZE
        return extended_env, stack_index

    def emit_closure(self, expr, env, stack_index, tail_position):
        self.compile_expr(expr.expressions[0], env, stack_index, False)
        self.save_on_stack(stack_index)
        closure_stack_index = stack_index
        return closure_stack_index

    def save_closure_register(self, stack_index):
        self.emitter.mov(RBX, RAX)
        self.save_on_stack(stack_index)
        return stack_index

    def emit_closure_app(self, closure_si, stack_index):
        self.load_from_stack(closure_si)
        self.emitter.mov(RAX, RBX)
        self.emitter.mov(offset(RAX, Compiler.WORDSIZE), RAX)
        self.adjust_base(stack_index + Compiler.WORDSIZE)
        self.emitter.call(dereference(RAX))
        self.adjust_base(- (stack_index + Compiler.WORDSIZE))

    def compile_application(self, expr, env, stack_index, tail_position):
        self.emitter.comment("Application: " + str(expr) + " is tail position: " + str(tail_position))
        closure_si = self.emit_closure(expr, env, stack_index, tail_position)
        if tail_position:
            self.emit_tail_call(expr, env, stack_index, closure_si)
        else:
            self.emit_call(expr, env, stack_index, closure_si)

    def emit_tail_call(self, expr, env, stack_index, closure_si):
        self.emitter.comment("Emit args: %s" % str(expr.expressions[1:]))
        stack_index = self.emit_application_arguments(expr.expressions[1:],
                                                      env, stack_index)
        # let's load the closure from stack to rbx, otherwise it
        # would be overwritten. we don't need to preserve old rbx,
        # as this is tail call.
        self.emitter.comment("Load closure from stack.")
        self.load_from_stack(closure_si)
        self.emitter.mov(RAX, RBX)
        self.shift_arguments_for_tail_call(stack_index, expr.expressions[1:])
        self.emitter.mov(offset(RBX, Compiler.WORDSIZE), RAX)
        self.emitter.jmp(dereference(RAX))

    def emit_call(self, expr, env, stack_index, closure_si):
        stack_index -= Compiler.WORDSIZE
        self.emitter.comment("Emit args.")
        self.emit_application_arguments(expr.expressions[1:],
                                        env, stack_index)
        closure_register_si = self.save_closure_register(stack_index)
        self.emit_closure_app(closure_si, stack_index)
        stack_index -= Compiler.WORDSIZE
        self.save_on_stack(stack_index)
        self.load_from_stack(closure_register_si)
        self.emitter.mov(RAX, RBX)
        self.load_from_stack(stack_index)

    def shift_arguments_for_tail_call(self, stack_index, arguments):
        delta = - (stack_index + Compiler.WORDSIZE)
        src = stack_index + ((len(arguments) - 1) * Compiler.WORDSIZE)
        dst = stack_index+delta
        self.emitter.comment("Shift arguments")
        for arg in arguments:
            self.load_from_stack(src)
            self.save_on_stack(dst)
            src -= Compiler.WORDSIZE
            dst -= Compiler.WORDSIZE

    def emit_application_arguments(self, args, env, stack_index):
        for arg in args:
            stack_index -= Compiler.WORDSIZE
            self.compile_expr(arg, env, stack_index, False)
            self.save_on_stack(stack_index)
        return stack_index

    def save_on_stack(self, stack_index):
        self.emitter.mov(RAX, offset(RSP, stack_index))

    def load_from_stack(self, stack_index):
        self.emitter.mov(offset(RSP, stack_index), RAX)

    def adjust_base(self, si):
        if si > 0:
            self.emitter.add(immediate_const(si), RSP)
        else:
            self.emitter.sub(immediate_const(-si), RSP)

    def _get_transformed_source(self):
        return [self._code_transformer.transform(e) for e in self.parser.parse()]


class LabelGenerator:
    def __init__(self):
        self.counter = 0

    def unique_label(self, prefix):
        label = prefix + ("__%d" % self.counter)
        self.counter += 1
        return label
