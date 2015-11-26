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
from yarpen.stack import Stack


class Compiler(object):
    BOOL_BIT = 6
    INT_MASK = 0x03
    INT_TAG = 0x00
    INT_SHIFT = 2
    BOOL_FALSE = 0x2F
    BOOL_TRUE = 0x6F
    CLOSURE_TAG = 0x02
    CONS_TAG = 0x01
    OBJECT_MASK = 0x07
    WORDSIZE = 8

    def __init__(self, source):
        self.label_generator = LabelGenerator()
        self.parser = Parser(source)
        self.emitter = Emitter()
        self.primitive_functions = {"integer?": self.compile_prim_integer_p,
                                    "fx+": self.compile_prim_add,
                                    "fx-": self.compile_prim_sub,
                                    "fx*": self.compile_prim_mul,
                                    "zero?": self.compile_prim_zero_p,
                                    "closure?": self.compile_prim_closure_p,
                                    "cons": self.compile_prim_cons,
                                    "car": self.compile_prim_car,
                                    "cdr": self.compile_prim_cdr,
                                    "cons?": self.compile_prim_cons_p,
                                    "set-car!": self.compile_prim_set_car,
                                    "set-cdr!": self.compile_prim_set_cdr}

        
        global_variables = [YarpenSymbol(fn) for fn
                            in self.primitive_functions]
        self._code_transformer = CodeTransformer(global_variables)

    def compile(self):
        """ Main entry point to the compiler.

        The first stage parses the input, then a series of code
        transformations is applied, producing an equivalent but easier
        to compile code. Next stage involves generation of (highly
        unoptimized) x64 assembly.

        Currently, only 5 registers are being used:
        (1) RAX holds the current result
        (2) RBX holds the current closure.
        (3) RDX is used for storing temporary results
        (4) RDI passing arguments to C functions
        (5) RSP for stack manipulation
        """
        exprs = self._get_transformed_source()
        self.emitter.entry_point_preamble("pyscm_start")
        self.compile_exprs(exprs, Environment(),
                           Stack(), True)
        self.emitter.ret()
        return self.emitter.emit()

    def compile_exprs(self, exprs, env, stack, tail_position):
        for expr in exprs[:-1]:
            self.compile_expr(expr, env, stack, False)
        self.compile_expr(exprs[-1], env, stack, tail_position)

    def compile_expr(self, expr, env, stack, tail_position):
        if is_number(expr):
            self.compile_number(expr)
        elif is_boolean(expr):
            self.compile_boolean(expr)
        elif is_variable(expr) or is_free_var_reference(expr) or is_boxed_value(expr):
            self.compile_variable_reference(expr, env, stack)
        elif is_if(expr):
            self.compile_if(expr, env, stack, tail_position)
        elif self.is_primitive_function(expr):
            self.compile_primitive_function(expr, env, stack)
        elif is_closure(expr):
            self.compile_closure(expr, env, stack, True)
        elif is_begin(expr):
            self.compile_begin(expr, env, stack, tail_position)
        elif is_assignment(expr):
            self.compile_assignment(expr, env, stack)
        elif is_application(expr):
            return self.compile_application(expr, env, stack, tail_position)
        else:
            raise Exception("Unknow expression %s %s", expr, type(expr))

        if ((is_number(expr) or is_boolean(expr)  or is_variable(expr)
             or is_free_var_reference(expr)) and tail_position):
            self.emitter.ret()

    def compile_number(self, num):
        self.emitter.mov(immediate_const(self.int_repr(num.number)), RAX)

    def compile_boolean(self, b):
        if b.bool:
            value = Compiler.BOOL_TRUE
        else:
            value = Compiler.BOOL_FALSE
        self.emitter.mov(immediate_const(value), RAX)

    def compile_primitive_function(self, expr, env, stack):
        prim = expr.expressions[0].symbol
        compile_fun = self.primitive_functions[prim]
        compile_fun(expr, env, stack)

    def is_primitive_function(self, expr):
        return any(is_tagged_list(expr, YarpenSymbol(prim))
                   for prim in self.primitive_functions)

    def compile_prim_integer_p(self, expr, env, stack):
        self.compare_type_tag(expr, env, stack, Compiler.INT_TAG,
                              Compiler.INT_MASK)

    def compile_prim_add(self, expr, env, stack):
        assert(len(expr.expressions) == 3)
        self.compile_expr(expr.expressions[1], env, stack, None)
        self.save_on_stack(stack.get_index())
        self.compile_expr(expr.expressions[2], env,
                          stack.next(), None)
        self.emitter.add(offset(RSP, stack.get_index()), RAX)

    def compile_prim_sub(self, expr, env, stack):
        assert(len(expr.expressions) == 3)
        self.compile_expr(expr.expressions[1], env, stack, None)
        self.save_on_stack(stack.get_index())
        self.compile_expr(expr.expressions[2], env,
                          stack.next(), None)
        self.emitter.sub(RAX, offset(RSP, stack.get_index()))
        self.load_from_stack(stack.get_index())

    def compile_prim_mul(self, expr, env, stack):
        assert(len(expr.expressions) == 3)
        self.compile_expr(expr.expressions[1], env, stack, None)
        self.save_on_stack(stack.get_index())
        self.compile_expr(expr.expressions[2], env,
                          stack.next(), None)
        self.emitter.shr(immediate_const(Compiler.INT_SHIFT), RAX)
        self.emitter.mul(offset(RSP, stack.get_index()))

    def compile_prim_zero_p(self, expr, env, stack):
        self.compare_type_tag(expr, env, stack, self.int_repr(0))

    def compile_prim_closure_p(self, expr, env, stack):
        self.compare_type_tag(expr, env, stack, Compiler.CLOSURE_TAG,
                              Compiler.OBJECT_MASK)

    def compile_prim_cons(self, expr, env, stack):
        assert(len(expr.expressions) == 3)
        self.compile_expr(expr.expressions[1], env, stack, None)
        self.save_on_stack(stack.get_index())
        car_index = stack
        stack = stack.next()
        self.compile_expr(expr.expressions[2], env, stack, None)
        cdr_index = stack
        stack = stack.next()
        self.save_on_stack(cdr_index.get_index())
        self.alloc_memory(stack, Compiler.WORDSIZE * 2)
        self.emitter.mov(RAX, RDX)
        self.load_from_stack(car_index.get_index())
        self.emitter.mov(RAX, offset(RDX, 0))
        self.load_from_stack(cdr_index.get_index())
        self.emitter.mov(RAX, offset(RDX, Compiler.WORDSIZE))
        self.emitter.mov(RDX, RAX)
        self.emitter.or_inst(immediate_const(Compiler.CONS_TAG), RAX)

    def compile_prim_car(self, expr, env, stack):
        assert(len(expr.expressions) == 2)
        self.compile_expr(expr.expressions[1], env, stack, None)
        self.emitter.mov(offset(RAX, -1), RAX)

    def compile_prim_cdr(self, expr, env, stack):
        assert(len(expr.expressions) == 2)
        self.compile_expr(expr.expressions[1], env, stack, None)
        self.emitter.mov(offset(RAX, Compiler.WORDSIZE - 1), RAX)

    def compile_prim_set_car(self, expr, env, stack):
        assert(len(expr.expressions) == 3)
        self.compile_expr(expr.expressions[2], env, stack, None)
        self.emitter.mov(RAX, RDX)
        self.compile_expr(expr.expressions[1], env, stack, None)
        self.emitter.mov(RDX, offset(RAX, -1))

    def compile_prim_set_cdr(self, expr, env, stack):
        assert(len(expr.expressions) == 3)
        self.compile_expr(expr.expressions[2], env, stack, None)
        self.emitter.mov(RAX, RDX)
        self.compile_expr(expr.expressions[1], env, stack, None)
        self.emitter.mov(RDX, offset(RAX, 7))        


    def compile_prim_cons_p(self, expr, env, stack):
        self.compare_type_tag(expr, env, stack, Compiler.CONS_TAG,
                              Compiler.OBJECT_MASK)

    def compare_type_tag(self, expr, env, stack, tag, mask=None):
        assert(len(expr.expressions) == 2)
        self.compile_expr(expr.expressions[1], env, stack, None)
        if mask:
            self.emitter.and_inst(immediate_const(mask), RAX)
        self.compare_with_constant(tag)
        self.emitter.sete(AL)
        self.emitter.movzbq(AL, RAX)
        self.emitter.shl(immediate_const(Compiler.BOOL_BIT), RAX)
        self.emitter.or_inst(immediate_const(Compiler.BOOL_FALSE), RAX)

    def int_repr(self, integer):
        return integer << Compiler.INT_SHIFT

    def compare_with_constant(self, constant):
        self.emitter.cmp(immediate_const(constant), RAX)

    def compile_variable_reference(self, expr, env, stack):
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

    def compile_if(self, expr, env, stack, tail_position):
        cond_false_label = self.label_generator.unique_label("false_branch")
        if_end_label = self.label_generator.unique_label("if_end")
        self.compile_expr(if_condition(expr), env, stack, False)        
        self.compare_with_constant(Compiler.BOOL_FALSE)
        self.emitter.jump_equal(cond_false_label)
        self.compile_expr(if_conseq(expr), env, stack, tail_position)
        self.emitter.jmp(if_end_label)
        self.emitter.label(cond_false_label)
        self.compile_expr(if_alternative(expr), env,
                          stack, tail_position)
        self.emitter.label(if_end_label)

    def compile_begin(self, expr, env, stack, tail_position):
        self.compile_exprs(begin_expressions(expr), env,
                           stack, tail_position)

    def compile_assignment(self, expr, env, stack):
        variable_index = env.get_var(assignment_variable(expr))
        self.emitter.comment('Assignment to variable: {0} at index: {1} in env: {2}'.format(str(expr), variable_index, env))
        self.compile_expr(assignment_value(expr), env, stack, None)
        self.emitter.comment("Done with assignment to " + str(assignment_variable(expr)))
        variable = assignment_variable(expr)
        if is_boxed_value(variable):
            self.assign_to_boxed_variable(variable.boxed_value, env, stack)
        elif is_variable(variable):
            self.emitter.comment("Saving bound variable: " + str(assignment_variable(expr)))
            self.save_on_stack(variable_index)
        else:
            self.emitter.comment("Saving free variable: " + str(assignment_variable(expr)))
            self.emitter.mov(RAX, offset(RBX, variable_index))

    def assign_to_boxed_variable(self, var, env, stack):
        self.save_on_stack(stack.get_index())
        # Load the variable address.
        self.compile_variable_reference(var, env, stack)
        self.emitter.mov(RAX, RDX)
        self.load_from_stack(stack.get_index())
        self.emitter.mov(RAX, offset(RDX,0))
            
    def alloc_memory(self, stack, size):
        stack = stack.prev()
        self.adjust_base(stack.get_index())
        self.emitter.mov(immediate_const(size), RDI)
        self.emitter.call("pyscm_alloc")
        self.adjust_base(- stack.get_index())

    def alloc_closure(self, stack, size_free_variables, label):
        """ We need to allocate a closure structure holding:
        (a) number of free variables,
        (b) addreses of label indicating start of closure's body,
        (c) list of free variables
        """
        self.alloc_memory(stack,
                          (2 + size_free_variables) * Compiler.WORDSIZE)
        self.emitter.mov(immediate_const(size_free_variables),
                         offset(RAX, 0))
        self.emitter.lea(label, RDI)
        self.emitter.mov(RDI, offset(RAX, Compiler.WORDSIZE))
        self.emitter.or_inst(immediate_const(Compiler.CLOSURE_TAG), RAX)

    def compile_closure(self, expr, env, stack, tail_position):
        self.emitter.comment("Compiling closure: " + str(expr) + " in env " + str(env))
        args = expr.parameters
        body = expr.body
        closure_env, si = self.extend_env_for_closure(args, Environment(),
                                                      Stack())
        closure_label = self.label_generator.unique_label("closure")
        closure_end = self.label_generator.unique_label("closure_end")

        self.emitter.comment("Allocating closure.")
        self.alloc_closure(stack, len(expr.free_variables),
                           closure_label)
        self.emitter.comment("Done allocating closure. Saving it on stack.")
        self.save_on_stack(stack.get_index())
        self.emitter.mov(RAX, RDX)
        self.untag_closure(RDX)
        closure_stack_index = stack
        stack = stack.next()
        closure_env = self.emit_closure_free_variables(expr.free_variables, env, closure_env, stack)
        self.emitter.jmp(closure_end)
        self.emitter.function_header(closure_label)
        self.emitter.comment("Allocating boxed values")
        self.allocate_boxed_parameters(args, closure_env, stack)
        self.emitter.comment("Compiling closure body: " + closure_label + " " + str(expr))
        self.compile_expr(body, closure_env, si, tail_position)
        self.emitter.ret()
        self.emitter.label(closure_end)
        self.load_from_stack(closure_stack_index.get_index())

    def emit_closure_free_variables(self, free_variables, env, closure_env, stack):
        var_offset = 2
        for fv in free_variables:
            try:
                env.get_var(fv)
            except KeyError:
                fv = YarpenFreeVarRef(fv.symbol)
            self.emitter.comment("Compiling free variable: " + str(fv))
            self.compile_variable_reference(fv, env, stack)
            index = var_offset * Compiler.WORDSIZE
            self.emitter.mov(RAX, offset(RDX, index))
            closure_env = closure_env.extend(YarpenFreeVarRef(fv.symbol),
                                             index)
            var_offset += 1
        return closure_env

    def allocate_boxed_parameters(self, parameters, env, stack):
        # TODO: could be optimized if we allocated an array for all boxed
        # variables instead of calling malloc per each var.
        for arg in filter(lambda a: is_boxed_value(a), parameters):
            self.emitter.comment("Allocating boxed variable: %s" % str(arg))
            arg_index = env.get_var(arg)
            self.alloc_memory(stack, Compiler.WORDSIZE)
            self.emitter.mov(RAX, RDX)
            self.load_from_stack(arg_index)
            self.emitter.mov(RAX, offset(RDX, 0))
            self.emitter.mov(RDX, RAX)
            self.save_on_stack(arg_index)

    def extend_env_for_closure(self, closure_args, env, stack):
        extended_env = env
        for arg in closure_args:
            extended_env = extended_env.extend(arg, stack.get_index())
            stack = stack.next()
        return extended_env, stack

    def emit_closure(self, expr, env, stack, tail_position):
        self.compile_expr(expr.expressions[0], env, stack, False)
        self.emitter.comment("FOO")
        self.untag_closure(RAX)
        self.save_on_stack(stack.get_index())
        closure_stack_index = stack
        return closure_stack_index

    def untag_closure(self, register):
        self.emitter.sub(immediate_const(Compiler.CLOSURE_TAG), register)

    def save_closure_register(self, stack):
        self.emitter.mov(RBX, RAX)
        self.save_on_stack(stack.get_index())
        return stack

    def emit_closure_app(self, closure_si, stack):
        self.load_from_stack(closure_si.get_index())
        self.emitter.mov(RAX, RBX)
        self.emitter.mov(offset(RAX, Compiler.WORDSIZE), RAX)
        stack = stack.prev()
        self.adjust_base(stack.get_index())
        self.emitter.call(dereference(RAX))
        self.adjust_base(- stack.get_index())

    def compile_application(self, expr, env, stack, tail_position):
        self.emitter.comment("Application: " + str(expr) + " is tail position: " + str(tail_position))
        closure_si = self.emit_closure(expr, env, stack, tail_position)
        if tail_position:
            self.emit_tail_call(expr, env, stack, closure_si)
        else:
            self.emit_call(expr, env, stack, closure_si)

    def emit_tail_call(self, expr, env, stack, closure_si):
        self.emitter.comment("Emit args: %s" % str(expr.expressions[1:]))
        stack = self.emit_application_arguments(expr.expressions[1:],
                                                env, stack)
        # let's load the closure from stack to rbx, otherwise it
        # would be overwritten. we don't need to preserve old rbx,
        # as this is tail call.
        self.emitter.comment("Load closure from stack.")
        self.load_from_stack(closure_si.get_index())
        self.emitter.mov(RAX, RBX)
        self.shift_arguments_for_tail_call(stack, expr.expressions[1:])
        self.emitter.mov(offset(RBX, Compiler.WORDSIZE), RAX)
        self.emitter.jmp(dereference(RAX))

    def emit_call(self, expr, env, stack, closure_si):
        stack = stack.next()
        self.emitter.comment("Emit args.")
        self.emit_application_arguments(expr.expressions[1:],
                                        env, stack)
        closure_register_si = self.save_closure_register(stack)
        self.emit_closure_app(closure_si, stack)
        stack = stack.next()
        self.save_on_stack(stack.get_index())
        self.load_from_stack(closure_register_si.get_index())
        self.emitter.mov(RAX, RBX)
        self.load_from_stack(stack.get_index())

    def shift_arguments_for_tail_call(self, stack, arguments):
        delta = -stack.prev().get_index()
        src = Stack(stack.get_index() + ((len(arguments) - 1) * Compiler.WORDSIZE))
        dst = Stack(stack.get_index()+delta)
        self.emitter.comment("Shift arguments")
        for arg in arguments:
            self.load_from_stack(src.get_index())
            self.save_on_stack(dst.get_index())
            src = src.next()
            dst = dst.next()

    def emit_application_arguments(self, args, env, stack):
        for arg in args:
            stack = stack.next()
            self.compile_expr(arg, env, stack, False)
            self.save_on_stack(stack.get_index())
        return stack

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
