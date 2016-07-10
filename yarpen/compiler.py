from .code_transformer import CodeTransformer
from .emitter import Emitter
from .environment import Environment
from .expression import YarpenFreeVarRef, YarpenSymbol, assignment_value, \
    assignment_variable, begin_expressions, if_alternative, if_condition, \
    if_conseq, is_application, is_assignment, is_begin, is_boolean, is_closure, \
    is_free_var_reference, is_if, is_number, is_tagged_list, is_variable, is_quoted, \
    is_character
from .parser import Parser
from .registers import AL, RAX, RBX, RCX, RDX, RDI, RSP, R12, RBP, \
dereference, immediate_const, offset, offset_register

from yarpen.expression import is_boxed_value
from yarpen.stack import Stack


class Compiler(object):
    ENABLE_TCO = True
    BOOL_BIT = 6
    INT_MASK = 0x03
    INT_TAG = 0x00
    INT_SHIFT = 2
    BOOL_FALSE = 0x2F
    BOOL_TRUE = 0x6F
    CHAR_TAG = 0x0F
    CHAR_MASK = 0x3F
    CHAR_SHIFT = 8
    CLOSURE_TAG = 0x02
    CONS_TAG = 0x01
    OBJECT_MASK = 0x07
    NIL_TAG = 0x47
    NIL_MASK = 0xCF
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
                                    "char?": self.compile_prim_char_p,
                                    "closure?": self.compile_prim_closure_p,
                                    "cons": self.compile_prim_cons,
                                    "car": self.compile_prim_car,
                                    "cdr": self.compile_prim_cdr,
                                    "cons?": self.compile_prim_cons_p,
                                    "set-car!": self.compile_prim_set_car,
                                    "set-cdr!": self.compile_prim_set_cdr,
                                    "nil?": self.compile_prim_nil_p}

        
        global_variables = [YarpenSymbol(fn) for fn
                            in self.primitive_functions]
        self._code_transformer = CodeTransformer(global_variables)

    def compile(self):
        """ Main entry point to the compiler.

        The first stage parses the input, then a series of code
        transformations is applied, producing an equivalent but easier
        to compile code. Next stage involves generation of (highly
        unoptimized) x64 assembly.

        The RBX register is used to store the current closure.
        """
        self.emitter.entry_point_preamble("pyscm_start")
        self.emitter.push(RBP)
        self.emitter.mov(RSP, RBP)

        self.emitter.comment("The implicit parameter with number of arguments")
        self.emitter.push(immediate_const(0))
        self.emitter.call("__real_pyscm_start")

        self.emitter.leave()
        self.emitter.ret()

        self.emitter.function_header("__real_pyscm_start")
        self.emitter.push(RBP)
        self.emitter.mov(RSP, RBP)
        exprs = self._get_transformed_source()
        self.compile_exprs(exprs, Environment(),
                           Stack(), Compiler.ENABLE_TCO)
        self.emitter.leave()
        self.emitter.ret()
        return self.emitter.emit()

    def compile_exprs(self, exprs, env, stack, tail_position):
        for expr in exprs[:-1]:
            self.compile_expr(expr, env, stack.copy(), False)
        self.compile_expr(exprs[-1], env, stack.copy(), tail_position)

    def compile_expr(self, expr, env, stack, tail_position):
        if is_number(expr):
            self.compile_number(expr)
        elif is_boolean(expr):
            self.compile_boolean(expr)
        elif is_character(expr):
            self.compile_character(expr)
        elif is_quoted(expr):
            self.compile_quoted(expr)
        elif is_variable(expr) or is_free_var_reference(expr) or is_boxed_value(expr):
            self.compile_variable_reference(expr, env, stack)
        elif is_if(expr):
            self.compile_if(expr, env, stack.copy(), tail_position)
        elif self.is_primitive_function(expr):
            self.compile_primitive_function(expr, env, stack)
        elif is_closure(expr):
            self.compile_closure(expr, env, stack.copy(), Compiler.ENABLE_TCO)
        elif is_begin(expr):
            self.compile_begin(expr, env, stack.copy(), tail_position)
        elif is_assignment(expr):
            self.compile_assignment(expr, env, stack.copy())
        elif is_application(expr):
            return self.compile_application(expr, env, stack.copy(), tail_position)
        else:
            raise Exception("Unknow expression %s %s", expr, type(expr))

        if ((is_number(expr) or is_boolean(expr)  or is_variable(expr)
             or is_free_var_reference(expr)) and tail_position):
            self.emitter.leave()
            self.emitter.ret()

    def compile_number(self, num):
        self.emitter.mov(immediate_const(self.int_repr(num.number)), RAX)

    def compile_boolean(self, b):
        if b.bool:
            value = Compiler.BOOL_TRUE
        else:
            value = Compiler.BOOL_FALSE
        self.emitter.mov(immediate_const(value), RAX)

    def compile_character(self, char):
        value = (ord(char.char) << Compiler.CHAR_SHIFT) | Compiler.CHAR_TAG
        self.emitter.mov(immediate_const(value), RAX)

    def compile_quoted(self, expr):
        e = expr.datum
        if is_application(e):
            if len(e.expressions) == 0:
                self.emitter.mov(immediate_const(Compiler.NIL_TAG), RAX)
            

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

    def compile_prim_char_p(self, expr, env, stack):
        self.compare_type_tag(expr, env, stack, Compiler.CHAR_TAG,
                              Compiler.CHAR_MASK)

    def compile_prim_add(self, expr, env, stack):
        assert(len(expr.expressions) == 3)
        self.compile_expr(expr.expressions[1], env, stack, None)
        self.save_on_stack(stack)
        partial_result = stack.push()
        self.compile_expr(expr.expressions[2], env,
                          stack.copy(), None)
        self.emitter.add(offset(RBP, partial_result.get_index()), RAX)

    def compile_prim_sub(self, expr, env, stack):
        assert(len(expr.expressions) == 3)
        self.compile_expr(expr.expressions[1], env, stack, None)
        self.save_on_stack(stack)
        partial_result = stack.push()
        self.compile_expr(expr.expressions[2], env,
                          stack.copy(), None)
        self.emitter.sub(RAX, offset(RBP, partial_result.get_index()))
        self.load_from_stack(partial_result.get_index())

    def compile_prim_mul(self, expr, env, stack):
        assert(len(expr.expressions) == 3)
        self.compile_expr(expr.expressions[1], env, stack, None)
        self.save_on_stack(stack)
        partial_result = stack.push()
        self.compile_expr(expr.expressions[2], env,
                          stack.copy(), None)
        self.emitter.shr(immediate_const(Compiler.INT_SHIFT), RAX)
        self.emitter.mul(offset(RBP, partial_result.get_index()))

    def compile_prim_zero_p(self, expr, env, stack):
        self.compare_type_tag(expr, env, stack, self.int_repr(0))

    def compile_prim_closure_p(self, expr, env, stack):
        self.compare_type_tag(expr, env, stack, Compiler.CLOSURE_TAG,
                              Compiler.OBJECT_MASK)

    def compile_prim_cons(self, expr, env, stack):
        assert(len(expr.expressions) == 3)
        self.compile_expr(expr.expressions[1], env, stack.copy(), None)
        self.save_on_stack(stack)
        car_index = stack.push()
        self.compile_expr(expr.expressions[2], env, stack.copy(), None)
        cdr_index = stack.push()
        self.save_on_stack(cdr_index)
        self.make_pair(offset(RBP, car_index.get_index()), offset(RBP, cdr_index.get_index()), stack.copy())

    def make_pair(self, car, cdr, stack):
        self.alloc_memory(stack.copy(), Compiler.WORDSIZE * 2)
        self.emitter.mov(RAX, RDX)
        self.emitter.mov(car, RAX)
        self.emitter.mov(RAX, offset(RDX, 0))
        self.emitter.mov(cdr, RAX)
        self.emitter.mov(RAX, offset(RDX, Compiler.WORDSIZE))
        self.emitter.mov(RDX, RAX)
        self.emitter.or_inst(immediate_const(Compiler.CONS_TAG), RAX)

    def compile_prim_car(self, expr, env, stack):
        assert(len(expr.expressions) == 2)
        self.compile_expr(expr.expressions[1], env, stack.copy(), None)
        self.emitter.mov(offset(RAX, -1), RAX)

    def compile_prim_cdr(self, expr, env, stack):
        assert(len(expr.expressions) == 2)
        self.compile_expr(expr.expressions[1], env, stack.copy(), None)
        self.emitter.mov(offset(RAX, Compiler.WORDSIZE - 1), RAX)

    def compile_prim_set_car(self, expr, env, stack):
        assert(len(expr.expressions) == 3)
        self.compile_expr(expr.expressions[2], env, stack.copy(), None)
        self.emitter.mov(RAX, RDX)
        self.compile_expr(expr.expressions[1], env, stack.copy(), None)
        self.emitter.mov(RDX, offset(RAX, -1))

    def compile_prim_set_cdr(self, expr, env, stack):
        assert(len(expr.expressions) == 3)
        self.compile_expr(expr.expressions[2], env, stack.copy(), None)
        self.emitter.mov(RAX, RDX)
        self.compile_expr(expr.expressions[1], env, stack.copy(), None)
        self.set_cdr(RAX, RDX)

    def set_cdr(self, pair_reg, new_cdr_reg):
        self.emitter.mov(new_cdr_reg, offset(pair_reg, 7))

    def compile_prim_nil_p(self, expr, env, stack):
        self.compare_type_tag(expr, env, stack.copy(), Compiler.NIL_TAG,
                              Compiler.NIL_MASK)

    def compile_prim_cons_p(self, expr, env, stack):
        self.compare_type_tag(expr, env, stack.copy(), Compiler.CONS_TAG,
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
        self.compile_expr(if_condition(expr), env, stack.copy(), False)
        self.compare_with_constant(Compiler.BOOL_FALSE)
        self.emitter.jump_equal(cond_false_label)
        self.compile_expr(if_conseq(expr), env, stack.copy(), tail_position)
        self.emitter.jmp(if_end_label)
        self.emitter.label(cond_false_label)
        self.compile_expr(if_alternative(expr), env,
                          stack.copy(), tail_position)
        self.emitter.label(if_end_label)

    def compile_begin(self, expr, env, stack, tail_position):
        self.compile_exprs(begin_expressions(expr), env,
                           stack, tail_position)

    def compile_assignment(self, expr, env, stack):
        variable_index = env.get_var(assignment_variable(expr))
        self.emitter.comment('Assignment to variable: {0} at index: {1} in env: {2}'.format(str(expr), variable_index, env))
        self.compile_expr(assignment_value(expr), env, stack.copy(), None)
        self.emitter.comment("Done with assignment to " + str(assignment_variable(expr)))
        variable = assignment_variable(expr)
        if is_boxed_value(variable):
            self.assign_to_boxed_variable(variable.boxed_value, env, stack)
        elif is_variable(variable):
            self.emitter.comment("Saving bound variable: " + str(assignment_variable(expr)))
            self.save_on_stack(Stack(variable_index))
        else:
            self.emitter.comment("Saving free variable: " + str(assignment_variable(expr)))
            self.emitter.mov(RAX, offset(RBX, variable_index))

    def assign_to_boxed_variable(self, var, env, stack):
        self.save_on_stack(stack)
        # Load the variable address.
        self.compile_variable_reference(var, env, stack.copy())
        self.emitter.mov(RAX, RDX)
        self.load_from_stack(stack.get_index())
        self.emitter.mov(RAX, offset(RDX,0))
            
    def alloc_memory(self, stack, size):
        stack.pop()
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
        self.alloc_memory(stack.copy(),
                          (2 + size_free_variables) * Compiler.WORDSIZE)
        self.emitter.mov(immediate_const(size_free_variables),
                         offset(RAX, 0))
        self.emitter.lea(label, RDI)
        self.emitter.mov(RDI, offset(RAX, Compiler.WORDSIZE))
        self.emitter.or_inst(immediate_const(Compiler.CLOSURE_TAG), RAX)

    def compile_closure(self, expr, env, stack, tail_position):
        self.emitter.comment("Compiling closure: " + str(expr) + " in env "
                             + str(env))
        variadic_args, args = self.get_closure_arguments(expr.parameters)
        body = expr.body
        closure_env = self.extend_env_for_closure(args, Environment())
        si = Stack()
        self.emitter.comment("Closure env" + str(closure_env))
        closure_label = self.label_generator.unique_label("closure")
        closure_end = self.label_generator.unique_label("closure_end")
        self.emitter.comment("Allocating closure.")
        self.alloc_closure(stack, len(expr.free_variables),
                           closure_label)
        self.emitter.comment("Done allocating closure. Saving it on stack.")
        self.save_on_stack(stack)
        self.emitter.mov(RAX, RDX)
        self.untag_closure(RDX)
        closure_stack_index = stack.push()
        closure_env = self.emit_closure_free_variables(expr.free_variables, env, closure_env, stack)
        self.emitter.jmp(closure_end)
        self.emitter.function_header(closure_label)
        self.emitter.push(RBP)
        self.emitter.mov(RSP, RBP)
        if variadic_args:
            optional_args_location = si.push()
            self.transform_optional_arguments(closure_env, args,
                                              optional_args_location, si.copy())
            # As the optional arg can be nil, we wouldn't have a space on
            # the stack allocated for it in that case. Therefore, let's move it to
            # the area of local variables, where we can guarantee we have space.
            closure_env.set_var(args[-1], optional_args_location.get_index())
        self.emitter.comment("Allocating boxed values")
        self.allocate_boxed_parameters(args, closure_env, stack.copy())
        self.emitter.comment("Compiling closure body: " + closure_label + " " + str(expr))
        self.compile_expr(body, closure_env, si, tail_position)
        self.emitter.leave()
        self.emitter.ret()
        self.emitter.label(closure_end)
        self.load_from_stack(closure_stack_index.get_index())

    def transform_optional_arguments(self, closure_env, args, result_location,
                                     si):
        """ Build a list of optional arguments.
        """
        self.emitter.comment("Handling variadic number of arguments in env %s" % closure_env)
        self.emitter.cmp(immediate_const(len(args) - 1), self._arg_count_location())
        nil_label = self.label_generator.unique_label("nil_label")
        non_nil_label = self.label_generator.unique_label("non_nil_label")
        done_with_variadic_arguments = self.label_generator.unique_label("done_with_variadic_arguments")
        self.emitter.jump_equal(nil_label)
        self.emitter.jmp(non_nil_label)
        self.emitter.label(nil_label)
        self.emitter.mov(immediate_const(Compiler.NIL_TAG),
                         offset(RBP, result_location.get_index()))
        self.emitter.jmp(done_with_variadic_arguments)
        self.emitter.label(non_nil_label)
        self.emitter.sub(immediate_const(len(args) - 1), self._arg_count_location())
        self.create_list_from_optional_arguments(Stack(closure_env.get_var(args[-1])), si)
        self.emitter.mov(RAX, offset(RBP, result_location.get_index()))
        self.emitter.label(done_with_variadic_arguments)

    def create_list_from_optional_arguments(self, first_optional_arg, si):
        self.emitter.comment("Storing nil on stack")
        self.emitter.mov(immediate_const(Compiler.NIL_TAG), offset(RBP, si.get_index()))
        result = si.push()
        self.emitter.comment("Initializing args offset")
        loop_label = self.label_generator.unique_label("optional_args_loop")
        done_label = self.label_generator.unique_label("optional_args_list_empty")

        self.emitter.label(loop_label)
        self.emitter.cmp(immediate_const(0), self._arg_count_location())
        self.emitter.jump_equal(done_label)
        self.emitter.sub(immediate_const(1), self._arg_count_location())
        self.emitter.comment("Loading next argument")
        self.load_next_optional_argument(first_optional_arg)
        self.emitter.comment("Wrap optional arg into pari")
        self.wrap_optional_argument_into_pair(RAX, result, si.copy())
        self.emitter.comment("Saving result")
        self.emitter.mov(RAX, offset(RBP, result.get_index()))
        self.emitter.jmp(loop_label)

        self.emitter.label(done_label)
        self.emitter.mov(offset(RBP, result.get_index()), RAX)

    def load_next_optional_argument(self, first_optional_arg):
        self.emitter.comment("Load arg offset")
        self.emitter.mov(self._arg_count_location(), RAX)
        self.emitter.comment("Load next arg")
        self.emitter.mov(offset_register(RBP,first_optional_arg.get_index(),
                                         RAX, Compiler.WORDSIZE),
                         RAX)

    def wrap_optional_argument_into_pair(self, car_location, cdr_location, si):
        self.emitter.mov(car_location, offset(RSP, si.get_index()))
        self.emitter.comment("Make next cons cell")
        car_location = si.push()
        self.make_pair(offset(RBP, car_location.get_index()), offset(RBP, cdr_location.get_index()), si)

    def get_closure_arguments(self, args):
        dot = YarpenSymbol(".")
        if dot not in args:
            return False, args
        if args.index(dot) != len(args) - 2:
            raise Exception("Invalid location of the dot in list of arguments.")
        args.remove(dot)
        return True, args

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
            self.alloc_memory(stack.copy(), Compiler.WORDSIZE)
            self.emitter.mov(RAX, RDX)
            self.load_from_stack(arg_index)
            self.emitter.mov(RAX, offset(RDX, 0))
            self.emitter.mov(RDX, RAX)
            self.save_on_stack(Stack(arg_index))

    def extend_env_for_closure(self, closure_args, env):
        extended_env = env
        # First position past RBP is return address, one before that the
        # number of arguments to the function. The next position contains
        # the first argument.
        stack = Stack()
        stack.push()
        stack.push()
        for arg in closure_args:
            if arg == YarpenSymbol("."):
                continue
            extended_env = extended_env.extend(arg, -stack.get_index())
            stack.push()
        return extended_env

    def emit_closure(self, expr, env, stack, tail_position):
        self.compile_expr(expr.expressions[0], env, stack, False)
        self.emitter.comment("FOO")
        self.untag_closure(RAX)
        self.save_on_stack(stack)
        closure_stack_index = stack
        return closure_stack_index

    def untag_closure(self, register):
        self.emitter.sub(immediate_const(Compiler.CLOSURE_TAG), register)

    def save_closure_register(self, stack):
        self.emitter.mov(RBX, RAX)
        self.save_on_stack(stack)
        return stack

    def emit_closure_app(self, closure_si, stack):
        self.load_from_stack(closure_si.get_index())
        self.emitter.mov(RAX, RBX)
        self.emitter.mov(offset(RAX, Compiler.WORDSIZE), RAX)
        self.adjust_base(stack.get_index())
        self.emitter.call(dereference(RAX))
        self.adjust_base(- stack.get_index())

    def compile_application(self, expr, env, stack, tail_position):
        self.emitter.comment("Application: " + str(expr) + " is tail position: " + str(tail_position))
        closure_si = self.emit_closure(expr, env, stack.copy(), tail_position)
        stack.push()
        if tail_position:
            self.emit_tail_call(expr, env, stack.copy(), closure_si)
        else:
            self.emit_call(expr, env, stack.copy(), closure_si)

    def emit_tail_call(self, expr, env, stack, closure_si):
        self.emitter.comment("Emit args: %s" % str(expr.expressions[1:]))
        stack = self.emit_application_arguments(expr.expressions[1:],
                                                env, stack)
        stack.push()
        args_size = len(expr.expressions) - 1
        self.emitter.comment("Saving number of arguments :%d" % args_size)
        self.emitter.mov(immediate_const(args_size), offset(RBP, stack.get_index()))
        stack.push()
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
        stack.push()
        self.emitter.comment("Emit args.")
        stack = self.emit_application_arguments(expr.expressions[1:],
                                                env, stack)
        stack.push()
        args_size = len(expr.expressions) - 1
        self.emitter.comment("Saving number of arguments :%d" % args_size)
        self.emitter.mov(immediate_const(args_size), offset(RBP, stack.get_index()))
        stack_before_function_call = stack.push()
        closure_register_si = self.save_closure_register(stack.copy())
        self.emit_closure_app(closure_si, stack_before_function_call)
        stack.push()
        self.save_on_stack(stack)
        self.load_from_stack(closure_register_si.get_index())
        self.emitter.mov(RAX, RBX)
        self.load_from_stack(stack.get_index())

    def shift_arguments_for_tail_call(self, stack, arguments):
        self.emitter.comment("Save old RBP")
        self.emitter.mov(offset(RBP, 0), R12)
        self.emitter.comment("Save return address")
        self.emitter.mov(offset(RBP, Compiler.WORDSIZE), RDI)

        src = Stack(stack.get_index() + ((len(arguments) + 1)* Compiler.WORDSIZE))

        self.emitter.comment("Load number of arguments in current frame")
        self.emitter.mov(self._arg_count_location(), RCX)
        self.emitter.comment("To find offset to the first arg, account for return address and saved RBP")
        self.emitter.add(immediate_const(2), RCX)

        self.emitter.comment("Shift arguments and args count")
        for arg in range(len(arguments) + 1):
            self.emitter.comment("Shifting argument")
            self.load_from_stack(src.get_index())
            self.emitter.mov(RAX, offset_register(RBP, 0, RCX,
                                                  Compiler.WORDSIZE))
            self.emitter.sub(immediate_const(1), RCX)
            src.push()

        self.emitter.comment("Saving return address")
        self.emitter.mov(RDI, offset_register(RBP, 0, RCX,
                                              Compiler.WORDSIZE))

        self.emitter.comment("Reset RSP")
        self.emitter.lea(offset_register(RBP, 0, RCX,
                                         Compiler.WORDSIZE), RSP)

        self.emitter.comment("Restore od RBP")
        self.emitter.mov(R12, RBP)

    def emit_application_arguments(self, args, env, stack):
        for arg in reversed(args):
            stack.push()
            self.emitter.comment("Compiling argument " + str(arg))
            self.compile_expr(arg, env, stack, False)
            self.emitter.comment("Saving argument " + str(arg))
            self.save_on_stack(stack)
        return stack

    def save_on_stack(self, stack):
        self.emitter.mov(RAX, offset(RBP, stack.get_index()))

    def load_from_stack(self, stack_index):
        self.emitter.mov(offset(RBP, stack_index), RAX)

    def adjust_base(self, si):
        if si > 0:
            self.emitter.add(immediate_const(si), RSP)
        else:
            self.emitter.sub(immediate_const(-si), RSP)

    def _get_transformed_source(self):
        return [self._code_transformer.transform(e) for e in self.parser.parse()]

    def _arg_count_location(self):
        return offset(RBP, 2*Compiler.WORDSIZE)


class LabelGenerator:
    def __init__(self):
        self.counter = 0

    def unique_label(self, prefix):
        label = prefix + ("__%d" % self.counter)
        self.counter += 1
        return label
