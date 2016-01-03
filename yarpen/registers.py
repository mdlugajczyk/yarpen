RAX = "%rax"
RBX = "%rbx"
RCX = "%rcx"
RDX = "%rdx"
RDI = "%rdi"
RSP = "%rsp"
RBP = "%rbp"
R8 = "%r8"
R9 = "%r9"
R10 = "%r10"
R11 = "%r11"
R12 = "%r12"
R13 = "%r13"
R14 = "%r14"
R15 = "%r15"
AL = "%al"


def offset(reg, offset):
    return '{0}({1})'.format(offset, reg)


def offset_register(base_reg, displacement, offset_reg, offset_multiplier):
    return '{0}({1}, {2}, {3})'.format(displacement, base_reg, offset_reg, offset_multiplier)


def immediate_const(const):
    return '${0}'.format(const)


def dereference(reg):
    return '*{0}'.format(reg)
