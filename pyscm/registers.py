RAX = "%rax"
RBX = "%rbx"
RDX = "%rdx"
RDI = "%rdi"
RSP = "%rsp"
AL = "%al"


def offset(reg, offset):
    return '{0}({1})'.format(offset, reg)


def immediate_const(const):
    return '${0}'.format(const)


def dereference(reg):
    return '*{0}'.format(reg)
