from parser import Parser


class Compiler(object):
    def __init__(self, source):
        self.parser = Parser(source)

    def compile(self):
        self.exprs = self.parser.parse()
        return """    .text
    .globl pyscm_start
    .type pyscm_start @function
     pyscm_start:
     ret
"""
