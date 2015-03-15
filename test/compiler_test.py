import sys
sys.path.insert(0, '.')

from subprocess import call, check_output
from pyscm.compiler import Compiler

tests = []

runtime_file = "runtime/pyscm_runtime.c"


def add_integration_test(name, prog, output):
    tests.append((name, prog, output))


def add_group(name, tests):
    for t in tests:
        add_integration_test(name, t[0], t[1])


def run_tests():
    for test in tests:
        print("Working on test: %s" % test[0])
        compiler = Compiler(test[1])
        code = compiler.compile()
        with open("pyscm_code.s", "w") as f:
            f.write(code)
        compilation_status = call(["gcc", "runtime/pyscm_runtime.c",
                                   "pyscm_code.s", "-o", "pyscm_test"])
        if compilation_status != 0:
            print("Failed to compile test.")
            break

        output = check_output("./pyscm_test")
        if output != test[2]:
            print("Test failed. Expected %s got %s" % (test[2], output))
            break

add_group("Integers", [("1", "1"),
                       ("44", "44"),
                       ("3", "3"),
                       ("-100", "-100"),
                       ("0", "0"),
                       ("-999999999999999999", "-999999999999999999"),
                       ("999999999999999999", "999999999999999999")])

add_group("Booleans", [("#t", "#t"),
                       ("#f", "#f")])

add_group("Integer functions.",
          [("(integer? 3)", "#t"),
           ("(integer? -10)", "#t"),
           ("(integer? #f)", "#f"),
           ("(integer? #t)", "#f"),
           ("(fx+ 2 3)", "5"),
           ("(fx+ -2 3)", "1"),
           ("(fx+ -10 10)", "0"),
           ("(fx+ -100 -100)", "-200"),
           ("(fx+ 100 234)", "334"),
           ("(fx- 100 234)", "-134"),
           ("(fx- 100 100)", "0"),
           ("(fx- -1 -2)", "1"),
           ("(fx- 10 3)", "7"),
           ("(fx* 10 3)", "30"),
           ("(fx* -10 3)", "-30"),
           ("(fx* -10 -10)", "100"),
           ("(fx* 10 1)", "10"),
           ("(fx* 0 1)", "0")])

run_tests()
