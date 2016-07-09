from __future__ import absolute_import
from __future__ import print_function
from subprocess import call, check_output

tests = []

runtime_file = "runtime/yarpen_runtime.c"


def add_integration_test(name, prog, output):
    tests.append((name, prog, output))


def add_group(name, tests):
    for t in tests:
        add_integration_test(name, t[0], t[1])


def run_tests():
    for test in reversed(tests):
        print(("Working on test: %s" % test[0]))
        with open("yarpen_code.s", "w") as f:
            f.write(test[1])
        compilation_status = call(["./bin/yarpen", "yarpen_code.s"])
        if compilation_status != 0:
            print("Failed to compile test.")
            break
        try:
            output = check_output("./yarpen_code.s.out").decode('ascii')
            if output != test[2]:
                print(("Test failed. Expected %s got %s\nCode: %s"
                       % (test[2], output, test[1])))
                break
        except Exception as e:
            print(("Test failed to execute properly. Expected %s got %s\nCode: %s"
                   % (test[2], str(e), test[1])))
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
           ("(integer? (lambda () 3))", "#f"),
           ("(integer? (cons 1 2))", "#f"),
           ("(integer? '())", "#f"),
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
           ("(fx* 0 1)", "0"),
           ("(zero? 0)", "#t"),
           ("(zero? 1)", "#f"),
           ("(zero? -1)", "#f"),
           ("(zero? #t)", "#f"),
           ("(zero? #f)", "#f"),
           ("(zero? (let ((f (lambda () 3))) f))", "#f"),
           ("(zero? (cons 1 2))", "#f"),
           ("(zero? '())", "#f")])


add_group("Let expression.",
          [("(let ((x 3)) x)", "3"),
           ("(let ((x 3)) (let ((x 4)) x))", "4"),
           ("(let ((x 3)) (let ((y (fx+ x x))) y))", "6"),
           ("""(let ((x (fx+ 1 2)))
                                 (let ((y (fx+ 3 4)))
                                   (fx+ x y)))""", "10"),
           ("""(let ((x (fx+ 1 2)))
                                 (let ((y (fx+ 3 4)))
                                   (fx- y x)))""", "4"),
           ("""(let ((t (let ((t (let ((t (let ((t (fx+ 1 2))) t))) t))) t)))
                   t)""",
            "3"),
           ("""(let ((x 12))
                                 (let ((x (fx+ x x)))
                                   (let ((x (fx+ x x)))
                                     (let ((x (fx+ x x)))
                                       (fx+ x x)))))""",
            "192")])

add_group("If",
          [("(if #t 1 2)", "1"),
           ("(if #f 1 2)", "2"),
           ("(if 1 1 2)", "1"),
           ("(if (if 0 #t #f) 1 2)", "1"),
           ("""(if (let ((x #t) (y #f))
                      (if #t x y))
                   1
                   2)""", "1")])


add_group("lambda",
          [("(let ((f (lambda (x) (fx+ x x)))) (f 2))", "4"),
           ("((lambda (x) (fx+ x x)) 2)", "4"),
           ("""(let ((f (lambda (x g) (g (g x))))
                     (h (lambda (x) (fx+ x x))))
                  (f 2 h))""", "8"),
           ("""(let ((f (lambda (x) (fx+ x 1)))
                     (g (lambda (x) (fx+ x x)))
                     (h (lambda (x) (fx- x 1))))
                  (h (g (f 10))))""", "21"),
           ("(let () 1 (fx+ 2 1))", "3")])

add_group("begin",
          [("(begin 1 2)", "2"),
           ("(begin #t #f)", "#f")])

add_group("set!",
          [("""(let ((x 1))
          (set! x (fx+ x 1))
              x)""", "2"),
           ("""(let ((x 1))
               (let ((x 2))
                   (set! x 3)
                   x)
               x)""", "1")])


add_group("closures",
          [("""(let ((counter-generator (lambda ()
                     (let ((cnt 0))
                         (lambda ()
                           (set! cnt (fx+ cnt 1))
                           cnt)))))
  (let ((cnt1 (counter-generator))
        (cnt2 (counter-generator)))
    (cnt1)
    (cnt1)
    (cnt1)
    (cnt2)
    (cnt2)))""", "2"),
           ("""(let ((Y (lambda (X)
    ((lambda (procedure)
       (X (lambda (arg) ((procedure procedure) arg))))
     (lambda (procedure)
       (X (lambda (arg) ((procedure procedure) arg))))))))
  (let ((F* (lambda (func-arg)
	    (lambda (n)
	      (if (zero? n)
		  1
		  (fx* n (func-arg (fx- n 1))))))))
  (let ((fact
   (Y F*))) (fact 5))))""", "120"),
           ("""(let ((x 3))
           (let ((z 4))
               (let ((y (lambda () x)))
                 (y))))""", "3"),
        ("""(let ((x 3))
                 (let ((y (lambda () x)))
                   (set! x 5)
                   (y)))""", "5")])


add_group("many arguments",
          [("""(let ((f (lambda (a b c d e f g h i j k l m)
                        (fx+ a (fx+ b (fx+ c (fx+ d (fx+ e (fx+ f (fx+ g (fx+ h (fx+ i (fx+ j (fx+ k (fx+ l m)))))))))))))))
            (f 1 2 3 4 5 6 7 8 9 10 11 12 13))""",
            "91"),
          ("""(let ((x 1)
          (y 2)
          (z 3)
          (a 4)
          (b 5)
          (c 6)
          (d 7))
          (fx+ x (fx+ y (fx+ z (fx+ a (fx+ b (fx+ c d)))))))""", "28")])

add_group("let*",
          [("(let* ((x 1) (y (fx+ x x))) y)", "2"),
           ("(let* ((x 1) (y (fx+ x x)) (z (fx+ y y ))) z)", "4")])

add_group("TCO",
          [("""(letrec ((odd (lambda (n) (if (zero? n) #f (even (fx- n 1)))))
                      (even (lambda (n) (if (zero? n) #t (odd (fx- n 1))))))
              (even 100000000))""", "#t"),
          ("""(letrec ((odd (lambda (n) (if (zero? n) #f (even (fx- n 1)))))
                      (even (lambda (n) (if (zero? n) #t (odd (fx- n 1))))))
              (even 100000001))""", "#f"),
          ("""(letrec ((odd (lambda (n) (if (zero? n) #f (even (fx- n 1)))))
                      (even (lambda (n) (if (zero? n) #t (odd (fx- n 1))))))
              (odd 112345671))""", "#t"),
          ("""(letrec ((odd (lambda (n) (if (zero? n) #f (even (fx- n 1)))))
                      (even (lambda (n) (if (zero? n) #t (odd (fx- n 1))))))
              (odd 198765431))""", "#t")])

add_group("closure?",
          [("(closure? 0)", "#f"),
           ("(closure? 1)", "#f"),
           ("(closure? -10)", "#f"),
           ("(closure? #t)", "#f"),
           ("(closure? #f)", "#f"),
           ("(closure? (let ((f (lambda () 2))) f))", "#t"),
           ("(closure? (cons 1 2))", "#f"),
           ("(closure? '())", "#f")])

add_group("cons",
          [("(car (cons 1 2))", "1"),
           ("(cdr (cons 1 2))", "2"),
           ("(let ((x (cons 1 2))) (car x))", "1"),
           ("(let ((x (cons 1 2))) (cdr x))", "2"),
           ("(let ((x (cons 1 (cons 2 3)))) (car x))", "1"),
           ("(let ((x (cons 1 (cons 2 3)))) (car (cdr x)))", "2"),
           ("(let ((x (cons 1 (cons 2 3)))) (cdr (cdr x)))", "3"),
           ("(let ((x (cons 1 (cons 2 3)))) (car (cdr (cons x x))))", "1"),
           ("(cons? 0)", "#f"),
           ("(cons? 1)", "#f"),
           ("(cons? -10)", "#f"),
           ("(cons? #t)", "#f"),
           ("(cons? #f)", "#f"),
           ("(cons? (let ((f (lambda () 2))) f))", "#f"),
           ("(cons? (cons 1 2))", "#t"),
           ("(let ((x (cons 1 2))) (set-car! x 10) (car x))", "10"),
           ("(let ((x (cons 1 2))) (set-car! x 10) (cdr x))", "2"),
           ("(let ((x (cons 1 2))) (set-cdr! x 10) (car x))", "1"),
           ("(let ((x (cons 1 2))) (set-cdr! x 10) (cdr x))", "10"),
           ("(letrec ((x (cons 1 2)) (f (lambda () (car x)))) (set-car! x 3) (f))", "3"),
           ("(letrec ((x (cons 1 2)) (y (cons x x))) (set-car! x 10) (cons (car (car y)) (car (cdr y))))", "(10 . 10)"),
           ("(letrec ((x (cons 1 2)) (y (cons x x))) (set-car! x 10) (cons (cdr (car y)) (cdr (cdr y))))", "(2 . 2)"),
           ("(cons 1 '())", "(1)"),
           ("(cons 1 (cons 2 (cons 3 '())))", "(1 2 3)"),
           ("(cons (cons 1 2) (cons 3 (cons (cons 4 5) '())))", "((1 . 2) 3 (4 . 5))")])

add_group("nil",
          [("'()", "()"),
           ("(nil? '())", "#t"),
           ("(nil? 0)", "#f"),
           ("(nil? 1)", "#f"),
           ("(nil? -10)", "#f"),
           ("(nil? #t)", "#f"),
           ("(nil? #f)", "#f"),
           ("(nil? (let ((f (lambda () 2))) f))", "#f"),
           ("(nil? (cons 1 2))", "#f")])

add_group('characters', [('#\\a', '#\\a'),
                         ('#\\b', '#\\b'),
                         ('#\\3', '#\\3'),
                         ('#\\space', ' '),
                         ('#\\newline', "\n"),
                         ("(char? 0)", "#f"),
                         ("(char? 1)", "#f"),
                         ("(char? -10)", "#f"),
                         ("(char? #t)", "#f"),
                         ("(char? #f)", "#f"),
                         ("(char? (let ((f (lambda () 2))) f))", "#f"),
                         ("(char? (cons 1 2))", "#f"),
                         ("(char? #\\a)", "#t"),
                         ("(char? #\\b)", "#t"),
                         ("(char? #\\1)", "#t"),
                         ("(char? #\\space)", "#t"),
                         ("(char? #\\newline)", "#t")])

add_group('dot in func arguments',
           [("((lambda (x . y) y) 100)", "()"),
            ("(let ((f (lambda (x . y) y))) (f 100))", "()"),
            ("((lambda (x . y) (let ((z 2)) (cons z y))) 100)", "(2)"),
            ("((lambda (x . y) (let ((a 2) (b 3) (c 4)) (cons a (cons b (cons c y))))) 100)", "(2 3 4)"),
            # ("(let ((f (lambda (x y . z) z))) (f 100 200))", "()"),
            # ("(let ((f (lambda (x y z . a) a))) (f 100 200 300))", "()"),
            # ("(let ((f (lambda (x y z . a) a))) (f 100 200 300))", "()"),
            # ("(let ((f (lambda (x . y) y))) (f 100 200))", "(200)"),
            # ("(let ((f (lambda (x . y) y))) (f 100 200 300 400))", "(200 300 400)"),
            # ("(let ((f (lambda (x y z . a) a))) (f 100 200 300))", "()"),
            # ("(let ((f (lambda (x y z . a) a))) (f 100 200 300 400))", "(400)"),
            # ("(let ((f (lambda (x . y) y))) (f 100 200 300))", "(200 300)"),
            # ("(let ((f (lambda (. x) x))) (f 100 200 300))", "(100 200 300)")
           ])

run_tests()
