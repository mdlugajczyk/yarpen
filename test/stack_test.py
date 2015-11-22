from unittest import TestCase
from yarpen.stack import Stack

class StackTest(TestCase):

    def initial_stack_index_test(self):
        self.assertEqual(Stack().get_index(), -8)

    def grows_downwards_by_wordsize_test(self):
        s = Stack()
        self.assertEqual(s.next().get_index(), -16)

    def each_stack_maintains_separate_index_test(self):
        s = Stack()
        s2 = s.next()
        s2 = s2.next()
        self.assertEqual(s2.get_index(), -24)
        self.assertEqual(s.get_index(), -8)
