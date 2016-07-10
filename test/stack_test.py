from unittest import TestCase
from yarpen.stack import Stack

class StackTest(TestCase):

    def initial_stack_index_test(self):
        self.assertEqual(Stack().get_index(), -8)

    def grows_downwards_by_wordsize_test(self):
        s = Stack()
        self.assertEqual(s.get_next_stack().get_index(), -16)

    def previous_index_test(self):
        s = Stack().get_next_stack()
        self.assertEqual(s.get_prev_stack().get_index(), -8)

    def each_stack_maintains_separate_index_test(self):
        s = Stack()
        s2 = s.get_next_stack()
        s2 = s2.get_next_stack()
        self.assertEqual(s2.get_index(), -24)
        self.assertEqual(s.get_index(), -8)

    def push_decrements_index_test(self):
        s = Stack()
        s.push()
        self.assertEqual(s.get_index(), -16)

    def push_returns_stack_with_unmodified_index_test(self):
        s = Stack()
        previous = s.push()
        self.assertEqual(previous.get_index(), -8)
