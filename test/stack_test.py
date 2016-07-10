from unittest import TestCase
from yarpen.stack import Stack

class StackTest(TestCase):

    def initial_stack_index_test(self):
        self.assertEqual(Stack().get_index(), -8)

    def push_decrements_index_test(self):
        s = Stack()
        s.push()
        self.assertEqual(s.get_index(), -16)

    def push_returns_stack_with_unmodified_index_test(self):
        s = Stack()
        previous = s.push()
        self.assertEqual(previous.get_index(), -8)

    def pop_increments_index_test(self):
        s = Stack()
        s.pop()
        self.assertEqual(s.get_index(), 0)

    def pop_returns_stack_with_unmodified_index_test(self):
        s = Stack()
        previous = s.pop()
        self.assertEqual(previous.get_index(), -8)

    def copy_returns_new_copy_test(self):
        s = Stack()
        s2 = s.copy()
        s.push()
        self.assertEqual(s.get_index(), -16)
        self.assertEqual(s2.get_index(), -8)
