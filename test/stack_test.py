from unittest import TestCase
from yarpen.stack import Stack

class StackTest(TestCase):

    def initial_stack_index_test(self):
        self.assertEqual(Stack().get_index(), -8)

    def grow_decrements_index_test(self):
        s = Stack()
        s.grow()
        self.assertEqual(s.get_index(), -16)

    def grow_returns_stack_with_unmodified_index_test(self):
        s = Stack()
        previous = s.grow()
        self.assertEqual(previous.get_index(), -8)

    def shrink_increments_index_test(self):
        s = Stack()
        s.shrink()
        self.assertEqual(s.get_index(), 0)

    def shrink_returns_stack_with_unmodified_index_test(self):
        s = Stack()
        previous = s.shrink()
        self.assertEqual(previous.get_index(), -8)

    def copy_returns_new_copy_test(self):
        s = Stack()
        s2 = s.copy()
        s.grow()
        self.assertEqual(s.get_index(), -16)
        self.assertEqual(s2.get_index(), -8)
