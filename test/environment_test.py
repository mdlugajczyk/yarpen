from __future__ import absolute_import
from yarpen.environment import Environment
from unittest import TestCase


class EnvironmentTest(TestCase):

    def test_returns_none_for_non_existent_variable(self):
        env = Environment()
        self.assertRaises(KeyError, env.get_var, "foo")

    def test_extend_new(self):
        env = Environment()
        extended = env.extend("foo", "bar")
        self.assertEqual(extended.get_var("foo"), "bar")

    def test_extend_overwrite(self):
        env = Environment(bindings={"foo": "bar"})
        extended = env.extend("foo", "fnord")
        self.assertEqual(extended.get_var("foo"), "fnord")
