from yarpen.expression import is_boxed_value


class Environment:
    def __init__(self, bindings={}, parent=None):
        self.bindings = bindings
        self.parent = parent

    def get_var(self, var):
        if is_boxed_value(var):
            var = var.boxed_value
        if var in self.bindings:
            return self.bindings[var]
        if self.parent:
            return self.parent.get_var(var)
        raise KeyError("Undefined variable %s" % var)

    def set_var(self, var, val):
        if is_boxed_value(var):
            var = var.boxed_value
        self.bindings[var] = val

    def extend(self, var, val):
        if is_boxed_value(var):
            var = var.boxed_value
        return Environment(bindings={var: val},
                           parent=self)

    def __repr__(self):
        return self.bindings.__repr__() + self.parent.__repr__()
