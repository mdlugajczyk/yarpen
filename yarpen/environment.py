class Environment:
    def __init__(self, bindings={}, parent=None):
        self.bindings = bindings
        self.parent = parent

    def get_var(self, var):
        if var in self.bindings:
            return self.bindings[var]
        if self.parent:
            return self.parent.get_var(var)
        return None

    def set_var(self, var, val):
        self.bindings[var] = val

    def extend(self, var, val):
        return Environment(bindings={var: val},
                           parent=self)
