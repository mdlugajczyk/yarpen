class Stack(object):
    WORDSIZE = 8
    def __init__(self, index=-WORDSIZE):
        self._index = index

    def get_index(self):
        return self._index;

    def get_next_stack(self):
        return Stack(self._index - Stack.WORDSIZE)

    def get_prev_stack(self):
        return Stack(self._index + Stack.WORDSIZE)

    def push(self):
        s = Stack(self._index)
        self._index -= Stack.WORDSIZE
        return s

    def pop(self):
        s = Stack(self._index)
        self._index += Stack.WORDSIZE
        return s

    def copy(self):
        return Stack(self._index)
