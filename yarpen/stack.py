class Stack(object):
    WORDSIZE = 8
    def __init__(self, index=-WORDSIZE):
        self._index = index

    def get_index(self):
        return self._index;

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
