class Stack(object):
    WORDSIZE = 8
    def __init__(self, index=-WORDSIZE):
        self._index = index

    def get_index(self):
        return self._index;

    def grow(self):
        s = Stack(self._index)
        self._index -= Stack.WORDSIZE
        return s

    def shrink(self):
        s = Stack(self._index)
        self._index += Stack.WORDSIZE
        return s

    def copy(self):
        return Stack(self._index)
