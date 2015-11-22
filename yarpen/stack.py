class Stack(object):
    WORDSIZE = 8
    def __init__(self, index=-WORDSIZE):
        self._index = index

    def get_index(self):
        return self._index;

    def next(self):
        return Stack(self._index - Stack.WORDSIZE)
