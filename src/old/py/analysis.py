basepath = '/home/corey/src/spp/data'

def datafile(basename):
    return '%s/%s.LST' % (basepath,  basename)


def chomp(s):
    '''Remove the trailing character from s if its a linefeed.'''
    if s[-1] == '\n':
        return s[:-1]
    else:
        return s


def readwords(basename):
    with open(datafile(basename)) as f:
        words = [chomp(s) for s in f.readlines()]
        words.sort()
        return words


def prefixes(w):
    return [w[0:i] for i in range(1,len(w))]

def suffixes(w):
    return [w[i:] for i in range(1,len(w))]


def starts(prefix, w):
    return prefix in prefixes(w) or w.startswith(prefix)

class WordList():

    def __init__(self, words):
        self.words = words

    def starting(self, prefix):
        return [w for w in self.words if starts(prefix, w)]


class State():

    def __init__(self, lhs, rhs):
        self.lhs = lhs
        self.rhs = rhs
        self.neighbors = []


class ASPair():

    def __init__(self, action, state):
        self.action = action
        self.state = state


def states(words):
    pass

