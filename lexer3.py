#!/usr/bin/env python3

import functools
import re

from hypothesis import given
import hypothesis.strategies as st

class RegExp:
    def __init__(self, **kwargs):
        for key, value in kwargs.items():
            setattr(self, key, value)

    def __str__(self):
        return StrVisitor().visit(self)

class REempty(RegExp):
    def __init__(self):
        super().__init__()

class REsymbol(RegExp):
    def __init__(self, symbol):
        super().__init__(symbol=symbol)

class REconcat(RegExp):
    def __init__(self, left, right):
        super().__init__(left=left, right=right)

class REalt(RegExp):
    def __init__(self, left, right):
        super().__init__(left=left, right=right)

class REstar(RegExp):
    def __init__(self, expr):
        super().__init__(expr=expr)

class RegExpVisitor:
    def visit(self, node, *args, **kwargs):
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node, *args, **kwargs)

    def generic_visit(self, node, *args, **kwargs):
        raise Exception('No visit_{} method'.format(type(node).__name__))

class StrVisitor(RegExpVisitor):
    class decorators:
        @staticmethod
        def paren(p):
            def decorator_paren(func):
                @functools.wraps(func)
                def wrapper(self, node, prec=0):
                    result = func(self, node, prec)
                    return '({})'.format(result) if prec > p else result
                return wrapper
            return decorator_paren

    def visit_REempty(self, node, prec=0):
        return 'ε'

    def visit_REsymbol(self, node, prec=0):
        return node.symbol

    @decorators.paren(1)
    def visit_REconcat(self, node, prec=0):
        return '{}{}'.format(self.visit(node.left, 2),
                             self.visit(node.right, 1))

    @decorators.paren(0)
    def visit_REalt(self, node, prec=0):
        return '{}|{}'.format(self.visit(node.left, 1),
                              self.visit(node.right, 0))

    @decorators.paren(2)
    def visit_REstar(self, node, prec=0):
        return '{}*'.format(self.visit(node.expr, 2))

class GenCorrectVisitor(RegExpVisitor):
    def visit_REempty(self, node, draw, n):
        return ''

    def visit_REsymbol(self, node, draw, n):
        return node.symbol

    def visit_REconcat(self, node, draw, n):
        k = draw(st.integers(min_value=0, max_value=n))
        return self.visit(node.left, draw, k) + self.visit(node.right, draw, n-k)

    def visit_REalt(self, node, draw, n):
        return draw(st.one_of(st.just(self.visit(node.left, draw, n)),
                              st.just(self.visit(node.right, draw, n))))

    def visit_REstar(self, node, draw, n):
        k = draw(st.integers(min_value=0, max_value=n))
        return ''.join(self.visit(node.expr, draw, n // k) for _ in range(k))

@st.composite
def correct(draw, r, size=50):
    return GenCorrectVisitor().visit(r, draw, size)

class GenWrongVisitor(RegExpVisitor):
    def visit_REempty(self, node, draw, n):
        return ""

    def visit_REsymbol(self, node, draw, n):
        az09 = "abc"
        #az09 = "abcdef0123456789" #THIS IS TOO MUCH FOR HYPOTHESIS
        minus = az09.replace(node.symbol, "")
        return draw(st.one_of(
            st.characters(whitelist_categories=[], whitelist_characters=minus),
            st.text(max_size=0) #return ""
        ))

    def visit_REconcat(self, node, draw, n):
        #frequency doesn't exist, so unless i draw an integer treat it as a probability i can't choose frequencies
        k = draw(st.integers(min_value=0, max_value=n))
        return draw(st.one_of(
            st.just(self.visit(node.left, draw, k) + GenCorrectVisitor().visit(node.right, draw, n-k)),
            st.just(GenCorrectVisitor().visit(node.left, draw, k) + self.visit(node.right, draw, n-k)),
            st.just(self.visit(node.left, draw, k) + self.visit(node.right, draw, n-k))
        ))

    def visit_REalt(self, node, draw, n):
        return draw(st.one_of(st.just(self.visit(node.left, draw, n)),
                              st.just(self.visit(node.right, draw, n))))

    def visit_REstar(self, node, draw, n):
        k = draw(st.integers(min_value=0, max_value=n))
        k2 = max(1, k)
        nk = n // k2
        c = draw(st.integers(min_value=0, max_value=k2-1))
        cs = ''.join(GenCorrectVisitor().visit(node.expr, draw, nk) for _ in range(c))
        w = self.visit(node.expr, draw, nk)
        ss = ''.join(self.visit(node.expr, draw, nk) for _ in range(k2-c-1))
        return cs + w + ss

@st.composite
def wrong(draw, r, size=50):
    s = GenWrongVisitor().visit(r, draw, size)
    while re.match(r"^"+str(r)+"$", s):
        s = GenWrongVisitor().visit(r, draw, size)
    return s

def test(r):
    @given(s=correct(r))
    def test_correct(s):
        # Check if s matches r.
        assert re.match(r"^"+str(r)+"$", s), \
            "Generated string \"{}\" does not match regexp {}".format(s, r)
        print(s)

    @given(s=wrong(r))
    def test_wrong(s):
        # Check if s doesn't match r.
        assert not re.match(r"^"+str(r)+"$", s), \
            "Generated string \"{}\" matches regexp {}".format(s, r)
        print(s)
    
    print('Testing:', r)
    print('Some correct inputs:')
    test_correct()
    print('Some wrong inputs:')
    test_wrong()

if __name__ == '__main__':
    test(REconcat(REsymbol('0'), REstar(REsymbol('1'))))
    test(REconcat(REstar(REalt(REsymbol('0'), REsymbol('1'))),
         REconcat(REsymbol('2'), REstar(REalt(REsymbol('0'), REsymbol('1'))))))
