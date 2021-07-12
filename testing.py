import re
import sys
import hypothesis.strategies as st
from hypothesis import given, settings, HealthCheck

from regex_parser import DIGITS, Token, TT_ALT, TT_CONCAT, TT_STAR, TT_PLUS, \
                         CharNode, UnaryOpNode, BinOpNode
from automata import regex_to_DFA
from generate import positive, negative

def gen_regex(max_leaves=10):
    return st.recursive(
        st.sampled_from(DIGITS).map(CharNode),
        lambda G:
            st.tuples(G, G).map(lambda t: BinOpNode(t[0], Token(TT_ALT), t[1]))
          | st.tuples(G, G).map(lambda t: BinOpNode(t[0], Token(TT_CONCAT), t[1]))
          | G.map(lambda r: UnaryOpNode(r, Token(TT_STAR)))
          | G.map(lambda r: UnaryOpNode(r, Token(TT_PLUS))),
        max_leaves=max_leaves
    ).map(str)

@settings(deadline=None)
@given(r=gen_regex())
def test_test(r):
    print(r)
    dfa = regex_to_DFA(r)

    @settings(suppress_health_check=HealthCheck.all())
    @given(s=positive(dfa, max_size=30))
    def test_positive(s):
        t = ''.join(s)
        if print_it: print('    ', t)
        # Check if t recognized by r.
        assert dfa.accepts(t), \
            'Generated string \'{}\' does not match regexp {}'.format(t, str(r))
        assert re.match(r"^"+str(r)+"$", t), \
            "Generated string \"{}\" does not match regexp {}".format(t, str(r))

    @settings(suppress_health_check=HealthCheck.all())
    @given(s=negative(dfa, max_size=30))
    def test_negative(s):
        t = ''.join(s)
        if print_it: print('    ', t)
        # Check if t not recognized by r.
        #re.match does not work here (tries to match first instance)
        assert not dfa.accepts(t), \
            'Generated string \'{}\' does not match regexp {}'.format(t, str(r))

    test_positive()
    test_negative()

@given(r=gen_regex())
def test_regex(r):
    print(r)

if __name__ == '__main__':
    n = len(sys.argv)
    print_it = False
    if n == 2 and sys.argv[1]=='print':
        print_it = True

    #test_regex()

    test_test()
    print('Passed all tests!')
