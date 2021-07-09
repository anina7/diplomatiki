import sys
import hypothesis.strategies as st
from hypothesis import given, settings

from regex_parser import DIGITS
from automata import DFA, regex_to_DFA
from generate import positive, negative

@st.composite
def gen_regex(draw, max=5):
    regex = draw(st.lists(st.just(None), min_size=1, max_size=max))
    
    paren_counter = 0
    
    regex[0] = draw(st.text(alphabet=DIGITS, min_size=1, max_size=1))
    for i in range(1, len(regex)):
        if paren_counter > 0:
            PAREN = ')'
        else:
            PAREN = ''
        
        if regex[i-1] in DIGITS or regex[i-1]==')':
            regex[i] = draw(st.text(alphabet=DIGITS+'|*+'+PAREN, min_size=1, max_size=1))
        elif regex[i-1]=='|':
            regex[i] = draw(st.text(alphabet=DIGITS+'(', min_size=1, max_size=1))
        elif regex[i-1]=='*' or regex[i-1]=='+':
            regex[i] = draw(st.text(alphabet=DIGITS+'|('+PAREN, min_size=1, max_size=1))
        elif regex[i-1]=='(':
            regex[i] = draw(st.text(alphabet=DIGITS, min_size=1, max_size=1))
        else:
            regex[i] = draw(st.text(alphabet=DIGITS+'|*+('+PAREN, min_size=1, max_size=1))
        
        if regex[i] == '(':
            paren_counter += 1
        if regex[i] == ')':
            paren_counter -= 1

    #if alt is last symbol, discard it
    if regex[-1] == '|':
        regex.pop()
    
    #while parenthesis is last symbol, discard it
    while regex[-1] == '(':
        regex.pop()
        paren_counter -= 1
    
    #close all opened paretheses
    while paren_counter > 0:
        regex.append(')')
        paren_counter -= 1
        
    return "".join(regex)

@settings(deadline=1000)
@given(r=gen_regex())
def test_test(r):
    print(r)
    dfa = regex_to_DFA(r)

    @given(s=positive(dfa, max_size=20))
    def test_positive(s):
        t = "".join(s)
        if print_it: print("    ", t)
        # Check if t accepted by r.
        assert dfa.accepts(t), \
            "Generated string \"{}\" does not match regexp {}".format(t, r)

    @given(s=negative(dfa, max_size=20))
    def test_negative(s):
        t = "".join(s)
        if print_it: print("    ", t)
        # Check if t not accepted by r.
        assert not dfa.accepts(t), \
            "Generated string \"{}\" matches regexp {}".format(t, r)
    
    test_positive()
    test_negative()
    print('pass')

#@given(r=gen_regex())
#def test_regex(r):
#    print(r)

if __name__ == '__main__':
    n = len(sys.argv)
    print_it = False
    if n == 2 and sys.argv[1]=='print':
        print_it = True
    
    test_test()
    print('Passed all tests!')