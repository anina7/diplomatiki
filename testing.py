import sys
import hypothesis.strategies as st
from hypothesis import given, settings, HealthCheck

from regex_parser import DIGITS
from automata import DFA, regex_to_DFA
from generate import positive, negative

@st.composite
def gen_regex(draw, max=10):
    regex = draw(st.lists(st.just(None), min_size=1, max_size=max))
    
    paren_counter = 0
    
    regex[0] = draw(st.text(alphabet=DIGITS+'(', min_size=1, max_size=1))
    if regex[0] == '(':
            paren_counter += 1
    for i in range(1, len(regex)):        
        if paren_counter > 0:
            PAREN = ')'
        else:
            PAREN = ''
        
        if regex[i-1] in DIGITS:
            regex[i] = draw(st.text(alphabet=DIGITS+'|*+'+PAREN, min_size=1, max_size=1))
        elif regex[i-1]=='|':
            regex[i] = draw(st.text(alphabet=DIGITS+'(', min_size=1, max_size=1))
        elif regex[i-1]=='*' or regex[i-1]=='+':
            regex[i] = draw(st.text(alphabet=DIGITS+'|('+PAREN, min_size=1, max_size=1))
        elif regex[i-1]=='(':
            regex[i] = draw(st.text(alphabet=DIGITS, min_size=1, max_size=1))
        elif regex[i-1]==')':
            regex[i] = draw(st.text(alphabet='*+', min_size=1, max_size=1))
        else:
            regex[i] = draw(st.text(alphabet=DIGITS+'|*+('+PAREN, min_size=1, max_size=1))
        
        if regex[i] == '(':
            paren_counter += 1
        if regex[i] == ')':
            paren_counter -= 1

    #while alt or parenthesis is last symbol, discard it
    while regex and (regex[-1] == '|' or regex[-1] == '('):
        if regex[-1] == '(':
            paren_counter -= 1
        regex.pop()
    
    #close all opened paretheses
    while paren_counter > 0:
        regex.append(')')
        paren_counter -= 1
    
    #if empty, try again
    if not regex:
        regex = draw(gen_regex())
    
    return "".join(regex)

@settings(deadline=None)
@given(r=gen_regex())
def test_test(r):
    print(r)
    dfa = regex_to_DFA(r)

    @settings(suppress_health_check=HealthCheck.all())
    @given(s=positive(dfa, max_size=30))
    def test_positive(s):
        t = "".join(s)
        if print_it: print("    ", t)
        # Check if t accepted by r.
        assert dfa.accepts(t), \
            "Generated string \"{}\" does not match regexp {}".format(t, r)

    @settings(suppress_health_check=HealthCheck.all())
    @given(s=negative(dfa, max_size=30))
    def test_negative(s):
        t = "".join(s)
        if print_it: print("    ", t)
        # Check if t not accepted by r.
        assert not dfa.accepts(t), \
            "Generated string \"{}\" matches regexp {}".format(t, r)
    
    test_positive()
    test_negative()

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