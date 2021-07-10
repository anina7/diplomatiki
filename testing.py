import re
import sys
import hypothesis.strategies as st
from hypothesis import given, settings, HealthCheck

from regex_parser import DIGITS
from automata import DFA, regex_to_DFA
from generate import positive, negative

@st.composite
def gen_regex(draw, max=10):
    length = draw(st.integers(1, max))
    
    paren_counter = 0
    
    regex = draw(st.sampled_from(DIGITS+'('))
    if regex == '(':
            paren_counter += 1
    
    for i in range(1, length):        
        if paren_counter > 0:
            PAREN = ')'
        else:
            PAREN = ''
        
        if regex[i-1] in DIGITS:
            regex += draw(st.sampled_from(DIGITS+'|*+'+PAREN))
        elif regex[i-1]=='|':
            regex += draw(st.sampled_from(DIGITS+'('))
        elif regex[i-1]=='*' or regex[i-1]=='+':
            regex += draw(st.sampled_from(DIGITS+'|('+PAREN))
        elif regex[i-1]=='(':
            regex += draw(st.sampled_from(DIGITS))
        elif regex[i-1]==')':
            regex += draw(st.sampled_from('*+'))
        else:
            regex += draw(st.sampled_from(DIGITS+'|*+('+PAREN))
        
        if regex[i] == '(':
            paren_counter += 1
        if regex[i] == ')':
            paren_counter -= 1

    #while alt or parenthesis is last symbol, discard it
    while regex and (regex[-1] == '|' or regex[-1] == '('):
        if regex[-1] == '(':
            paren_counter -= 1
        regex = regex[:-1]
    
    #close all opened paretheses
    while paren_counter > 0:
        regex += ')'
        paren_counter -= 1
    
    #if empty, try again
    if not regex:
        regex = draw(gen_regex())
    
    return regex

@settings(deadline=None)
@given(r=gen_regex())
def test_test(r):
    print(r)
    dfa = regex_to_DFA(r)

    #@settings(suppress_health_check=HealthCheck.all())
    @given(s=positive(dfa, max_size=30))
    def test_positive(s):
        t = ''.join(s)
        if print_it: print('    ', t)
        # Check if t recognized by r.
        assert re.match(r"^"+str(r)+"$", t), \
            'Generated string \'{}\' does not match regexp {}'.format(t, r)
    
    #@settings(suppress_health_check=HealthCheck.all())
    @given(s=negative(dfa, max_size=30))
    def test_negative(s):
        t = ''.join(s)
        if print_it: print('    ', t)
        # Check if t not recognized by r.
        #re.match does not work here (tries to match first instance)
        assert not dfa.accepts(t), \
            'Generated string \'{}\' does not match regexp {}'.format(t, r)
    
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