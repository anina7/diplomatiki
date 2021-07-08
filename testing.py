import re
import hypothesis.strategies as st
from hypothesis import given

from regex_parser import DIGITS
from automata import DFA, regex_to_DFA
from generate import positive, negative

@st.composite
def gen_regex(draw):
    regex = draw(st.lists(st.just(None), min_size=1, max_size=None))
    
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
    
    #if parenthesis is last symbol, discard it
    if regex[-1] == '(':
        regex.pop()
        paren_counter -= 1
    
    #close all opened paretheses
    while paren_counter > 0:
        regex.append(')')
        paren_counter -= 1
        
    return "".join(regex)

@given(r=gen_regex())
def test_test(r):
    dfa = regex_to_DFA(r)

    @given(s=positive(dfa))
    def test_positive(s):
        t = "".join(s)
        # Check if s matches r.
        assert re.match(r"^" + r + "$", t), \
            "Generated string \"{}\" does not match regexp {}".format(t, r)
        #print(s)

    @given(s=negative(dfa))
    def test_negative(s):
        t = "".join(s)
        # Check if s doesn't match r.
        assert not re.match(r"^" + r + "$", t), \
            "Generated string \"{}\" matches regexp {}".format(t, r)
        #print(s)
    
    test_positive()
    test_negative()
    print('pass')

#@given(r=gen_regex())
#def test_regex(r):
#    print(r)

if __name__ == '__main__':
    test_test()