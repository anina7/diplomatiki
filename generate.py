#!/usr/bin/env python3

from collections import Counter

from hypothesis import given
from hypothesis import settings, HealthCheck, Phase, Verbosity
import hypothesis.strategies as st

from automata import regex_to_DFA

@st.composite
def positive(draw, dfa, max_size=None):
    return dfa.generate(draw, True, max_size=max_size)

@st.composite
def negative(draw, dfa, max_size=None):
    return dfa.generate(draw, False, max_size=max_size)

def test(dfa, test_only=None):
    all_positive = []
    @settings(suppress_health_check=HealthCheck.all())
    @given(s=positive(dfa))
    #@settings(phases=[Phase.generate, Phase.shrink])
    def test_positive(s):
        all_positive.append("".join(s))
        assert dfa.accepts(s)

    all_negative = []
    @settings(suppress_health_check=HealthCheck.all())
    @given(s=negative(dfa))
    #@settings(phases=[Phase.generate, Phase.shrink])
    def test_negative(s):
        all_negative.append("".join(s))
        assert not dfa.accepts(s)

    if test_only != "negative":
        print('Some positive test-cases:')
        test_positive()
        c = Counter(all_positive)
        for k in sorted(c):
            print("{}: {} times".format(k, c[k]))
        print(len(all_positive), "test-case(s) generated")
        print(len(set(all_positive)), "unique test-case(s)")
        print(max(map(len, all_positive)), "max length")
        print(sum(map(len, all_positive))/len(all_positive), "avg length")
        print()

    if test_only != "positive":
        print('Some negative test-cases:')
        test_negative()
        c = Counter(all_negative)
        for k in sorted(c):
            print("{}: {} times".format(k, c[k]))
        print(len(all_negative), "test-case(s) generated")
        print(len(set(all_negative)), "unique test-case(s)")
        print(max(map(len, all_negative)), "max length")
        print(sum(map(len, all_negative))/len(all_negative), "avg length")
        print()

if __name__ == '__main__':
    while True:
        regex = str(input())

        if regex == 'EXIT':
            exit()

        while not regex:
            regex = str(input("Please give valid regex: "))

        dfa = regex_to_DFA(regex)
        print("Testing DFA:")
        print(dfa)
        print()

        test(dfa)
