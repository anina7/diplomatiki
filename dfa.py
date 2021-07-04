#!/usr/bin/env python3

from collections import Counter

from hypothesis import given
from hypothesis import settings, Phase, Verbosity
import hypothesis.strategies as st

class DFA:
    def __init__(self, states, alphabet, initial, final, transition):
        self.states = states
        self.alphabet = alphabet
        self.initial = initial
        self.final = final
        self.transition = transition
        self.valid = {s: sorted(d.keys()) for s, d in self.transition.items()}
        set_alphabet = set(alphabet)
        self.invalid = {s: sorted(set_alphabet - set(v))
                        for s, v in self.valid.items()}

    def __str__(self):
        L = ["\t".join([""] + [str(x) for x in self.alphabet] + ["-|", "|-"])]
        for s in self.states:
            L.append("\t".join(
                [str(s)] +
                [str(self.transition[s].get(x, "")) for x in self.alphabet] +
                ["yes" if s in self.final else "",
                 "yes" if s == self.initial else ""]
            ))
        return "\n".join(L)

    def accepts(self, input):
        s = self.initial
        for x in input:
            s = self.transition[s].get(x)
            if s == None: return False
        return s in self.final

    def generate(self, draw, valid=True, min_size=0, max_size=None):
        transitions = draw(st.lists(st.just(None), min_size=min_size,
                           max_size=max_size))
        s = self.initial
        last_good = None
        i = 0
        while True:
            x = None
            # Keep this state, if it is good, in case nothing better is found.
            if valid and s in self.final or not valid and s not in self.final:
                last_good = i
            # If we're past the designated moves, stop with the last good state.
            if last_good is not None and i >= len(transitions):
                break
            # If we're good to stop with one final move, do it.
            if i >= len(transitions)-1:
                if valid:
                    choices = sorted(x for x, t in self.transition[s].items()
                                       if t in self.final)
                else:
                    choices = sorted(x for x, t in self.transition[s].items()
                                       if t not in self.final) + self.invalid[s]
                if choices:
                    x = draw(st.sampled_from(choices))
            # If there's nothing better, keep making valid moves.
            if x is None and self.valid[s]:
                x = draw(st.sampled_from(self.valid[s]))
            # If there's no move to be made, give up.
            if x is None:
                break
            # Register the move and proceed to the next state
            if i < len(transitions):
                transitions[i] = x
            else:
                transitions.append(x)
            s = self.transition[s].get(x)
            i += 1
        # If we failed, return None.
        if last_good is None:
            return None
        # Return either the full list or its last good slice.
        if last_good == len(transitions):
            return transitions
        else:
            return transitions[:last_good]

@st.composite
def correct(draw, dfa):
    return dfa.generate(draw, True)

@st.composite
def wrong(draw, dfa):
    return dfa.generate(draw, False)

def test(dfa, test_only=None):
    all_correct = []
    @given(s=correct(dfa))
    #@settings(phases=[Phase.generate, Phase.shrink])
    def test_correct(s):
        all_correct.append("".join(s))
        assert dfa.accepts(s)

    all_wrong = []
    @given(s=wrong(dfa))
    #@settings(phases=[Phase.generate, Phase.shrink])
    def test_wrong(s):
        all_wrong.append("".join(s))
        assert not dfa.accepts(s)

    if test_only != "wrong":
        print('Some correct inputs:')
        test_correct()
        c = Counter(all_correct)
        for k in sorted(c):
            print("{}: {} times".format(k, c[k]))
        print(len(all_correct), "testcase(s) generated")
        print(len(set(all_correct)), "unique testcase(s)")
        print(max(map(len, all_correct)), "max length")
        print(sum(map(len, all_correct))/len(all_correct), "avg length")

    if test_only != "correct":
        print('Some wrong inputs:')
        test_wrong()
        c = Counter(all_wrong)
        for k in sorted(c):
            print("{}: {} times".format(k, c[k]))
        print(len(all_wrong), "testcase(s) generated")
        print(len(set(all_wrong)), "unique testcase(s)")
        print(max(map(len, all_wrong)), "max length")
        print(sum(map(len, all_wrong))/len(all_wrong), "avg length")

def test1():
    # Example 1: all strings of 0/1 with an odd number of 1s.
    ex = DFA("ab", "01", "a", "a", {
        "a": {"0": "a", "1": "b"},
        "b": {"0": "b", "1": "a"},
    })
    print("Testing DFA:")
    print(ex)
    # Some manual testing.
    assert ex.accepts("01001011") == True
    assert ex.accepts("010010111") == False
    assert ex.accepts("0100101100001") == False
    # Some automatic testing.
    test(ex)

def test2():
    # Example 2: all strings of 0/1 with an even number of 1s.
    ex = DFA("ab", "01", "a", "b", {
        "a": {"0": "a", "1": "b"},
        "b": {"0": "b", "1": "a"},
    })
    print("Testing DFA:")
    print(ex)
    # Some manual testing.
    assert ex.accepts("01001011") == False
    assert ex.accepts("010010111") == True
    assert ex.accepts("0100101100001") == True
    # Some automatic testing.
    test(ex)

def test3():
    # Example 3: the string "hello".
    ex = DFA([0, 1, 2, 3, 4, 5], "helo", 0, [5], {
        0: {"h": 1}, 1: {"e": 2}, 2: {"l": 3}, 3: {"l": 4}, 4: {"o": 5}, 5: {}
    })
    print("Testing DFA:")
    print(ex)
    # Some manual testing.
    assert ex.accepts("hello") == True
    assert ex.accepts("world") == False
    # Some automatic testing.
    test(ex)

def test4():
    # Example 4: Pascal identifiers.
    ex = DFA("zf", "LD", "z", ["f"], {
        "z": {"L": "f"},
        "f": {"L": "f", "D": "f"},
    })
    print("Testing DFA:")
    print(ex)
    # Some manual testing.
    assert ex.accepts("L") == True
    assert ex.accepts("LL") == True
    assert ex.accepts("LD") == True
    assert ex.accepts("DL") == False
    # Some automatic testing.
    test(ex)

if __name__ == '__main__':
    test1()
    test2()
    test3()
    test4()
