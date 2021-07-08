#!/usr/bin/env python3

from regex_parser import ILLEGAL, State, regex_to_NFAb
import hypothesis.strategies as st
import itertools

class NFA:
    def __init__(self, states, alphabet, initial, final):
        self.states = states
        self.alphabet = alphabet
        self.initial = initial
        self.final = final

    def __str__(self):
        L = ["\t".join([""] + [str(x) for x in self.alphabet] + ['\u03B5'] + ["-|", "|-"])]
        for s in self.states:
            L.append("\t".join(
                [str(s)] +
                [str(s.transitions.get(x, "")) for x in self.alphabet] +
                [str(s.epsilon)] +
                ["yes" if str(s) in self.final else "",
                 "yes" if s == self.initial else ""]
            ))
        return "\n".join(L)

    def calcEclosure(self):
        for s in self.states:
            st_list = [s]
            while st_list != []:
                curr = st_list.pop(0)
                s.eclosure.append(curr)
                st_list += [t for t in curr.epsilon if t not in st_list]

    def NFAtoDFA(self):
        self.calcEclosure()      # first and foremost calculate e-closure
    
        state_list = []
        last = []
        is_first = True

        dfa_states = [self.initial.eclosure]

        incr = 0
        while incr < len(dfa_states):
            #get first element of list to check
            current = dfa_states[incr]

            dict = {}
            is_last = False
            for i in self.alphabet:  #for every input find transitions
                lista = []
                for s in current:  # for every state in the e-closure
                    trans = s.transitions.get(i)
                    if trans:
                        lista += trans.eclosure

                    if s.is_end:
                        is_last = True

                # this is transition for input 'i' for current state
                dict[i] = lista

                #only add to check unique states
                if lista and lista not in dfa_states:
                    dfa_states.append(lista)

            a = State('t' + str(len(state_list)))
            a.original = current    #list of original nfa states
            a.transitions = dict
            a.is_end = is_last

            if is_last:
                last.append(a)
            if is_first:
                first = a
                is_first = False

            state_list.append(a)
            incr += 1

        for s in state_list:
            for i in self.alphabet:
                trans = s.transitions.get(i)
                if trans:
                    j = dfa_states.index(trans)
                    s.transitions[i] = state_list[j]
                else:
                    s.transitions[i] = ""

        return DFA(state_list, self.alphabet, first, last)

class DFA:
    def __init__(self, states, alphabet, initial, final):
        self.states = states
        self.alphabet = alphabet
        self.initial = initial
        self.final = final

        self.transition = {s: {i: s.transitions.get(i)
            for i in alphabet if s.transitions.get(i)} for s in states}
        self.valid = {s: [i for i in alphabet if s.transitions.get(i)]
            for s in states}
        self.invalid = {s: [i for i in alphabet if not s.transitions.get(i)]
                                + list(ILLEGAL)
            for s in states}

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

    def minimize(self):
        S = []  #same type
        T = []  #different type

        comb = itertools.combinations(self.states, 2)
        for c in comb:
            if (c[0] in self.final) == (c[1] in self.final):
                S.append((c[0], c[1]))
            else:
                T.append((c[0], c[1]))

        S.reverse()     #reverse list to start from last    #bug for 0110 due to (delta0, delta1) not yet in T
        Sremove = []
        for c in S:
            for i in self.alphabet:
                delta0 = self.transition[c[0]].get(i)
                delta1 = self.transition[c[1]].get(i)

                if (delta0, delta1) in T or (delta1, delta0) in T or (delta0 and not delta1) or (not delta0 and delta1):
                    Sremove.append(c)
                    T.append(c)
                    break

        unpackS = list(itertools.chain(*[x for x in S if x not in Sremove]))    #all states left in S after removal
        equals_last = [x for x in unpackS if x.is_end]
        equals_not = [x for x in unpackS if not x.is_end]

        min_state_list = []     #list of new states for minimized dfa
        mindfa_origstates = []  #original states in the same order as min_state_list
        for st in self.states:
            if equals_last and st == equals_last[0]:
                a = State('q' + str(len(min_state_list)))
                a.original = equals_last    #list of original dfa states
                a.transitions = {x: self.transition[s].get(x) for x in self.alphabet for s in a.original}
                a.is_end = True
                min_state_list.append(a)
                mindfa_origstates.append(a.original)

            elif equals_not and st == equals_not[0]:
                a = State('q' + str(len(min_state_list)))
                a.original = equals_not    #list of original dfa states
                a.transitions = {x: self.transition[s].get(x) for x in self.alphabet for s in a.original}
                a.is_end = False
                min_state_list.append(a)
                mindfa_origstates.append(a.original)

            elif st not in equals_last and st not in equals_not:
                a = State('q' + str(len(min_state_list)))
                a.original = [st]    #list of original dfa states
                a.transitions = self.transition[st]
                a.is_end = st.is_end
                min_state_list.append(a)
                mindfa_origstates.append(a.original)

        last = []
        for s in min_state_list:
            if s.is_end:
                last.append(s)
            if self.initial in s.original:
                first = s

            for i in self.alphabet:
                trans = s.transitions.get(i)
                if trans:
                    j, *_ = [k for k in range(len(mindfa_origstates)) if trans in mindfa_origstates[k]]
                    s.transitions[i] = min_state_list[j]
                else:
                    s.transitions[i] = ""

        return DFA(min_state_list, self.alphabet, first, last)

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
            assert s is not None or not valid
            # Keep this state, if it is good, in case nothing better is found.
            if valid and s in self.final or not valid and s not in self.final:
                last_good = i
            # If we're past the designated moves, stop with the last good state.
            if last_good is not None and i >= len(transitions):
                break
            # If we're in a non-existent state (already generated a negative
            # example), keep adding symbols.
            elif s is None:
                x = draw(st.sampled_from(self.alphabet))
            # If we're good to stop with one final move, do it.
            elif i >= len(transitions)-1:
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
            # If there's nothing better and we need a negative example,
            # make an invalid move.
            if x is None and not valid and self.invalid[s]:
                x = draw(st.sampled_from(self.invalid[s]))
            # If there's no move to be made, give up.
            if x is None:
                break
            # Register the move and proceed to the next state
            if i < len(transitions):
                transitions[i] = x
            else:
                transitions.append(x)
            if s is not None:
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

def regex_to_DFA(regex):
    nfa = NFA(*regex_to_NFAb(regex))

    dfa = nfa.NFAtoDFA()

    min_dfa = dfa.minimize()

    return min_dfa

'''
if __name__ == '__main__':
    while True:
        regex = str(input())

        nfa = NFA(*regex_to_NFAb(regex))
        #print("==== ΝFA ====")
        #print(nfa)
        #print()

        dfa = nfa.NFAtoDFA()
        print("==== DFA ====")
        print(dfa)
        print()

        s = dfa.minimize()
        print("==== min DFA ====")
        print(s)
        print()
'''