from regex_parser import State, regex_to_NFAb

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

    def NFAtoDFA(self):
        state_list = []
        last = []
        is_first = True

        dfa_states = [self.initial.epsilon+[self.initial]]

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
                        lista += [trans] + trans.epsilon

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
    def __init__(self, states, alphabet, initial, final, transition=None):
        self.states = states
        self.alphabet = alphabet
        self.initial = initial
        self.final = final
        '''
        self.transition = transition
        self.valid = {s: sorted(d.keys()) for s, d in self.transition.items()}
        set_alphabet = set(alphabet)
        self.invalid = {s: sorted(set_alphabet - set(v))
                        for s, v in self.valid.items()}
                        '''

    def __str__(self):
        L = ["\t".join([""] + [str(x) for x in self.alphabet] + ["-|", "|-"])]
        for s in self.states:
            L.append("\t".join(
                [str(s)] +
                [str(s.transitions.get(x, "")) for x in self.alphabet] +
                ["yes" if s in self.final else "",
                 "yes" if s == self.initial else ""]
            ))
        return "\n".join(L)




if __name__ == '__main__':
    while True:
        regex = str(input())

        nfa = NFA(*regex_to_NFAb(regex))
        print(nfa)
        print()

        dfa = nfa.NFAtoDFA()
        print(dfa)
        print()
