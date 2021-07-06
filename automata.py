from regex_parser import State, regex_to_NFA

class NFAprint:
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

def NFAtoDFA(first, alphabet):
    state_list = []
    init = True
    
    if first.epsilon:
        dfa_states = [first.epsilon]
    else:
        #if 1st e-closure is empty
        dfa_states = [[first]]
        
    incr = 0
    while incr < len(dfa_states):
        #get first element of list to check
        current = dfa_states[incr]
        
        dict = {}
        fin = False
        for i in alphabet:  #for every input find transitions
            lista = []
            for s in current:  # for every state in the enclosure
                trans = s.transitions.get(i) 
                if trans:
                    lista += trans.epsilon
                
                if s.is_end:
                    fin = True
                
            # this is transition for input 'i' for current state
            dict[i] = lista
            
            #only add to check unique states
            if lista and lista not in dfa_states:
                dfa_states.append(lista)
        
        a = State('t' + str(len(state_list)))
        a.original = current    #list of original nfa states
        a.transitions = dict
        a.is_end = fin
        if fin:
            final.append(a)
        if init:
            initial = a
            init = False
            
        state_list.append(a)
        incr += 1
        
    for s in state_list:
        for i in alphabet:
            trans = s.transitions.get(i)
            if trans:
                j = dfa_states.index(trans)
                s.transitions[i] = state_list[j]
            else:
                s.transitions[i] = ""

    return state_list, initial, final

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
        nfa_state_list, alphabet, initial = regex_to_NFA(regex)
        
        final = [str(s) for s in nfa_state_list if s.is_end]
        
        nfaPrint = NFAprint(nfa_state_list, alphabet, initial, final)
        print(nfaPrint)
        print()
        
        dfa_state_list, dfa_initial, dfa_final = NFAtoDFA(initial, alphabet)
        dfa = DFA(dfa_state_list, alphabet, dfa_initial, dfa_final)
        print(dfa)