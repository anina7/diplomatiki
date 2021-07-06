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
                 "yes" if str(s) == self.initial else ""]
            ))
        return "\n".join(L)
        
if __name__ == '__main__':
    while True:
        regex = str(input())
        state_list, alphabet, initial = regex_to_NFA(regex)
        
        final = [str(s) for s in state_list if s.is_end]
        
        nfaPrint = NFAprint(state_list, alphabet, initial, final)
        print(nfaPrint)
        print()