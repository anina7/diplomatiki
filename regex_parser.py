#!/usr/bin/env python3

# Token, Lexer, Parser, Nodes from https://github.com/davidcallanan/py-myopl-code

DIGITS = '0123456789abcdefghijklmnopqrstuvwxyz'

# TOKENS
TT_EOF      = 'EOF'
TT_CHAR     = 'CHAR'
TT_CONCAT   = 'CONCAT'
TT_ALT      = 'ALT'
TT_STAR     = 'STAR'
TT_PLUS     = 'PLUS'
TT_LPAREN   = 'LPAREN'
TT_RPAREN   = 'RPAREN'

class Token:
    def __init__(self, type_, value=None):
        self.type = type_
        self.value = value

    def __repr__(self):
        if self.value:
            return f'{self.type}:{self.value}'
        return f'{self.type}'

class Lexer:
    def __init__(self, text):
        self.text = text
        self.pos = -1
        self.current_char = None
        self.advance()

    def advance(self):
        self.pos += 1
        self.current_char = self.text[self.pos] if self.pos < len(self.text) else None

    def make_tokens(self):
        tokens = []

        while self.current_char != None:
            if self.current_char in DIGITS:
                tokens.append(Token(TT_CHAR, self.current_char))
                self.advance()
            #elif self.current_char == '\x08':
            #    tokens.append(Token(TT_CONCAT))
            #    self.advance()
            elif self.current_char == '|':
                tokens.append(Token(TT_ALT))
                self.advance()
            elif self.current_char == '*':
                tokens.append(Token(TT_STAR))
                self.advance()
            elif self.current_char == '+':
                tokens.append(Token(TT_PLUS))
                self.advance()
            elif self.current_char == '(':
                tokens.append(Token(TT_LPAREN))
                self.advance()
            elif self.current_char == ')':
                tokens.append(Token(TT_RPAREN))
                self.advance()

        return tokens

    def get_ab(self, tokens):
        alphabet = []
        for t in tokens:
            if t.type == TT_CHAR:
                alphabet.append(t.value)
        return sorted(set(alphabet))

#NODES
class CharNode:
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return f'{self.value}'

class BinOpNode:
    def __init__(self, left, op_tok, right):
        self.left = left
        self.op_tok = op_tok
        self.right = right

    def __repr__(self):
        return f'({self.left}, {self.op_tok}, {self.right})'

class UnaryOpNode:
    def __init__(self, left, op_tok):
        self.left = left
        self.op_tok = op_tok

    def __repr__(self):
        return f'({self.left}, {self.op_tok})'

'''
GRAMMAR:
expr    : expr | term   # ALT
        | term

term    : term factor   # CONCAT
        | factor

factor  : factor *      # STAR
        | factor +      # PLUS
        | id

id      : CHAR          # CHAR
        | LPAREN expr RPAREN
'''

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.tok_idx = -1
        self.advance()

    def advance(self):
        self.tok_idx += 1
        if self.tok_idx < len(self.tokens):
            self.current_tok = self.tokens[self.tok_idx]
        else:
            self.current_tok = Token(TT_EOF, None)
        return self.current_tok

    def parse(self):
        return self.expr()

    # GRAMMAR
    def expr(self):
        """
        expr ::= term ( "|" term )*
        """
        left = self.term()
        while self.current_tok.type == TT_ALT:
            op_tok = self.current_tok
            self.advance()
            right = self.term()
            left = BinOpNode(left, op_tok, right)
        return left

    def term(self):
        """
        term ::= factor+
        """
        left = self.factor()
        while self.current_tok.type not in [TT_ALT, TT_RPAREN, TT_EOF]:
            op_tok = Token(TT_CONCAT)
            self.tokens.insert(self.tok_idx, op_tok)
            self.advance()
            right = self.term()
            left = BinOpNode(left, op_tok, right)
        return left

    def factor(self):
        """
        factor ::= id ("*" | "+")*
        """
        left = self.id()
        while self.current_tok.type in [TT_STAR, TT_PLUS]:
            op_tok = self.current_tok
            self.advance()
            left = UnaryOpNode(left, op_tok)
        return left

    def id(self):
        """
        id ::= char | "(" expr ")"
        """
        if self.current_tok.type == TT_CHAR:
            char = self.current_tok.value
            self.advance()
            return CharNode(char)

        elif self.current_tok.type == TT_LPAREN:
            self.advance()
            expr = self.expr()
            assert self.current_tok.type == TT_RPAREN, "missing ')'"
            self.advance()
            return expr

        assert False, "unexpected {}".format(self.current_tok)

class State:
    def __init__(self, name):
        self.name = name
        self.epsilon = []       # epsilon-closure / only for nfa
        self.transitions = {}   # char : state
        self.is_end = False
        self.original = []      #only for dfa

    def __str__(self):
        return self.name

    def __repr__(self):
        return str(self)

class NFAbuilder:
    def __init__(self, start, end):
        self.start = start
        self.end = end # start and end states
        end.is_end = True

class NodeVisitor():
    def visit(self, node, nfa_stack, state_list):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self, method_name, self.no_visit_method)
        return method(node, nfa_stack, state_list)

    def no_visit_method(self, node, nfa_stack, state_list):
        raise Exception(f'No visit_{type(node).__name__} method defined')

    def visit_CharNode(self, node, nfa_stack, state_list):
        a = State('s' + str(len(state_list)))
        state_list.append(a)
        b = State('s' + str(len(state_list)))
        state_list.append(b)

        a.transitions[node.value] = b
        nfab = NFAbuilder(a, b)
        nfa_stack.append(nfab)

    def visit_BinOpNode(self, node, nfa_stack, state_list):
        self.visit(node.left, nfa_stack, state_list)
        self.visit(node.right, nfa_stack, state_list)

        b = nfa_stack.pop()
        a = nfa_stack.pop()

        if node.op_tok.type == TT_CONCAT:
            a.end.is_end = False
            a.end.epsilon.append(b.start)
            nfab = NFAbuilder(a.start, b.end)
            nfa_stack.append(nfab)

        elif node.op_tok.type == TT_ALT:
            x = State('s' + str(len(state_list)))
            state_list.append(x)
            x.epsilon = [a.start, b.start]

            y = State('s' + str(len(state_list)))
            state_list.append(y)

            a.end.epsilon.append(y)
            b.end.epsilon.append(y)
            a.end.is_end = False
            b.end.is_end = False
            nfab = NFAbuilder(x, y)
            nfa_stack.append(nfab)

    def visit_UnaryOpNode(self, node, nfa_stack, state_list):
        self.visit(node.left, nfa_stack, state_list)

        a = nfa_stack.pop()
        x = State('s' + str(len(state_list)))
        state_list.append(x)

        y = State('s' + str(len(state_list)))
        state_list.append(y)

        x.epsilon = [a.start]
        if node.op_tok.type == TT_STAR:
            x.epsilon.append(y)
        a.end.epsilon.extend([y, a.start])
        a.end.is_end = False
        nfab = NFAbuilder(x, y)
        nfa_stack.append(nfab)

def regex_to_NFAb(regex):
    lexer = Lexer(regex)
    tokens = lexer.make_tokens()
    alphabet = lexer.get_ab(tokens)
    
    parser = Parser(tokens)
    nodes = parser.parse()
    #print(nodes)

    nfa_stack = []
    state_list = []
    NodeVisitor().visit(nodes, nfa_stack, state_list)
    nfab = nfa_stack.pop()
    
    final = [str(s) for s in state_list if s.is_end]

    return state_list, alphabet, nfab.start, final

'''
if __name__ == '__main__':
    while True:
        regex = str(input())
        state_list, initial = regex_to_NFA(regex)

        final = [s for s in state_list if s.is_end]

        nfaPrint = NFAprint(state_list, alphabet, initial, final)
        print(nfaPrint)
        print()


        print("#### NFA ####")
        print("states:", state_list)
        print("alphabet:", alphabet)
        print("initial:", initial)
        print("final:", final)
        print()
'''
