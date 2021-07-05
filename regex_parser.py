# Token, Lexer, Parser, Nodes from https://github.com/davidcallanan/py-myopl-code

DIGITS = '0123456789'

# TOKENS
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
            return self.type + ":" + self.value
        return self.type

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
            elif self.current_char == '\x08':
                tokens.append(Token(TT_CONCAT))
                self.advance()
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

#NODES

class CharNode:
    def __init__(self, tok):
        self.tok = tok

    def __repr__(self):
        return f'{self.tok}'

class BinOpNode:
    def __init__(self, left_node, op_tok, right_node):
        self.left_node = left_node
        self.op_tok = op_tok
        self.right_node = right_node

    def __repr__(self):
        return f'({self.left_node}, {self.op_tok}, {self.right_node})'

class UnaryOpNode:
    def __init__(self, node, op_tok):
        self.node = node
        self.op_tok = op_tok

    def __repr__(self):
        return f'({self.node}, {self.op_tok})'

'''
GRAMMAR:
expr    : term | term   # ALT
        | term
        
term    : factor term   # CONCAT
        | factor

factor  : id *          # STAR
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
        return self.current_tok

    def parse(self):
        return self.expr()

    # GRAMMAR
    def expr(self):
        left = self.term()
        
        if self.current_tok.type == TT_ALT:
            op_tok = self.current_tok
            self.advance()
            right = self.term()
            left = BinOpNode(left, op_tok, right)
    
        return left
        
    def term(self):
        left = self.factor()
        
        if self.current_tok.type in (TT_CHAR, TT_LPAREN):
            op_tok = Token(TT_CONCAT, '\x08')
            self.tokens.append(op_tok)  # len(tokens) += 1
            right = self.term()
            left = BinOpNode(left, op_tok, right)
            
        return left

    def factor(self):
        left = self.id()
        
        if self.current_tok.type in (TT_STAR, TT_PLUS):
            op_tok = self.current_tok
            left = UnaryOpNode(left, op_tok)    
    
        return left
    
    def id(self):
        if self.current_tok.type == TT_CHAR:
            char = self.current_tok
            self.advance()
            return CharNode(char)

        elif self.current_tok.type == TT_LPAREN:
            self.advance()
            expr = self.expr()
            if self.current_tok.type == TT_RPAREN:
                self.advance()
                return expr

    


def run(text):
    lexer = Lexer(text)
    tokens = lexer.make_tokens()

    # Generate AST
    parser = Parser(tokens)
    ast = parser.parse()

    return ast

if __name__ == '__main__':
    while True:
        regex = str(input())
        print(run(regex))