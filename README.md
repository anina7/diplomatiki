# Automated testing for Lexical Analysers

This is a tool that tests lexical analysers by generating random test-cases, both positive and negative, given a regular expression.  


## Description
This tool parses a regular expression from input, converts it to equivalent NFA and converts the NFA to equivalent DFA. Then, using the DFA, it generates positive and negative test-cases for the regular expresion, namely words that the regular expresion recognises and words that it doesn't.

Developed for my Diploma Thesis, in Electrical and Computer Engineering, NTUA, Greece.

Developed in Python 3.9.6, using hypothesis-6.14.1.

## Usage

Run:
```bash
python generate.py
```

User input:  
regular expression or 'EXIT' to exit

Output:  
equivalent DFA, positive test-cases, negative test-cases

## Files
regex_parser.py holds the parser and NFAbuilder  
automata.py holds NFA and DFA classes, NFAtoDFA method, generate method  
generate.py runs and prints test-cases

## Supported regular expressions
|Regular expressions|Symbols|
|----|-----|
|Characters:|0-9, a-z|
|Alteration:|&#124;|
|Repetition operators:|*, +|
|Parenthesis:|(, )|
