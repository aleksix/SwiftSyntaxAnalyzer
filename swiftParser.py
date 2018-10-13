import ply.yacc as yacc
import swiftLexer

tokens = swiftLexer.tokens

swiftLexer.build()


def p_import(p):
    '''
    int : EXPRESSION_LITERAL_KEYPATH
    '''
    p[0] = p[1]
    print(p[1])

yacc.yacc()

s = '#keyPath'
yacc.parse(s)
