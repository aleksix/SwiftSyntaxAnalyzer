import ply.yacc as yacc
import swiftLexer

tokens = swiftLexer.tokens

swiftLexer.build()


def p_import(p):
    '''
    import : IMPORT IDENTIFIER
    '''
    p[0] = p[1]
    print(p[0])


yacc.yacc()

s = "import x"
yacc.parse(s)
