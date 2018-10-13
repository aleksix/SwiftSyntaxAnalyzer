import ply.yacc as yacc
import swiftLexer

tokens = swiftLexer.tokens

swiftLexer.build()


def p_import(p):
    '''
    import : STRING_LITERAL
    '''
    p[0] = p[1]
    print(p[1])


yacc.yacc()

s = '''
/*Hello*/
"/*Hello*/"
'''
yacc.parse(s)
