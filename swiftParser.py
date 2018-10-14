import ply.yacc as yacc
import swiftLexer

tokens = swiftLexer.tokens

swiftLexer.build()

constants = []

'''
        CONSTANTS
'''


def p_constantDeclarationList(p):
    '''
    constantDeclarationList : LET IDENTIFIER
                            | constantDeclarationList COMMA IDENTIFIER
    '''
    p[0] = p[1]


def p_constantDeclaration(p):
    '''
    constantDeclaration :  constantDeclarationList COLON type
    '''
    p[0] = p[1]


def p_constantAssignment(p):
    '''
    constantAssignment : constantDeclaration EQUAL expression
    '''
    p[0] = p[1]


'''
        VARIABLES
'''


def p_variableModifiers(p):
    '''
    variableModifiers : WEAK
                      | UNOWNED
                      | FILEPRIVATE
                      | epsilon
    '''
    p[0] = p[1]


def p_variableDeclarationStart(p):
    '''
    variableDeclarationStart : variableModifiers VAR IDENTIFIER
    '''
    p[0] = p[1]


def p_variableDeclaration(p):
    '''
    variableDeclaration : variableDeclarationStart variableAssignment
                        | variableDeclarationStart COLON type variableAssignment
                        | variableDeclarationList COLON type
    '''
    p[0] = p[1]


def p_variableDeclarationList(p):
    '''
    VariableDeclarationList : variableDeclarationStart
                            | variableDeclarationList COMMA IDENTIFIER
    '''
    p[0] = p[1]


def p_variableAssignment(p):
    '''
    variableAssignment ::= EQUAL expression
    '''
    p[0] = p[1]


'''
        COMMON RULES
'''


def p_type(p):
    '''
    type : IMPORT
    '''
    p[0] = {'type': p[0]}


def p_expression(p):
    '''
    expression : INOUT
    '''
    p[0] = p[1]


'''
        TECHNICAL
'''


def p_epsilon(p):
    'epsilon :'
    pass


yacc.yacc(start="constantAssignment")

s = '''let x : import = inout'''
yacc.parse(s)
