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
    variableAssignment : EQUAL expression
    '''
    p[0] = p[1]


'''
        FUNCTIONS
'''


def p_functionDeclaration(p):
    '''
    functionDeclaration : functionModifiers FUNC IDENTIFIER BRACKET_L argumentList BRACKET_R throws returnType functionAssignment blockBody
    '''
    p[0] = p[1]


def p_argumentList(p):
    '''
    argumentList : argumentDeclaration
                 | argumentList COMMA argumentDeclaration
    '''
    p[0] = p[1]


def p_argumentDeclaration(p):
    '''
    argumentDeclaration : argumentDescription
                        | argumentLabel argumentDescription
    '''
    p[0] = p[1]


def p_argumentDescription(p):
    '''
    argumentDescription : IDENTIFIER COLON argumentInout TYPE argumentType
    '''
    p[0] = p[1]


def p_argumentInout(p):
    '''
    argumentInout : INOUT | epsilon
    '''
    p[0] = p[1]


def p_argumentType(p):
    '''
    argumentType : RANGE_OPERATOR
                 | EQUAL assignable
                 | epsilon
    '''
    p[0] = p[1]


def p_argumentLabel(p):
    '''
    argumentLabel : IDENTIFIER
                    | underscore
    '''
    p[0] = p[1]


def p_throws(p):
    '''
    throws : THROWS
           | epsilon
    '''
    p[0] = p[1]


def p_returnType(p):
    '''
    returnType : ARROW type
               | epsilon
    '''
    p[0] = p[1]


def p_functionAssignment(p):
    '''
    functionAssignment : EQUAL functionName
                       | epsilon
    '''
    p[0] = p[1]


def p_functionModifiers(p):
    '''
    functionModifiers : FINAL
                      | OVERRIDE
                      | PRIVATE
                      | epsilon
    '''
    p[0] = p[1]


def p_functionCall(p):
    '''
    functionCall : functionName BRAKET_L callArgumentList BRACKET_R
    '''
    p[0] = p[1]


def p_callArgumentList(p):
    '''
    callArgumentList : callArgument
                     | callArgumentList COMMA callArgument
    '''
    p[0] = p[1]


def p_callArgument(p):
    '''
    callArgument : callArgumentReference callArgumentLabel assignable
    '''
    p[0] = p[1]


def p_callArgumentReference(p):
    '''
    callArgumentReference : AMPERSAND
                          | epsilon
    '''
    p[0] = p[1]


def p_callArgumentLabel(p):
    '''
    callArgumentLabel : IDENTIFIER COLON
                      | epsilon
    '''
    p[0] = p[1]


'''
        ERROR HANDLING
'''


def p_do(p):
    '''
    do : DO CURLY_L doBody CURLY_R catch
    '''
    p[0] = p[1]


def p_doBody(p):
    '''
    doBody : statements doBody
           | TRY statements doBody
           | epsilon
    '''
    p[0] = p[1]


def p_catch(p):
    '''
    catch : CATCH IDENTIFIER CURLY_L catchBody CURLY_R
          | epsilon
    '''
    p[0] = p[1]


def p_catchBody(p):
    '''
    catchBody : statements
              | TRY statements
              | catchBody
              | epsilon
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
