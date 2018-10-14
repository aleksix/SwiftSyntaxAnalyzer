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
        CLASSES
'''


def p_class(p):
    '''
    Class : ClassDeclaration ClassBody
    '''
    p[0] = p[1]


def p_classDeclaration(p):
    '''
    ClassDeclaration: CLASS IDENTIFIER
                    | CLASS IDENTIFIER COLON <Type>
    '''
    p[0] = p[1]


def p_classBodyMain(p):
    '''
    ClassBodyMain: CURLY_L ClassBodyMain Statement
                 | ClassBodyMain ClassItem
    '''
    p[0] = p[1]


def p_classBody(p):
    '''
    ClassBody: ClassBodyMain CURLY_R
    '''
    p[0] = p[1]


def p_classItem(p):
    '''
    ClassItem: <ClassAttribute>
             | <ClassInit>
             | <ClassDeinit
    '''
    p[0] = p[1]


def p_classAttribute(p):
    '''
    ClassAttribute: VAR IDENTIFIER COLON Type BlockBody
    '''
    p[0] = p[1]


def p_classInit(p):
    '''
    ClassInit: ConvenienceInit INIT BRACKET_L ArgumentList BRACKET_R BlockBody
    '''
    p[0] = p[1]


def p_classDeinit(p):
    '''
    ClassDeinit: DEINIT BlockBody
    '''
    p[0] = p[1]


def p_ConvenienceInit(p):
    '''
    ConvenienceInit : CONVENIENCE
                    | e
    '''
    p[0] = p[1]


'''
        EXPRESSION        
'''


def p_bitwiseShift(p):
    '''
    BitwiseShift: Multiplication
                | BitwiseShift BitwiseShiftLevelOp Multiplication
    '''
    p[0] = p[1]


def p_multiplication(p):
    '''
    Multiplication  : Addition
                    | Multiplication MultiplicationLevelOp Addition
    '''
    p[0] = p[1]


def p_addition(p):
    '''
    Addition: RangeFormation
            | Addition AdditionLevelOp RangeFormation
    '''
    p[0] = p[1]


def p_rangeFormation(p):
    '''
    RangeFormation  : Casting
                    | RangeFormation RangeFormationLevelOp Casting
    '''
    p[0] = p[1]


def p_casting(p):
    '''
    Casting : NilCoalescing
            | Casting CastingLevelOp NilCoalescing
    '''
    p[0] = p[1]


def p_nilCoalescing(p):
    '''
    NilCoalescing   : Comparison
                    | NilCoalescing NilCoalescingLevelOp Comparison
    '''
    p[0] = p[1]


def p_comparison(p):
    '''
    Comparison  : LogicalConjugation>
                | Comparison ComparsionLevelOp LogicalConjugation
    '''
    p[0] = p[1]


def p_logicalConjugation(p):
    '''
    LogicalConjugation  : Default
                        | LogicalConjugation LogicalConjugationLevelOp Default
    '''
    p[0] = p[1]


def p_default(p):
    '''
    Default : Ternary
            | Default DefaultLevelOp Ternary
    '''
    p[0] = p[1]


def p_ternary(p):
    '''
    Ternary : Assignment
            | Ternary TernaryLevelOp Assignment
    '''
    p[0] = p[1]


def p_assignment(p):
    '''
    Assignment  : Term
                | Assignment AssignmentLevelOp Term
    '''
    p[0] = p[1]


def p_term(p):
    '''
    Term: Function
        | Variable
        | Constant
        | INTEGER_LITERAL
        | DOUBLE_LITERAL
        | STRING_LITERAL
        | NIL_LITERAL
        | BOOLEAN_LITERAL
        | EXPRESSION_LITERAL
        | BRACKET_L Expression BRACKET_R
        | ExpressionOperator Expression
    '''
    p[0] = p[1]


def p_prefixOperator(p):
    '''
    PrefixOperator  : EXCLAMATION_MARK
                    | BITWISE
                    | UNARY_PLUS
                    | UNARY_MINUS
                    | HALF_OPEN_RANGE
                    | RANGE
                    | DOT
    '''
    p[0] = p[1]


def p_postfixOperator(p):
    '''
    PostfixOperator: RANGE
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
    expression  : Bitwise Shift
                | PrefixOperator BitwiseShift
                | BitwiseShift PostfixOperator
    '''
    p[0] = p[1]


def p_blockBodyMain(p):
    '''
    BlockBodyMain : CURLY_L BlockBody
    '''
    p[0] = p[1]


def p_blockBody(p):
    '''
    BlockBody : BlockBodyMain CURLY_R
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
