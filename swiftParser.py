import ply.yacc as yacc
import swiftLexer

tokens = swiftLexer.tokens

# symbol tables
# TODO : Fill the initial values for the tables
types = {"Bool": 1, "Int": 1, "Int8": 1, "Int16": 1, "Int32": 1, "Int64": 1, "UInt": 1, "UInt8": 1, "UInt16": 1,
         "UInt32": 1, "UInt64": 1, "Float": 1, "Float80": 1, "Double": 1, "Character": 1, "String": 1,
         "Equatable": 1, "Comparable": 1}
functions = {"print": 1, "abs": 1, "min": 1, "max": 1, "filter": 1, "reduce": 1}
variables = {}
constants = {}

# Explanation:
# The lookup is conducted by the operator itself. We can have an operator in 3 different types :
#  prefix, infix and postfix
# For prefix and postfix, we only care that the operator exists in such a state.
# But for infix we need to know the precedence of the operator
# Thus, the structure is {"operator" : {"prefix":1, "infix":"precedenceGroup", "postfix":1}} - best case

# I don't think we can quite use the PLY built-in precedence table since, best-case, operators can get added dynamically
# I know nothing about dynamic rule generation in PLY, so this version will have to do
operatorsInfo = {"!": {"prefix": 1},
                 "~": {"prefix": 1},
                 "+": {"prefix": 1, "infix": "Addition"},
                 "-": {"prefix": 1, "infix": "Addition"},
                 "..<": {"prefix": 1, "infix": "RangeFormation"},
                 "...": {"prefix": 1, "infix": "RangeFormation", "postfix": 1},
                 "<<": {"infix": "BitwiseShift"},
                 ">>": {"infix": "BitwiseShift"},
                 "*": {"infix": "Multiplication"},
                 "/": {"infix": "Multiplication"},
                 "%": {"infix": "Multiplication"},
                 "&*": {"infix": "Multiplication"},
                 "&": {"infix": "Multiplication"},
                 "&+": {"infix": "Addition"},
                 "&-": {"infix": "Addition"},
                 "|": {"infix": "Addition"},
                 "^": {"infix": "Addition"},
                 "is": {"infix": "Casting"},
                 "as": {"infix": "Casting"},
                 "as?": {"infix": "Casting"},
                 "and": {"infix": "Casting"},
                 "as!": {"infix": "Casting"},
                 "??": {"infix": "NilCoalescing"},
                 "<": {"infix": "Comparison"},
                 "<=": {"infix": "Comparison"},
                 ">": {"infix": "Comparison"},
                 ">=": {"infix": "Comparison"},
                 "==": {"infix": "Comparison"},
                 "!=": {"infix": "Comparison"},
                 "===": {"infix": "Comparison"},
                 "!===": {"infix": "Comparison"},
                 "~=": {"infix": "Comparison"},
                 "&&": {"infix": "LogicalConjunction"},
                 "||": {"infix": "LogicalDisjunction"},
                 "?:": {"infix": "Ternary"},
                 "=": {"infix": "Assignment"},
                 "*=": {"infix": "Assignment"},
                 "/=": {"infix": "Assignment"},
                 "%=": {"infix": "Assignment"},
                 "+=": {"infix": "Assignment"},
                 "-=": {"infix": "Assignment"},
                 "<<=": {"infix": "Assignment"},
                 ">>=": {"infix": "Assignment"},
                 "&=": {"infix": "Assignment"},
                 "|=": {"infix": "Assignment"},
                 "^=": {"infix": "Assignment"},
                 }


def type_lookup(identifier):
    return types.get(identifier, "ID") != "ID"


def function_lookup(identifier):
    return functions.get(identifier, "ID") != "ID"


def variable_lookup(identifier):
    return variables.get(identifier, "ID") != "ID"


def constant_lookup(identifier):
    return constants.get(identifier, "ID") != "ID"


def operator_lookup(operator):
    return operatorsInfo.get(operator, "OP")


swiftLexer.function_lookup = function_lookup
swiftLexer.type_lookup = type_lookup
swiftLexer.variable_lookup = variable_lookup
swiftLexer.constant_lookup = constant_lookup
swiftLexer.operator_lookup = operator_lookup

# lexer = swiftLexer.build()

'''
        CONSTANTS
'''


def p_constantDeclarationList(p):
    '''
    constantDeclarationList : LET IDENTIFIER
                            | constantDeclarationList COMMA IDENTIFIER
    '''
    p[0] = p[1]
    constants[p[len(p) - 1]] = 1
    pass


def p_constantDeclaration(p):
    '''
    constantDeclaration :  constantDeclarationList COLON type
    '''
    p[0] = p[1]
    pass


def p_constantAssignment(p):
    '''
    constantAssignment : constantDeclaration EQUAL expression
    '''
    p[0] = p[1]
    pass


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
    if p[1] is None:
        p[0] = ''
    pass


def p_variableDeclarationStart(p):
    '''
    variableDeclarationStart : variableModifiers VAR IDENTIFIER
    '''
    variables[p[len(p) - 1]] = 1
    p[0] = p[1]
    pass


def p_variableDeclaration(p):
    '''
    variableDeclaration : variableDeclarationList COLON type
                        | variableDeclarationStart COLON type
                        | variableDeclarationStart variableAssignment
                        | variableDeclarationStart COLON type variableAssignment
    '''
    p[0] = p[1]
    pass


def p_variableDeclarationList(p):
    '''
    variableDeclarationList : variableDeclarationStart
                            | variableDeclarationList COMMA IDENTIFIER
    '''
    p[0] = p[1]
    if len(p) > 2:
        variables[p[len(p) - 1]] = 1
    pass


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
    functions[p[3]] = 1


def p_returnType(p):
    '''
    returnType : ARROW type
                | epsilon
    '''
    p[0] = p[1]


def p_functionModifiers(p):
    '''
    functionModifiers   : FINAL
                        | OVERRIDE
                        | PRIVATE
                        | epsilon
    '''
    p[0] = p[1]
    pass


def p_argumentList(p):
    '''
    argumentList : argumentDeclaration
                 | argumentList COMMA argumentDeclaration
                 | epsilon
    '''
    p[0] = p[1]
    pass


def p_argumentDeclaration(p):
    '''
    argumentDeclaration : argumentDescription
                        | argumentLabel argumentDescription
    '''
    p[0] = p[1]


def p_argumentDescription(p):
    '''
    argumentDescription : IDENTIFIER COLON argumentInout type argumentType
    '''
    p[0] = p[1]


def p_argumentInout(p):
    '''
    argumentInout   : INOUT
                    | epsilon
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
                 | UNDERSCORE
    '''
    p[0] = p[1]


def p_throws(p):
    '''    
    throws  : THROWS
            | epsilon
    '''
    p[0] = p[1]


def p_functionAssignment(p):
    '''
    functionAssignment  : EQUAL FUNCTION
                        | epsilon
    '''
    p[0] = p[1]


def p_functionCall(p):
    '''    
    functionCall : FUNCTION BRACKET_L callArgumentList BRACKET_R
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


def p_callArgumentLabel(p):
    '''
    callArgumentLabel : IDENTIFIER COLON
                      | epsilon
    '''
    p[0] = p[1]


def p_callArgumentReference(p):
    '''
    callArgumentReference : AMPERSAND
                          | epsilon
    '''
    p[0] = p[1]


'''
        LOOPS
'''


def p_loop(p):
    '''
    loop : loopLabel forLoop
         | loopLabel whileLoop
         | loopLabel repeatLoop
    '''
    p[0] = p[1]


def p_loopLabel(p):
    '''
    loopLabel : IDENTIFIER COLON
              | epsilon
    '''
    p[0] = p[1]


def p_foorLoop(p):
    '''
    forLoop : FOR IDENTIFIER IN expression blockBody
    '''
    p[0] = p[1]


def p_whileLoop(p):
    '''
    whileLoop : WHILE expression blockBody
    '''
    p[0] = p[1]


def p_repeatLoop(p):
    '''
    repeatLoop : REPEAT blockBody WHILE expression
    '''
    p[0] = p[1]


'''
        CONDITIONALS
'''


def p_branch(p):
    '''
    branch : if
           | guard
           | switch
    '''
    p[0] = p[1]


def p_if(p):
    '''
    if : ifElseIf
       | ifElseIf elseEnd
    '''
    p[0] = p[1]


def p_ifElseIf(p):
    '''
    ifElseIf : IF expression blockBody
             | ifElseIf ELSE ifElseIf
    '''
    p[0] = p[1]


def p_elseEnd(p):
    '''
    elseEnd : ELSE blockBody
    '''
    p[0] = p[1]


def p_guard(p):
    '''
    guard : GUARD expression elseEnd
    '''
    p[0] = p[1]


def p_switch(p):
    '''
    switch : SWITCH expression CURLY_L switchBody
    '''
    p[0] = p[1]


def p_switchBody(p):
    '''
    switchBody : switchBodyMain CURLY_R
               | switchBodyMain caseBodyDefault CURLY_R
    '''
    p[0] = p[1]


def p_switchBodyMain(p):
    '''
    switchBodyMain : caseBody
                   | switchBodyMain caseBody
    '''
    p[0] = p[1]


def p_caseBody(p):
    '''
    caseBody : caseHeader COLON statement
             | caseBody statement
    '''
    p[0] = p[1]


def p_caseHeader(p):
    '''
    caseHeader : CASE caseCondition
               | CASE caseCondition WHERE expression
    '''
    p[0] = p[1]


def p_caseBodyDefault(p):
    '''
    caseBodyDefault : DEFAULT COLON statement
                    | caseBodyDefault statement
    '''
    p[0] = p[1]


def p_caseCondition(p):
    '''
    caseCondition : expression
                  | caseCondition COMMA expression
    '''
    p[0] = p[1]


'''
        CLASSES
'''


def p_class(p):
    '''
    class : classDeclaration classBody
    '''
    p[0] = p[1]


def p_classDeclaration(p):
    '''
    classDeclaration : CLASS IDENTIFIER
                     | CLASS IDENTIFIER COLON type
    '''
    p[0] = p[1]
    types[p[2]] = 1
    # Needed to make constructors work
    functions[p[2]] = 1


def p_classBodyMain(p):
    '''
    classBodyMain : CURLY_L
                  | classBodyMain classItem
    '''
    p[0] = p[1]


def p_classBody(p):
    '''
    classBody : classBodyMain CURLY_R
    '''
    p[0] = p[1]


def p_classItem(p):
    '''
    classItem : classAttribute
              | classInit
              | classDeinit
              | variableDeclaration
              | functionDeclaration
    '''
    p[0] = p[1]


def p_classAttribute(p):
    '''
    classAttribute : VAR IDENTIFIER COLON type blockBody
    '''
    p[0] = p[1]


def p_classInit(p):
    '''
    classInit : convenienceInit INIT BRACKET_L argumentList BRACKET_R blockBody
    '''
    p[0] = p[1]


def p_classDeinit(p):
    '''
    classDeinit : DEINIT blockBody
    '''
    p[0] = p[1]


def p_convenienceInit(p):
    '''
    convenienceInit : CONVENIENCE
                    | epsilon
    '''
    p[0] = p[1]


'''
        EXPRESSION
'''


def p_bitwiseShift(p):
    '''
    bitwiseShift : multiplication
                 | bitwiseShift BITWISESHIFTLEVELOP multiplication
    '''
    p[0] = p[1]


def p_multiplication(p):
    '''
    multiplication : addition
                   | multiplication MULTIPLICATIONLEVELOP addition
    '''
    p[0] = p[1]


def p_addition(p):
    '''
    addition : rangeFormation
             | addition ADDITIONLEVELOP rangeFormation
    '''
    p[0] = p[1]


def p_rangeFormation(p):
    '''
    rangeFormation  : casting
                    | rangeFormation RANGEFORMATIONLEVELOP casting
    '''
    p[0] = p[1]


def p_casting(p):
    '''
    casting : nilCoalescing
            | casting CASTINGLEVELOP nilCoalescing
    '''
    p[0] = p[1]


def p_nilCoalescing(p):
    '''
    nilCoalescing : comparison
                  | nilCoalescing NILCOALESCINGLEVELOP comparison
    '''
    p[0] = p[1]


def p_comparison(p):
    '''
    comparison  : logicalConjugation
                | comparison COMPARISONLEVELOP logicalConjugation
    '''
    p[0] = p[1]


def p_logicalConjugation(p):
    '''
    logicalConjugation  : default
                        | logicalConjugation LOGICALCONJUGATIONLEVELOP default
    '''
    p[0] = p[1]


def p_default(p):
    '''
    default : ternary
            | default DEFAULTLEVELOP ternary
    '''
    p[0] = p[1]


def p_ternary(p):
    '''
    ternary : assignment
            | ternary TERNARYLEVELOP assignment
    '''
    p[0] = p[1]


def p_assignment(p):
    '''
    assignment  : term
                | assignment ASSIGNMENTLEVELOP term
    '''
    p[0] = p[1]


def p_term(p):
    '''
    term : assignable
         | BRACKET_L expression BRACKET_R
    '''
    p[0] = p[1]


def p_postfixOperator(p):
    '''
    postfixOperator : RANGE_OPERATOR
                    | POSTFIX_OPERATOR
    '''
    p[0] = p[1]


'''
    ERROR
    HANDLING
'''


def p_do(p):
    '''
    do : DO CURLY_L doBody CURLY_R catch
    '''
    p[0] = p[1]


def p_doBody(p):
    '''
    doBody  : statements doBody
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
    catchBody   : statements
                | TRY statements
                | catchBody
                | epsilon
    '''
    p[0] = p[1]


'''
        COMMON RULES
'''


def p_sourceFile(p):
    '''
    sourceFile : statements
    '''
    p[0] = p[1]


def p_import(p):
    '''
    import : IMPORT IDENTIFIER
           | IMPORT importType IDENTIFIER PERIOD IDENTIFIER
           | import PERIOD IDENTIFIER
    '''
    p[0] = p[1]


def p_importType(p):
    '''
    importType : TYPEALIAS
               | STRUCT
               | CLASS
               | ENUM
               | PROTOCOL
               | LET
               | VAR
               | FUNC
    '''
    p[0] = p[1]
    pass


def p_statements(p):
    '''
    statements : statement delimiter
               | statements statement delimiter
    '''
    p[0] = p[1]
    pass


def p_statement(p):
    '''
    statement : loop
              | branch
              | import
              | functionDeclaration
              | functionCall
              | variableDeclaration
              | constantAssignment
              | variableAssignment
              | returnStatement
              | class
              | do
    '''
    p[0] = p[1]


def p_delimiter(p):
    '''
    delimiter : SEMICOLON
              | epsilon
    '''
    p[0] = p[1]


def p_returnStatement(p):
    '''
    returnStatement : RETURN expression
    '''
    p[0] = p[1]


def p_type(p):
    '''
    type : TYPE
         | TYPE QUESTION_MARK
    '''
    p[0] = p[1]
    pass


def p_expression(p):
    '''
    expression  : bitwiseShift
                | PREFIX_OPERATOR bitwiseShift
                | bitwiseShift postfixOperator
    '''
    p[0] = p[1]


def p_blockBodyMain(p):
    '''
    blockBodyMain : statement
                  | blockBodyMain statement
    '''
    p[0] = p[1]


def p_blockBody(p):
    '''
    blockBody : CURLY_L blockBodyMain CURLY_R
              | CURLY_L CURLY_R
    '''
    p[0] = p[1]


'''
        TECHNICAL
'''


def p_epsilon(p):
    'epsilon :'
    pass


def p_assignable(p):
    '''
    assignable : FUNCTION
               | expression
               | VARIABLE
               | CONSTANT
               | literal
    '''
    p[0] = p[1]


def p_literal(p):
    '''
    literal : INT_LITERAL
            | FLOAT_LITERAL
            | STRING_LITERAL
            | NIL_LITERAL
            | BOOLEAN_LITERAL
            | EXPRESSION_LITERAL
    '''
    p[0] = p[1]


def p_error(p):
    if p:
        print("Error at line " + str(p.lineno) + " column " + str(lexer.find_tok_column(p)))
        print(p)
    else:
        print("EOF error")


yacc.yacc(start="sourceFile")

lexer = swiftLexer.LexerWrap()

s = ''' class c {
    init(x:Bool) { }
}'''
yacc.parse(s, lexer=lexer)
pass
