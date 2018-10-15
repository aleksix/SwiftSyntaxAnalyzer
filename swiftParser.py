import ply.yacc as yacc
import argparse
import swiftLexer
import json

# Tokens, needed by yacc here from lex
tokens = swiftLexer.tokens

# Resulting tree
res = None

# Output file
outfile = open("out.txt", "w")

'''
 Explanation:
 The lookup is conducted by the operator itself. We can have an operator in 3 different types :
  prefix, infix and postfix
 For prefix and postfix, we only care that the operator exists in such a state.
 But for infix we need to know the precedence of the operator
 Thus, the structure is {"operator" : {"prefix":1, "infix":"precedenceGroup", "postfix":1}} - best case

 I don't think we can quite use the PLY built-in precedence table since, best-case, operators can get added dynamically
 I know nothing about dynamic rule generation in PLY, so this version will have to do
'''
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


# Try to get the possible operator type in it's position
def operator_lookup(opeartor):
    return operatorsInfo.get(opeartor, "ID")


# Function binding
swiftLexer.operator_lookup = operator_lookup

'''
        CONSTANTS
'''


def p_constantDeclaration(p):
    '''
    constantDeclaration : variableModifiers LET varName
                        | constantDeclaration COMMA varName
    '''

    if not isinstance(p[1], dict):
        # single constant
        p[0] = {"constants": [p[3]]}
        if p[1] is not None:
            p[0]["modifiers"] = p[1]
    else:
        # multiple constants
        p[0] = {"constants": p[1]["constants"] + [p[3]]}
        if p[1].get("modifiers", False):
            p[0]["modifiers"] = p[1]["modifiers"]


'''
        VARIABLES
'''


def p_variableModifiers(p):
    '''
    variableModifiers : FILEPRIVATE
                      | epsilon
    '''
    p[0] = p[1]


def p_varName(p):
    '''
        varName : IDENTIFIER varNameTrailer
    '''
    p[0] = {"varName": p[1]}
    if p[2] is not None:
        for key in p[2]:
            p[0][key] = p[2][key]


def p_varNameTrailer(p):
    '''
    varNameTrailer : EQUAL expression
                   | COLON type
                   | COLON type EQUAL expression
                   | epsilon
    '''

    if len(p) == 2:
        # epsilon
        pass
    elif len(p) == 3:
        # equality or type
        p[0] = {}
        # equality
        if p[1] == "=":
            p[0]["equal"] = p[2]
        else:
            # type
            p[0]["type"] = p[2]
    else:
        # equality AND type
        p[0] = {"equal": p[4], "type": p[2]}


def p_variableDeclaration(p):
    '''
    variableDeclaration : variableModifiers VAR varName
                        | variableDeclaration COMMA varName
    '''
    if not isinstance(p[1], dict):
        # single constant
        p[0] = {"variables": [p[3]]}
        if p[1] is not None:
            p[0]["modifiers"] = p[1]
    else:
        # multiple constants
        p[0] = {"variables": p[1]["variables"] + [p[3]]}
        if p[1].get("modifiers", False):
            p[0]["modifiers"] = p[1]["modifiers"]


'''
        FUNCTIONS
'''


def p_functionDeclaration(p):
    '''
    functionDeclaration : FUNC IDENTIFIER BRACKET_L argumentList BRACKET_R throws returnType blockBody
    '''
    p[0] = {"function": p[2], "arguments": p[4], "body": p[8]}
    if p[6] is not None:
        p[0]["throws"] = True
    if p[7] is not None:
        p[0]["returns"] = p[7]


def p_returnType(p):
    '''
    returnType : ARROW type
                | epsilon
    '''
    if p[1] is not None:
        p[0] = p[2]


def p_argumentList(p):
    '''
    argumentList : argumentDeclaration
                 | argumentList COMMA argumentDeclaration
                 | epsilon
    '''
    if p[1] is not None:
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1] + [p[3]]


def p_argumentDeclaration(p):
    '''
    argumentDeclaration : argumentDescription
                        | argumentLabel argumentDescription
    '''
    p[0] = {}
    if len(p) == 3:
        p[0] = p[2]
        p[0]["label"] = p[1]
    else:
        p[0] = p[1]


def p_argumentDescription(p):
    '''
    argumentDescription : IDENTIFIER COLON argumentInout type argumentType
    '''
    p[0] = {"name": p[1]}
    if p[3] is not None:
        p[0]["inout"] = p[3]
    p[0]["type"] = p[4]
    if p[5] is not None:
        for key in p[5]:
            p[0][key] = p[5][key]


def p_argumentInout(p):
    '''
    argumentInout   : INOUT
                    | epsilon
    '''
    if p[1] is not None:
        p[0] = True


def p_argumentType(p):
    '''
    argumentType : RANGE_OPERATOR
                | EQUAL expression
                | epsilon
    '''
    if p[1] == "...":
        p[0] = {"variadic": True}
    elif p[1] == "=":
        p[0] = {"defaultValue": p[2]}


def p_argumentLabel(p):
    '''
    argumentLabel : IDENTIFIER
                 | UNDERSCORE
                 | epsilon
    '''
    p[0] = p[1]


def p_throws(p):
    '''    
    throws  : THROWS
            | epsilon
    '''
    if p[1] is not None:
        p[0] = True


def p_functionCall(p):
    '''    
    functionCall : assignable BRACKET_L callArgumentList BRACKET_R
    '''
    p[0] = {"functionCall": p[1]["item"], "arguments": p[3]}


def p_callArgumentList(p):
    '''
    callArgumentList : callArgument
                     | callArgumentList COMMA callArgument
                     | epsilon
    '''
    if len(p) == 2:
        if p[1] is not None:
            p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]


def p_callArgument(p):
    '''
    callArgument : callArgumentReference expression
                 | callArgumentReference callArgumentLabel expression
    '''
    'The first part is a hacky one - PLY gets confused by consecutive IDENTIFIER from different rules'
    p[0] = {}
    if len(p) == 4:
        p[0]["label"] = p[2]
        p[0]["callArgument"] = p[3]
    else:
        p[0]["callArgument"] = p[2]
    if p[1] is not None:
        p[0]["referenced"] = True


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
        LOOPS
'''


def p_loop(p):
    '''
    loop : loopLabel forLoop
         | loopLabel whileLoop
         | loopLabel repeatLoop
    '''
    if p[1] is not None:
        p[2]["label"] = p[1]
    p[0] = p[2]


def p_loopLabel(p):
    '''
    loopLabel : IDENTIFIER COLON
              | epsilon
    '''
    p[0] = {"loopLabel": p[1]}


def p_forLoop(p):
    '''
    forLoop : FOR IDENTIFIER IN expression blockBody
    '''
    p[0] = {"for": {"body": p[5], "expression": p[4]}}


def p_whileLoop(p):
    '''
    whileLoop : WHILE expression blockBody
    '''
    p[0] = {"while": {"body": p[3], "condition": p[2]}}


def p_repeatLoop(p):
    '''
    repeatLoop : REPEAT blockBody WHILE expression
    '''
    p[0] = {"repeat": {"body": p[2], "condition": p[4]}}


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
    if len(p) == 3:
        p[0]["else"] = {"body": p[2]}


def p_ifElseIf(p):
    '''
    ifElseIf : IF expression blockBody
             | IF constantDeclaration blockBody
             | ifElseIf ELSE if
    '''
    if p[1] == "if":
        p[0] = {"if": {"condition": p[2], "body": p[3]}}
    else:
        p[1]["else"] = p[3]
        p[0] = p[1]


def p_elseEnd(p):
    '''
    elseEnd : ELSE blockBody
    '''
    p[0] = p[2]


def p_guard(p):
    '''
    guard : GUARD expression elseEnd
    '''
    p[0] = {"guard": p[2], "else": {"body": p[3]}}


def p_switch(p):
    '''
    switch : SWITCH expression CURLY_L switchBody
    '''
    p[0] = {"switch": {"statement": p[2], "body": p[4]}}


def p_switchBody(p):
    '''
    switchBody : switchBodyMain CURLY_R
               | switchBodyMain caseBodyDefault CURLY_R
    '''
    p[0] = buildTree("switch", p[1:len(p) - 1])


def p_switchBodyMain(p):
    '''
    switchBodyMain : caseBody
                   | switchBodyMain caseBody
    '''
    p[0] = p[1:]


def p_caseBody(p):
    '''
    caseBody : caseHeader COLON statement
             | caseBody statement
    '''
    p[0] = p[1]
    if len(p) == 4:
        p[0]["body"] = [p[3]]
    else:
        p[0]["body"] += [p[2]]


def p_caseHeader(p):
    '''
    caseHeader : CASE caseCondition
               | CASE caseCondition WHERE expression
    '''
    p[0] = {"case": {"conditions": p[2]}}
    if len(p) == 5:
        p[0]["case"]["specification"] = p[4]


def p_caseBodyDefault(p):
    '''
    caseBodyDefault : DEFAULT COLON statement
                    | caseBodyDefault statement
    '''
    if len(p) == 4:
        p[0] = {"defaultCase": {"body": [p[3]]}}
    else:
        p[0] = p[1]
        p[0]["defaultCase"]["body"] += [p[2]]


def p_caseCondition(p):
    '''
    caseCondition : expression
                  | caseCondition COMMA expression
    '''
    if len(p) == 2:
        # only 1 expression
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]


'''
        CLASSES
'''


def p_class(p):
    '''
    class : classDeclaration classBody
    '''
    p[0] = {"class": p[1:]}


def p_classDeclaration(p):
    '''
    classDeclaration : CLASS IDENTIFIER
                     | CLASS IDENTIFIER COLON type
    '''
    p[0] = {"className": p[2]}
    if len(p) == 5:
        p[0]["inherits"] = p[4]


def p_classBodyMain(p):
    '''
    classBodyMain : classItem
                  | classBodyMain classItem
    '''
    p[0] = p[1:]


def p_classBody(p):
    '''
    classBody : CURLY_L classBodyMain CURLY_R
    '''
    p[0] = {"classBody": p[2]}


def p_classItem(p):
    '''
    classItem : classItemModifier classAttribute
              | classItemModifier classInit
              | classDeinit
              | classItemModifier variableDeclaration
              | classItemModifier functionDeclaration
    '''
    if len(p) == 2:
        p[0] = {"classItem": p[2]}
        if p[1] is not None:
            p[0]["modifier"] = p[1]
    else:
        p[0] = {"classItem": p[1]}


def p_classItemModifier(p):
    '''
    classItemModifier   : FINAL
                        | OVERRIDE
                        | PRIVATE
                        | epsilon
    '''
    p[0] = p[1]


def p_classAttribute(p):
    '''
    classAttribute : VAR IDENTIFIER COLON type blockBody
    '''
    p[0] = {"classAttribute": p[2], "type": p[4], "body": p[5]}


def p_classInit(p):
    '''
    classInit : convenienceInit INIT BRACKET_L argumentList BRACKET_R blockBody
    '''
    p[0] = {}
    if p[1] is not None:
        p[0]["type"] = "convenience"
    p[0]["init"] = p[6]
    p[0]["arguments"] = p[4]


def p_classDeinit(p):
    '''
    classDeinit : DEINIT blockBody
    '''
    p[0] = {"deinit", p[2]}


def p_convenienceInit(p):
    '''
    convenienceInit : CONVENIENCE
                    | epsilon
    '''
    if p[1]:
        p[0] = {"convenience": p[1]}


'''
        EXPRESSION
'''


def p_expression(p):
    '''
    expression  : assignment
                | PREFIX_OPERATOR assignment
                | assignment postfixOperator
    '''
    p[0] = {"expression": p[1:]}


def p_bitwiseShift(p):
    '''
    bitwiseShift : term
                 | bitwiseShift BITWISESHIFTLEVELOP term
    '''
    p[0] = buildExpressionTree(p)


def p_multiplication(p):
    '''
    multiplication : bitwiseShift
                   | multiplication MULTIPLICATIONLEVELOP bitwiseShift
    '''
    p[0] = buildExpressionTree(p)


def p_addition(p):
    '''
    addition : multiplication
             | addition ADDITIONLEVELOP multiplication
    '''
    p[0] = buildExpressionTree(p)


def p_rangeFormation(p):
    '''
    rangeFormation  : addition
                    | rangeFormation RANGEFORMATIONLEVELOP addition
                    | rangeFormation RANGE_OPERATOR addition
    '''
    p[0] = buildExpressionTree(p)


def p_casting(p):
    '''
    casting : rangeFormation
            | casting CASTINGLEVELOP rangeFormation
    '''
    p[0] = buildExpressionTree(p)


def p_nilCoalescing(p):
    '''
    nilCoalescing : casting
                  | nilCoalescing NILCOALESCINGLEVELOP casting
    '''
    p[0] = buildExpressionTree(p)


def p_comparison(p):
    '''
    comparison  : nilCoalescing
                | comparison COMPARISONLEVELOP nilCoalescing
                | comparison LESS_THAN nilCoalescing
                | comparison GREATER_THAN nilCoalescing
    '''
    'The rules with LESS_THAN/GREATER_THEN were done because they could also mean a generic'
    p[0] = buildExpressionTree(p)


def p_logicalConjugation(p):
    '''
    logicalConjugation  : comparison
                        | logicalConjugation LOGICALCONJUGATIONLEVELOP comparison
    '''
    p[0] = buildExpressionTree(p)


def p_default(p):
    '''
    default : logicalConjugation
            | default DEFAULTLEVELOP logicalConjugation
    '''
    p[0] = buildExpressionTree(p)


def p_ternary(p):
    '''
    ternary : default
            | ternary TERNARYLEVELOP default
            | expression QUESTION_MARK expression COLON expression
    '''
    if len(p) > 2 and p[2] == "?":
        p[0] = {"ternaryOperator": {"condition": p[1], "true": p[3], "false": p[5]}}
    else:
        p[0] = buildExpressionTree(p)


def p_assignment(p):
    '''
    assignment  : ternary
                | assignment ASSIGNMENTLEVELOP ternary
                | assignment EQUAL ternary
    '''
    p[0] = buildExpressionTree(p)


def p_term(p):
    '''
    term : assignable
         | BRACKET_L expression BRACKET_R
    '''
    p[0] = buildExpressionTree(p)


def p_postfixOperator(p):
    '''
    postfixOperator : RANGE_OPERATOR
                    | POSTFIX_OPERATOR
    '''
    p[0] = {"postfixOperator": p[1]}


'''
    ERROR
    HANDLING
'''


def p_do(p):
    '''
    do : DO CURLY_L doBody CURLY_R catch
    '''
    p[0] = buildTree("do", p[1:])


def p_doBody(p):
    '''
    doBody  : statements doBody
            | TRY statements doBody
            | epsilon
    '''
    p[0] = buildTree("doBody", p[1:])


def p_catch(p):
    '''
    catch : CATCH IDENTIFIER CURLY_L catchBody CURLY_R
          | epsilon
    '''
    p[0] = buildTree("catch", p[1:])


def p_catchBody(p):
    '''
    catchBody   : statements
                | TRY statements
                | epsilon
    '''
    p[0] = buildTree("catchBody", p[1:])


'''
        COMMON RULES
'''


def p_sourceFile(p):
    '''
    sourceFile : statements
    '''
    p[0] = {"sourceFile": p[1]}
    global res
    res = p[0]


def p_import(p):
    '''
    import : IMPORT IDENTIFIER
           | IMPORT importType IDENTIFIER PERIOD IDENTIFIER
           | import PERIOD IDENTIFIER
    '''
    if len(p) == 3:
        p[0] = {"import": p[2]}
    elif p[2].get("importType", False):
        p[0] = {"import": ''.join(p[3:]), "importType": p[2]["importType"]}
    else:
        p[0] = {"import": ''.join(p[2:])}


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


def p_statements(p):
    '''
    statements : statement delimiter
               | statements statement delimiter
    '''
    if len(p) == 3:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]


def p_statement(p):
    '''
    statement : loop
              | branch
              | import
              | functionDeclaration
              | functionCall
              | variableDeclaration
              | constantDeclaration
              | returnStatement
              | class
              | do
              | expression
              | typeAlias
              | flowControl
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
    p[0] = {"return": p[2]}


def p_typealias(p):
    '''
    typeAlias : TYPEALIAS IDENTIFIER EQUAL type
    '''
    p[0] = {"typealias": {"alias": p[2], "type": p[4]}}


def p_type(p):
    '''
    type : IDENTIFIER
         | IDENTIFIER POSTFIX_QUESTION
         | BRACKET_L tupleType BRACKET_R
         | BRACKET_L functionType BRACKET_R
         | BRACKET_L functionType BRACKET_R ARROW type
    '''
    '''
    Soooooo, tupleType with no labels IS functionType.
    SCREW YOU SWIFT
    '''
    if p[1] != "(":
        # Non-tuple type
        p[0] = {"type": p[1]}
        if len(p) == 3:
            p[0]["optional"] = True
    else:
        if p[len(p) - 2] == "->":
            # functionType
            p[2]["return"] = p[5]
            p[0] = {"functionType": p[2]}
        else:
            # Tuple
            p[0] = {"tupleType": p[2]}


def p_functionType(p):
    '''
    functionType : type
                 | functionType COMMA type
    '''
    if len(p) == 2:
        # single type
        p[0] = {"argumentTypes": [p[1]]}
    else:
        p[0] = {"argumentTypes": p[1]["argumentTypes"] + [p[3]]}


def p_tupleType(p):
    '''
    tupleType : argumentLabel type
              | tupleType COMMA tupleType
    '''
    if len(p) == 3:
        # single type
        item = {"item": p[2]}
        if p[1] is not None:
            item["label"] = p[2]
        p[0] = {"tupleTypes": [item]}
    else:
        p[0] = {"tupleTypes": p[1]["tupleTypes"] + [p[3]["tupleTypes"]]}


def p_flowControl(p):
    '''
    flowControl : flowControlName loopLabel
    '''
    p[0] = p[1]


def p_flowControlName(p):
    '''
    flowControlName : BREAK
                    | CONTINUE
                    | FALLTHROUGH
                    | RETURN
                    | THROW
    '''
    p[0] = p[1]


def p_blockBody(p):
    '''
    blockBody : CURLY_L statements CURLY_R
              | CURLY_L CURLY_R
    '''
    if len(p) == 4:
        p[0] = {"codeBlock": p[2]}


def p_idlist(p):
    '''
    idlist : IDENTIFIER
           | idlist COMMA IDENTIFIER
    '''
    p[0] = p[1:]


'''
        TECHNICAL
'''


# PLY doesn't have a built-in epsilon
def p_epsilon(p):
    'epsilon :'
    pass


def p_assignable(p):
    '''
    assignable : literal
               | IDENTIFIER
               | assignable trailer
               | assignable PERIOD assignable
               | assignable EXCLAMATION_MARK
               | assignable POSTFIX_QUESTION
               | functionCall
    '''
    if len(p) == 2:
        if isinstance(p[1], dict):
            p[0] = p[1]
        else:
            p[0] = {"item": p[1]}
    else:
        p[0] = {}
        if isinstance(p[1], dict) and p[1].get("item", False):
            p[0]["item"] = p[1]["item"]
        else:
            p[0]["item"] = p[1]
        # Moved for readability
        p[0]["postfixes"] = p[2:]


def p_trailer(p):
    '''
    trailer : SQUARE_L idlist SQUARE_R
            | SQUARE_L expression SQUARE_R
            | SQUARE_L trailer SQUARE_R
    '''
    p[0] = {"trailer": p[2]}


def p_literal(p):
    '''
    literal : INT_LITERAL
            | FLOAT_LITERAL
            | STRING_LITERAL
            | NIL_LITERAL
            | BOOLEAN_LITERAL
            | EXPRESSION_LITERAL
    '''
    p[0] = {"literal": lexer.literal_type(p[1]), "value": p[1]}


def p_error(p):
    if p:
        print("Error at line " + str(p.lineno) + " column " + str(lexer.find_tok_column(p)))
        print(p)
        outfile.write("Error at line " + str(p.lineno) + " column " + str(lexer.find_tok_column(p)) + "\n")
        outfile.write(str(p) + "\n")
    else:
        print("EOF error")
        outfile.write("EOF error\n")


yacc.yacc(start="sourceFile", errorlog=yacc.NullLogger())

lexer = swiftLexer.LexerWrap()


def buildTree(name, contents):
    return {"name": name, "contents": contents}


def buildExpressionTree(p):
    # Term condensed into 1 item
    if len(p) == 2:
        p[0] = p[1]
    # Bracketed term
    elif len(p) == 3:
        p[0] = p[2]
    else:
        p[0] = {"operator": p[2], "left": p[1], "right": p[3]}
    return p[0]


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Syntax Analyzer')
    parser.add_argument('input', metavar='in', type=str,
                        help='Full or relative (according to project folder) path  to file to process by the program')

    args = parser.parse_args()
    input_file = args.input

    with open(input_file) as f:
        content = f.read()
        yacc.parse(content, lexer=lexer)
        if res is not None:
            outfile.write(json.dumps(res, indent=4))
        outfile.close()
