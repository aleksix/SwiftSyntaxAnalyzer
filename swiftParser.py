import ply.yacc as yacc
import swiftLexer
import json

tokens = swiftLexer.tokens

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


def operator_lookup(opeartor):
    return operatorsInfo.get(opeartor, "ID")


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
    p[0] = buildTree("constantDeclarationList", p[1:])


def p_constantDeclaration(p):
    '''
    constantDeclaration : constantDeclarationList
                        | constantDeclarationList COLON type
    '''
    p[0] = buildTree("constantDeclaration", p[1:])


def p_constantAssignment(p):
    '''
    constantAssignment : constantDeclaration EQUAL expression
    '''
    p[0] = {"constant": p[1], "value": p[3]}


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
    p[0] = {"modifier": p[1], "variable": p[3]}


def p_variableDeclaration(p):
    '''
    variableDeclaration : variableDeclarationList COLON type
                        | variableDeclarationStart COLON type
                        | variableDeclarationStart variableAssignment
                        | variableDeclarationStart COLON type variableAssignment
    '''
    p[0] = buildTree("variableDeclaration", p[1:])


def p_variableDeclarationList(p):
    '''
    variableDeclarationList : variableDeclarationStart
                            | variableDeclarationList COMMA IDENTIFIER
    '''
    p[0] = buildTree("declarationList", p[1:])


def p_variableAssignment(p):
    '''
    variableAssignment : EQUAL expression
    '''
    p[0] = p[2]


'''
        FUNCTIONS
'''


def p_functionDeclaration(p):
    '''
    functionDeclaration : functionModifiers FUNC IDENTIFIER BRACKET_L argumentList BRACKET_R throws returnType blockBody
    '''
    p[0] = {"modifier": p[1], "function": p[3], "arguments": p[5], "throws": p[7],
            "returns": p[8], "body": p[9]}


def p_returnType(p):
    '''
    returnType : ARROW type
                | epsilon
    '''
    if p[1] is not None:
        p[0] = p[2]


def p_functionModifiers(p):
    '''
    functionModifiers   : FINAL
                        | OVERRIDE
                        | PRIVATE
                        | epsilon
    '''
    p[0] = p[1]


def p_argumentList(p):
    '''
    argumentList : argumentDeclaration
                 | argumentList COMMA argumentDeclaration
                 | epsilon
    '''
    if p[1] is not None:
        p[0] = {"arguments": p[1:]}


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
    '''
    if p[1] != "_":
        p[0] = p[1]


def p_throws(p):
    '''    
    throws  : THROWS
            | epsilon
    '''
    p[0] = False
    if p[1] is not None:
        p[0] = True


def p_functionCall(p):
    '''    
    functionCall : assignable BRACKET_L callArgumentList BRACKET_R
    '''
    p[0] = {"functionCall": p[1], "arguments": p[3]}


def p_callArgumentList(p):
    '''
    callArgumentList : callArgument
                     | callArgumentList COMMA callArgument
    '''
    if len(p) == 2:
        p[0] = {"callArgumentList": [p[1]]}
    else:
        p[0] = p[1]
        p[0]["callArgumentList"].append(p[3])


def p_callArgument(p):
    '''
    callArgument : callArgumentReference expression
                 | callArgumentReference callArgumentLabel expression
    '''
    'The first part is a hacky one - PLY gets confused by consecutive IDENTIFIER from different rules'
    p[0] = {"referenced": p[1] is not None}
    if len(p) == 4:
        p[0]["label"] = p[2]
        p[0]["callArgument"] = p[3]
    else:
        p[0]["callArgument"] = p[2]


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
    p[0] = buildTree("loop", p[1:])


def p_loopLabel(p):
    '''
    loopLabel : IDENTIFIER COLON
              | epsilon
    '''
    p[0] = buildTree("loopLabel", p[1:])


def p_forLoop(p):
    '''
    forLoop : FOR IDENTIFIER IN expression blockBody
    '''
    p[0] = buildTree("for", p[1:])


def p_whileLoop(p):
    '''
    whileLoop : WHILE expression blockBody
    '''
    p[0] = buildTree("while", p[1:])


def p_repeatLoop(p):
    '''
    repeatLoop : REPEAT blockBody WHILE expression
    '''
    p[0] = buildTree("repeat", p[1:])


'''
        CONDITIONALS
'''


def p_branch(p):
    '''
    branch : if
           | guard
           | switch
    '''
    p[0] = buildTree("branch", p[1:])


def p_if(p):
    '''
    if : ifElseIf
       | ifElseIf elseEnd
    '''
    p[0] = buildTree("if", p[1:])


def p_ifElseIf(p):
    '''
    ifElseIf : IF expression blockBody
             | IF constantAssignment blockBody
             | ifElseIf ELSE if
    '''
    p[0] = buildTree("ifElse", p[1:])


def p_elseEnd(p):
    '''
    elseEnd : ELSE blockBody
    '''
    p[0] = buildTree("else", p[1:])


def p_guard(p):
    '''
    guard : GUARD expression elseEnd
    '''
    p[0] = buildTree("guard", p[1:])


def p_switch(p):
    '''
    switch : SWITCH expression CURLY_L switchBody
    '''
    p[0] = buildTree("switch", p[1:])


def p_switchBody(p):
    '''
    switchBody : switchBodyMain CURLY_R
               | switchBodyMain caseBodyDefault CURLY_R
    '''
    p[0] = buildTree("switchBody", p[1:])


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
    p[0] = buildTree("caseBody", p[1:])


def p_caseHeader(p):
    '''
    caseHeader : CASE caseCondition
               | CASE caseCondition WHERE expression
    '''
    p[0] = buildTree("caseHeader", p[1:])


def p_caseBodyDefault(p):
    '''
    caseBodyDefault : DEFAULT COLON statement
                    | caseBodyDefault statement
    '''
    p[0] = buildTree("caseBody", p[1:])


def p_caseCondition(p):
    '''
    caseCondition : expression
                  | caseCondition COMMA expression
    '''
    p[0] = buildTree("caseCondition", p[1:])


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
    classItem : classAttribute
              | classInit
              | classDeinit
              | variableDeclaration
              | functionDeclaration
    '''
    p[0] = buildTree("classItem", p[1:])


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
    p[0] = buildTree("deinit", p[1:])


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
                | assignable
    '''
    p[0] = p[1]


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
    '''
    p[0] = buildExpressionTree(p)


def p_assignment(p):
    '''
    assignment  : ternary
                | assignment ASSIGNMENTLEVELOP ternary
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
    p[0] = p[1]
    global res
    res = p[0]


def p_import(p):
    '''
    import : IMPORT IDENTIFIER
           | IMPORT importType IDENTIFIER PERIOD IDENTIFIER
           | import PERIOD IDENTIFIER
    '''
    if p[2].get("importType", False):
        p[0] = {"import": ''.join(str[3:]), "importType": p[2]["importType"]}
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
    p[0] = {"importType": p[1]}


def p_statements(p):
    '''
    statements : statement delimiter
               | statements statement delimiter
    '''
    p[0] = {"statement": p[1]}


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
              | expression
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
    p[0] = {"name": "returnStatement", "element": p[1]}


def p_typealias(p):
    '''
    typeAlias : TYPEALIAS IDENTIFIER EQUAL type
    '''
    p[0] = buildTree("typealieas", p[1:])


def p_type(p):
    '''
    type : IDENTIFIER
         | IDENTIFIER QUESTION_MARK
    '''
    p[0] = {"type": p[1], "optional": False}
    if p[2] is not None:
        p[0]["optional"] = True


def p_loopControl(p):
    '''
    loopControl : loopControls loopLabel
    '''
    p[0] = p[1]


def p_loopControls(p):
    '''
    loopControls : BREAK
                 | CONTINUE
    '''
    p[0] = p[1]


def p_blockBody(p):
    '''
    blockBody : CURLY_L statements CURLY_R
              | CURLY_L CURLY_R
    '''
    p[0] = buildTree("blockBody", p[1:])


def p_idlist(p):
    '''
    idlist : IDENTIFIER
           | idlist COMMA IDENTIFIER
    '''
    p[0] = buildTree("idlist", p[1:])


'''
        TECHNICAL
'''


def p_epsilon(p):
    'epsilon :'
    pass


def p_assignable(p):
    '''
    assignable : literal
               | IDENTIFIER
               | assignable trailer
               | assignable PERIOD IDENTIFIER
               | assignable EXCLAMATION_MARK
               | assignable QUESTION_MARK
    '''
    p[0] = p[1]


def p_trailer(p):
    '''
    trailer : SQUARE_L idlist SQUARE_R
            | SQUARE_L expression SQUARE_R
            | SQUARE_L trailer SQUARE_R
    '''
    p[0] = buildTree("trailer", p[1:])


def p_literal(p):
    '''
    literal : INT_LITERAL
            | FLOAT_LITERAL
            | STRING_LITERAL
            | NIL_LITERAL
            | BOOLEAN_LITERAL
            | EXPRESSION_LITERAL
    '''
    p[0] = buildTree("literal", p[1])


def p_error(p):
    if p:
        print("Error at line " + str(p.lineno) + " column " + str(lexer.find_tok_column(p)))
        print(p)
    else:
        print("EOF error")


yacc.yacc(start="sourceFile")

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


s = '''if keys[index] == key {
      if isLeaf {
        keys.remove(at: index)
        values.remove(at: index)
        owner.numberOfKeys -= 1
      } else {
        let predecessor = children![index].inorderPredecessor
        keys[index] = predecessor.keys.last!
        values[index] = predecessor.values.last!
        children![index].remove(keys[index])
        if children![index].numberOfKeys < owner.order {
          fix(childWithTooFewKeys: children![index], atIndex: index)
        }
      }
    } else if key < keys[index] {
      if let leftChild = children?[index] {
        leftChild.remove(key)
        if leftChild.numberOfKeys < owner.order {
          fix(childWithTooFewKeys: leftChild, atIndex: index)
        }
      } else {
        print("The key:\(key) is not in the tree.")
      }
    } else {
      if let rightChild = children?[(index + 1)] {
        rightChild.remove(key)
        if rightChild.numberOfKeys < owner.order {
          fix(childWithTooFewKeys: rightChild, atIndex: (index + 1))
        }
      } else {
        print("The key:\(key) is not in the tree")
      }
    }'''
s = '''5 + 10 * 11'''
yacc.parse(s, lexer=lexer)
print(json.dumps(res, indent=1))
