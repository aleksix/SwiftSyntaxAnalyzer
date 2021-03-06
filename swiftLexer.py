import ply.lex as lex
import re
from ply.lex import TOKEN

# All of the keywords
keywords = ["class", "deinit", "enum", "extension", "func", "import", "init", "internal", "let", "operator",
            "private",
            "protocol", "public", "static", "struct", "subscript", "typealias", "var", "open", "inout",
            "fileprivate",
            "break", "case", "continue", "default", "do", "else", "fallthrough", "for", "if", "in", "return",
            "switch",
            "where", "while", "defer", "guard", "repeat", "as", "dynamicType", "is", "self", "Self", "super", "Any",
            "catch", "rethrows", "throw", "throws", "try"]

# All of the keywords that are keywords only in certain cases
context_keywords = ["associativity", "convenience", "dynamic", "didSet", "final", "get", "infix", "indirect",
                    "lazy",
                    "left", "mutating", "none", "nonmutating", "optional", "override", "postfix", "precedence",
                    "prefix", "Protocol", "required", "right", "set", "Type", "unowned", "weak", "willSet"]

# Expression literals
expression_literals = ["#keyPath", "#line", "#selector", "#file",
                       "#column", "#function", "#dsohandle", "#sourceLocation", "#warning",
                       "#error", "#if", "#else", "#elseif", "#endif",
                       "#available", "#fileLiteral", "#imageLiteral", "#colorLiteral"]


def generate_token_map(literals):
    out_map = {}
    for c in range(len(literals)):
        out_map[literals[c]] = literals[c].upper()
        literals[c] = literals[c].upper()
    return out_map, literals


keyword_map, keywords = generate_token_map(keywords)
context_keyword_map, context_keywords = generate_token_map(context_keywords)

expression_literals_map = {}
# Bad, but it works
for c in range(len(expression_literals)):
    expression_literals_map[expression_literals[c]] = "EXPRESSION_LITERAL_" + expression_literals[c][1:].upper()
    expression_literals[c] = "EXPRESSION_LITERAL_" + expression_literals[c][1:].upper()

operator_groups = ["Assignment", "Ternary", "Default", "LogicalConjugation", "Comparison", "NilCoalescing", "Casting",
                   "RangeFormation", "Addition", "Multiplication", "BitwiseShift"]

operator_groups = [og.upper() + "LEVELOP" for og in operator_groups]

# All of the tokens
# list(set()) is here to cull the tokens that were automatically generated
tokens = list(set(keywords + context_keywords + operator_groups + ["IDENTIFIER", "STRING_LITERAL", "INT_LITERAL",
                                                                   "FLOAT_LITERAL", "BOOLEAN_LITERAL", "NIL_LITERAL",
                                                                   "BRACKET_L", "BRACKET_R", "CURLY_L", "CURLY_R",
                                                                   "SQUARE_L",
                                                                   "SQUARE_R", "SEMICOLON", "COLON", "COMMA", "PERIOD",
                                                                   "AT",
                                                                   "POUND", "POSTFIX_QUESTION", "PREFIX_DOT",
                                                                   "AMPERSAND",
                                                                   "UNDERSCORE", "GREATER_THAN", "LESS_THAN", "ARROW",
                                                                   "EQUAL",
                                                                   "RANGE_OPERATOR", "BACKTICK", "QUESTION_MARK",
                                                                   "EXCLAMATION_MARK", "ERROR", "PREFIX_AMPERSAND",
                                                                   "PREFIX_OPERATOR", "POSTFIX_OPERATOR",
                                                                   "EXPRESSION_LITERAL"]))

# BEHOLD THE UNHOLY ABOMINATION OF REGULAR EXPRESSIVENESS
# I really feel like I'm writing some sort of a spell from some forbidden book with this one.

'''

    NOTE:
    DO NOT MESS WITH THESE STRINGS TOO MUCH
    BREAKING THEM ON SEPARATE STRINGS MESSES WITH THE REGEXPS

'''

identifier = r"""('?)([\u0041-\u005A]|[\u0061-\u007A]|[\u00B2-\u00B5]|[\u00B7-\u00BA]|[\u00BC-\u00BE]|[\u00C0-\u00D6]|[\u00D8-\u00F6]|[\u00F8-\u00FF]|[\u0100-\u02FF]|[\u0370-\u167F]|[\u1681-\u180D]|[\u180F-\u1DBF]|[\u1E00-\u1FFF]|[\u200B-\u200D]|[\u202A-\u202E]|[\u203F-\u2040]|[\u2060-\u206F]|[\u2070-\u20CF]|[\u2100-\u218F]|[\u2460-\u24FF]|[\u2776-\u2793]|[\u2C00-\u2DFF]|[\u2E80-\u2FFF]|[\u3004-\u3007]|[\u3021-\u302F]|[\u3031-\u303F]|[\u3040-\uD7FF]|[\uF900-\uFD3D]|[\uFD40-\uFDCF]|[\uFDF0-\uFE1F]|[\uFE30-\uFE44]|[\uFE47-\uFFFD]|[\U00010000-\U0001FFFD]|[\U00020000-\U0002FFFD]|[\U00030000-\U0003FFFD]|[\U00040000-\U0004FFFD]|[\U00050000-\U0005FFFD]|[\U00060000-\U0006FFFD]|[\U00070000-\U0007FFFD]|[\U00080000-\U0008FFFD]|[\U00090000-\U0009FFFD]|[\U000A0000-\U000AFFFD]|[\U000B0000-\U000BFFFD]|[\U000C0000-\U000CFFFD]|[\U000D0000-\U000DFFFD]|[\U000E0000-\U000EFFFD]|\u00A8|\u00AA|\u00A8|\u00AA|\u005F|\u00AD|\u00AF|\u2054)(([\u0041-\u005A]|[\u0061-\u007A]|[\u00B2-\u00B5]|[\u00B7-\u00BA]|[\u00BC-\u00BE]|[\u00C0-\u00D6]|[\u00D8-\u00F6]|[\u00F8-\u00FF]|[\u0100-\u02FF]|[\u0370-\u167F]|[\u1681-\u180D]|[\u180F-\u1DBF]|[\u1E00-\u1FFF]|[\u200B-\u200D]|[\u202A-\u202E]|[\u203F-\u2040]|[\u2060-\u206F]|[\u2070-\u20CF]|[\u2100-\u218F]|[\u2460-\u24FF]|[\u2776-\u2793]|[\u2C00-\u2DFF]|[\u2E80-\u2FFF]|[\u3004-\u3007]|[\u3021-\u302F]|[\u3031-\u303F]|[\u3040-\uD7FF]|[\uF900-\uFD3D]|[\uFD40-\uFDCF]|[\uFDF0-\uFE1F]|[\uFE30-\uFE44]|[\uFE47-\uFFFD]|[\U00010000-\U0001FFFD]|[\U00020000-\U0002FFFD]|[\U00030000-\U0003FFFD]|[\U00040000-\U0004FFFD]|[\U00050000-\U0005FFFD]|[\U00060000-\U0006FFFD]|[\U00070000-\U0007FFFD]|[\U00080000-\U0008FFFD]|[\U00090000-\U0009FFFD]|[\U000A0000-\U000AFFFD]|[\U000B0000-\U000BFFFD]|[\U000C0000-\U000CFFFD]|[\U000D0000-\U000DFFFD]|[\U000E0000-\U000EFFFD]|\u00A8|\u00AA|\u00A8|\u00AA|\u005F|\u00AD|\u00AF|\u2054)|[\u0030-\u0039]|[\u0300-\u036F]|[\u1DC0-\u1DFF]|[\u20D0-\u20FF]|[\uFE20-\uFE2F])*('?)"""
# Good luck with peer review, by the way

operator = r"""([\u00A1-\u00A7]|[\u2020-\u2027]|[\u2030-\u203E]|[\u2041-\u2053]|[\u2055-\u205E]|[\u2190-\u23FF]|[\u2500-\u2775]|[\u2794-\u2BFF]|[\u2E00-\u2E7F]|[\u3001-\u3003]|[\u3008-\u3020]|\/|\=|\-|\+|\!|\*|\%|\<|\>|\&|\||\^|\~|\?|\.|\@|\{|\}|\(|\)|\[|\]|\,|\:|\;|\u3030|\u2016|\u2017|\u00A9|\u00AB|\u00AC|\u00AE|\u00B0|\u00B1|\u00B6|\u00BB|\u00BF|\u00D7|\u00F7)(([\u00A1-\u00A7]|[\u2020-\u2027]|[\u2030-\u203E]|[\u2041-\u2053]|[\u2055-\u205E]|[\u2190-\u23FF]|[\u2500-\u2775]|[\u2794-\u2BFF]|[\u2E00-\u2E7F]|[\u3001-\u3003]|[\u3008-\u3020]|\/|\=|\-|\+|\!|\*|\%|\<|\>|\&|\||\^|\~|\?|\.|\@|\{|\}|\(|\)|\[|\]|\,|\:|\;|\u3030|\u2016|\u2017|\u00A9|\u00AB|\u00AC|\u00AE|\u00B0|\u00B1|\u00B6|\u00BB|\u00BF|\u00D7|\u00F7)|([\u0300-\u036F]|[\u1DC0-\u1DFF]|[\u20D0-\u20FF]|[\uFE00-\uFE0F]|[\uFE20-\uFE2F]|[\U000E0100-\U000E01EF]))*"""

t_EXPRESSION_LITERAL = r"""\#(keyPath|line|selector|file|column|function|dsohandle|sourceLocation|warning|error|if|else|elseif|endif|available|fileLiteral|imageLiteral|colorLiteral)"""
t_STRING_LITERAL = r'".*?"|"""\n(\s*).*\n(\s*)"""'

t_FLOAT_LITERAL = r'''0x[0-9A-Fa-f][0-9A-Fa-f_]*(\.[0-9A-Fa-f][0-9A-Fa-f_]*)?[pP][+-]?[0-9][0-9_]*|[0-9][0-9_]*[eE][+-]?[0-9][0-9_]*|[0-9][0-9]*\.[0-9][0-9_]*[eE][+-]?[0-9][0-9_]*|[0-9][0-9]*\.[0-9][0-9_]*'''

t_INT_LITERAL = r'0x[0-9a-fA-F][0-9a-fA-F_]*|0o[0-7][0-7_]*|0b[01][01_]*|[0-9][0-9_]*'

t_ignore = ' \t'


# Count newlines
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


# Get identifiers
@TOKEN(identifier)
def t_IDENTIFIER(t):
    if t.value[0] != "'" and t.value[len(t.value) - 1] != "'":
        if keyword_map.get(t.value, "ID") != 'ID':
            t.type = keyword_map[t.value].upper()
        elif context_keyword_map.get(t.value, "ID") != 'ID':
            t.type = context_keyword_map[t.value].upper()
        elif t.value == "true" or t.value == "false":
            t.type = "BOOLEAN_LITERAL"
        elif t.value == "nil":
            t.type = "NIL_LITERAL"
    return t


@TOKEN(operator)
def t_OPERATOR(t):
    t = check_operator(t)

    # Incorrect operator - due to swift handling existing operators separately, without regexps
    while t is not None and t.type == "OPERATOR":
        # SCREW PLY
        # For some reason, comments are NOT ignored by the lexer, even if the rules have higher precedence
        if t.value == "//":
            while t.lexer.lexpos < len(t.lexer.lexdata) and t.lexer.lexdata[t.lexer.lexpos] != '\n':
                t.lexer.lexpos += 1
            t = t.lexer.token()
        elif t.value == "/*":
            while t.lexer.lexpos < len(t.lexer.lexdata) and (
                    t.lexer.lexdata[t.lexer.lexpos - 1] != '*' or t.lexer.lexdata[t.lexer.lexpos] != "/"):
                if t.lexer.lexdata[t.lexer.lexpos] == "\n":
                    t.lexer.lineno += 1
                t.lexer.lexpos += 1
            t.lexer.lexpos += 1
            t = t.lexer.token()
        else:
            t.lexer.lexpos -= 1
            t.value = t.value[:len(t.value) - 1]
            t = check_operator(t)
    return t


# Tries to match the operator gotten from t_OPERATOR
def check_operator(operator):
    lexdata = operator.lexer.lexdata
    prefix = False
    postfix = False
    if operator.lexpos > 0:
        prefix = lexdata[operator.lexpos - 1].isspace()
    if operator.lexpos + len(operator.value) < len(lexdata):
        postfix = lexdata[operator.lexpos + len(operator.value)].isspace()

        # All the values below might have special meaning, hence the distinction
    if operator.value == '(':
        operator.type = "BRACKET_L"
    elif operator.value == ')':
        operator.type = "BRACKET_R"
    elif operator.value == '{':
        operator.type = "CURLY_L"
    elif operator.value == '}':
        operator.type = 'CURLY_R'
    elif operator.value == '[':
        operator.type = "SQUARE_L"
    elif operator.value == ']':
        operator.type = "SQUARE_R"
    elif operator.value == ',':
        operator.type = "COMMA"
    elif operator.value == '.':
        operator.type = "PERIOD"
        if prefix is True and prefix != postfix:
            operator.type = "PREFIX_DOT"
    elif operator.value == ';':
        operator.type = "SEMICOLON"
    elif operator.value == ':':
        operator.type = "COLON"
    elif operator.value == '@':
        operator.type = "AT"
    elif operator.value == '#':
        operator.type = "POUND"
    elif operator.value == '&':
        operator.type = "AMPERSAND"
        if prefix is True and prefix != postfix:
            operator.type = "PREFIX_AMPERSAND"
    elif len(operator.value) == 2 and operator.value[0] == '-' and operator.value[1] == '>':
        operator.type = "ARROW"
    elif operator.value == '`':
        operator.type = "BACKTICK"
    elif operator.value == '?':
        operator.type = "QUESTION_MARK"
        if postfix is True and prefix != postfix:
            operator.type = "POSTFIX_QUESTION"
    elif operator.value == '!':
        operator.type = "EXCLAMATION_MARK"
    elif operator.value == "<":
        operator.type = "LESS_THAN"
    elif operator.value == ">":
        operator.type = "GREATER_THAN"
    elif operator.value == "=":
        operator.type = "EQUAL"
    elif operator.value == "...":
        operator.type = "RANGE_OPERATOR"

    if operator.type == "OPERATOR":
        op = operator_lookup(operator.value)
        if op != "ID":
            if prefix == postfix:
                operator.type = op["infix"].upper() + "LEVELOP"
            elif prefix:
                operator.type = "PREFIX_OPERATOR"
            else:
                operator.type = "POSTFIX_OPERATOR"
    return operator


# Simple error function
def t_error(t):
    print("Illegal/ignored value '%s'" % t.value)
    t.lexer.skip(1)


# Function to check if an operator is an operator
# Was intended to work with custom operators, but we ran out of time
operator_lookup = None


# Lexer wrapper
class LexerWrap:
    def __init__(self, **kwargs):
        self.lexer = lex.lex(**kwargs)
        # Regexp patterns for matching literals
        self.int_pattern = re.compile(t_INT_LITERAL)
        self.float_pattern = re.compile(t_FLOAT_LITERAL)
        self.string_pattern = re.compile(t_STRING_LITERAL)
        self.expression_pattern = re.compile(t_EXPRESSION_LITERAL)

    def input(self, text):
        self.lexer.input(text)

    def token(self):
        tok = self.lexer.token()
        return tok

    def find_tok_column(self, token):
        """
        Find the column of the token in its line.
        :param token: token to be checked
        :return: column of the token
        """
        last_cr = self.lexer.lexdata.rfind('\n', 0, token.lexpos)
        return token.lexpos - last_cr

    def literal_type(self, value):
        """

        :param value:
        :return:
        """
        if self.int_pattern.match(value):
            return "integer"
        elif self.float_pattern.match(value):
            return "float"
        elif self.string_pattern.match(value):
            return "string"
        elif self.expression_pattern.match(value):
            return "expressionLiteral"
        elif value == "True" or value == "False":
            return "boolean"
        elif value == "nil":
            return "nil"
        # Shouldn't happen ever
        return None
