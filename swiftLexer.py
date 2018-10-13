import ply.lex as lex
from ply.lex import TOKEN
import re

keywords = ["class", "deinit", "enum", "extension", "func", "import", "init", "internal", "let", "operator",
            "private",
            "protocol", "public", "static", "struct", "subscript", "typealias", "var", "open", "inout",
            "fileprivate",
            "break", "case", "continue", "default", "do", "else", "fallthrough", "for", "if", "in", "return",
            "switch",
            "where", "while", "defer", "guard", "repeat", "as", "dynamicType", "is", "self", "Self", "super", "Any",
            "catch", "rethrows", "throw", "throws", "try"]

context_keywords = ["associativity", "convenience", "dynamic", "didSet", "final", "get", "infix", "indirect",
                    "lazy",
                    "left", "mutating", "none", "nonmutating", "optional", "override", "postfix", "precedence",
                    "prefix", "Protocol", "required", "right", "set", "Type", "unowned", "weak", "willSet"]

keyword_map = {}
for c in range(len(keywords)):
    keyword_map[keywords[c]] = keywords[c].upper()
    keywords[c] = keywords[c].upper()

context_keyword_map = {}
for c in range(len(context_keywords)):
    context_keyword_map[context_keywords[c]] = context_keywords[c].upper()
    context_keywords[c] = context_keywords[c].upper()

# All of the tokens
tokens = keywords + context_keywords + ["IDENTIFIER", "STRING_LITERAL", "BRACKET_L", "BRACKET_R", "CURLY_L", "CURLY_R",
                                        "SQUARE_L", "SQUARE_R", "SEMICOLON", "COLON", "COMMA", "PERIOD", "AT", "POUND",
                                        "AMPERSAND", "ARROW", "BACKTICK", "QUESTION_MARK", "EXCLAMATION_MARK", "ERROR"]

t_ignore = ' \t'

# BEHOLD THE UNHOLY ABOMINATION OF REGULAR EXPRESSIVENESS
# I really feel like I'm writing some sort of a spell from some forbidden book with this one.
# Good luck with peer review

identifier = r"""([\u0041-\u005A]|[\u0061-\u007A]|[\u00B2-\u00B5]|[\u00B7-\u00BA]|[\u00BC-\u00BE]|[\u00C0-\u00D6]|[\u00D8-\u00F6]|[\u00F8-\u00FF]|[\u0100-\u02FF]|[\u0370-\u167F]|[\u1681-\u180D]|[\u180F-\u1DBF]|[\u1E00-\u1FFF]|[\u200B-\u200D]|[\u202A-\u202E]|[\u203F-\u2040]|[\u2060-\u206F]|[\u2070-\u20CF]|[\u2100-\u218F]|[\u2460-\u24FF]|[\u2776-\u2793]|[\u2C00-\u2DFF]|[\u2E80-\u2FFF]|[\u3004-\u3007]|[\u3021-\u302F]|[\u3031-\u303F]|[\u3040-\uD7FF]|[\uF900-\uFD3D]|[\uFD40-\uFDCF]|[\uFDF0-\uFE1F]|[\uFE30-\uFE44]|[\uFE47-\uFFFD]|[\U00010000-\U0001FFFD]|[\U00020000-\U0002FFFD]|[\U00030000-\U0003FFFD]|[\U00040000-\U0004FFFD]|[\U00050000-\U0005FFFD]|[\U00060000-\U0006FFFD]|[\U00070000-\U0007FFFD]|[\U00080000-\U0008FFFD]|[\U00090000-\U0009FFFD]|[\U000A0000-\U000AFFFD]|[\U000B0000-\U000BFFFD]|[\U000C0000-\U000CFFFD]|[\U000D0000-\U000DFFFD]|[\U000E0000-\U000EFFFD]|\u00A8|\u00AA|\u00A8|\u00AA|\u005F|\u00AD|\u00AF|\u2054)(([\u0041-\u005A]|[\u0061-\u007A]|[\u00B2-\u00B5]|[\u00B7-\u00BA]|[\u00BC-\u00BE]|[\u00C0-\u00D6]|[\u00D8-\u00F6]|[\u00F8-\u00FF]|[\u0100-\u02FF]|[\u0370-\u167F]|[\u1681-\u180D]|[\u180F-\u1DBF]|[\u1E00-\u1FFF]|[\u200B-\u200D]|[\u202A-\u202E]|[\u203F-\u2040]|[\u2060-\u206F]|[\u2070-\u20CF]|[\u2100-\u218F]|[\u2460-\u24FF]|[\u2776-\u2793]|[\u2C00-\u2DFF]|[\u2E80-\u2FFF]|[\u3004-\u3007]|[\u3021-\u302F]|[\u3031-\u303F]|[\u3040-\uD7FF]|[\uF900-\uFD3D]|[\uFD40-\uFDCF]|[\uFDF0-\uFE1F]|[\uFE30-\uFE44]|[\uFE47-\uFFFD]|[\U00010000-\U0001FFFD]|[\U00020000-\U0002FFFD]|[\U00030000-\U0003FFFD]|[\U00040000-\U0004FFFD]|[\U00050000-\U0005FFFD]|[\U00060000-\U0006FFFD]|[\U00070000-\U0007FFFD]|[\U00080000-\U0008FFFD]|[\U00090000-\U0009FFFD]|[\U000A0000-\U000AFFFD]|[\U000B0000-\U000BFFFD]|[\U000C0000-\U000CFFFD]|[\U000D0000-\U000DFFFD]|[\U000E0000-\U000EFFFD]|\u00A8|\u00AA|\u00A8|\u00AA|\u005F|\u00AD|\u00AF|\u2054)|[\u0030-\u0039]|[\u0300-\u036F]|[\u1DC0-\u1DFF]|[\u20D0-\u20FF]|[\uFE20-\uFE2F])*"""

operator = r'''([\u00A1-\u00A7]|[\u2020-\u2027]|[\u2030-\u203E]|[\u2041-\u2053]|[\u2055-\u205E]|[\u2190-\u23FF]|[\u2500-\u2775]|[\u2794-\u2BFF]|[\u2E00-\u2E7F]|[\u3001-\u3003]|[\u3008-\u3020]|\/|\=|\-|\+|\!|\*|\%|\<|\>|\&|\||\^|\~|\?|\.|\@|\{|\}|\(|\)|\[|\]|\,|\:|\;|\u3030|\u2016|\u2017|\u00A9|\u00AB|\u00AC|\u00AE|\u00B0|\u00B1|\u00B6|\u00BB|\u00BF|\u00D7|\u00F7)(([\u00A1-\u00A7]|[\u2020-\u2027]|[\u2030-\u203E]|[\u2041-\u2053]|[\u2055-\u205E]|[\u2190-\u23FF]|[\u2500-\u2775]|[\u2794-\u2BFF]|[\u2E00-\u2E7F]|[\u3001-\u3003]|[\u3008-\u3020]|\/|\=|\-|\+|\!|\*|\%|\<|\>|\&|\||\^|\~|\?|\.|\@|\{|\}|\(|\)|\[|\]|\,|\:|\;|\u3030|\u2016|\u2017|\u00A9|\u00AB|\u00AC|\u00AE|\u00B0|\u00B1|\u00B6|\u00BB|\u00BF|\u00D7|\u00F7)|([\u0300-\u036F]|[\u1DC0-\u1DFF]|[\u20D0-\u20FF]|[\uFE00-\uFE0F]|[\uFE20-\uFE2F]|[\U000E0100-\U000E01EF]))*'''

t_STRING_LITERAL = r'".*?"'


# t_INT_LITERAL = r''

# t_BINARY_OPERATOR = r'\s' + t_OPERATOR + r'\s'

# t_POSTFIX_OPERATOR = t_OPERATOR + r'\s'

# t_PREFIX_OPERATOR = r'\s' + t_OPERATOR

@TOKEN(identifier)
def t_IDENTIFIER(t):
    if t.value[0] != "'" and t.value[len(t.value) - 1] != "'":
        if keyword_map.get(t.value, "ID") != 'ID':
            t.type = keyword_map[t.value].upper()
    return t


@TOKEN(operator)
def t_OPERATOR(t):
    if t.value[0] == '(' and len(t.value) == 1:
        t.type = "BRACKET_L"
    elif t.value == ')':
        t.type = "BRACKET_R"
    elif t.value == '{':
        t.type = "CURLY_L"
    elif t.value == '}':
        t.type == 'CURLY_R'
    elif t.value == '[':
        t.type = "SQUARE_L"
    elif t.value == ']':
        t.type = "SQUARE_R"
    elif t.value == ',':
        t.type = "COMMA"
    elif t.value == '.':
        t.type = "PERIOD"
    elif t.value == ';':
        t.type = "SEMICOLON"
    elif t.value == ':':
        t.type = "COLON"
    elif t.value == '@':
        t.type = "AT"
    elif t.value == '#':
        t.type = "POUND"
    elif t.value == '&':
        t.type = "AMPERSAND"
    # TODO PREFIX_AMPERSAND
    elif len(t.value) == 2 and t.value[0] == '-' and t.value[1] == '>':
        t.type = "ARROW"
    elif t.value == '`':
        t.type = "BACKTICK"
    elif t.value == '?':
        t.type = "QUESTION_MARK"
    # TODO POSTFIX_QUESTION
    elif t.value == '!':
        t.type = "EXCLAMATION_MARK"
    # TODO PREFIX_DOT
    else:
        t.type = "ERROR"
    return t


def build():
    lex.lex()
