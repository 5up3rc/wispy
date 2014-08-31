"""
    wispy.grammar
    ~~~~~~~~~~~~~

    The grammar implementation of the *wispy* engine.
"""
# pylint: disable=missing-docstring

from modgrammar import (
    Grammar, OR, WORD, REPEAT, ANY_EXCEPT,
    OPTIONAL, WHITESPACE,
)


class Newline(Grammar):
    grammar = OR("\n", "\r", "\r\n")


class InputCharacter(Grammar):
    grammar = ANY_EXCEPT("\r\n", max=1)


class InputCharacters(Grammar):
    grammar = REPEAT(InputCharacter)


class SingleLineComment(Grammar):
    # todo: stops at '# '
    grammar = ("#", OPTIONAL(WHITESPACE), OPTIONAL(InputCharacters))


class Keyword(Grammar):
    # TODO: catches `if` in `ifuntil`.
    grammar = OR("begin", "break", "catch", "class",
                 "continue", "data", "define", "do",
                 "dynamicparam", "else", "elseif", "end",
                 "exit", "filter", "finally", "for",
                 "foreach", "from", "function", "if",
                 "in", "param", "process", "return",
                 "switch", "throw", "trap", "try",
                 "until", "using", "var", "while")


class HexadecimalDigit(Grammar):
    grammar = WORD('0-9a-fA-F', max=1)


class HexadecimalDigits(Grammar):
    grammar = REPEAT(HexadecimalDigit)


class LongTypeSuffix(Grammar):
    grammar = WORD("l", max=1)


class DecimalTypeSuffix(Grammar):
    grammar = WORD("d", max=1)


class NumericTypeSuffix(Grammar):
    grammar = OR(DecimalTypeSuffix, LongTypeSuffix)


class NumericMultiplier(Grammar):
    grammar = REPEAT(OR("kb", "mb", "tb", "pb", "gb"), max=1)


class HexadecimalIntegerLiteral(Grammar):
    grammar = ("0x", HexadecimalDigits,
               OPTIONAL(LongTypeSuffix),
               OPTIONAL(NumericMultiplier))


class DecimalDigit(Grammar):
    grammar = WORD('0-9', max=1)


class DecimalDigits(Grammar):
    grammar = REPEAT(DecimalDigit)


class DecimalIntegerLiteral(Grammar):
    grammar = (DecimalDigits,
               OPTIONAL(DecimalTypeSuffix),
               OPTIONAL(NumericMultiplier))
