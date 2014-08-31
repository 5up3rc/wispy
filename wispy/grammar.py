"""
    wispy.grammar
    ~~~~~~~~~~~~~

    The Powershell grammar implementation of the *wispy* engine.
    The version used is PowerShell Language Specification Version 3.0.

"""
# pylint: disable=missing-docstring, no-init, too-few-public-methods

from modgrammar import (
    Grammar, OR, WORD, REPEAT, ANY_EXCEPT,
    OPTIONAL, WHITESPACE,
)


class Newline(Grammar):
    grammar = OR("\r\n", "\n", "\r")


class InputCharacter(Grammar):
    grammar = ANY_EXCEPT("\r\n", max=1)


class InputCharacters(Grammar):
    grammar = REPEAT(InputCharacter)


class SingleLineComment(Grammar):
    grammar = ("#", OPTIONAL(WHITESPACE), OPTIONAL(InputCharacters))


class Keyword(Grammar):
    grammar = OR("begin", "break", "catch", "class",
                 "continue", "data", "define", "do",
                 "dynamicparam", "else", "elseif", "end",
                 "exit", "filter", "finally", "for",
                 "foreach", "from", "function", "if",
                 "in", "param", "process", "return",
                 "switch", "throw", "trap", "try",
                 "until", "using", "var", "while")

# Literals


class NumericMultiplier(Grammar):
    grammar = REPEAT(OR("kb", "mb", "tb", "pb", "gb"), max=1)


class LongTypeSuffix(Grammar):
    grammar = WORD("l", max=1)


class DecimalTypeSuffix(Grammar):
    grammar = WORD("d", max=1)


class NumericTypeSuffix(Grammar):
    grammar = OR(DecimalTypeSuffix, LongTypeSuffix)


class DecimalDigit(Grammar):
    grammar = WORD('0-9', max=1)


class DecimalDigits(Grammar):
    grammar = REPEAT(DecimalDigit)


class DecimalIntegerLiteral(Grammar):
    grammar = (DecimalDigits,
               OPTIONAL(DecimalTypeSuffix),
               OPTIONAL(NumericMultiplier))


class HexadecimalDigit(Grammar):
    grammar = WORD('0-9a-fA-F', max=1)


class HexadecimalDigits(Grammar):
    grammar = REPEAT(HexadecimalDigit)


class HexadecimalIntegerLiteral(Grammar):
    grammar = ("0x", HexadecimalDigits,
               OPTIONAL(LongTypeSuffix),
               OPTIONAL(NumericMultiplier))


class IntegerLiteral(Grammar):
    grammar = OR(DecimalIntegerLiteral, HexadecimalIntegerLiteral)


class Dash(Grammar):
    grammar = OR("-", "\u2013", "\u2014", "\u2015")


class Sign(Grammar):
    grammar = OR("+", Dash, max=1)


class ExponentPart(Grammar):
    grammar = (OR("e", "E"), OPTIONAL(Sign), DecimalDigits)


class RealLiteral(Grammar):
    grammar = OR(
        (DecimalDigits, ".", DecimalDigits, OPTIONAL(ExponentPart),
         OPTIONAL(DecimalTypeSuffix), OPTIONAL(NumericMultiplier)),

        (".", DecimalDigits, OPTIONAL(ExponentPart),
         OPTIONAL(DecimalTypeSuffix), OPTIONAL(NumericMultiplier)),

        (DecimalDigits, ExponentPart, OPTIONAL(DecimalTypeSuffix),
         OPTIONAL(NumericMultiplier)))