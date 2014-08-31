"""
Tests for wispy.grammar.
"""

# Some pylint scaffolding.
# pylint: disable=too-many-public-methods, invalid-name, no-self-use
# pylint: disable=missing-docstring, import-error
# pylint: disable=bad-builtin


import unittest
import string
from itertools import chain

from modgrammar import ParseError
from wispy.grammar import (
    InputCharacter, InputCharacters,
    NewLineCharacter,
    SingleLineComment,
    NumericMultiplier,
    LongTypeSuffix, DecimalTypeSuffix, NumericTypeSuffix,
    DecimalDigit, DecimalDigits, DecimalIntegerLiteral,
    HexadecimalDigit, HexadecimalDigits, HexadecimalIntegerLiteral,
    IntegerLiteral,
    Dash, Sign,
    ExponentPart, RealLiteral,
    EscapedCharacter,
    VariableCharacter, VariableCharacters,
    VariableScope, VariableNamespace,
    BracedVariableCharacter, BracedVariableCharacters, BracedVariable,
)


class GrammarTest(unittest.TestCase):

    def _parse(self, grammar, text):
        return grammar.parser().parse_text(text, eof=True)

    def _test_expected_pairs(self, grammar, text_pairs):
        for text, expected in text_pairs:
            parsed = self._parse(grammar, text)
            #print(text, expected, str(parsed))
            self.assertEqual(str(parsed), expected)

    def _test_expected(self, grammar, texts):
        text_pairs = zip(texts, texts)
        self._test_expected_pairs(grammar, text_pairs)

    def test_newline(self):
        self._test_expected(NewLineCharacter, ["\r", "\n", "\r\n"])

    def test_input_character(self):
        self._test_expected_pairs(InputCharacter, [("abc", "a")])
        self._test_expected(InputCharacter, ["@"])

        with self.assertRaises(ParseError):
            self._parse(InputCharacter, "\n")

    def test_input_characters(self):
        pairs = [
            ("hoptrop", "hoptrop"),
            ("troptzop\n", "troptzop"),
            ("t", "t")
        ]
        self._test_expected_pairs(InputCharacters, pairs)

    def test_single_line_comment(self):
        pairs = [
            ("#", "#"),
            ("#top", "#top"),
            ("# hop", "# hop"),
            ("#	tab", "#	tab"),
            ("#   three", "#   three")
        ]
        self._test_expected_pairs(SingleLineComment, pairs)

        with self.assertRaises(ParseError):
            self._parse(SingleLineComment, "t#rrop")

    def test_numeric_multiplier(self):
        multipliers = ["kb", "mb", "gb", "tb", "pb"]
        self._test_expected(NumericMultiplier, multipliers)

        with self.assertRaises(ParseError):
            self._parse(NumericMultiplier, "bkb")

    def test_long_type_suffix(self):
        suffixes = ["l", "L"]
        self._test_expected(LongTypeSuffix, suffixes)

        with self.assertRaises(ParseError):
            self._parse(LongTypeSuffix, "d")

    def test_decimal_type_suffix(self):
        suffixes = ["d", "D"]
        self._test_expected(DecimalTypeSuffix, suffixes)

        with self.assertRaises(ParseError):
            self._parse(DecimalTypeSuffix, "l")

    def test_numeric_type_suffix(self):
        suffixes = ["l", "L", "d", "D"]
        self._test_expected(NumericTypeSuffix, suffixes)

    def test_decimal_digit(self):
        numbers = list(map(str, range(10)))
        self._test_expected(DecimalDigit, numbers)

        self._test_expected_pairs(DecimalDigit, [("10", "1")])

        with self.assertRaises(ParseError):
            self._parse(DecimalDigit, "a")

    def test_decimal_digits(self):
        numbers = ["10", "1"]
        self._test_expected(DecimalDigits, numbers)

    def test_decimal_integer_literal(self):
        numbers = ["10", "10d", "10D"]
        self._test_expected(DecimalIntegerLiteral, numbers)

        multipliers = [("10" + mul) for mul in
                       ("kb", "mb", "gb", "tb", "pb", "dkb")]
        self._test_expected(DecimalIntegerLiteral, multipliers)

        with self.assertRaises(ParseError):
            self._parse(DecimalIntegerLiteral, "d10kb")

    def test_hexadecimal_digit(self):
        self._test_expected(HexadecimalDigit, string.hexdigits)

        with self.assertRaises(ParseError):
            self._parse(HexadecimalDigit, "j")

    def test_hexadecimal_digits(self):
        literals = ["aa", "a1", "a1a2"]
        self._test_expected(HexadecimalDigits, literals)

    def test_hexadecimal_integer_literal(self):
        literals = [
            "0x1",
            "0x1L",
            "0x1l",
            "0x1kb",
            "0x2pb",
            "0x1lpb"
        ]
        self._test_expected(HexadecimalIntegerLiteral, literals)

    def test_integer_literal(self):
        literals = ["0d", "0xa1"]
        self._test_expected(IntegerLiteral, literals)

    def test_dash(self):
        literals = [
            "-",
            "\u2013",
            "\u2014"
        ]
        self._test_expected(Dash, literals)

    def test_sign(self):
        literals = ["+", "\u2013"]
        self._test_expected(Sign, literals)

    def test_exponent_part(self):
        literals = ["e44", "E44", "e-44", "E-42"]
        self._test_expected(ExponentPart, literals)

    def test_real_literal(self):
        literals = [
            "1.4", "1.4e44", "1.4e44kb",
            ".4", ".4e44", ".4e4d", ".4e4dkb", ".4Dkb",
            "4e4", "4e4D", "4e4d", "4e4dkb"
        ]
        self._test_expected(RealLiteral, literals)

    def test_escaped_character(self):
        self._test_expected(EscapedCharacter, ["\u0060a"])

    def test_braced_variable_character(self):
        literals = ["a", "\u0060a"]
        self._test_expected(BracedVariableCharacter, literals)

        with self.assertRaises(ParseError):
            self._parse(BracedVariableCharacter, "`")
        with self.assertRaises(ParseError):
            self._parse(BracedVariableCharacter, "}")

    def test_braced_variable_characters(self):
        self._test_expected(BracedVariableCharacters, ["aaa"])

    def test_variable_character(self):
        literals = list(chain(string.digits, string.ascii_letters, ["?"]))
        self._test_expected(VariableCharacter, literals)

    def test_variable_characters(self):
        literals = ["a", "abc"]
        self._test_expected(VariableCharacters, literals)

    def test_variable_namespace(self):
        literals = ["abc:", "a:"]
        self._test_expected(VariableNamespace, literals)

        with self.assertRaises(ParseError):
            self._parse(VariableNamespace, ":a")

    def test_variable_scope(self):
        scopes = ["globe:", "local:", "private:", "script:"]
        self._test_expected(VariableScope, scopes)

        self._test_expected(VariableScope, ["abc:"])

    def test_braced_variable(self):
        literals = ["${global:a}", "${a}"]
        self._test_expected(BracedVariable, literals)

        with self.assertRaises(ParseError):
            self._parse(BracedVariable, "${a")
