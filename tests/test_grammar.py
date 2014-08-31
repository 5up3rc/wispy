"""
Tests for wispy.grammar.
"""

# Some pylint scaffolding.
# pylint: disable=too-many-public-methods, invalid-name, no-self-use
# pylint: disable=missing-docstring, import-error

import unittest
import string

from modgrammar import ParseError
from wispy.grammar import (
    InputCharacter, InputCharacters,
    Newline,
    SingleLineComment,
    NumericMultiplier,
    LongTypeSuffix, DecimalTypeSuffix, NumericTypeSuffix,
    DecimalDigit, DecimalDigits, DecimalIntegerLiteral,
    HexadecimalDigit, HexadecimalDigits, HexadecimalIntegerLiteral,
    IntegerLiteral,
    Dash, Sign,
    ExponentPart, RealLiteral,
)


class GrammarTest(unittest.TestCase):
    def _parse(self, grammar, text):
        return grammar.parser().parse_text(text, eof=True)

    def test_newline(self):
        for newline in ("\r", "\n", "\r\n"):
            parsed = self._parse(Newline, newline)
            self.assertEqual(str(parsed), newline)

    def test_input_character(self):
        parsed = self._parse(InputCharacter, "abc")
        self.assertEqual(str(parsed), "a")

        parsed = self._parse(InputCharacter, "@")
        self.assertEqual(str(parsed), "@")

        with self.assertRaises(ParseError):
            self._parse(InputCharacter, "\n")

    def test_input_characters(self):
        parsed = self._parse(InputCharacters, "hoptrop")
        self.assertEqual(str(parsed), "hoptrop")

        parsed = self._parse(InputCharacters, "troptzop\n")
        self.assertEqual(str(parsed), "troptzop")

        parsed = self._parse(InputCharacters, "t")
        self.assertEqual(str(parsed), "t")

    def test_single_line_comment(self):
        parsed = self._parse(SingleLineComment, "#")
        self.assertEqual(str(parsed), "#")

        parsed = self._parse(SingleLineComment, "#top")
        self.assertEqual(str(parsed), "#top")

        parsed = self._parse(SingleLineComment, "# hop")
        self.assertEqual(str(parsed), "# hop")

        parsed = self._parse(SingleLineComment, "#	tab")
        self.assertEqual(str(parsed), "#	tab")

        parsed = self._parse(SingleLineComment, "#   three")
        self.assertEqual(str(parsed), "#   three")

        with self.assertRaises(ParseError):
            self._parse(SingleLineComment, "t#rrop")

    def test_numeric_multiplier(self):
        for multiplier in ("kb", "mb", "gb", "tb", "pb"):
            parsed = self._parse(NumericMultiplier, multiplier)
            self.assertEqual(str(parsed), multiplier)

        with self.assertRaises(ParseError):
            self._parse(NumericMultiplier, "bkb")

    def test_long_type_suffix(self):
        parsed = self._parse(LongTypeSuffix, "l")
        self.assertEqual(str(parsed), "l")

        parsed = self._parse(LongTypeSuffix, "L")
        self.assertEqual(str(parsed), "L")

        with self.assertRaises(ParseError):
            self._parse(LongTypeSuffix, "d")

    def test_decimal_type_suffix(self):
        parsed = self._parse(DecimalTypeSuffix, "d")
        self.assertEqual(str(parsed), "d")

        parsed = self._parse(DecimalTypeSuffix, "D")
        self.assertEqual(str(parsed), "D")

        with self.assertRaises(ParseError):
            self._parse(DecimalTypeSuffix, "l")

    def test_numeric_type_suffix(self):
        for suffix in ("l", "L", "d", "D"):
            parsed = self._parse(NumericTypeSuffix, suffix)
            self.assertEqual(str(parsed), suffix)

    def test_decimal_digit(self):
        for number in range(10):
            parsed = self._parse(DecimalDigit, str(number))
            self.assertEqual(str(parsed), str(number))

        parsed = self._parse(DecimalDigit, "10")
        self.assertEqual(str(parsed), "1")

        with self.assertRaises(ParseError):
            self._parse(DecimalDigit, "a")

    def test_decimal_digits(self):
        parsed = self._parse(DecimalDigits, "10")
        self.assertEqual(str(parsed), "10")

        parsed = self._parse(DecimalDigits, "1")
        self.assertEqual(str(parsed), "1")

    def test_decimal_integer_literal(self):
        parsed = self._parse(DecimalIntegerLiteral, "10")
        self.assertEqual(str(parsed), "10")

        parsed = self._parse(DecimalIntegerLiteral, "10d")
        self.assertEqual(str(parsed), "10d")
        parsed = self._parse(DecimalIntegerLiteral, "10D")
        self.assertEqual(str(parsed), "10D")

        for multiplier in ("kb", "mb", "gb", "tb", "pb"):
            parsed = self._parse(DecimalIntegerLiteral, "10" + multiplier)
            self.assertEqual(str(parsed), "10" + multiplier)

        parsed = self._parse(DecimalIntegerLiteral, "10dkb")
        self.assertEqual(str(parsed), "10dkb")

        with self.assertRaises(ParseError):
            self._parse(DecimalIntegerLiteral, "d10kb")

    def test_hexadecimal_digit(self):
        for digit in string.hexdigits:
            parsed = self._parse(HexadecimalDigit, digit)
            self.assertEqual(str(parsed), digit)

        with self.assertRaises(ParseError):
            self._parse(HexadecimalDigit, "j")

    def test_hexadecimal_digits(self):
        parsed = self._parse(HexadecimalDigits, "aa")
        self.assertEqual(str(parsed), "aa")

        parsed = self._parse(HexadecimalDigits, "a1")
        self.assertEqual(str(parsed), "a1")

        parsed = self._parse(HexadecimalDigits, "a1a2")
        self.assertEqual(str(parsed), "a1a2")

    def test_hexadecimal_integer_literal(self):
        parsed = self._parse(HexadecimalIntegerLiteral, "0x1")
        self.assertEqual(str(parsed), "0x1")

        parsed = self._parse(HexadecimalIntegerLiteral, "0x1L")
        self.assertEqual(str(parsed), "0x1L")
        parsed = self._parse(HexadecimalIntegerLiteral, "0x1l")
        self.assertEqual(str(parsed), "0x1l")

        parsed = self._parse(HexadecimalIntegerLiteral, "0x1kb")
        self.assertEqual(str(parsed), "0x1kb")
        parsed = self._parse(HexadecimalIntegerLiteral, "0x2pb")
        self.assertEqual(str(parsed), "0x2pb")

        parsed = self._parse(HexadecimalIntegerLiteral, "0x1lpb")
        self.assertEqual(str(parsed), "0x1lpb")

    def test_integer_literal(self):
        parsed = self._parse(IntegerLiteral, "0d")
        self.assertEqual(str(parsed), "0d")

        parsed = self._parse(IntegerLiteral, "0xa1")
        self.assertEqual(str(parsed), "0xa1")

    def test_dash(self):
        parsed = self._parse(Dash, "-")
        self.assertEqual(str(parsed), "-")

        parsed = self._parse(Dash, "\u2013")
        self.assertEqual(str(parsed), "\u2013")

        parsed = self._parse(Dash, "\u2014")
        self.assertEqual(str(parsed), "\u2014")

    def test_sign(self):
        parsed = self._parse(Sign, "+")
        self.assertEqual(str(parsed), "+")

        parsed = self._parse(Sign, "\u2013")
        self.assertEqual(str(parsed), "\u2013")

    def test_exponent_part(self):
        parsed = self._parse(ExponentPart, "e44")
        self.assertEqual(str(parsed), "e44")
        parsed = self._parse(ExponentPart, "E44")
        self.assertEqual(str(parsed), "E44")

        parsed = self._parse(ExponentPart, "e-44")
        self.assertEqual(str(parsed), "e-44")
        parsed = self._parse(ExponentPart, "E-42")
        self.assertEqual(str(parsed), "E-42")

    def test_real_literal(self):
        parsed = self._parse(RealLiteral, "1.4")
        self.assertEqual(str(parsed), "1.4")
        parsed = self._parse(RealLiteral, "1.4e44")
        self.assertEqual(str(parsed), "1.4e44")
        parsed = self._parse(RealLiteral, "1.4e44d")
        self.assertEqual(str(parsed), "1.4e44d")
        parsed = self._parse(RealLiteral, "1.4e44kb")
        self.assertEqual(str(parsed), "1.4e44kb")

        parsed = self._parse(RealLiteral, ".4")
        self.assertEqual(str(parsed), ".4")
        parsed = self._parse(RealLiteral, ".4e44")
        self.assertEqual(str(parsed), ".4e44")
        parsed = self._parse(RealLiteral, ".4e4d")
        self.assertEqual(str(parsed), ".4e4d")
        parsed = self._parse(RealLiteral, ".4e4dkb")
        self.assertEqual(str(parsed), ".4e4dkb")
        parsed = self._parse(RealLiteral, ".4Dkb")
        self.assertEqual(str(parsed), ".4Dkb")
        parsed = self._parse(RealLiteral, ".4Dkb")

        parsed = self._parse(RealLiteral, "4e4")
        self.assertEqual(str(parsed), "4e4")
        parsed = self._parse(RealLiteral, "4e4D")
        self.assertEqual(str(parsed), "4e4D")
        parsed = self._parse(RealLiteral, "4e4d")
        self.assertEqual(str(parsed), "4e4d")
        parsed = self._parse(RealLiteral, "4e4dkb")
        self.assertEqual(str(parsed), "4e4dkb")
