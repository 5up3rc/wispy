"""
Tests for wispy.grammar.
"""

# Some pylint scaffolding.
# pylint: disable=too-many-public-methods, invalid-name, no-self-use
# pylint: disable=missing-docstring, import-error

import unittest

from modgrammar import ParseError
from wispy.grammar import (
    InputCharacter, InputCharacters,
    Newline,
    SingleLineComment,
    NumericMultiplier,
    LongTypeSuffix, DecimalTypeSuffix, NumericTypeSuffix,
    DecimalDigit, DecimalDigits, DecimalIntegerLiteral,
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
            self._parse(DecimalIntegerLiteral, "10kbd")
