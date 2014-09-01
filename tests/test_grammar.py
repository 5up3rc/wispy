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
    SimpleNameFirstCharacter, SimpleNameCharacter,
    SimpleNameCharacters, SimpleName,
    Dollars, DoubleQuoteCharacter,
    SingleQuoteCharacter,
    Keyword,
    InputCharacter, InputCharacters,
    NewLineCharacter,
    Hashes, NotGreaterThanOrHash,
    DelimitedCommentSection, DelimitedComment, DelimitedCommentText,
    SingleLineComment, Comment,
    NumericMultiplier,
    LongTypeSuffix, DecimalTypeSuffix, NumericTypeSuffix,
    DecimalDigit, DecimalDigits, DecimalIntegerLiteral,
    HexadecimalDigit, HexadecimalDigits, HexadecimalIntegerLiteral,
    IntegerLiteral,
    Dash, Sign,
    ExponentPart, RealLiteral,
    EscapedCharacter,
    VariableCharacter, VariableCharacters,
    VariableScope, VariableNamespace, VerbatimStringLiteral,
    VerbatimHereStringLiteral, VerbatimStringPart, VerbatimStringCharacters,
    VerbatimHereStringCharacters,
    BracedVariableCharacter, BracedVariableCharacters, BracedVariable,
    FileRedirectionOperator, FormatOperator,
    AssignmentOperator, ComparisonOperator, OperatorOrPunctuator,
    TypeCharacter, TypeCharacters, TypeIdentifier, TypeName,
    ArrayTypeName, GenericTypeName,
)


class GrammarTest(unittest.TestCase):

    def _parse(self, grammar, text):
        return grammar.parser().parse_text(text,
                                           eof=True, matchtype='complete')

    def _test_expected_pairs(self, grammar, text_pairs):
        for text, expected in text_pairs:
            parsed = self._parse(grammar, text)
            self.assertEqual(str(parsed), expected)

    def _test_expected(self, grammar, texts):
        text_pairs = zip(texts, texts)
        self._test_expected_pairs(grammar, text_pairs)

    def test_simple_name_first_character(self):
        self._test_expected(SimpleNameFirstCharacter, string.ascii_letters)

    def test_simple_name_character(self):
        self._test_expected(SimpleNameCharacter, string.ascii_letters)

    def test_simple_name_characters(self):
        literals = chain(string.ascii_letters, ["\u005F"])
        self._test_expected(SimpleNameCharacters,
                            list(map(lambda x: x + x, literals)))

    def test_simple_name(self):
        self._test_expected(SimpleName, ["tzop", "trop", "pop"])

    def test_dollars(self):
        self._test_expected(Dollars, ["$", "$$"])

    def test_double_quote_character(self):
        self._test_expected(DoubleQuoteCharacter,
                            ["\u0022", "\u201C", "\u201D", "\u201E"])

    def test_file_redirection_operator(self):
        self._test_expected(FileRedirectionOperator,
                            [">>", ">", "<", "2>>", "2>"])

    def test_single_quote_character(self):
        literals = ["\u0027", "\u2018", "\u2019", "\u201A", "\u201B"]
        self._test_expected(SingleQuoteCharacter, literals)

    def test_format_operator(self):
        literals = list(map(lambda x: x + "f", ["-", "\u2013", "\u2014"]))
        self._test_expected(FormatOperator, literals)

    def test_keyword(self):
        literals = ("begin", "break", "catch", "class",
                    "continue", "data", "define", "do",
                    "dynamicparam", "else", "elseif", "end",
                    "exit", "filter", "finally", "for",
                    "foreach", "from", "function", "if",
                    "in", "param", "process", "return",
                    "switch", "throw", "trap", "try",
                    "until", "using", "var", "while")
        self._test_expected(Keyword, literals)

    def test_newline(self):
        self._test_expected(NewLineCharacter, ["\r", "\n", "\r\n"])

    def test_input_character(self):
        self._test_expected(InputCharacter, string.ascii_letters)

        with self.assertRaises(ParseError):
            self._parse(InputCharacter, "\n")

    def test_input_characters(self):
        elements = ["hoptrop", "troptzop", "t"]
        self._test_expected(InputCharacters, elements)

        for element in ("\r", "\n", "\r\n"):
            with self.assertRaises(ParseError):
                self._parse(InputCharacters, "tropatropa" + element)

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

    def test_assignment_operator(self):
        literals = [
            "=", "-=", "+=",
            "*=", "/=", "%="
        ]
        self._test_expected(AssignmentOperator, literals)

        with self.assertRaises(ParseError):
            self._parse(AssignmentOperator, "&=")

    def test_comparison_operator(self):
        ops = [
            "as", "ccontains", "ceq",
            "cge", "cgt", "cle",
            "clike", "clt", "cmatch",
            "cne", "cnotcontains", "cnotlike",
            "cnotmatch", "contains", "creplace",
            "csplit", "eq", "ge",
            "gt", "icontains", "ieq",
            "ige", "igt", "ile",
            "ilike", "ilt", "imatch",
            "ine", "inotcontains", "inotlike",
            "inotmatch", "ireplace", "is",
            "isnot", "isplit", "join",
            "le", "like", "lt",
            "match", "ne", "notcontains",
            "notlike", "notmatch", "replace",
            "split"
        ]
        literals = ["-{}".format(op) for op in ops]
        self._test_expected(ComparisonOperator, literals)

    def test_operator_or_punctuator(self):
        literals = [
            "{", "}", "[", "]", "(", ")", "@(", "@{", "$(", ";",
            "&&", "||", "&", "|", ",", "++", "..", "::", ".",
            "!", "*", "/", "%", "+", "2>&1", "1>&2",
            "-", "--",
            "-and", "-band", "-bnot",
            "-bor", "-bxor", "-not",
            "-or", "-xor",
            "+=", "*=",
            ">>",
            "-inotlike",
            "-f"
        ]
        self._test_expected(OperatorOrPunctuator, literals)

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

    def test_hashes(self):
        self._test_expected(Hashes, ["#", "##"])

    def test_not_greater_than_or_hash(self):
        self._test_expected(NotGreaterThanOrHash, string.ascii_letters)
        with self.assertRaises(ParseError):
            self._parse(NotGreaterThanOrHash, "#")
        with self.assertRaises(ParseError):
            self._parse(NotGreaterThanOrHash, ">")

    def test_delimited_comment_section(self):
        self._test_expected(DelimitedCommentSection, [">"])
        self._test_expected(DelimitedCommentSection, ["#4"])
        # the hash is optional
        self._test_expected(DelimitedCommentSection, ["4"])

        with self.assertRaises(ParseError):
            self._parse(DelimitedCommentSection, "#>")
        with self.assertRaises(ParseError):
            self._parse(DelimitedCommentSection, "##")

    def test_delimited_comment_text(self):
        self._test_expected(DelimitedCommentText, [">", ">>", "#4#4"])

    def test_delimited_comment(self):
        literals = ["<##>", "<# trop tropa #>"]
        self._test_expected(DelimitedComment, literals)

        with self.assertRaises(ParseError):
            self._parse(DelimitedComment, "#>")

    def test_comment(self):
        literals = ["<# trop tzop #>", "# hophop"]
        self._test_expected(Comment, literals)

    def test_type_character(self):
        literals = list(chain(string.ascii_letters, ["\u005F"]))
        self._test_expected(TypeCharacter, literals)

    def test_type_characters(self):
        literals = chain(string.ascii_letters, ["\u005F"])
        self._test_expected(TypeCharacters,
                            list(map(lambda x: x + x, literals)))

        # TypeIdentifier is the same as TypeCharacters
        self._test_expected(TypeIdentifier, string.ascii_letters)

    def test_type_name(self):
        self._test_expected(TypeName, ["tzop"])
        self._test_expected(TypeName, ["tzop.hop"])

        with self.assertRaises(ParseError):
            self._parse(TypeName, ".trop")

    def test_array_type_name(self):
        self._test_expected(ArrayTypeName, ["tzop[", "tzop.hop["])
        # GenericTypeName is the same as ArrayTypeName
        self._test_expected(GenericTypeName, ["hop[", "bop["])

    def test_verbatim_string_literal(self):
        self._test_expected(VerbatimStringLiteral, ["''", "'red'"])
        with self.assertRaises(ParseError):
            self._parse(VerbatimStringLiteral, "red")

    def test_verbatim_here_string_literal(self):
        test_ok = [
            "@'\n\n'@",
            "@'\nline1\n'@",
            "@'\nline1\nline2\n'@"
        ]
        test_fail = ["@'\n'@", "@'\n@", "@'\nline\n@"]
        self._test_expected(VerbatimHereStringLiteral, test_ok)
        for item in test_fail:
            with self.assertRaises(ParseError):
                self._parse(VerbatimHereStringLiteral, item)

    def test_verbatim_string_part(self):
        test_ok = ["a", "b", "c", "''"]
        self._test_expected(VerbatimStringPart, test_ok)
        with self.assertRaises(ParseError):
            self._parse(VerbatimStringPart, "'a")

    def test_verbatim_string_characters(self):
        test_ok = [
            "any except single-quote-character\n''",
            "line1\n''"
        ]
        self._test_expected(VerbatimStringCharacters, test_ok)

    def test_verbatim_here_string_characters(self):
        test_ok = [
            "line1\nexcept_singe_quote_character\n'any_char"
        ]
        self._test_expected(VerbatimHereStringCharacters, test_ok)
