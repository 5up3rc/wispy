"""
Tests for wispy.grammar.
"""

# Some pylint scaffolding.
# pylint: disable=too-many-public-methods, invalid-name, no-self-use
# pylint: disable=missing-docstring, import-error
# pylint: disable=bad-builtin, star-args


import unittest
import string
import logging
from itertools import chain

from modgrammar import ParseError
from modgrammar.debugging import DEBUG_ALL
from wispy.grammar import (
    SimpleNameFirstCharacter, SimpleNameCharacter,
    SimpleNameCharacters, SimpleName,
    Dollars, DoubleQuoteCharacter,
    SingleQuoteCharacter,
    Keyword, StringLiteral,
    ExpandableStringPart, ExpandableStringLiteral, ExpandableHereStringLiteral,
    GenericTokenChar, GenericTokenPart, GenericTokenParts,
    GenericToken, GenericTokenWithSubexprStart,
    Dimension,
    TypeSpec, GenericTypeArguments, TypeLiteral,
    Colon,
    ParameterCharacter, ParameterCharacters,
    FirstParameterCharacter, CommandParameter,
    InputCharacter, InputCharacters,
    NewLineCharacter, NewLines,
    Hashes, NotGreaterThanOrHash,
    DelimitedCommentSection, DelimitedComment, DelimitedCommentText,
    SingleLineComment, Comment,
    NumericMultiplier,
    LongTypeSuffix, DecimalTypeSuffix, NumericTypeSuffix,
    DecimalDigit, DecimalDigits, DecimalIntegerLiteral,
    HexadecimalDigit, HexadecimalDigits, HexadecimalIntegerLiteral,
    IntegerLiteral, Literal,
    Dash, Sign,
    ExponentPart, RealLiteral,
    EscapedCharacter,
    VariableCharacter, VariableCharacters,
    VariableScope, VariableNamespace, VerbatimStringLiteral,
    VerbatimHereStringLiteral, VerbatimStringPart, VerbatimStringCharacters,
    VerbatimHereStringCharacters, VerbatimHereStringPart, Variable,
    BracedVariableCharacter, BracedVariableCharacters, BracedVariable,
    FileRedirectionOperator, FormatOperator,
    AssignmentOperator, ComparisonOperator, OperatorOrPunctuator,
    TypeCharacter, TypeCharacters, TypeIdentifier, TypeName,
    ArrayTypeName, GenericTypeName,
    ExpandableStringCharacters,
    ExpandableStringWithSubexprStart, ExpandableStringWithSubexprEnd,
    ExpandableHereStringPart, ExpandableHereStringCharacters,
    ExpandableHereStringWithSubexprStart, ExpandableHereStringWithSubexprEnd,
    CommandInvocationOperator,
    AttributeName, CommandName,
    StatementTerminator, StatementTerminators,
    BlockName, DataName,
    SwitchParameter, SwitchParameters,
    FlowControlStatement,
    Redirection,
    PreDecrementExpression, PreIncrementExpression,
    RedirectedFileName,
    MultiplicativeExpression, RangeExpression,
)

logging.basicConfig(level=logging.DEBUG)


class GrammarTest(unittest.TestCase):

    def _parse(self, grammar, text, *, debug=False):
        if debug:
            kwargs = {"debug": True, "debug_flags": DEBUG_ALL}
        else:
            kwargs = {}
        parser = grammar.parser(**kwargs)
        return parser.parse_text(text, eof=True, matchtype='complete')

    def _test_expected_pairs(self, grammar, text_pairs, *, debug=False):
        for text, expected in text_pairs:
            parsed = self._parse(grammar, text, debug=debug)
            self.assertEqual(str(parsed), expected)

    def _test_expected(self, grammar, texts, *, debug=False):
        text_pairs = zip(texts, texts)
        self._test_expected_pairs(grammar, text_pairs, debug=debug)

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

    def test_expandable_string_part(self):
        for char in "$\u0022\u201C\u201D\u201E\u0060":
            with self.assertRaises(ParseError):
                self._parse(ExpandableStringPart, char)

        for char in "({\u0022\u201C\u201D\u201E\u0060":
            with self.assertRaises(ParseError):
                self._parse(ExpandableStringPart, "$" + char)

        self._test_expected(ExpandableStringPart,
                            list(map(lambda x: "$" + x, string.ascii_letters)))
        self._test_expected(ExpandableStringPart, ["$\u0060a"])
        self._test_expected(ExpandableStringPart, ["\u0060b"])

        quotes = list(map(lambda x: x + x,
                          ["\u0022", "\u201C", "\u201D", "\u201E"]))
        self._test_expected(ExpandableStringPart, quotes)
        elements = [
            '$totalCost',
            '$Maximum_Count_26',
            '${Maximum_Count_26}',
            '${Name with`twhite space and `{punctuation`}}',
            r'${E:\File.txt}'
        ]
        self._test_expected(ExpandableStringPart, elements)

    def test_expandable_string_literal(self):
        literals = [
            "\"\"", "\"test\"",
            "\"variable$$\"", "\"$$somet$$hing$$$$\"",
            "\"$$$$$$\""
        ]
        self._test_expected(ExpandableStringLiteral, literals)

    def test_expandable_here_string_literal(self):
        literals = [
            "@\"\n\n\"@",
            "@\"\nline1\n\"@",
            "@\"\nline1\nline2\nline3\n\"@"
        ]
        self._test_expected(ExpandableHereStringLiteral, literals)

    def test_string_literal(self):
        literals = [
            "\"tralalala\"",
            "'tralalala'",
            "@\"\ntest1\ntest2\n\"@",
            "@'\ntest1\ntest2\n'@"
        ]
        self._test_expected(StringLiteral, literals)

    def test_expandable_here_string_characters(self):
        literals = [
            "xxx",
            "{{xx",
            "$\nxx",
            "\nx\nxxx",
            "\n\"aa"
        ]
        self._test_expected(ExpandableHereStringCharacters, literals)

        with self.assertRaises(ParseError):
            self._parse(ExpandableHereStringCharacters, "$$\n\n\n")

    def test_expandable_here_string_part(self):
        literals = [
            "x",
            "{",
            "$x",
            "$\nx",
            "$\n\"x",
            "\nx",
            "\n\"x"
        ]
        self._test_expected(ExpandableHereStringPart, literals)

        with self.assertRaises(ParseError):
            self._parse(ExpandableHereStringPart, "$${")

    def test_expandable_here_string_with_subexpr_start(self):
        literals = [
            "@\"\n$(",
            "@\"    \n$(",
            "@\" \t \n$(",
            "@\" \t \nxxxx$("
        ]
        self._test_expected(ExpandableHereStringWithSubexprStart, literals)

        with self.assertRaises(ParseError):
            self._parse(ExpandableHereStringWithSubexprStart, "@\" dasd\nx$(")

    def test_expandable_here_string_with_subexpr_end(self):
        self._test_expected(ExpandableHereStringWithSubexprEnd, ["\n\"@"])

        with self.assertRaises(ParseError):
            self._parse(ExpandableHereStringWithSubexprEnd, "\n\"\"@")

    def test_expandable_string_with_subexpr_start(self):
        literals = ["\"$(", "\"test$("]
        self._test_expected(ExpandableStringWithSubexprStart, literals)

        with self.assertRaises(ParseError):
            self._parse(ExpandableStringWithSubexprStart, "\"test$(test")

    def test_expandable_string_with_subexpr_end(self):
        self._test_expected(ExpandableStringWithSubexprEnd, ["\""])

        with self.assertRaises(ParseError):
            self._parse(ExpandableStringWithSubexprEnd, "\"test")

    def test_generic_token_char(self):
        self._test_expected(GenericTokenChar, ["\u0060a"])
        self._test_expected(GenericTokenChar, string.ascii_letters)

        invalid = chain(["\u0027", "\u2018", "\u2019", "\u201A", "\u201B"],
                        ["\u0022", "\u201C", "\u201D", "\u201E"],
                        ["\n", "\r"],
                        "{}();,|&$\u0060")
        for char in invalid:
            with self.assertRaises(ParseError):
                self._parse(GenericTokenChar, char)

    def test_generic_token_part(self):
        self._test_expected(GenericTokenPart, string.ascii_letters)

        variable_items = [
            "$totalCost", "$Maximum_Count_26", "${Maximum_Count_26}",
            "${Name with`twhite space and `{punctuation`}}",
            r"${E:\File.txt}", "$$", "$?", "$^",
            "$global:test_variable",
            "$local:test_variable",
            "$private:test_variable",
            "$script:test_variable",
            "@global:test_variable",
            "@local:test_variable",
            "@private:test_variable",
            "@script:test_variable",
        ]
        self._test_expected(GenericTokenPart, variable_items)

        # Test ExpandableStringLiteral support in GenericTokenPart
        double_quotes = ["\u0022", "\u201C", "\u201D", "\u201E"]
        expandable_characters = [
            '$totalCost',
            '$Maximum_Count_26',
            '${Maximum_Count_26}',
            '${Name with`twhite space and `{punctuation`}}',
            r'${E:\File.txt}'
        ]
        tests = [quote + char + quote
                 for char in expandable_characters
                 for quote in double_quotes]
        self._test_expected(GenericTokenPart, tests)

        self._test_expected(GenericTokenPart, ["@' \n\n'@", "@'\nyoshi\n'@"])

    def test_generic_token_parts(self):
        # Mostly tested by test_generic_token_part
        self._test_expected(GenericTokenParts, ["@' \n\n'@", "@'\nyoshi\n'@"])
        self._test_expected(GenericTokenParts, ["@script:test_variable"])

    def test_generic_token(self):
        # GenericToken is GenericTokenParts
        self._test_expected(GenericToken, ["@script:test_variable"])

    def test_generic_token_with_subexpr_start(self):
        elements = [
            "@script:test_variable$(",
            '$Maximum_Count_26$(',
            '${Maximum_Count_26}$('
        ]
        self._test_expected(GenericTokenWithSubexprStart, elements)

    def test_dimension(self):
        self._test_expected(Dimension, [",", ",,"])

    def test_type_spec(self):
        self._test_expected(TypeSpec, ["int[,]", "int[]"])
        self._test_expected(TypeSpec, ["int", "float", "double"])
        self._test_expected(TypeSpec, ["Dictionary[float,double]"])

    def test_type_literal(self):
        self._test_expected(TypeLiteral, ["[object[]]", "[int]", "[int[,,]]"])

    def test_colon(self):
        self._test_expected(Colon, [":"])

    def test_parameter_character(self):
        literals = [":", "\r", "\n", "{", "}", "(", ")", ";", ",", "|",
                    "&", ".", "["]
        for literal in literals:
            with self.assertRaises(ParseError):
                self._parse(ParameterCharacter, literal)

        self._test_expected(ParameterCharacter, string.ascii_letters)

    def test_parameter_characters(self):
        self._test_expected(ParameterCharacters, string.ascii_letters)
        self._test_expected(ParameterCharacters, ["char", "float", "double"])

    def test_first_parameter_character(self):
        literals = list(chain(string.ascii_letters, ["\u005F", "?"]))
        self._test_expected(FirstParameterCharacter, literals)

        for digit in string.digits:
            with self.assertRaises(ParseError):
                self._parse(FirstParameterCharacter, digit)

    def test_command_parameter(self):
        dashes = "-", "\u2013", "\u2014", "\u2015"
        parts = [dash + letter + "wispy"
                 for dash in dashes
                 for letter in string.ascii_letters]
        self._test_expected(CommandParameter, parts)
        self._test_expected(CommandParameter, [part + ":" for part in parts])

    def test_generic_type_arguments(self):
        self._test_expected(GenericTypeArguments,
                            ["int,float", "int[,],float[,]"])

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
        numbers = ["10", "10d", "10D", "10l", "10L"]
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

    def test_verbatim_here_string_part(self):
        test_ok = [
            "l",
            "\nl",
            "\n'a"
        ]
        test_fail = ["\n", "\n'", "\n'@"]
        self._test_expected(VerbatimHereStringPart, test_ok)
        for item in test_fail:
            with self.assertRaises(ParseError):
                self._parse(VerbatimHereStringPart, item)

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
            "line1\nexcept_singe_quote_character\n'any_char",
            "a\na\n'a"
        ]
        self._test_expected(VerbatimHereStringCharacters, test_ok)

    def test_variable(self):
        test_ok = [
            "$totalCost", "$Maximum_Count_26", "${Maximum_Count_26}",
            "${Name with`twhite space and `{punctuation`}}",
            r"${E:\File.txt}", "$$", "$?", "$^",
            "$global:test_variable",
            "$local:test_variable",
            "$private:test_variable",
            "$script:test_variable",
            "@global:test_variable",
            "@local:test_variable",
            "@private:test_variable",
            "@script:test_variable",
        ]
        test_fail = [
            "test_variable", "test:variable", "${incomplete",
            "$incomplete}", "$@variable"
        ]
        self._test_expected(Variable, test_ok)
        for item in test_fail:
            with self.assertRaises(ParseError):
                self._parse(Variable, item)

    def test_expandable_string_characters(self):
        test_ok = [
            "a", "ab", "abc",
            "$something", "${global:a}", "${a}",
            '""'
        ]

        test_fail = [
            "$", "\u0022", "\u201C", "\u201D", "\u201E", "\u0060",
            'a$variable"', "a$variable`"
        ]

        self._test_expected(ExpandableStringCharacters, test_ok)
        for item in test_fail:
            with self.assertRaises(ParseError):
                self._parse(ExpandableStringCharacters, item)

    def test_newlines(self):
        self._test_expected(NewLines, ["\n", "\n\n", "\r", "\r\r"])

    def test_command_invocation_operator(self):
        self._test_expected(CommandInvocationOperator, [".", "&"])

    def test_attribute_name(self):
        self._test_expected(AttributeName, ["int[,]", "int[]"])
        self._test_expected(AttributeName, ["int", "float", "double"])
        self._test_expected(AttributeName, ["Dictionary[float,double]"])

    def test_command_name(self):
        elements = [
            "@script:test_variable$(",
            "$Maximum_Count_26$(",
            "${Maximum_Count_26}$(",
            "@script:test_variable"
        ]
        self._test_expected(CommandName, elements)

    def test_statement_terminator(self):
        self._test_expected(StatementTerminator, [";", "\n", "\r"])

    def test_statement_terminators(self):
        self._test_expected(StatementTerminators, [";;", "\n\n"])

    def test_block_name(self):
        names = ["dynamicparam", "begin", "process", "end"]
        self._test_expected(BlockName, names)

    def test_data_name(self):
        self._test_expected(DataName, ["tzop", "trop", "pop"])

    def test_switch_parameters(self):
        params = ["-regex", "-wildcard", "-exact", "-casesensitive"]
        self._test_expected(SwitchParameter, params)

        params = [param + " " + param
                  for param in params]
        self._test_expected(SwitchParameters, params)

    def test_literal(self):
        literals = [
            "1.4", "1.4e44", "1.4e44kb",
            ".4", ".4e44", ".4e4d", ".4e4dkb", ".4Dkb",
            "4e4", "4e4D", "4e4d", "4e4dkb",
            "0d", "0xa1",
            "\"tralalala\"",
            "'tralalala'",
            "@\"\ntest1\ntest2\n\"@",
            "@'\ntest1\ntest2\n'@"
        ]
        self._test_expected(Literal, literals)

    def test_flow_control_statement(self):
        parts = [
            "break",
            "break $lab",
            "break labelA",

            "continue",
            "continue $lab",
            "continue labelA",

            "throw",
            "throw 100",
            # Pathological case: 'throw "No such record in file"',

            "return 1",
            "return $4",

            "exit",
            "exit $4",
        ]
        self._test_expected(FlowControlStatement, parts)

    def test_redirection(self):
        parts = [
            "2>&1", "1>&2",
            # with whitespace
            ">> filename", "> a.txt", "< b.txt", "2>> c.txt", "2> d.txt",
            # without whitespace
            ">>filename", ">a.txt", "<b.txt", "2>>c.txt", "2>d.txt",
            # variables
            ">$null", "2>>$null",
        ]
        self._test_expected(Redirection, parts)

    def test_pre_decrement_expression(self):
        self._test_expected(PreDecrementExpression, ["--$k", "--${k}"])

        for literal in ("$--i", "--i"):
            # Although valid according to grammar spec.
            with self.assertRaises(ParseError):
                self._parse(PreDecrementExpression, literal)

    def test_pre_increment_expression(self):
        self._test_expected(PreIncrementExpression, ["++$k", "++${k}"])

        for literal in ("$++i", "++i"):
            # Although valid according to grammar spec.
            with self.assertRaises(ParseError):
                self._parse(PreIncrementExpression, literal)

    def test_redirected_file_name(self):
        parts = ["output.txt", '"$abc"', "$null"]
        self._test_expected(RedirectedFileName, parts)

    def test_multiplicative_expression(self):
        parts = [
            "12 * -10",
            "-10.300D * 12",
            "10.6 * 12",
            '12 * "0xabc"',
            "12 / -10",
            "-10.300D / 12",
            "10.6 / 12",
            '12 / "0xabc"',
            "12 % -10l",
            "-10.300D % 12",
            "10.6 % 12",
            '12 % "0xabc"',

            "(10 * 4) % 5",
            "(10* (5 % 4)) /4",
        ]
        self._test_expected(MultiplicativeExpression, parts)

    def test_range_expression(self):
        parts = [
            "1..10",
            "-500..-495",
            "16..16",
            "$x..5.40D",
            "$true..3",
            "-2..$null",
            '"0xf".."0xa"',
        ]
        self._test_expected(RangeExpression, parts)
