# pylint: disable=too-many-lines
"""
Tests for wispy.grammar.
"""

# Some pylint scaffolding.
# pylint: disable=too-many-public-methods, invalid-name, no-self-use
# pylint: disable=missing-docstring, import-error
# pylint: disable=bad-builtin, star-args
# pylint: disable=wildcard-import, unused-wildcard-import
# pylint: disable=anomalous-backslash-in-string
# pylint: disable=undefined-variable

import unittest
import string
import logging
from itertools import chain, tee
from textwrap import dedent

from modgrammar import ParseError
from modgrammar.debugging import DEBUG_ALL
from wispy.grammar import *


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
        text_pairs = zip(*tee(texts))
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

    def test_token(self):
        tokens = [
            # Keywords
            "begin", "break", "catch", "class",
            "continue", "data", "define", "do",
            "dynamicparam", "else", "elseif", "end",
            "exit", "filter", "finally", "for",
            "foreach", "from", "function", "if",
            "in", "param", "process", "return",
            "switch", "throw", "trap", "try",
            "until", "using", "var", "while",

            # Integer literal
            "0d", "0xa1",

            # Real literal
            "1.4", "1.4e44", "1.4e44kb",
            ".4", ".4e44", ".4e4d", ".4e4dkb", ".4Dkb",
            "4e4", "4e4D", "4e4d", "4e4dkb",

            # Type literal
            "[object[]]", "[int]", "[int[,,]]",

            # String literal
            "\"tralalala\"", "'tralalala'",
            "@\"\ntest1\ntest2\n\"@", "@'\ntest1\ntest2\n'@",

            # Command
            'Get-Factorial 5', '& Get-Factorial 5',

            # Operator or punctuator
            "{", "}", "[", "]", "(", ")", "@(", "@{", "$(", ";",
            "&&", "||", "&", "|", ",", "++", "..", "::", ".",
            "!", "*", "/", "%", "+", "2>&1", "1>&2",
            "-", "--",
            "-and", "-band", "-bnot",
            "-bor", "-bxor", "-not",
            "-or", "-xor", "+=", "*=",
            ">>", "-inotlike", "-f"

            # Command
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
        self._test_expected(Token, tokens)

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
        literals = chain(literals,
                         [literal.swapcase() for literal in literals])
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

    def test_expandable_here_string_part(self):
        literals = [
            "x",
            "{",
            "$x",
            "$\nx",
            "$\n\"x",
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

    def test_expandable_string_with_subexpr_part(self):
        literals = [
            "$x",
            "\"\"",
            "$()",
            "$(function test {})"
        ]
        self._test_expected(ExpandableStringWithSubexprPart, literals)

    def test_expandable_string_with_subexpr_characters(self):
        literals = [
            "$a"
            "\"\""
            "$(function my_func {$a = 1})"
        ]
        self._test_expected(ExpandableStringWithSubexprCharacters,
                            literals)

    def test_expandable_string_literal_with_subexpr(self):
        literals = [
            "\"test$()$a\"\"$(function test {})\""
        ]
        self._test_expected(ExpandableStringLiteralWithSubexpr, literals)

    def test_expandable_here_string_with_subexpr_part(self):
        literals = [
            "$({data {}} {})",
            "$\nx"
        ]
        self._test_expected(ExpandableHereStringWithSubexprPart, literals)

    def test_expandable_here_string_with_subexpr_characters(self):
        literals = [
            "$({} {})"
            "$\nx",
            "$({data {}} {})"
            "$\nx"
        ]
        self._test_expected(ExpandableHereStringWithSubexprCharacters,
                            literals)

    def test_expandable_here_string_literal_with_subexpr(self):
        literals = [
            "@\"\n$($y)$\nx\n\"@",
            "@\"\n if($grade -ge 90){\"Grade A\"} $($({} {})$\nx\n\"@"
        ]
        self._test_expected(ExpandableHereStringLiteralWithSubexpr, literals)

    def test_string_literal_with_subexpression(self):
        literals = [
            "@\"\n$($({} {})$\nx\n\"@",
            "\"test$()$a\"\"$(function test {})\""
        ]
        self._test_expected(StringLiteralWithSubexpression, literals)

    def test_requires_comment(self):
        literals = [
            "#requires New-Object"
        ]
        self._test_expected(RequiresComment, literals)

    def test_script_block(self):
        literals = [
            "",
            "\n[test[]]\nparam\n($var\n$var\n)",
            ";",
            "process { }",
            "\n[test[]]\nparam\n($var\n$var\n)"
            ";"
        ]
        self._test_expected(ScriptBlock, literals)

    def test_script_block_body(self):
        literals = [
            'for ()\n{ "$i" }'
            'if($grade -ge 90){"Grade A"}'
        ]
        self._test_expected(ScriptBlockBody, literals)

    def test_script_block_expression(self):
        literals = [
            "{process { $a = 1 }}",
            "{\nprocess { $a = 1 }\n}"
        ]
        self._test_expected(ScriptBlockExpression, literals)

        with self.assertRaises(ParseError):
            self._parse(ScriptBlockExpression, "process")

    def test_script_parameter(self):
        literals = [
            "$var",
            "\n$var=1",
            "\n\n[Smth[]]$var=''",
            "$var4 =   42",
        ]
        self._test_expected(ScriptParameter, literals)

        with self.assertRaises(ParseError):
            self._parse(ScriptParameter, "test")

    def test_script_parameter_default(self):
        literals = [
            "=0",
            "\n=\n\n''",
            "\n = $tobi",
        ]
        self._test_expected(ScriptParameterDefault, literals)

        with self.assertRaises(ParseError):
            self._parse(ScriptParameterDefault, "=$")

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
        self._test_expected(TypeSpec, ["int[, ]", "int[, ,]", "int[,, ]",
                                       "int[, , ]"])
        self._test_expected(TypeSpec, ["int", "float", "double"])
        self._test_expected(TypeSpec, ["Dictionary[float,double]"])
        self._test_expected(TypeSpec, ["Dictionary[int[float]]"])

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
        literals = chain(string.ascii_letters, ["\u005F", "?"])
        self._test_expected(FirstParameterCharacter, literals)

        for digit in string.digits:
            with self.assertRaises(ParseError):
                self._parse(FirstParameterCharacter, digit)

    def test_command(self):
        parts = [
            'Get-Factorial 5',
            '& Get-Factorial 5',
            '& "Get-Factorial" 5',
            "New-Object 'int[,]' '3,2'",
        ]
        self._test_expected(Command, parts)

    def test_command_parameter(self):
        dashes = "-", "\u2013", "\u2014", "\u2015"
        parts = [dash + letter + "wispy"
                 for dash in dashes
                 for letter in string.ascii_letters]
        self._test_expected(CommandParameter, parts)
        self._test_expected(CommandParameter, [part + ":" for part in parts])

    def test_command_name_expr(self):
        names = ['Get-Factorial', 'New-Object']
        self._test_expected(CommandNameExpr, names)

    def test_command_argument(self):
        # The same as CommandNameExpr
        names = ['Get-Factorial', 'New-Object']
        self._test_expected(CommandArgument, names)

    def test_command_element(self):
        parts = chain(
            ['Get-Factorial'],
            ["-" + letter for letter in string.ascii_letters],
            ["2>&1", "1>&2",
             # with whitespace
             ">> filename", "> a.txt", "< b.txt", "2>> c.txt", "2> d.txt",
             # without whitespace
             ">>filename", ">a.txt", "<b.txt", "2>>c.txt", "2>d.txt",
             # variables
             ">$null", "2>>$null"]
        )
        self._test_expected(CommandElement, parts)

    def test_command_elements(self):
        parts = ["Get-Factorial -wispy", "Redirect >>filename"]
        self._test_expected(CommandElements, parts)

    def test_generic_type_arguments(self):
        arguments = [
            "string[]",
            "int[,]",
            "int[,,]",
            "int[,,]",
            "int,float",
            "int[,],float[,]",
            "int[,,], float[,,]",
            "string[,], int, float"
        ]
        self._test_expected(GenericTypeArguments, arguments)

    def test_newline(self):
        self._test_expected(NewLineCharacter, ["\r", "\n", "\r\n"])

    def test_input_characters(self):
        elements = chain(["hoptrop", "troptzop", "t"], string.ascii_letters)
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
        multipliers = ["kb", "mb", "gb", "tb", "pb",
                       "Kb", "MB", "gB", "TB", "Pb"]
        self._test_expected(NumericMultiplier, multipliers)

        with self.assertRaises(ParseError):
            self._parse(NumericMultiplier, "bkb")

    def test_long_type_suffix(self):
        suffixes = ["l", "L"]
        self._test_expected(LongTypeSuffix, suffixes)

        with self.assertRaises(ParseError):
            self._parse(LongTypeSuffix, "d")

    def test_decimal_type_suffix(self):
        suffixes = ["d", "D", "l", "L"]
        self._test_expected(DecimalTypeSuffix, suffixes)

        with self.assertRaises(ParseError):
            for char in string.ascii_letters:
                if char not in suffixes:
                    self._parse(DecimalTypeSuffix, char)

    def test_numeric_type_suffix(self):
        suffixes = ["l", "L", "d", "D"]
        self._test_expected(NumericTypeSuffix, suffixes)

    def test_decimal_digits(self):
        numbers = list(map(str, range(10)))
        self._test_expected(DecimalDigits, numbers)

        with self.assertRaises(ParseError):
            self._parse(DecimalDigits, "a")

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
        self._test_expected(DashDash,
                            [literal + literal for literal in literals])

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
            "4e4", "4e4D", "4e4d", "4e4dkb",
            "1.4L", "1.4e4L",
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
        literals = chain(literals,
                         [literal.swapcase() for literal in literals])
        self._test_expected(ComparisonOperator, literals)

    def test_operator_or_punctuator(self):
        literals = [
            "{", "}", "[", "]", "(", ")", "@(", "@{", "$(", ";",
            "&&", "||", "&", "|", ",", "++", "..", "::", ".",
            "!", "*", "/", "%", "+", "2>&1", "1>&2",
            "-", "--",
            "-and", "-band", "-bnot",
            "-bor", "-bxor", "-not",
            "-or", "-xor", "-AND", "-BAND", "-BNOT",
            "-BOR", "-Bxor", "-nOt",
            "+=", "*=",
            ">>", "-INOTliKe", "-inotlike",
            "-f"
        ]
        self._test_expected(OperatorOrPunctuator, literals)

    def test_variable_characters(self):
        literals = chain(["a", "abc"],
                         string.digits,
                         string.ascii_letters,
                         ["?"])
        self._test_expected(VariableCharacters, literals)

    def test_variable_namespace(self):
        literals = ["abc:", "a:"]
        self._test_expected(VariableNamespace, literals)

        with self.assertRaises(ParseError):
            self._parse(VariableNamespace, ":a")

    def test_variable_scope(self):
        scopes = ["globe:", "global:", "local:", "private:", "script:",
                  "GLOBE:", "GLOBAL:", "Local:", "Private:", "SCRIPT:"]
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
        literals = chain(string.ascii_letters, ["\u005F"])
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

    def test_verbatim_command_string(self):
        literals = [
            "\"x\"",
            "\"xxx\"",
            "\"a#@$^%$  ++\""
        ]
        self._test_expected(VerbatimCommandString, literals)

        with self.assertRaises(ParseError):
            self._parse(VerbatimCommandString, "\"aaa")

    def test_verbatim_command_argument_part(self):
        literals = [
            "\"test\"",
            "&x",
            "t"
        ]
        self._test_expected(VerbatimCommandArgumentPart, literals)

        with self.assertRaises(ParseError):
            self._parse(VerbatimCommandArgumentPart, "&&")

    def test_verbatim_command_argument_chars(self):
        literals = [
            "\"test\""
            "aaa"
            "x"
            "&a"
        ]
        self._test_expected(VerbatimCommandArgumentChars, literals)

        with self.assertRaises(ParseError):
            self._parse(VerbatimCommandArgumentChars, "||")

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
            '$totalCost',
            '$Maximum_Count_26',
            '${Maximum_Count_26}',
            '${Name with`twhite space and `{punctuation`}}',
            r'${E:\File.txt}'
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

    def test_attribute(self):
        elements = [
            "[A(10,IgnoreCase=$true)]",
            "[Parameter(Mandatory = $true)]",
            "[Parameter(Mandatory = $true, TropaTropa=$Penelopa)]",
            '[Alias("CN")]',
            '[Alias("name","system")]',
            '[string[]]',
            '[Parameter(Mandatory = $true,\n'
            'ParameterSetName = "Computer")]',
        ]
        self._test_expected(Attribute, elements)

    def test_attribute_argument(self):
        elements = [
            "IgnoreCase=$true",
            "Mandatory= $true",
            "$true",
        ]
        self._test_expected(AttributeArgument, elements)

    def test_attribute_arguments(self):
        elements = [
            "IgnoreCase=$true, Tobi=$Good_boy",
            "Juubi=$Kaguya \n , \n Obito=$Tobi",
        ]
        self._test_expected(AttributeArguments, elements)

    def test_range_argument_expression(self):
        literals = [
            "23",
            "2..100..3",
            "2..100..\n3"
        ]
        self._test_expected(RangeArgumentExpression, literals)

    def test_multiplicative_argument_expression(self):
        literals = [
            "1..10..1*2..3..1",
            "1..10..1/\n2..3..1",
        ]
        self._test_expected(MultiplicativeArgumentExpression, literals)

        with self.assertRaises(ParseError):
            self._parse(MultiplicativeArgumentExpression,
                        "1..10..2+2..4..2")

    def test_format_argument_expression(self):
        literals = [
            "1..10..1",
            "1..10..1-f2..3..1",
            "1..10..1-f\n2..3..1"
        ]
        self._test_expected(FormatArgumentExpression, literals)

        with self.assertRaises(ParseError):
            self._parse(FormatArgumentExpression, "4..5..1-f\n\n")

    def test_attribute_list(self):
        elements = [
            "[A(10,IgnoreCase=$true)]"
            "[Parameter(Mandatory = $true)]",

            "[Parameter(Mandatory = $true, TropaTropa=$Penelopa)]\n\n"
            '[Alias("CN")]',

            '[Alias("name","system")]'
            '[string[]]'
            '[Parameter(Mandatory = $true,\n'
            'ParameterSetName = "Computer")]',
        ]
        self._test_expected(AttributeList, elements)

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
        names = ["dynamicparam", "begin", "process", "end",
                 "DynamicParam", "BEGIN", "PrOcEss", "End"]
        self._test_expected(BlockName, names)

    def test_switch_parameters(self):
        params = ["-regex", "-wildcard", "-exact", "-casesensitive"]
        self._test_expected(SwitchParameter, params)
        self._test_expected(SwitchParameter,
                            [param.upper() for param in params])

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
            "Break $lab",

            "continue",
            "continue $lab",
            "continue labelA",
            "CONTINUE labelB",

            "throw",
            "throw 100",
            'throw "No such record in file"',
            "Throw 42",

            "return 1",
            "return $4",
            "Return $52",

            "exit",
            "exit $4",
            "EXIT $42",
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

    def test_parenthesized_expression(self):
        parts = [
            "($x)",
            "($a = 1234 * 3.5)",
            "(($a = -23))",
            "($b = 0)",
            "($b--)",
            "(\n$b-- )",
            "(        $a=1246 *5 )",
        ]
        self._test_expected(ParenthesizedExpression, parts)

        with self.assertRaises(ParseError):
            self._parse(ParenthesizedExpression, "($b--")

    def test_member_access(self):
        parts = [
            "$a.Length",
            "(10,20,30).Length",
            "$a.$property",
            "[int]::MinValue",
            "[long]::$property",
            "(10,20,30).\nlength",
            "[long]::\n  $property",
        ]
        # We test with PrimaryExpression, because MemberAccess grammar
        # is written in a different way for solving the left
        # recursion problem.
        self._test_expected(PrimaryExpression, parts)

    def test_primary_expression(self):
        parts = [
            # Member access
            "$a.Length",
            "(10,20,30).Length",
            "$a.$property",
            "[int]::MinValue",
            "[long]::$property",
            "(10,20,30).\nlength",
            "[long]::\n  $property",

            # Element access
            "$a[1]",
            "$a[-1]",
            "$a[0,0]",
            "$a[0,0]++",
            "$list[2][1]",
            '$a[1]["B"]',
            "$a[1][$true]",
            "$a[1,3,5]",
            "$a[,5]",
            "$a[@()]",
            "$a[-1..-3]",
            "$a[(0,1),(1,0)]",
            "$h1[$null,'IDNum']",

            # Invocation expression
            "[math]::Sqrt(2.0)",
            '[char]::IsUpper("a")',
            "[math]::Sqrt(2D)",
            "[math]::Sqrt($true)",
            "$a.Invoke(2.0)",
            '[math]::("Sq"+"rt")',

            # Post increment and decrement
            "$i++",
            "$i--",
            "$a[$i++]",

            # Mixed
            "$a[1][1,3,5]++",
            "$a[1]::Sqrt(2.0)",
            "[math]::Result(2D)[1]++",
        ]
        self._test_expected(PrimaryExpression, parts)

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

    def test_cast_expression(self):
        parts = [
            "[bool]-10",
            "[int]-10.70D",
            "[int]10.7",
            '[long]"+2.3e+3"',
            '[char[]]"Hello"',
        ]
        self._test_expected(CastExpression, parts)

    def test_bitwise_expression(self):
        parts = [
            "0x0F0F -band 0xFE",
            "0x0F0F -band 0xFEL",
            "0x0F0F -band 14.6",
            "0x0F0F -bor 0xFE",
            "0x0F0F -bor 0xFEL",
            "0x0F0F -bor 14.40D",
            "0x0F0F -bxor 0xFE",
            "0x0F0F -bxor 0xFEL",
            "0x0F0F -bxor 14.40D",
            "0x0F0F -bxor 14.6",
            "0x0F0F -BXOR 14",
            "0x0F0F -BOR 24",
            "0x0F0F -BaND 23",
        ]
        self._test_expected(BitwiseExpression, parts)

    def test_while_condition(self):
        self._test_expected(WhileCondition, ["$i -le 5", "\n\n$i -gt 4"])

    def test_comparison_expression(self):
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
        parts = ["$i {}{} 5".format(dash, op)
                 for op in ops
                 for dash in ("-", "\u2013", "\u2014", "\u2015")]
        self._test_expected(ComparisonExpression, parts)

    def test_logical_expression(self):
        parts = [
            "($j -gt 5) -and (++$k -lt 15)",
            "($j -gt 5) -and ($k -le 21)",
            "($j++ -gt 5) -and ($j -le 10)",
            "($j -eq 5) -and (++$k -gt 15)",
            "($j++ -gt 5) -or (++$k -lt 15)",
            "($j -eq 10) -or ($k -gt 15)",
            "($j -eq 10) -OR ($k -gt 15)",
            "($j -eq 10) -XoR ($k -gt 15)",
            "($j -eq 10) -AND ($k -gt 15)",
        ]
        self._test_expected(LogicalExpression, parts)

    def test_expression(self):
        parts = [
            "($j -gt 5) -and (++$k -lt 15)",
            "($j -gt 5) -and ($k -le 21)",
            "0x0F0F -band 0xFE",
            "0x0F0F -band 0xFEL",
            "$j -isnot 5", "$j -isplit $x",
            "12 + -10L",
            "-10.300D + 12",
            "-10.300D % 12",
            "10.6 % 12",
            '12 % "0xabc"',
            "(10 * 4) % 5",
            "(10* (5 % 4)) /4",
            '">{0,-3}<" -f 5',
            '">{0,3:000}<" -f 5',
            '">{0,5:0.00}<" -f 5.0',
            '">{0:C}<" -f     1234567.888',
            "1..10",
            "-500..-495",
            "16..16",
            "$x..5.40D",
            "2,4,6",
            "(2,4),6",
            "(2,4,6),12,(2..4)",
            '2,4,6,"red",$null,$true',
        ]
        self._test_expected(Expression, parts)

    def test_additive_expression(self):
        parts = [
            "12 + -10L",
            "-10.300D + 12",
            "10.6 + 12",
            '12 + "0xabc"',
            "12 + $A + (14 + -10d)",
        ]
        self._test_expected(AdditiveExpression, parts)

    def test_argument_list(self):
        parts = [
            "(2.0)", '("a")', "()",
            "($true)", '("Sq"+"rt")',
            "( $true, $false)",
        ]
        self._test_expected(ArgumentList, parts)

    def test_argument_expression_list(self):
        parts = [
            "2.0, 4.0", '"a", "b"', "$true,\n$false",
            '"Sq"+"rt",$a, $b'
        ]
        self._test_expected(ArgumentExpressionList, parts)

    def test_array_expression(self):
        parts = [
            "@($i = 10)",
            "@(($i = 10))",
            "@($i = 10; $j)",
            "@(($i = 10); $j)",
            "@(($i = 10); ++$j)",
            "@(($i = 10); (++$j))",
            "@($i = 10; ++$j)",
            "@(2,4,6)",
            "@($a)",
            "@(@($a))",
            "@(2, 4,    6)",
        ]
        self._test_expected(ArrayExpression, parts)

    def test_array_literal_expression(self):
        parts = [
            "2,4,6",
            "(2,4),6",
            "(2,4,6),12,(2..4)",
            '2,4,6,"red",$null,$true',
            "2,    4,      6",
        ]
        self._test_expected(ArrayLiteralExpression, parts)

    def test_format_expression(self):
        parts = [
            '"{2} <= {0} + {1}`n" -f $i,$j,($i+$j)',
            '">{0,3}<" -f 5',
            '">{0,-3}<" -f 5',
            '">{0,3:000}<" -f 5',
            '">{0,5:0.00}<" -f 5.0',
            '">{0:C}<" -f     1234567.888',
            '">{0:C}<" -f     -1234.56',
            '">{0,12:e2}<" -f 123.456e2',
            '">{0,-12:p}<" -f -0.252',
            '$format -f    123455',
            '$format -f5',
            '$format-f5',
        ]
        self._test_expected(FormatExpression, parts)

    def test_assignment_expression(self):
        parts = [
            "$a = 20",
            "$hypot = [Math]::Sqrt(3*3 + 4*4)",
            "$a = $b = $c = 10.20D",
            "$a = (10,20,30),(1,2)",
            "[int]$x = 10.6",
            '[long]$x = "0xabc"',
            "$a = [float]",
        ]
        self._test_expected(AssignmentExpression, parts)

    def test_label_expression(self):
        self._test_expected(LabelExpression, ["abc", "$abc"])

    def test_hash_literal_expression(self):
        parts = [
            '@{ FirstName = "James"; LastName = "Anderson"; IDNum = 123 }',
            '@{ FirstName = "James"; LastName = $last; IDNum = $IDNum + 3 }',
            '@{ }',
            '@{ 10 = "James"; 20.5 = "Anderson"; $true = 123 }',
            '@{FirstName="James"}',
            '@{FirstName= "James"; }',
            '@{}',
        ]
        self._test_expected(HashLiteralExpression, parts)

    def test_hash_literal_body(self):
        parts = [
            'FirstName = "James"; LastName = "Anderson"; IDNum = 123',
            'FirstName = "James"; LastName = $last; IDNum = $IDNum + 3',
            '10 = "James"; 20.5 = "Anderson"; $true = 123',
            'FirstName="James"',
            'FirstName= "James";',
        ]
        self._test_expected(HashLiteralBody, parts)

    def test_hash_entry(self):
        parts = [
            'FirstName = "James"',
            'IDNum = 123',
            'LastName = $last',
            'IDNum = $IDNum + 3',
            '10 = "James"',
            '$true = 123',
            'FirstName="James"',
            'FirstName= "James"',
        ]
        self._test_expected(HashEntry, parts)

    def test_key_expression(self):
        self._test_expected(KeyExpression, ["$true", "LastName"])

    def test_if_statement(self):
        parts = [
            dedent('''if ($grade -ge 90) { "Grade A" }
                   elseif ($grade -ge 80) { "Grade B" }
                   elseif ($grade -ge 70) { "Grade C" }
                   elseif ($grade -ge 60) { "Grade D" }
                   else { "Grade F" }'''),
            dedent('''if ($grade -ge 90) { "Grade A" }
                   elseif ($grade -ge 80) { "Grade B" }'''),
            dedent('''if ($grade -ge 90) { "Grade A" }
                   else { "Grade F" }'''),
            dedent('''if ($grade -ge 90) { "Grade A" }'''),
            dedent('''if($grade -ge 90){"Grade A"}'''),
            dedent('''IF($grade -ge 90){"Grade A"}'''),
            dedent('''If($grade -ge 90){"Grade A"}'''),
        ]
        self._test_expected(IfStatement, parts)

    def test_else_clause(self):
        parts = [
            'else { "grade f" }',
            'else{"grafe f"\n}',
            'else{"giraffe"}',
            'ELSE{"giraffe"}',
            'Else{"giraffe"}',
        ]
        self._test_expected(ElseClause, parts)

    def test_else_if_clause(self):
        parts = [
            'elseif ($grade -ge 80) { "Grade B" }',
            'elseif($grade -ge 70){ "Grade C" }',
            'elseif($grade -ge 60)\n{"Grade D"}',
            'ELSEIF($grade -ge 60)\n{"Grade D"}',
            'ElseIf($grade -ge 60)\n{"Grade D"}',
        ]
        self._test_expected(ElseIfClause, parts)

    def test_else_if_clauses(self):
        parts = [
            'elseif ($grade -ge 80) { "Grade B" }\n'
            'elseif($grade -ge 70){ "Grade C" }',
        ]
        self._test_expected(ElseIfClauses, parts)

    def test_for_statement(self):
        parts = [
            'for ($i = 1; $i -le 10; ++$i) { "$i" }',
            'for ($i = 1; $i -le 10; ++$i)\n{ "$i*$i" }',
            'for\n($i = 1; $i -le 10; ++$i)\n{ "$i*$i" }',
            'for ($i = 1; $i -le 10;)\n{ "$i" }',
            'for ($i = 1;)\n{ "$i" }',
            'for ()\n{ "$i" }',
            'for (){ "$i" }',
            'For (){ "$i" }',
            'FOR (){ "$i" }',
        ]
        self._test_expected(ForStatement, parts)

    def test_for_condition(self):
        parts = ["$i -le 10", "$i -gt 40"]
        self._test_expected(ForCondition, parts)

    def test_for_iterator(self):
        parts = ["++$i", "--$i"]
        self._test_expected(ForIterator, parts)

    def test_for_initializer(self):
        parts = ["$i = 1"]
        self._test_expected(ForInitializer, parts)

    def test_while_statement(self):
        parts = [
            'while ($j -le 100) { $j }',
            'while ($i++ -lt 2) { $i }',
            'while ($i++ -lt 2)\n{ $i }',
            'while (\n$i++ -lt 2\n) { $i }',
            'while (1) { $i }',
            'WHILE (1) { $i }',
            'While (1) { $i }',
        ]
        self._test_expected(WhileStatement, parts)

    def test_do_statement(self):
        parts = [
            'do\n{\n$i;\n}\nwhile (++$i -le 5)',
            'do\n{\n$i;}\nuntil (++$i -gt 5)',
            'do { $i } while ($i)',
            'do { $i } until ($i -le 85)',
            'DO { $i } Until ($i -le 85)',
            'Do { $i } WHILE ($i)',
        ]
        self._test_expected(DoStatement, parts)

    def test_foreach_statement(self):
        parts = [
            'foreach ($e in $a) {}',
            'foreach ($e in -5..5)\n{}',
            'foreach ($t in [byte],[int],[long]) {$t::MaxValue}',
            'foreach ($f in dir *.txt)\n\n{}',
            'foreach ($e in $h1.Keys) {}',
            'Foreach ($e In $h1.Keys) {}',
            'FOREACH ($e IN $h1.Keys) {}',
        ]
        self._test_expected(ForeachStatement, parts)

    def test_foreach_parameter(self):
        params = ["-parallel", "-Parallel", "-PARALLEL"]
        self._test_expected(ForeachParameter, params)

    def test_switch_clause(self):
        parts = [
            '"`n"  { ++$lineCount }',
            '"`n"  { ++$lineCount };',
            'a*      { "a*, $_" }',
            '?B?     { "?B? , $_" }',
            'default { "default, $_" }',
            '{a$_ -lt 20 }	{ "-lt 20" }',
            '{$_ -band 1 }	{ "Odd" }',
            '{$_ -eq 19 }	{ "-eq 19" }'
        ]
        self._test_expected(SwitchClause, parts)

    def test_switch_clauses(self):
        parts = [
            '"`n"  { ++$lineCount }\n'
            '"`n"  { ++$lineCount };',

            'a*      { "a*, $_" }\n\n'
            '?B?     { "?B? , $_" }',
        ]
        self._test_expected(SwitchClauses, parts)

    def test_switch_clause_condition(self):
        parts = ['"`n"', 'a*', '?B', 'default', '^a*']
        self._test_expected(SwitchClauseCondition, parts)

    def test_switch_condition(self):
        parts = [
            '(0,1,19,20,21)',
            '($s[$i])',
            '("abc")',
            '($someString.ToLower())',
            '-file C:\\obito',
            '-file $ps1',

            '($PSBoundParameters.GetEnumerator().\n'
            'Where({$_.Value -eq $true}).Key)',
        ]
        self._test_expected(SwitchCondition, parts)

    def test_switch_filename(self):
        parts = ['C:\\obito', '$ps1']
        self._test_expected(SwitchFilename, parts)

    def test_switch_body(self):
        parts = [
            dedent('''{
                       "`n"  { ++$lineCount }
                       "`f"  { ++$pageCount }
                       "`t"  { }
                       default { ++$otherCount }
                   }'''.strip()),
            dedent('{"`n"  { ++$lineCount };"`f"  { ++$pageCount };"`t" { };'
                   '  default { ++$otherCount }}'),

            dedent('''{
                        { $_ -lt 20 }	{ "-lt 20" }
                        { $_ -band 1 }	{ "Odd" }
                        { $_ -eq 19 }	{ "-eq 19" }
                        default { "default" }
                      }'''.strip()),
        ]
        self._test_expected(SwitchBody, parts)

    def test_switch_statement(self):
        stmts = [
            dedent('''switch ($s[$i])
                      {
                      "`n"  { ++$lineCount }
                      "`f"  { ++$pageCount }
                      "`t"  { }
                      " "  { }
                      default { ++$otherCount }
                   }'''),

            dedent('''switch -wildcard ("abc")
            {
              a*      { ++$lineCount }
            }'''),
            dedent('''Switch -wildcard ("abc")
            {
              a*      { ++$lineCount }
            }'''),
            dedent('''SWITCH -wildcard ("abc")
            {
              a*      { ++$lineCount }
            }'''),

            dedent('''switch -regex -casesensitive ("abc")
            {
              ^a* { "a*" }
              ^A* { "A*" }
            }''')
        ]
        self._test_expected(SwitchStatement, stmts)

    def test_member_name(self):
        elements = ["Sqrt", "IsUpper", "ToUpper"]
        self._test_expected(MemberName, elements)

    def test_unary_expression(self):
        parts = [
            ",10",
            ",(10, 'red')",
            ",,10",
            "!'xyz'",
            "-not $true",
            "+123L",
            "+'0xabc'",
        ]
        self._test_expected(UnaryExpression, parts)

    def test_expression_with_unary_operator(self):
        parts = [
            operator + " " + "$true"
            for operator in
            (",", "-bnot", "-not", "-split", "-join", "!", "+")
        ]
        parts = chain(
            parts,
            [literal.swapcase() for literal in parts],
            [
                "[bool]-10",
                "[int]-10.70D",
                "[int]10.7",
                '[long]"+2.3e+3"',
                '[char[]]"Hello"',
                '++$k', '++${k}',
            ])
        self._test_expected(ExpressionWithUnaryOperator, parts)

    def test_merging_redirection_operator(self):
        operators = (
            '*>&1', '2>&1', '3>&1', '4>&1', '5>&1', '6>&1',
            '*>&2', '1>&2', '3>&2', '4>&2', '5>&2', '6>&2'
        )
        self._test_expected(MergingRedirectionOperator, operators)

    def test_non_ampersand_character(self):
        self._test_expected(NonAmpersandCharacter, string.ascii_letters)
        self._test_expected(NonAmpersandCharacter, string.digits)

        with self.assertRaises(ParseError):
            self._parse(NonAmpersandCharacter, '&')

    def test_non_double_quote_character(self):
        self._test_expected(NonDoubleQuoteCharacter, string.ascii_letters)
        self._test_expected(NonDoubleQuoteCharacter, string.digits)

        for char in ("\u0022", "\u201C", "\u201D", "\u201E"):
            with self.assertRaises(ParseError):
                self._parse(NonDoubleQuoteCharacter, char)

        self._test_expected(NonDoubleQuoteCharacters, ['tobi', 'is', 'obito'])

    def test_signature_begin(self):
        parts = ['\n# SIG # Begin signature block\n']
        self._test_expected(SignatureBegin, parts)

    def test_signature_end(self):
        parts = ['\n# SIG # End signature block\n']
        self._test_expected(SignatureEnd, parts)

    def test_signature(self):
        parts = [
            '# MIIEMwYJKoZIhvcNAQcCoIIEJDCCBCACAQEx\n'
            '# gjcCAQSgWzBZMDQGCisGAQQBgjcCAR4wJgID\n'
            '# tobi is obito\n'
            '# Juubi is Shinju\n'
            '# Juubi is Kaguya'
        ]
        self._test_expected(Signature, parts)

    def test_signature_block(self):
        parts = [
            '\n# SIG # Begin signature block\n'
            '# MIIEMwYJKoZIhvcNAQcCoIIEJDCCBCACAQEx\n'
            '# gjcCAQSgWzBZMDQGCisGAQQBgjcCAR4wJgID\n'
            '# tobi is obito\n'
            '# Juubi is Shinju\n'
            '# Juubi is Kaguya\n'
            '# SIG # End signature block\n'
        ]
        self._test_expected(SignatureBlock, parts)

    def test_trap_statement(self):
        stmts = [
            'trap { $j = 2}',
            'trap { $j =2; continue }',
            'trap {$j =2; break }',
            'trap{}',
            'Trap{}',
            'TRAP{}',
        ]
        self._test_expected(TrapStatement, stmts)

    def test_finally_clause(self):
        clauses = [
            'finally\n{ "Tobi is a good boy" }',
            'finally{$a=4}',
            'Finally{$a=4}',
            'FINALLY{$a=4}',
        ]
        self._test_expected(FinallyClause, clauses)

    def test_catch_clause(self):
        clauses = [
            'catch\n{"Caught unexpected exception"}',
            'Catch\n{"Caught unexpected exception"}',
            'CATCH\n{"Caught unexpected exception"}',
            dedent('''catch [IndexOutOfRangeException]
                   {
                        "Handling out-of-bounds index, >$_<`n"
                        $i = 5
                      }'''),
            dedent('''catch [IndexOutOfRangeException], [Exception]
                   {
                        "Handling out-of-bounds index, >$_<`n"
                         $i = 5
                      }'''),
            dedent('''catch [IndexOutOfRangeException],
                            [Exception]
                   {
                        "Handling out-of-bounds index, >$_<`n"
                        $i = 5
                      }'''),
        ]
        self._test_expected(CatchClause, clauses)

    def test_catch_clauses(self):
        clauses = [
            'catch\n{"Caught unexpected exception"}',
            dedent('''catch [IndexOutOfRangeException]
                   {
                        "Handling out-of-bounds index, >$_<`n"
                        $i = 5
                      }
                   catch [IndexOutOfRangeException], [Exception]
                   {
                        "Handling out-of-bounds index, >$_<`n"
                         $i = 5
                      }'''),
            dedent('''catch [IndexOutOfRangeException]
                   {
                        "Handling out-of-bounds index, >$_<`n"
                        $i = 5
                      }catch [IndexOutOfRangeException], [Exception]{
                        "Handling out-of-bounds index, >$_<`n"
                         $i = 5
                      }'''),
        ]
        self._test_expected(CatchClauses, clauses)

    def test_catch_type_list(self):
        parts = [
            '[IndexOutofRangeException]',
            '[IndexOutofRangeException], [TobiIsObito]',
            '[KaguyaIsShinju], \n'
            '[MadaraIsThirdMizukage]',
        ]
        self._test_expected(CatchTypeList, parts)

    def test_value(self):
        parts = [
            "($x)",
            "($a = 1234 * 3.5)",
            "(($a = -23))",
            "($b = 0)",
            "($b--)",
            "(\n$b-- )",
            "(        $a=1246 *5 )",
            "@($i = 10)",
            "@(($i = 10))",
            "@($i = 10; $j)",
            "@(($i = 10); $j)",
            "@(2, 4,    6)",
            '@{ FirstName = "James"; LastName = "Anderson"; IDNum = 123 }',
            '@{ 10 = "James"; 20.5 = "Anderson"; $true = 123 }',
            '@{FirstName="James"}',
            "1.4", "1.4e44", "1.4e44kb",
            ".4", ".4e44", ".4e4d", ".4e4dkb", ".4Dkb",
            "\"tralalala\"",
            "@\"\ntest1\ntest2\n\"@",
        ]
        self._test_expected(Value, parts)

    def test_input_element(self):
        parts = [
            ' ', '# Batman is Clark Kent', '$a',
            'until'
        ]
        self._test_expected(InputElement, parts)

    def test_input_elements(self):
        parts = [
            '# Tobi is Obito\n'
            '$tobi = $Good_Boy'
        ]
        self._test_expected(InputElements, parts)

    def test_input(self):
        parts = [
            '$tobi = $GoodBoy\n'
            '\n# SIG # Begin signature block\n'
            '# MIIEMwYJKoZIhvcNAQcCoIIEJDCCBCACAQEx\n'
            '# gjcCAQSgWzBZMDQGCisGAQQBgjcCAR4wJgID\n'
            '# tobi is obito\n'
            '# Juubi is Shinju\n'
            '# Juubi is Kaguya\n'
            '# SIG # End signature block'
        ]
        self._test_expected(Input, parts)

    def test_function_name(self):
        names = ['Get-Power', 'Find-Str', 'Test']
        self._test_expected(FunctionName, names)

    def test_parameter_list(self):
        parts = [
            '[long]$base, [int]$exponent',
            '[string]$str, [int]$start_pos = 0',
            '[switch]$trace, $p1, $p2',
            '$a, $b, $c, $d',
            '$tobi',
            '[long]$base,\n'
            '[int]$exponent'
        ]
        self._test_expected(ParameterList, parts)

    def test_function_parameter_declaration(self):
        params = [
            '([long]$base, [int]$exponent)',
            '([string]$str, [int]$start_pos = 0)',
            '([switch]$trace, \n'
            '$p1, $p2)',

            '($a, $b, $c, $d)',
            '(  [long]$base,\n'
            '[int]$exponent)'
        ]
        self._test_expected(FunctionParameterDeclaration, params)

    def test_function_statement(self):
        funcs = [
            dedent('''filter Get-Square2
                   {
                       $_ * $_
                   }'''),
            dedent('''FILTER Get-Square2
                   {
                       $_ * $_
                   }'''),
            dedent('''function Get-Power ([long]$base, [int]$exponent)
                   {
                       $result = 1
                       for ($i = 1; $i -le $exponent; ++$i)
                       {
                         $result *= $base
                       }
                       return $result
                   }'''),
            'function Find-Str ([string]$str, [int]$start_pos = 0) { $str }',
            'Function Find-Str ([string]$str, [int]$start_pos = 0) { $str }',
            dedent('''function Get-Square1
                   {
                    foreach ($i in $input)
                    {
                     $i * $i
                    }
                   }'''),
            dedent('''function Switch-Item {
                   param ([switch]$on)
                   if ($on) { "Switch on" }
                   else { "Switch off" }
                   }'''),
            dedent('''workflow\nSwitch-Item {
                   param ([switch]$on)
                   if ($on) { "Switch on" }
                   else { "Switch off" }
                   }'''),
            dedent('''workflow paralleltest {
                   parallel {
                      Get-Service -Name s*}}'''),
            dedent('''WORKflow paralleltest {
                   parallel {
                      Get-Service -Name s*}}'''),
        ]
        self._test_expected(FunctionStatement, funcs)

    def test_statement_list(self):
        stmts = [
            dedent('''switch ($s[$i])
                      {
                      "`n"  { ++$lineCount }
                      "`f"  { ++$pageCount }
                      "`t"  { }
                      " "  { }
                      default { ++$otherCount }
                   }
                   do {$i;}
                   while (++$i -le 5)'''),
            dedent('''function Get-Power ([long]$base, [int]$exponent)
                   {
                       $result = 1
                       for ($i = 1; $i -le $exponent; ++$i)
                       {
                         $result *= $base
                       }
                       return $result
                   }
                   if ($grade -ge 90) { "Grade A" }
                   elseif ($grade -ge 80) { "Grade B" }
                   elseif ($grade -ge 70) { "Grade C" }
                   elseif ($grade -ge 60) { "Grade D" }
                   else { "Grade F" }'''),
            '{ $value * 10 }',
        ]
        self._test_expected(StatementList, stmts)

    def test_statement_block(self):
        stmts = [
            dedent('''{
                       switch ($s[$i])
                       {
                        "`n"  { ++$lineCount }
                        "`f"  { ++$pageCount }
                        "`t"  { }
                        " "  { }
                        default { ++$otherCount }
                       }

                       do {$i;}
                       while (++$i -le 5)
                      }'''),
            dedent('''{function Get-Power ([long]$base, [int]$exponent)
                   {
                       $result = 1
                       for ($i = 1; $i -le $exponent; ++$i)
                       {
                         $result *= $base
                       }
                       return $result
                   }
                   if ($grade -ge 90) { "Grade A" }
                   elseif ($grade -ge 80) { "Grade B" }
                   elseif ($grade -ge 70) { "Grade C" }
                   elseif ($grade -ge 60) { "Grade D" }
                   else { "Grade F" }
                   }'''),
        ]
        self._test_expected(StatementBlock, stmts)

    def test_labeled_statement(self):
        stmts = [
            dedent(''':go_here switch ($s[$i])
                       {
                        "`n"  { ++$lineCount }
                        "`f"  { ++$pageCount }
                        "`t"  { }
                        " "  { }
                        default { ++$otherCount }
                       }'''),
            ':TobiIsObito foreach ($e in $a) {}',
            ':KaguyaIsJuubi for ($i = 1; $i -le 10; ++$i) { "$i" }',
            ':MadaraIsThirdMizukage while ($j -le 100) { $j }',
            ':HagoromoIsSageOfSixPaths do\n{\n$i;\n}\nwhile (++$i -le 5)',
        ]
        self._test_expected(LabeledStatement, stmts)

    def test_inlinescript_statement(self):
        stmts = [
            'inlinescript {"Inline A0 = $a"}',
            'inlinescript{$a = $Using:a+1; $a}',
            'InlineScript{$a = $Using:a+1; $a}',

        ]
        self._test_expected(InlinescriptStatement, stmts)

    def test_parallel_statement(self):
        stmts = [
            'parallel {"Inline A0 = $a"}',
            'parallel{$a = $Using:a+1; $a}',
            'Parallel{$a = $Using:a+1; $a}',
            'PARALLEL{$a = $Using:a+1; $a}',
        ]
        self._test_expected(ParallelStatement, stmts)

    def test_sequence_statement(self):
        stmts = [
            'sequence {"Inline A0 = $a"}',
            'sequence {$a = $Using:a+1; $a}',
            'Sequence {$a = $Using:a+1; $a}',
        ]
        self._test_expected(SequenceStatement, stmts)

    def test_param_block(self):
        blocks = [
            dedent('''param ([Parameter(Mandatory = $true)]
                             [string[]] $ComputerName )'''),
            dedent('''PARAM ([Parameter(Mandatory = $true)]
                             [string[]] $ComputerName )'''),
            dedent('''param (
                       [Parameter(Mandatory=$true, ValueFromPipeline=$true)]
                       [string[]]$ComputerName )'''),
            dedent(
                '''param([Parameter(Position = 0, ParameterSetname = "SetA")]
                         [decimal]$dec,
                         [Parameter(Position=0, ParameterSetname = "SetB")]
                         [int]$in)'''),

        ]
        self._test_expected(ParamBlock, blocks)

    def test_subexpression(self):
        exprs = [
            '$($i = 10)',
            '$(($i = 10))',
            '$($i = 10; $j)',
            '$(($i = 10); $j)',
            '$(($i = 10); ++$j )',
            '$( ( $i = 10); (++$j))',
            '$(\n$i = 10; ++$j\n)',
        ]
        self._test_expected(SubExpression, exprs)

    def test_named_block(self):
        blocks = [
            dedent('''{} {{
                       switch ($s[$i])
                       {{
                        "`n"  {{ ++$lineCount }}
                        "`f"  {{ ++$pageCount }}
                        "`t"  {{ }}
                        " "  {{ }}
                        default {{ ++$otherCount }}
                       }}

                       do {{$i;}}
                       while (++$i -le 5)
                      }}'''.format(block))
            for block in ("dynamicparam", "begin", "process", "end")
        ]
        self._test_expected(NamedBlock, blocks)

    def test_named_block_list(self):
        blocks = [
            dedent('''{block} {{
                        switch ($s[$i])
                        {{
                         "`n"  {{ ++$lineCount }}
                         "`f"  {{ ++$pageCount }}
                         "`t"  {{ }}
                         " "  {{ }}
                         default {{ ++$otherCount }}
                        }}
                      }}

                       {block} {{
                         do {{$i;}}
                         while (++$i -le 5)
                      }}'''.format(block=block))
            for block in ("dynamicparam", "begin", "process", "end")
        ]
        self._test_expected(NamedBlockList, blocks)

    def test_pipeline(self):
        pipelines = [
            "$i > output1.txt",
            "++$i >> output1.txt",
            "type file1.txt 2> error1.txt",
            "type file2.txt 2>> error1.txt",
            "dir -Verbose 4> verbose1.txt",
            "dir -Verbose -Debug -WarningAction Continue *> output2.txt",
            "dir -Verbose 4>&2 2> error2.txt",
            "dir e:\PowerShell\Scripts\*statement*.ps1 | "
            "Foreach-Object {$_.Length}",

            # TODO: Unsupported case
            # 'dir e:\\PowerShell\Scripts\*.ps1 | '
            # 'Select-String -List "catch" | '
            # 'Format-Table path,linenumber -AutoSize',
        ]
        self._test_expected(Pipeline, pipelines)

    def test_pipeline_tail(self):
        pipelines = [
            "| Foreach-Object {$_.Length}",
            ' | Select-String -List "catch"',
            ' | Tobi-is-Obito -List "catch-bijuus" |\n'
            'KaguyaIsShinju "get-all-chakra"',
        ]
        self._test_expected(PipelineTail, pipelines)

    def test_data_command(self):
        commands = ["ConvertFrom-StringData", "ConvertTo-Xml"]
        self._test_expected(DataCommand, commands)

    def test_data_commands_list(self):
        commands = [
            "ConvertFrom-String, ConvertTo-Xml",
            "ConvertFrom-Xml ,\nConvertTo-Json",
        ]
        self._test_expected(DataCommandsList, commands)

    def test_data_commands_allowed(self):
        commands = [
            "-supportedcommand ConvertFromString",
            "-supportedcommand TobiIsObito",
            "-SupportedCommand TobiIsObito",
        ]
        self._test_expected(DataCommandsAllowed, commands)

    def test_data_statement(self):
        stmts = [
            # TODO: pathological case
            # "data -supportedcommand Format-XML { "
            # "Format-XML -strings string1, string2, string3}",
            "DATA {}",
            "data -supportedcommand Format-XML { "
            "Format-XML -strings string1}",

            '''data {
                "Tobi is Obito."
                "Kaguya is Shinju."
                "Sasuke is evil."}''',

            '''data {
                 ConvertFrom-StringData -stringdata @'
                    Text001 = Windows 7
                    Text002 = Windows Server 2008 R2
                 '@
              }''',
            '''data {
                 if ($null) {
                   "To get help for this cmdlet"
                 }
               }''',
        ]
        self._test_expected(DataStatement, stmts)

    def test_try_statement(self):
        statement = [
            'try { $value / 10 }\ncatch   { Break }',
            'Try { $value / 10 }\nCatch   { Break }',
            'TRY { $value / 10 }\ncatch   { Break }',
            'try { $value / 10 }\nfinally { $status=1 }',

            'try\n{\n$value / 10\n}\ncatch\n{\nBreak\n}',
            'try\n{\n$value / 10\n}\nfinally\n{\n$status=1\n}',

            # TODO: Find o method to fix the following examples:
            # 'try { $value * 10 }\n'
            # 'catch [ System.OutOfMemoryException ] { $err = "Naruto" }\n'
            # 'catch { $err = $_.Exception.Message }',

            # 'try { $value * 10 }\n'
            # 'catch [System.OutOfMemoryException] { $err = "Obito" }\n'
            # 'catch { $err = $_.Exception.Message }\n'
            # 'finally { $Time=Get-Date}'
        ]
        self._test_expected(TryStatement, statement, debug=False)

    def test_statement(self):
        statements = [
            dedent('''if ($grade -ge 90) { "Grade A" }'''),
            "':TobiIsObito foreach ($e in $a) {}'",
            dedent('''function Get-Square1
                   {
                    foreach ($i in $input)
                    {
                     $i * $i
                    }
                   }'''),
            "return 100;",
            "trap {$a = 1}",
            'try { $value / 10 }\ncatch   { Break }',
            "data -supportedcommand Format-XML { "
            "Format-XML -strings string1}",
            'inlinescript{$a = $Using:a+1; $a}',
            'parallel{$a = $Using:a+1; $a}',
            'sequence {"Inline A0 = $a"}',
            "Foreach-Object {$_.Length};"
        ]
        self._test_expected(Statement, statements)

    def test_additive_argument_expression(self):
        expressions = [
            "$naruto[0,0]++",
            "$(($i = 10); (++$j))",
            "$($i = 10; ++$j)",
            "@(($i = 10); (++$j))",
            "@($i = 10; ++$j)",
            "++$i"
        ]
        self._test_expected(AdditiveArgumentExpression, expressions)

    def test_command_module(self):
        expressions = [
            # Invocation expression
            "[math]::Sqrt(2.0)",
            '[char]::IsUpper("a")',
            "[math]::Sqrt(2D)",
            "[math]::Sqrt($true)",
            "$a.Invoke(2.0)",
            '[math]::("Sq"+"rt")',

            # Post increment and decrement
            "$i++",
            "$i--",
            "$a[$i++]",
        ]
        self._test_expected(CommandModule, expressions)

    def test_logical_argument_expression(self):
        expressions = [
            # FIXME: Add relevant scenarios.
            '-not $true', '-not 0', '-not 1.23', '!"xyz"',
            "-not -not $false",
            "-bnot ($Kakashi -lt 100)",
            "-not ($Obito -eq $Madara)",
            "-join ($Hashirama, $Tobirama, $Hiruzen, $Minato)",
        ]
        self._test_expected(LogicalArgumentExpression, expressions)

    def test_comparison_argument_expression(self):
        expressions = [
            # FIXME: Add relevant scenarios.
            '-split "Hashirama, Tobirama, Hiruzen, Minato"',
        ]
        self._test_expected(ComparisonArgumentExpression, expressions)

    def test_bitwise_argument_expression(self):
        expressions = [
            # FIXME: Add relevant scenarios.
            '-split "Hashirama, Tobirama, Hiruzen, Minato"',
        ]
        self._test_expected(BitwiseArgumentExpression, expressions)
