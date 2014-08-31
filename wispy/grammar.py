"""
    wispy.grammar
    ~~~~~~~~~~~~~

    The Powershell grammar implementation of the *wispy* engine.
    The version used is PowerShell Language Specification Version 3.0.

"""
# pylint: disable=missing-docstring, no-init, too-few-public-methods
# pylint: disable=anomalous-unicode-escape-in-string

from modgrammar import (
    Grammar, OR, WORD, REPEAT, ANY_EXCEPT,
    OPTIONAL, WHITESPACE, ANY, REF, EXCEPT
)


class NewLineCharacter(Grammar):
    grammar = OR("\r\n", "\n", "\r")


class Hashes(Grammar):
    grammar = REPEAT("#")


class NotGreaterThanOrHash(Grammar):
    grammar = ANY_EXCEPT("#>")


class InputCharacter(Grammar):
    grammar = ANY_EXCEPT("\r\n", max=1)


class InputCharacters(Grammar):
    grammar = REPEAT(InputCharacter)


class SingleLineComment(Grammar):
    grammar = ("#", OPTIONAL(WHITESPACE), OPTIONAL(InputCharacters))


class DelimitedCommentSection(Grammar):
    grammar = OR(">", (OPTIONAL(Hashes), NotGreaterThanOrHash))


class DelimitedCommentText(Grammar):
    grammar = OR(DelimitedCommentSection,
                 (REF('DelimitedCommentText', DelimitedCommentSection)))


class DelimitedComment(Grammar):
    grammar = ("<#", OPTIONAL(DelimitedCommentText), Hashes, ">")


class Comment(Grammar):
    grammar = OR(SingleLineComment, DelimitedComment)


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
    grammar = OR("l", "L")


class DecimalTypeSuffix(Grammar):
    grammar = OR("d", "D")


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
    grammar = OR(REPEAT(HexadecimalDigit),
                 (HexadecimalDigit, DecimalDigits))


class HexadecimalIntegerLiteral(Grammar):
    grammar = ("0x", HexadecimalDigits,
               OPTIONAL(LongTypeSuffix),
               OPTIONAL(NumericMultiplier))


class IntegerLiteral(Grammar):
    grammar = OR(HexadecimalIntegerLiteral, DecimalIntegerLiteral)


class Dash(Grammar):
    grammar = OR("-", "\u2013", "\u2014", "\u2015")


class Sign(Grammar):
    grammar = OR("+", Dash, max=1)


class ExponentPart(Grammar):
    grammar = (OR("e", "E"), OPTIONAL(Sign), DecimalDigits)


class RealLiteral(Grammar):
    grammar = OR(
        (DecimalDigits, ".", DecimalDigits, OPTIONAL(ExponentPart),
         OPTIONAL(NumericMultiplier)),

        (".", DecimalDigits, OPTIONAL(ExponentPart),
         OPTIONAL(DecimalTypeSuffix), OPTIONAL(NumericMultiplier)),

        (DecimalDigits, ExponentPart, OPTIONAL(DecimalTypeSuffix),
         OPTIONAL(NumericMultiplier)))


class AssignmentOperator(Grammar):
    grammar = OR(
        "=", (Dash, "="), "+=", "*=", "/=", "%="
    )


class FileRedirectionOperator(Grammar):
    grammar = OR(
        ">>", ">", "<", "2>>", "2>"
    )


class ComparisonOperator(Grammar):
    grammar = OR(
        (Dash, "as"), (Dash, "ccontains"), (Dash, "ceq"),
        (Dash, "cge"), (Dash, "cgt"), (Dash, "cle"),
        (Dash, "clike"), (Dash, "clt"), (Dash, "cmatch"),
        (Dash, "cne"), (Dash, "cnotcontains"), (Dash, "cnotlike"),
        (Dash, "cnotmatch"), (Dash, "contains"), (Dash, "creplace"),
        (Dash, "csplit"), (Dash, "eq"), (Dash, "ge"),
        (Dash, "gt"), (Dash, "icontains"), (Dash, "ieq"),
        (Dash, "ige"), (Dash, "igt"), (Dash, "ile"),
        (Dash, "ilike"), (Dash, "ilt"), (Dash, "imatch"),
        (Dash, "ine"), (Dash, "inotcontains"), (Dash, "inotlike"),
        (Dash, "inotmatch"), (Dash, "ireplace"), (Dash, "is"),
        (Dash, "isnot"), (Dash, "isplit"), (Dash, "join"),
        (Dash, "le"), (Dash, "like"), (Dash, "lt"),
        (Dash, "match"), (Dash, "ne"), (Dash, "notcontains"),
        (Dash, "notlike"), (Dash, "notmatch"), (Dash, "replace"),
        (Dash, "split")
    )


class FormatOperator(Grammar):
    grammar = (Dash, "f")


class OperatorOrPunctuator(Grammar):
    grammar = OR(
        "{", "}", "[", "]", "(", ")", "@(", "@{", "$(", ";",
        "&&", "||", "&", "|", ",", "++", "..", "::", ".",
        "!", "*", "/", "%", "+", "2>&1", "1>&2",
        Dash, (Dash, Dash),
        (Dash, "and"), (Dash, "band"), (Dash, "bnot"),
        (Dash, "bor"), (Dash, "bxor"), (Dash, "not"),
        (Dash, "or"), (Dash, "xor"),
        AssignmentOperator,
        FileRedirectionOperator,
        ComparisonOperator,
        FormatOperator
    )


# Variables
class VariableCharacter(Grammar):
    grammar = OR(
        WORD("A-Z", max=1),  # Letter, Uppercase
        WORD("a-z", max=1),  # Letter, Lowercase

        # Letter, Titlecase
        WORD("\u01C5"), WORD("\u01C8"), WORD("\u01CB"), WORD("\u01F2"),
        WORD("\u1F88"), WORD("\u1F89"), WORD("\u1F8A"), WORD("\u1F8B"),
        WORD("\u1F8C"), WORD("\u1F8D"), WORD("\u1F8E"), WORD("\u1F8F"),
        WORD("\u1F98"), WORD("\u1F99"), WORD("\u1F9A"), WORD("\u1F9B"),
        WORD("\u1F9C"), WORD("\u1F9D"), WORD("\u1F9E"), WORD("\u1F9F"),
        WORD("\u1FA8"), WORD("\u1FA9"), WORD("\u1FAA"), WORD("\u1FAB"),
        WORD("\u1FAC"), WORD("\u1FAD"), WORD("\u1FAE"), WORD("\u1FAF"),
        WORD("\u1FBC"), WORD("\u1FCC"), WORD("\u1FFC"),

        # Letter, Modifier
        WORD("\u02B0-\u02EE", max=1),
        WORD("\u0374"), WORD("\u037A"), WORD("\u0559"), WORD("\u0640"),
        WORD("\u06E5"), WORD("\u06E6"), WORD("\u07F4"), WORD("\u07F5"),
        WORD("\u07FA"), WORD("\u081A"), WORD("\u0824"), WORD("\u0828"),
        # TODO: Add more characters from the 'Letter, Modifier` Category
        # TODO: Add characters from the 'Letter, Other' Category

        # Number, Decimal Digit
        WORD("0-9", max=1),
        WORD("\u0660-\u0669", max=1),  # ARABIC-INDIC DIGIT
        WORD("\u06F0-\u06F9", max=1),  # EXTENDED ARABIC-INDIC DIGIT
        # TODO: Add more character from the 'Number, Decimal Digit' Category

        WORD("\u005F"),                # The underscore character
        WORD("?")
    )


class VariableCharacters(Grammar):
    grammar = REPEAT(VariableCharacter)


class VariableNamespace(Grammar):
    grammar = (VariableCharacters, ":")


class VariableScope(Grammar):
    grammar = OR("global:", "local:", "private:", "script:",
                 VariableNamespace)


class EscapedCharacter(Grammar):
    grammar = ("\u0060", ANY)


class BracedVariableCharacter(Grammar):
    grammar = OR(ANY_EXCEPT("\u007D\u0060", max=1), EscapedCharacter)


class BracedVariableCharacters(Grammar):
    grammar = REPEAT(BracedVariableCharacter)


class BracedVariable(Grammar):
    grammar = ("$", "{", OPTIONAL(VariableScope),
               BracedVariableCharacters, "}")


class Variable(Grammar):
    grammar = OR(
        "$$", "$?", "$^",
        ("$", OPTIONAL(VariableScope), VariableCharacters),
        ("@", OPTIONAL(VariableScope), VariableCharacters),
        BracedVariable
    )

# String Literals


class DoubleQuoteCharacter(Grammar):
    grammar = OR("\u0022", "\u201C", "\u201D", "\u201E")


class ExpandableStringPart(Grammar):
    grammar = OR(
        ANY_EXCEPT("$\u0022\u201C\u201D\u201E\u0060"),
        BracedVariable,
        ("$", ANY_EXCEPT("({\u0022\u201C\u201D\u201E\u0060")),
        ("$", EscapedCharacter),
        EscapedCharacter,
        (DoubleQuoteCharacter, DoubleQuoteCharacter)
    )


class Dollars(Grammar):
    grammar = REPEAT("$")


class ExpandableStringCharacters(Grammar):
    grammar = REPEAT(ExpandableStringPart)


class ExpandableHereStringPart(Grammar):
    grammar = OR(
        EXCEPT(ANY_EXCEPT("$", max=1), NewLineCharacter),
        BracedVariable,
        ("$", EXCEPT(ANY_EXCEPT("(", max=1), NewLineCharacter)),
        ("$", NewLineCharacter, EXCEPT(ANY, DoubleQuoteCharacter)),
        ("$", NewLineCharacter, DoubleQuoteCharacter, ANY_EXCEPT("@")),
        (NewLineCharacter, EXCEPT(ANY, DoubleQuoteCharacter)),
        (NewLineCharacter, DoubleQuoteCharacter, ANY_EXCEPT("@"))
    )


class ExpandableStringWithSubexprStart(Grammar):
    grammar = (DoubleQuoteCharacter, OPTIONAL(ExpandableStringCharacters),
               "$", "(")


class ExpandableStringWithSubexprEnd(Grammar):
    grammar = DoubleQuoteCharacter


class ExpandableHereStringCharacters(Grammar):
    grammar = REPEAT(ExpandableHereStringPart)


class ExpandableHereStringWithSubexprStart(Grammar):
    grammar = (
        "@", DoubleQuoteCharacter, OPTIONAL(WHITESPACE),
        NewLineCharacter, OPTIONAL(ExpandableHereStringCharacters),
        "$", "("
    )


class ExpandableHereStringWithSubexprEnd(Grammar):
    grammar = (NewLineCharacter, DoubleQuoteCharacter, "@")


class SingleQuoteCharacter(Grammar):
    grammar = OR("\u0027", "\u2018", "\u2019", "\u201A", "\u201B")


class VerbatimStringPart(Grammar):
    grammar = OR(
        EXCEPT(ANY, SingleQuoteCharacter),
        (SingleQuoteCharacter, SingleQuoteCharacter)
    )


class VerbatimStringCharacters(Grammar):
    grammar = REPEAT(VerbatimStringPart)


class VerbatimHereStringPart(Grammar):
    grammar = OR(
        EXCEPT(ANY, NewLineCharacter),
        (NewLineCharacter, EXCEPT(ANY, SingleQuoteCharacter)),
        (NewLineCharacter, SingleQuoteCharacter, ANY_EXCEPT("@"))
    )


class VerbatimHereStringCharacters(Grammar):
    grammar = REPEAT(VerbatimHereStringPart)


class ExpandableStringLiteral(Grammar):

    """Expandable string literal (single-line double-quoted), which is
    a sequence of zero or more characters delimited by a pair of
    double-quote-characters.

    Examples are "" and "red".
    """
    grammar = (DoubleQuoteCharacter, OPTIONAL(ExpandableStringCharacters),
               OPTIONAL(Dollars), DoubleQuoteCharacter)


class ExpandableHereStringLiteral(Grammar):

    """Expandable here string literal (multi-line double-quoted), which is
    a sequence of zero or more characters delimited by the character pairs
    @double-quote-character and double-quote-character@, respectively,
    all contained on two or more source lines.

    Examples are:
    ::
    @"
    "@

    @"
    line 1
    "@

    @"
    line 1
    line 2
    "@
    """
    grammar = ("@", DoubleQuoteCharacter, OPTIONAL(WHITESPACE),
               NewLineCharacter, OPTIONAL(ExpandableHereStringCharacters),
               NewLineCharacter, DoubleQuoteCharacter, "@")


class VerbatimStringLiteral(Grammar):

    """Verbatim string literal (single-line single-quoted), which is a
    sequence of zero or more characters delimited by a pair
    of SingleQuoteCharacters.

    Examples are '' and 'red'.
    """

    grammar = (SingleQuoteCharacter, OPTIONAL(VerbatimStringCharacters),
               SingleQuoteCharacter)


class VerbatimHereStringLiteral(Grammar):

    """Verbatim here string literal (multi-line single-quoted), which is
    a sequence of zero or more characters delimited by the character pairs
    @single-quote-character and single-quote-character@, respectively, all
    contained on two or more source lines.

    Examples are:

    ::
    @'
    '@

    @'
    line 1
    line 2
    '@
    """
    grammar = ("@", SingleQuoteCharacter, OPTIONAL(WHITESPACE),
               NewLineCharacter, OPTIONAL(VerbatimHereStringCharacters),
               NewLineCharacter, SingleQuoteCharacter, "@")


class StringLiteral(Grammar):

    """String literal is one of the following:
        * ExpandableStringLiteral
        * ExpandableHereStringLiteral
        * VerbatimStringLiteral
        * VerbatimHereStringLiteral
    """
    grammar = OR(ExpandableStringLiteral,
                 ExpandableHereStringLiteral,
                 VerbatimStringLiteral,
                 VerbatimHereStringLiteral)


# Type names.

class TypeCharacter(Grammar):
    grammar = OR(
        WORD("A-Z", max=1),  # Letter, Uppercase
        WORD("a-z", max=1),  # Letter, Lowercase,

        # Letter, Titlecase
        WORD("\u01C5"), WORD("\u01C8"), WORD("\u01CB"), WORD("\u01F2"),
        WORD("\u1F88"), WORD("\u1F89"), WORD("\u1F8A"), WORD("\u1F8B"),
        WORD("\u1F8C"), WORD("\u1F8D"), WORD("\u1F8E"), WORD("\u1F8F"),
        WORD("\u1F98"), WORD("\u1F99"), WORD("\u1F9A"), WORD("\u1F9B"),
        WORD("\u1F9C"), WORD("\u1F9D"), WORD("\u1F9E"), WORD("\u1F9F"),
        WORD("\u1FA8"), WORD("\u1FA9"), WORD("\u1FAA"), WORD("\u1FAB"),
        WORD("\u1FAC"), WORD("\u1FAD"), WORD("\u1FAE"), WORD("\u1FAF"),
        WORD("\u1FBC"), WORD("\u1FCC"), WORD("\u1FFC"),

        # Letter, Modifier
        WORD("\u02B0-\u02EE", max=1),
        WORD("\u0374"), WORD("\u037A"), WORD("\u0559"), WORD("\u0640"),
        WORD("\u06E5"), WORD("\u06E6"), WORD("\u07F4"), WORD("\u07F5"),
        WORD("\u07FA"), WORD("\u081A"), WORD("\u0824"), WORD("\u0828"),
        # TODO: Add more characters from the 'Letter, Modifier` Category
        # TODO: Add characters from the 'Letter, Other' Category

        WORD("\u005F"),
    )


class TypeCharacters(Grammar):
    grammar = REPEAT(TypeCharacter)


class TypeIdentifier(Grammar):
    grammar = TypeCharacters


class TypeName(Grammar):
    grammar = OR(TypeIdentifier, (REF('TypeName'), '.', TypeIdentifier))


class ArrayTypeName(Grammar):
    grammar = (TypeName, "[")


class GenericTypeName(Grammar):
    grammar = ArrayTypeName


# Commands
class GenericTokenChar(Grammar):
    grammar = OR(
        EXCEPT(
            ANY_EXCEPT("{}();,|&$\u0060"),
            ANY(
                DoubleQuoteCharacter,
                SingleQuoteCharacter,
                WHITESPACE,
                NewLineCharacter
            )
        ),
        EscapedCharacter
    )


class GenericTokenPart(Grammar):
    grammar = OR(
        ExpandableStringLiteral,
        VerbatimHereStringLiteral,
        Variable,
        GenericTokenChar
    )


class GenericTokenParts(Grammar):
    grammar = REPEAT(GenericTokenPart)


class GenericTokenWithSubexprStart(Grammar):
    grammar = (GenericTokenParts, "$", "(")


class GenericToken(Grammar):
    grammar = GenericTokenParts


class SimpleNameFirstCharacter(Grammar):
    grammar = TypeCharacter


class SimpleNameCharacter(Grammar):
    grammar = SimpleNameFirstCharacter


class SimpleNameCharacters(Grammar):
    grammar = REPEAT(SimpleNameCharacter)


class SimpleName(Grammar):
    grammar = (SimpleNameFirstCharacter, SimpleNameCharacters)
