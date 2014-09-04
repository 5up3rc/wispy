# pylint: disable=too-many-lines
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
    OPTIONAL, WHITESPACE, ANY, EXCEPT,
    LIST_OF, REF,
)


# Line terminators
class NewLineCharacter(Grammar):
    grammar = OR("\r\n", "\n", "\r")


class NewLines(Grammar):
    grammar = REPEAT(NewLineCharacter)


class Hashes(Grammar):
    grammar = REPEAT("#")


class Colon(Grammar):
    grammar = WORD(":")


class NotGreaterThanOrHash(Grammar):
    grammar = ANY_EXCEPT("#>")


class InputCharacter(Grammar):
    grammar = ANY_EXCEPT("\r\n", max=1)


class InputCharacters(Grammar):
    grammar = REPEAT(InputCharacter)


class SingleLineComment(Grammar):

    """A :class:`SingleLineComment` begins with the character `#` and ends
    with a :class:`NewLineCharacter`.
    """
    grammar = ("#", OPTIONAL(WHITESPACE), OPTIONAL(InputCharacters))


class DelimitedCommentSection(Grammar):
    grammar = OR(">", (OPTIONAL(Hashes), NotGreaterThanOrHash))


class DelimitedCommentText(Grammar):
    grammar = REPEAT(DelimitedCommentSection)


class DelimitedComment(Grammar):

    """ A :class:`DelimitedComment` begins with the character pair <# and ends
    with the character pair #>. It can occur as part of a source line,
    as a whole source line, or it can span any number of source lines.
    """

    grammar = ("<#", OPTIONAL(DelimitedCommentText), Hashes, ">")


class Comment(Grammar):

    """A comment is treated as white space.

    The productions above imply that
        * Comments do not nest.
        * The character sequences <# and #> have no special meaning in a
        :class:`SingleLineComment`.
        * The character # has no special meaning in a delimited comment.
    """

    grammar = OR(SingleLineComment, DelimitedComment)


class Keyword(Grammar):
    grammar = OR("begin", "break", "catch", "class",
                 "continue", "data", "define", "do",
                 "dynamicparam", "elseif", "else", "end",
                 "exit", "filter", "finally", "foreach",
                 "for", "from", "function", "if",
                 "in", "param", "process", "return",
                 "switch", "throw", "trap", "try",
                 "until", "using", "var", "while")

# Literals


class NumericMultiplier(Grammar):
    grammar = OR("kb", "mb", "tb", "pb", "gb")


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
    grammar = (
        Dash, OR(
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
            "inotmatch", "ireplace", "split",
            "isnot", "isplit", "join",
            "le", "like", "lt",
            "match", "ne", "notcontains",
            "notlike", "notmatch", "replace",
            "is")
    )


class FormatOperator(Grammar):
    grammar = (Dash, "f")


class OperatorOrPunctuator(Grammar):
    grammar = OR(
        AssignmentOperator,
        ComparisonOperator,
        FormatOperator,
        "{", "}", "[", "]", "(", ")", "@(", "@{", "$(", ";",
        "&&", "||", "&", "|", ",", "++", "..", "::", ".",
        "!", "*", "/", "%", "+", "2>&1", "1>&2",
        FileRedirectionOperator,
        (Dash, OR("and", "band", "bnot", "bor",
                  "bxor", "not", "or", "xor", Dash)),
        Dash,
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

    """An escaped character is a way to assign a special interpretation
    to a character by giving it a prefix Backtick character."""

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


class Literal(Grammar):
    grammar = OR(IntegerLiteral, RealLiteral, StringLiteral)


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
    grammar = LIST_OF(TypeIdentifier, sep=".")


class ArrayTypeName(Grammar):
    grammar = (TypeName, "[")


class GenericTypeName(Grammar):
    grammar = ArrayTypeName


class Dimension(Grammar):
    grammar = REPEAT(",")


class TypeSpec(Grammar):
    grammar = OR(
        (GenericTypeName, REF("GenericTypeArguments"), "]"),
        (ArrayTypeName, OPTIONAL(Dimension), "]"),
        TypeName
    )


class TypeLiteral(Grammar):
    grammar = ("[", TypeSpec, "]")


class GenericTypeArguments(Grammar):
    grammar = LIST_OF(TypeSpec, sep=",")


# Commands
class GenericTokenChar(Grammar):
    grammar = OR(
        EXCEPT(ANY,
               OR(
                   DoubleQuoteCharacter,
                   SingleQuoteCharacter,
                   WHITESPACE,
                   NewLineCharacter,
                   "{", "}", "(", ")", ";", ",", "|", "&", "$", "\u0060",
               )),
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


class ParameterCharacter(Grammar):
    grammar = EXCEPT(ANY, OR(Colon, WHITESPACE, NewLineCharacter,
                             "{", "}", "(", ")", ";", ",", "|",
                             "&", ".", "["))


class ParameterCharacters(Grammar):
    grammar = REPEAT(ParameterCharacter)


class FirstParameterCharacter(Grammar):
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

        WORD("\u005F"),                # The underscore character
        WORD("?")
    )


class CommandParameter(Grammar):
    grammar = (Dash, FirstParameterCharacter,
               ParameterCharacters, OPTIONAL(Colon))


class SimpleNameFirstCharacter(Grammar):
    grammar = TypeCharacter


class SimpleNameCharacter(Grammar):
    grammar = SimpleNameFirstCharacter


class SimpleNameCharacters(Grammar):
    grammar = REPEAT(SimpleNameCharacter)


class SimpleName(Grammar):
    grammar = (SimpleNameFirstCharacter, SimpleNameCharacters)


class Value(Grammar):
    grammar = OR(
        REF("ParenthesizedExpression"),
        REF("SubExpression"),
        REF("ArrayExpression"),
        REF("ScriptBlockExpression"),
        REF("HashLiteralExpression"),
        Literal,
        TypeLiteral,
        Variable
    )


class MemberName(Grammar):
    grammar = OR(
        SimpleName, StringLiteral, REF('StringLiteralWithSubexpression'),
        REF('ExpressionWithUnaryOperator'), Value
    )


class MemberAccessPrime(Grammar):
    # Use this idiom to get rid of left recursion.
    grammar = (OR(".", "::"), MemberName, OPTIONAL(REF('MemberAccessPrime')))


class ElementAccessPrime(Grammar):
    # Use this idiom to get rid of left recursion.
    grammar = ("[", OPTIONAL(NewLines), REF("Expression"),
               OPTIONAL(NewLines), "]",
               OPTIONAL(REF("ElementAccessPrime")))


class PostIncrementExpressionPrime(Grammar):
    # Use this idiom to get rid of left recursion.
    grammar = ("++", OPTIONAL(REF("PostIncrementExpressionPrime")))


class PostDecrementExpressionPrime(Grammar):
    # Use this idiom to get rid of left recursion.
    grammar = (Dash, Dash, OPTIONAL(REF("PostDecrementExpressionPrime")))


class PrimaryExpression(Grammar):
    grammar = OR(
        # This production rule might be wrong..
        (
            Value,
            OR(MemberAccessPrime,
               ElementAccessPrime,
               REF('InvocationExpressionPrime'),
               PostIncrementExpressionPrime,
               PostIncrementExpressionPrime),
            OPTIONAL(REF('PrimaryExpression')),
        ),
        Value,
    )


class ExpressionWithUnaryOperator(Grammar):
    grammar = OR(
        (OR(",", "-bnot", "-not", "-split", "-join", "!", "+", Dash),
         OPTIONAL(NewLines), REF("UnaryExpression")),
        REF("PreIncrementExpression"),
        REF("PreDecrementExpression"),
        REF("CastExpression"),
    )


class UnaryExpression(Grammar):
    grammar = OR(
        PrimaryExpression,
        ExpressionWithUnaryOperator,
    )


class PreIncrementExpression(Grammar):
    grammar = ("++", OPTIONAL(NewLines), UnaryExpression)


class PreDecrementExpression(Grammar):
    grammar = (Dash, Dash, OPTIONAL(NewLines), UnaryExpression)


class CastExpression(Grammar):
    grammar = (TypeLiteral, UnaryExpression)


class ArrayLiteralExpression(Grammar):
    grammar = LIST_OF(UnaryExpression,
                      sep=(",", OPTIONAL(NewLines)))


class RangeExpression(Grammar):
    grammar = LIST_OF(ArrayLiteralExpression,
                      sep=("..", OPTIONAL(NewLines)))


class FormatExpression(Grammar):
    grammar = LIST_OF(RangeExpression,
                      sep=(FormatOperator, OPTIONAL(NewLines)))


class MultiplicativeExpression(Grammar):
    grammar = LIST_OF(FormatExpression,
                      sep=(OR("*", "/", "%"), OPTIONAL(NewLines)))


class AdditiveExpression(Grammar):
    grammar = LIST_OF(MultiplicativeExpression,
                      sep=(OR("+", Dash), OPTIONAL(NewLines)))


class ComparisonExpression(Grammar):
    grammar = LIST_OF(AdditiveExpression,
                      sep=(ComparisonOperator, OPTIONAL(NewLines)))


class BitwiseExpression(Grammar):
    grammar = LIST_OF(ComparisonExpression,
                      sep=(OR("-band", "-bor", "-bxor"), OPTIONAL(NewLines)))


class LogicalExpression(Grammar):
    grammar = LIST_OF(BitwiseExpression,
                      sep=(OR("-and", "-or", "-xor"), OPTIONAL(NewLines)))


class Expression(Grammar):
    grammar = LogicalExpression


# Attributes
class AttributeArgument(Grammar):
    grammar = OR(
        (OPTIONAL(NewLines), Expression),
        (OPTIONAL(NewLines), SimpleName, "=", OPTIONAL(NewLines),
         Expression)
    )


class AttributeArguments(Grammar):
    grammar = LIST_OF(AttributeArgument,
                      sep=(OPTIONAL(NewLines), ","))


class AttributeName(Grammar):

    """ The :class:`AttributeName` is a reserved attribute type or some
    implementation-defined attribute type.
    """
    grammar = TypeSpec


class Attribute(Grammar):

    """ An attribute consists of an :class:`AttributeName` and an optional
    list of positional and named arguments.

    The positional arguments (if any) precede the named arguments.
    A positional argument consists of a :class:`SimpleName`, followed by an
    equal sign, followed by an :class:`Expression`.
    """

    grammar = OR(
        ("[", AttributeName, "(", AttributeArguments, OPTIONAL(NewLines),
         ")", OPTIONAL(NewLines), "]"),
        TypeLiteral
    )


class AttributeList(Grammar):
    grammar = LIST_OF(Attribute,
                      sep=OPTIONAL(NewLines))


# Statements
class CommandName(Grammar):
    grammar = OR(GenericToken, GenericTokenWithSubexprStart)


class CommandNameExpr(Grammar):
    grammar = OR(CommandName, PrimaryExpression)


class CommandArgument(Grammar):
    grammar = CommandNameExpr


class CommandElement(Grammar):
    grammar = OR(CommandParameter, CommandArgument, REF('Redirections'))


class CommandElements(Grammar):
    grammar = REPEAT(CommandElement)


class RedirectedFileName(Grammar):
    grammar = OR(CommandArgument, PrimaryExpression)


class Redirection(Grammar):
    grammar = OR(
        "2>&1", "1>&2",
        (FileRedirectionOperator, RedirectedFileName)
    )


class Redirections(Grammar):
    grammar = REPEAT(Redirection)


class CommandModule(Grammar):
    grammar = PrimaryExpression


class CommandInvocationOperator(Grammar):
    grammar = OR("&", ".")


class Command(Grammar):
    grammar = OR(
        (CommandName, OPTIONAL(CommandElements)),
        (CommandInvocationOperator, OPTIONAL(CommandModule), CommandNameExpr,
         OPTIONAL(CommandElements))
    )


class PipelineTail(Grammar):
    grammar = (
        "|", OPTIONAL(NewLines),
        Command, OPTIONAL(REF('PipelineTail'))
    )


class Pipeline(Grammar):

    """A pipeline is a series of one or more commands each separated by
    the pipe operator | (U+007C).
    Each command receives input from its predecessor and writes output
    to its successor."""

    grammar = OR(
        REF('AssignmentExpression'),
        (Expression, OPTIONAL(Redirections), OPTIONAL(PipelineTail)),
        (Command, OPTIONAL(PipelineTail))
    )


class StatementTerminator(Grammar):
    grammar = OR(";", NewLineCharacter)


class StatementTerminators(Grammar):
    grammar = REPEAT(StatementTerminator)


class StatementList(Grammar):
    grammar = REPEAT(REF('Statement'))


class StatementBlock(Grammar):

    """A statement-block allows a set of statements to be grouped
    into a single syntactic unit."""

    grammar = (
        OPTIONAL(NewLines), "{", OPTIONAL(StatementList),
        OPTIONAL(NewLines), "}"
    )


class BlockName(Grammar):
    grammar = OR("dynamicparam", "begin", "process", "end")


class NamedBlock(Grammar):
    grammar = (BlockName, StatementBlock, OPTIONAL(StatementTerminators))


class NamedBlockList(Grammar):
    grammar = REPEAT(NamedBlock)


class ForInitializer(Grammar):
    grammar = Pipeline


class ForCondition(Grammar):
    grammar = Pipeline


class ForIterator(Grammar):
    grammar = Pipeline


class WhileCondition(Grammar):
    grammar = (OPTIONAL(NewLines), Pipeline)


class ElseIfClause(Grammar):
    grammar = (
        OPTIONAL(NewLines), "elseif", OPTIONAL(NewLines), "(",
        OPTIONAL(NewLines), Pipeline, OPTIONAL(NewLines), ")",
        StatementBlock
    )


class ElseIfClauses(Grammar):
    grammar = REPEAT(ElseIfClause)


class ElseClause(Grammar):
    grammar = (OPTIONAL(NewLines), "else", StatementBlock)


class ScriptParameterDefault(Grammar):
    grammar = (
        OPTIONAL(NewLines), "=", OPTIONAL(NewLines), Expression
    )


class ScriptParameter(Grammar):
    grammar = (
        OPTIONAL(NewLines), OPTIONAL(AttributeList), OPTIONAL(NewLines),
        Variable, OPTIONAL(ScriptParameterDefault)
    )


class LabelExpression(Grammar):
    grammar = OR(SimpleName, UnaryExpression)


class FinallyClause(Grammar):
    grammar = (OPTIONAL(NewLines), "finally", StatementBlock)


class CatchTypeList(Grammar):
    grammar = OPTIONAL(
        (OPTIONAL(NewLines), TypeLiteral),
        (REF('CatchTypeList'), OPTIONAL(NewLines), ",", OPTIONAL(NewLines),
         TypeLiteral)
    )


class CatchClause(Grammar):
    grammar = (OPTIONAL(NewLines), "catch", OPTIONAL(CatchTypeList),
               StatementBlock)


class CatchClauses(Grammar):
    grammar = REPEAT(CatchClause)


class DataName(Grammar):
    grammar = SimpleName


class DataCommand(Grammar):
    grammar = CommandNameExpr


class DataCommandsList(Grammar):
    grammar = OR(
        (OPTIONAL(NewLines), DataCommand),
        (REF('DataCommandsList'), OPTIONAL(NewLines), DataCommand)
    )


class DataCommandsAllowed(Grammar):
    grammar = (OPTIONAL(NewLines), "-supportedcommand", DataCommandsList)


class IfStatement(Grammar):
    grammar = (
        "if", OPTIONAL(NewLines), "(", OPTIONAL(NewLines), Pipeline,
        OPTIONAL(NewLines), ")", StatementBlock, OPTIONAL(ElseIfClauses),
        OPTIONAL(ElseClause)
    )


class TrapStatement(Grammar):
    grammar = (
        "trap", OPTIONAL(NewLines), OPTIONAL(TypeLiteral),
        OPTIONAL(NewLines), StatementBlock
    )


class TryStatement(Grammar):

    """The try statement provides a mechanism for catching exceptions that
    occur during execution of a block. The try statement also provides
    the ability to specify a block of code that is always executed when
    control leaves the try statement.
    """

    grammar = OR(
        ("try", StatementBlock, CatchClauses),
        ("try", StatementBlock, FinallyClause),
        ("try", StatementBlock, CatchClauses, FinallyClause)
    )


class DataStatement(Grammar):
    grammar = (
        "data", OPTIONAL(NewLines), DataName, OPTIONAL(DataCommandsAllowed),
        StatementBlock
    )


class FlowControlStatement(Grammar):
    grammar = OR(
        ("break", OPTIONAL(LabelExpression)),
        ("continue", OPTIONAL(LabelExpression)),
        ("throw", OPTIONAL(Pipeline)),
        ("return", OPTIONAL(Pipeline)),
        ("exit", OPTIONAL(Pipeline))
    )


class ParameterList(Grammar):
    grammar = OR(
        ScriptParameter,
        (REF('ParameterList'), OPTIONAL(NewLines), ScriptParameter)
    )


class FunctionParameterDeclaration(Grammar):
    grammar = (OPTIONAL(NewLines), "(", ParameterList, OPTIONAL(NewLines), ")")


class ParamBlock(Grammar):
    grammar = (
        OPTIONAL(NewLines), OPTIONAL(AttributeList), OPTIONAL(NewLines),
        "param", OPTIONAL(NewLines), "(", OPTIONAL(ParameterList),
        OPTIONAL(NewLines), ")"
    )


class FunctionName(Grammar):
    grammar = CommandArgument


class FunctionStatement(Grammar):
    # FIXME: Remove REF after the ScriptBlock grammar is added.
    grammar = (
        OR("function", "filter"), OPTIONAL(NewLines), FunctionName,
        OPTIONAL(FunctionParameterDeclaration), "{", REF('ScriptBlock'), "}"
    )


class ScriptBlockBody(Grammar):
    grammar = OR(NamedBlockList, StatementList)


class SwitchParameter(Grammar):

    """A :class:`SwitchParameter` may be abbreviated; any distinct leading
    part of a parameter may be used.

    For example, -regex, -rege, -reg, -re, and -r are equivalent.
    """

    grammar = OR(
        "-regex", "-rege", "-reg", "-re", "-r", "-wildcard", "-wildcar",
        "-wildca", "-wildc", "-wild", "-wil", "-wi", "-w", "-exact",
        "-exac", "-exa", "-ex", "-e", "-casesensitive", "-casesensitiv",
        "-casesensiti", "-casesensit", "-casesensi", "-casesens",
        "-casesen", "-casese", "-cases", "-case", "-cas", "-ca", "-c"
    )


class SwitchParameters(Grammar):
    grammar = LIST_OF(SwitchParameter, sep=WHITESPACE)


class SwitchFilename(Grammar):
    grammar = OR(CommandArgument, PrimaryExpression)


class SwitchCondition(Grammar):

    """A switch must contain one or more :class:`SwitchClauses`, each starting
    with a pattern (a non-default switch clause), or the keyword default
    (a default switch clause).

    A switch must contain zero or one default switch clauses, and zero or
    more non-default switch clauses.

    Switch clauses may be written in any order.
    """

    grammar = OR(
        ("(", OPTIONAL(NewLines), Pipeline, OPTIONAL(NewLines), ")"),
        ("-file", OPTIONAL(NewLines), SwitchFilename)
    )


class SwitchClauseCondition(Grammar):
    grammar = (CommandArgument, PrimaryExpression)


class SwitchClause(Grammar):
    grammar = (SwitchClauseCondition, StatementBlock,
               OPTIONAL(StatementTerminators))


class SwitchClauses(Grammar):
    grammar = REPEAT(SwitchClause)


class SwitchBody(Grammar):
    grammar = (
        OPTIONAL(NewLines), "{", OPTIONAL(NewLines), SwitchClauses, "}"
    )


class SwitchStatement(Grammar):
    grammar = (
        "switch", OPTIONAL(NewLines), OPTIONAL(SwitchParameters),
        SwitchCondition, SwitchBody
    )


class ForeachStatement(Grammar):
    grammar = (
        "foreach", OPTIONAL(NewLines), "(", OPTIONAL(NewLines), Variable,
        OPTIONAL(NewLines), "in", OPTIONAL(NewLines), Pipeline,
        OPTIONAL(NewLines), ")", StatementBlock
    )


class ForStatement(Grammar):
    grammar = OR(
        (
            "for", OPTIONAL(NewLines), "(", OPTIONAL(ForInitializer),
            StatementTerminator, OPTIONAL(NewLines), OPTIONAL(ForCondition),
            StatementTerminator, OPTIONAL(NewLines), OPTIONAL(ForIterator),
            OPTIONAL(NewLines), ")", StatementBlock
        ),
        (
            "for", OPTIONAL(NewLines), "(", OPTIONAL(ForInitializer),
            StatementTerminator, OPTIONAL(NewLines), OPTIONAL(ForCondition),
            OPTIONAL(NewLines), ")", StatementBlock
        ),
        (
            "for", OPTIONAL(NewLines), "(", OPTIONAL(ForInitializer),
            OPTIONAL(NewLines), ")", StatementBlock
        ),
    )


class WhileStatement(Grammar):
    grammar = (
        "while", OPTIONAL(NewLines), "(", OPTIONAL(NewLines), WhileCondition,
        OPTIONAL(NewLines), ")", StatementBlock
    )


class DoStatement(Grammar):
    grammar = (
        "do", StatementBlock, OPTIONAL(NewLines), OR("while", "until"),
        OPTIONAL(NewLines), "(", WhileCondition, OPTIONAL(NewLines), ")"
    )


class LabeledStatement(Grammar):
    grammar = OR(
        SwitchStatement,
        ForeachStatement,
        ForStatement,
        WhileStatement,
        DoStatement
    )


class InlinescriptStatement(Grammar):
    grammar = ("inlinescript", StatementBlock)


class ParallelStatement(Grammar):
    grammar = ("parallel", StatementBlock)


class SequenceStatement(Grammar):
    grammar = ("sequence", StatementBlock)


class Statement(Grammar):

    """A statement specifies some sort of action that is to be performed.

    Unless indicated otherwise within this clause,statements are executed
    in lexical order."""

    grammar = OR(
        IfStatement,
        (OPTIONAL("label"), LabeledStatement),
        FunctionStatement,
        (FlowControlStatement, StatementTerminator),
        TrapStatement,
        TryStatement,
        DataStatement,
        InlinescriptStatement,
        ParallelStatement,
        SequenceStatement,
        (Pipeline, OPTIONAL(StatementTerminator))
    )


class ParenthesizedExpression(Grammar):
    grammar = (
        "(", OPTIONAL(NewLines), Pipeline, OPTIONAL(NewLines), ")"
    )


class SubExpression(Grammar):
    grammar = ("$(", OPTIONAL(NewLines),
               OPTIONAL(StatementList), OPTIONAL(NewLines), ")")


class ArrayExpression(Grammar):
    grammar_whitespace_mode = "optional"
    grammar = ("@(", OPTIONAL(NewLines), OPTIONAL(StatementList),
               OPTIONAL(NewLines), ")")


class ScriptBlockExpression(Grammar):
    grammar = ("{", OPTIONAL(NewLines), REF("ScriptBlock"),
               OPTIONAL(NewLines), "}")


class KeyExpression(Grammar):
    grammar = OR(
        SimpleName,
        UnaryExpression
    )


class HashEntry(Grammar):
    grammar = (KeyExpression, "=", OPTIONAL(NewLines), Statement)


class HashLiteralBodyPrime(Grammar):
    grammar = (StatementTerminators, HashEntry,
               OPTIONAL(REF("HashLiteralBodyPrime")))


class HashLiteralBody(Grammar):
    grammar = (HashEntry, OPTIONAL(HashLiteralBodyPrime))


class HashLiteralExpression(Grammar):
    grammar = ("@{", OPTIONAL(NewLines), OPTIONAL(HashLiteralBody),
               OPTIONAL(NewLines), "}")


class RangeArgumentExpression(Grammar):
    grammar = OR(
        UnaryExpression,
        (RangeExpression, "..", OPTIONAL(NewLines), UnaryExpression)
    )


class FormatArgumentExpression(Grammar):
    grammar = OR(
        RangeArgumentExpression,
        (
            REF('FormatArgumentExpression'), FormatOperator,
            OPTIONAL(NewLines), RangeArgumentExpression
        )
    )


class MultiplicativeArgumentExpression(Grammar):
    grammar = OR(
        FormatArgumentExpression,
        (
            REF('MultiplicativeArgumentExpression'), OR("*", "/", "%"),
            OPTIONAL(NewLines), FormatArgumentExpression
        ),

    )


class AdditiveArgumentExpression(Grammar):
    grammar = OR(
        MultiplicativeArgumentExpression,
        (
            REF('AdditiveArgumentExpression'), OR("+", Dash),
            OPTIONAL(NewLines), MultiplicativeArgumentExpression
        )
    )


class ComparisonArgumentExpression(Grammar):
    grammar = OR(
        AdditiveArgumentExpression,
        (
            REF('ComparisonArgumentExpression'), ComparisonOperator,
            OPTIONAL(NewLines), AdditiveArgumentExpression
        )
    )


class BitwiseArgumentExpression(Grammar):
    grammar = OR(
        ComparisonArgumentExpression,
        (
            REF('BitwiseArgumentExpression'), OR("-band", "-bor", "-bxor"),
            OPTIONAL(NewLines), ComparisonArgumentExpression
        )
    )


class LogicalArgumentExpression(Grammar):
    grammar = OR(
        BitwiseArgumentExpression,
        (
            REF('LogicalArgumentExpression'), OR("-and", "-or", "-xor"),
            OPTIONAL(NewLines), BitwiseArgumentExpression
        ),
    )


class ArgumentExpression(Grammar):
    grammar = (OPTIONAL(NewLines), LogicalArgumentExpression)


class ArgumentExpressionList(Grammar):
    grammar = OR(
        ArgumentExpression,
        (ArgumentExpression, OPTIONAL(NewLines),
         ",", REF("ArgumentExpressionList"))
    )


class ArgumentList(Grammar):
    grammar = ("(", OPTIONAL(ArgumentExpressionList),
               OPTIONAL(NewLines), ")")


class AssignmentExpression(Grammar):
    grammar = (Expression, AssignmentOperator, Statement)


class ExpandableHereStringWithSubexprPart(Grammar):
    grammar = OR(SubExpression, ExpandableHereStringPart)


class ExpandableHereStringWithSubexprCharacters(Grammar):
    grammar = REPEAT(ExpandableHereStringWithSubexprPart)


class ExpandableStringWithSubexprPart(Grammar):
    grammar = OR(SubExpression, ExpandableStringPart)


class ExpandableStringWithSubexprCharacters(Grammar):
    grammar = REPEAT(ExpandableStringWithSubexprPart)


class ExpandableStringLiteralWithSubexpr(Grammar):
    grammar = OR(
        (
            ExpandableStringWithSubexprStart, OPTIONAL(StatementList),
            ")", ExpandableStringWithSubexprCharacters,
            ExpandableStringWithSubexprEnd
        ),
        (
            ExpandableHereStringWithSubexprStart, OPTIONAL(StatementList),
            ExpandableHereStringWithSubexprCharacters,
            ExpandableHereStringWithSubexprEnd
        )
    )


class StringLiteralWithSubexpression(Grammar):
    # FIXME: Remove REF after the ExpandableHereStringLiteralWithSubexpr
    # grammar is added.
    grammar = OR(
        ExpandableStringLiteralWithSubexpr,
        REF('ExpandableHereStringLiteralWithSubexpr')
    )


class InvocationExpressionPrime(Grammar):
    grammar = (OR(".", "::"), MemberName,
               ArgumentList, OPTIONAL(REF("InvocationExpressionPrime")))
