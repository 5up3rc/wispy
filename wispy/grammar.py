# pylint: disable=too-many-lines
"""
    wispy.grammar
    ~~~~~~~~~~~~~

    The Powershell grammar implementation of the *wispy* engine.
    The version used is PowerShell Language Specification Version 3.0.

"""
# pylint: disable=missing-docstring, no-init, too-few-public-methods
# pylint: disable=anomalous-unicode-escape-in-string, bad-builtin, star-args

import re

from modgrammar import (
    Grammar, OR, WORD, REPEAT, ANY_EXCEPT,
    OPTIONAL, ANY, EXCEPT,
    LIST_OF, REF, WHITESPACE,
)
from modgrammar.extras import RE


# pylint: disable=invalid-name
def RE_LITERAL(regex, *args, regex_flags=re.I | re.MULTILINE, **kwargs):
    """ A Literal grammar which uses a regular expression instead
    of a simple string check.

    This has the benefit that certain flags can be applied to the
    regex, before building the grammar, using *regex_flags* keyword argument.
    By default, using *RE_LITERAL* ignores the case of the match.
    """

    regex = re.compile(regex, regex_flags)
    return RE(regex, *args, **kwargs)


def ignore_case_literals(*args):
    """ Receive a list of strings and return a list of grammars
    for each of those strings.

    In this case, the grammars ignores the case of the match.
    """
    return list(map(RE_LITERAL, args))


# Grammars without dependencies
class EscapedCharacter(Grammar):

    """An escaped character is a way to assign a special interpretation
    to a character by giving it a prefix Backtick character."""

    grammar = ("\u0060", ANY)


class Colon(Grammar):
    grammar = "\u003A"


class Dimension(Grammar):
    grammar_whitespace_mode = "optional"
    grammar = REPEAT((",", OPTIONAL(WHITESPACE)))


class NonAmpersandCharacter(Grammar):
    grammar = ANY_EXCEPT("&", max=1)


class DoubleQuoteCharacter(Grammar):
    grammar = OR("\u0022", "\u201C", "\u201D", "\u201E")


class NonDoubleQuoteCharacter(Grammar):
    grammar = EXCEPT(ANY, DoubleQuoteCharacter, max=1)


class NonDoubleQuoteCharacters(Grammar):
    grammar = REPEAT(NonDoubleQuoteCharacter)


class Dollars(Grammar):
    grammar = REPEAT("$")


class NewLineCharacter(Grammar):
    grammar = OR("\u000D\u000A", "\u000D", "\u000A")


class StatementTerminator(Grammar):
    grammar = OR(";", NewLineCharacter)


class StatementTerminators(Grammar):
    grammar = REPEAT(StatementTerminator)
# End of Grammars without dependencies


# Grammar for line terminators
class NewLines(Grammar):
    grammar = REPEAT(NewLineCharacter)


class Spaces(Grammar):
    grammar = (OPTIONAL(WHITESPACE),
               OPTIONAL(NewLines),
               OPTIONAL(WHITESPACE))
# End of grammar for line terminators


# Grammar for input character
class InputCharacter(Grammar):
    grammar = EXCEPT(ANY, NewLineCharacter, max=1)


class InputCharacters(Grammar):
    grammar = REPEAT(InputCharacter)
# End of grammar for input character


class Dash(Grammar):
    grammar = OR("\u002D", "\u2013", "\u2014", "\u2015")


class DashDash(Grammar):
    grammar = (Dash, Dash)


# Grammar for Operators and punctuators
class FormatOperator(Grammar):
    grammar = (Dash, "f")


class ComparisonOperator(Grammar):
    operators = ignore_case_literals(
        "as", "ccontains", "ceq", "cge", "cgt", "cle", "clike",
        "clt", "cmatch", "cne", "cnotcontains", "cnotlike",
        "cnotmatch", "contains", "creplace", "csplit", "eq",
        "ge", "gt", "icontains", "ieq", "ige", "igt", "ile",
        "ilike", "ilt", "imatch", "in", "ine", "inotcontains",
        "inotlike", "inotmatch", "ireplace", "is", "isnot",
        "isplit", "join", "le", "like", "lt", "match", "ne",
        "notcontains", "notin", "notlike", "notmatch", "replace",
        "shl", "shr", "split"
    )
    grammar = (Dash, OR(*operators))


class FileRedirectionOperator(Grammar):
    grammar = OR(
        ">", ">>", "2>", "2>>", "3>", "3>>", "4>", "4>>",
        "5>", "5>>", "6>", "6>>", "*>", "*>>", "<"
    )


class MergingRedirectionOperator(Grammar):
    grammar = OR(
        '*>&1', '2>&1', '3>&1', '4>&1', '5>&1', '6>&1',
        '*>&2', '1>&2', '3>&2', '4>&2', '5>&2', '6>&2')


class AssignmentOperator(Grammar):
    grammar = OR(
        "=", (Dash, "="), "+=", "*=", "/=", "%="
    )


class OperatorOrPunctuator(Grammar):
    grammar = OR(
        "{", "}", "[", "]", "(", ")", "@(", "@{", "$(", ";",
        "&&", "||", "&", "|", ",", "++", "..", "::", ".",
        "!", "*", "/", "%", "+",
        (Dash,
         OR(Dash,
            *ignore_case_literals(
                "and", "band", "bnot", "bor",
                "bxor", "not", "or", "xor"
            ))),
        Dash,
        AssignmentOperator,
        MergingRedirectionOperator,
        FileRedirectionOperator,
        ComparisonOperator,
        FormatOperator,
    )

# End of grammar Operators and punctuators


# Grammars for Type Names
class TypeCharacter(Grammar):
    grammar = OR(
        WORD("A-Za-z", max=1),
        WORD("\u005F"),
    )


class TypeCharacters(Grammar):
    grammar = WORD("A-Za-z\u005F")


class TypeIdentifier(Grammar):
    grammar = TypeCharacters


class TypeName(Grammar):
    grammar = LIST_OF(TypeIdentifier, sep=".")

# End of grammars for type names


class GenericTypeArguments(Grammar):
    grammar_whitespace_mode = "optional"
    grammar = LIST_OF(REF('TypeSpec'), sep=(",", OPTIONAL(WHITESPACE)))


class SimpleNameFirstCharacter(Grammar):
    grammar = TypeCharacter


class SimpleNameCharacter(Grammar):
    grammar = SimpleNameFirstCharacter


class SimpleNameCharacters(Grammar):
    grammar = REPEAT(SimpleNameCharacter)


class SimpleName(Grammar):
    grammar = (SimpleNameFirstCharacter, SimpleNameCharacters)


# Grammar for Variables
class BracedVariableCharacter(Grammar):
    grammar = OR(
        ANY_EXCEPT("\u007D\u0060", max=1),
        EscapedCharacter
    )


class BracedVariableCharacters(Grammar):
    grammar = REPEAT(BracedVariableCharacter)


class VariableCharacters(Grammar):
    grammar = WORD("A-Za-z0-9?\u005F")


class VariableNamespace(Grammar):
    grammar = (VariableCharacters, ":")


class VariableScope(Grammar):
    grammar = OR(
        VariableNamespace,
        *ignore_case_literals("global:", "local:", "private:", "script:")
    )


class BracedVariable(Grammar):
    grammar = ("${", OPTIONAL(VariableScope),
               BracedVariableCharacters, "}")


class Variable(Grammar):
    grammar = OR(
        "$$", "$?", "$^",
        (OR("$", "@"), OPTIONAL(VariableScope), VariableCharacters),
        BracedVariable
    )
# End of grammar for variables


# Grammar for Literals
class SingleQuoteCharacter(Grammar):
    grammar = OR("\u0027", "\u2018", "\u2019", "\u201A", "\u201B")


class VerbatimHereStringPart(Grammar):
    grammar = OR(
        EXCEPT(ANY, NewLineCharacter),
        (NewLineCharacter, EXCEPT(ANY, SingleQuoteCharacter)),
        (NewLineCharacter, SingleQuoteCharacter, ANY_EXCEPT("@", max=1))
    )


class VerbatimHereStringCharacters(Grammar):
    grammar = REPEAT(VerbatimHereStringPart)


class VerbatimHereStringLiteral(Grammar):
    grammar = ("@", SingleQuoteCharacter, OPTIONAL(WHITESPACE),
               NewLineCharacter, OPTIONAL(VerbatimHereStringCharacters),
               NewLineCharacter, SingleQuoteCharacter, "@")


class VerbatimStringPart(Grammar):
    grammar = OR(
        EXCEPT(ANY, SingleQuoteCharacter),
        (SingleQuoteCharacter, SingleQuoteCharacter)
    )


class VerbatimStringCharacters(Grammar):
    grammar = REPEAT(VerbatimStringPart)


class VerbatimStringLiteral(Grammar):
    grammar = (SingleQuoteCharacter, OPTIONAL(VerbatimStringCharacters),
               SingleQuoteCharacter)


class ExpandableStringPart(Grammar):
    grammar = OR(
        EXCEPT(ANY, OR('$', '\u0060', DoubleQuoteCharacter), max=1),
        BracedVariable,
        ("$", ANY_EXCEPT("({\u0060", max=1) - DoubleQuoteCharacter),
        ("$", EscapedCharacter),
        EscapedCharacter,
        (DoubleQuoteCharacter, DoubleQuoteCharacter),
    )


class ExpandableHereStringPart(Grammar):
    grammar = OR(
        ANY_EXCEPT("$", max=1) - NewLineCharacter,
        BracedVariable,
        ("$", ANY_EXCEPT("(", max=1) - NewLineCharacter),
        ("$", NewLineCharacter, ANY - DoubleQuoteCharacter),
        ("$", NewLineCharacter, DoubleQuoteCharacter, ANY_EXCEPT("@", max=1)),
        (NewLineCharacter, ANY - DoubleQuoteCharacter),
        (NewLineCharacter, DoubleQuoteCharacter, ANY_EXCEPT("@", max=1))
    )


class ExpandableStringCharacters(Grammar):
    grammar = REPEAT(ExpandableStringPart)


class ExpandableStringWithSubexprStart(Grammar):
    grammar = (DoubleQuoteCharacter, OPTIONAL(ExpandableStringCharacters),
               "$(")


class ExpandableStringWithSubexprEnd(Grammar):
    grammar = DoubleQuoteCharacter


class ExpandableStringLiteral(Grammar):
    grammar = (DoubleQuoteCharacter, OPTIONAL(ExpandableStringCharacters),
               OPTIONAL(Dollars), DoubleQuoteCharacter)


class ExpandableHereStringCharacters(Grammar):
    grammar = REPEAT(ExpandableHereStringPart)


class ExpandableHereStringWithSubexprStart(Grammar):
    grammar = (
        "@", DoubleQuoteCharacter, OPTIONAL(WHITESPACE),
        NewLineCharacter, OPTIONAL(ExpandableHereStringCharacters),
        "$("
    )


class ExpandableHereStringWithSubexprEnd(Grammar):
    grammar = (NewLineCharacter, DoubleQuoteCharacter, "@")


class ExpandableHereStringLiteral(Grammar):
    grammar = ("@", DoubleQuoteCharacter, OPTIONAL(WHITESPACE),
               NewLineCharacter, OPTIONAL(ExpandableHereStringCharacters),
               NewLineCharacter, DoubleQuoteCharacter, "@")


class StringLiteral(Grammar):
    grammar = OR(ExpandableStringLiteral,
                 ExpandableHereStringLiteral,
                 VerbatimStringLiteral,
                 VerbatimHereStringLiteral)


class DecimalDigit(Grammar):
    grammar = WORD('0-9', max=1)


class DecimalDigits(Grammar):
    grammar = REPEAT(DecimalDigit)


class NumericMultiplier(Grammar):
    grammar = OR(*ignore_case_literals("kb", "mb", "tb", "pb", "gb"))


class LongTypeSuffix(Grammar):
    grammar = OR("l", "L")


class DecimalTypeSuffix(Grammar):
    grammar = OR("d", "D")


class NumericTypeSuffix(Grammar):
    grammar = OR(DecimalTypeSuffix, LongTypeSuffix)


class Sign(Grammar):
    grammar = OR("+", Dash, max=1)


class ExponentPart(Grammar):
    grammar = (OR("e", "E"), OPTIONAL(Sign), DecimalDigits)


class HexadecimalDigit(Grammar):
    grammar = WORD('0-9a-fA-F', max=1)


class HexadecimalDigits(Grammar):
    grammar = OR(REPEAT(HexadecimalDigit),
                 (HexadecimalDigit, DecimalDigits))


class RealLiteral(Grammar):
    grammar = OR(
        (DecimalDigits, ".", DecimalDigits, OPTIONAL(ExponentPart),
         OPTIONAL(NumericMultiplier)),

        (".", DecimalDigits, OPTIONAL(ExponentPart),
         OPTIONAL(DecimalTypeSuffix), OPTIONAL(NumericMultiplier)),

        (DecimalDigits, ExponentPart, OPTIONAL(DecimalTypeSuffix),
         OPTIONAL(NumericMultiplier)))


class HexadecimalIntegerLiteral(Grammar):
    grammar = ("0x", HexadecimalDigits,
               OPTIONAL(LongTypeSuffix),
               OPTIONAL(NumericMultiplier))


class DecimalIntegerLiteral(Grammar):
    grammar = (DecimalDigits,
               OPTIONAL(NumericTypeSuffix),
               OPTIONAL(NumericMultiplier))


class IntegerLiteral(Grammar):
    grammar = OR(HexadecimalIntegerLiteral, DecimalIntegerLiteral)


class Literal(Grammar):
    grammar = OR(IntegerLiteral, RealLiteral, StringLiteral)
# End of grammar for Literals


# Grammar for Commands
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
    grammar = (GenericTokenParts, "$(")


class GenericToken(Grammar):
    grammar = GenericTokenParts


class FirstParameterCharacter(Grammar):
    grammar = OR(
        WORD("A-Za-z", max=1),
        # The underscore character and question mark
        "\u005F", "?"
    )


class ParameterCharacter(Grammar):
    grammar = EXCEPT(ANY, OR(Colon, WHITESPACE, NewLineCharacter,
                             "{", "}", "(", ")", ";", ",", "|",
                             "&", ".", "["))


class ParameterCharacters(Grammar):
    grammar = REPEAT(ParameterCharacter)


class CommandParameter(Grammar):
    grammar = (Dash, FirstParameterCharacter,
               ParameterCharacters, OPTIONAL(Colon))
# End of grammar for Commands


class StatementList(Grammar):
    grammar_whitespace_mode = "optional"
    grammar = LIST_OF(REF('Statement'), sep=OPTIONAL(WHITESPACE))


class StatementBlock(Grammar):
    grammar_whitespace_mode = "optional"
    grammar = ("{", OPTIONAL(StatementList), "}")


class BlockName(Grammar):
    grammar = OR(
        *ignore_case_literals("dynamicparam", "begin", "process", "end")
    )


class NamedBlock(Grammar):
    grammar = (BlockName, Spaces,
               StatementBlock,
               OPTIONAL(StatementTerminators))


class NamedBlockList(Grammar):
    grammar = LIST_OF(NamedBlock, sep=Spaces)


class ScriptBlockBody(Grammar):
    grammar = OR(NamedBlockList, StatementList)


class ParamBlock(Grammar):
    # FIXME: Remove References
    grammar_whitespace_mode = "optional"
    grammar = (
        RE_LITERAL("param"), OPTIONAL(NewLines),
        "(", OPTIONAL(REF('ParameterList')), ")"
    )


class ScriptBlock(Grammar):
    # This grammar can be considered the root grammar.
    grammar = (
        OPTIONAL(ParamBlock), OPTIONAL(StatementTerminators),
        OPTIONAL(ScriptBlockBody)
    )


class CommandName(Grammar):
    grammar = OR(GenericToken, GenericTokenWithSubexprStart)


class CommandNameExpr(Grammar):
    grammar = OR(CommandName, REF('PrimaryExpression'))


class CommandArgument(Grammar):
    grammar = CommandNameExpr


class VerbatimCommandString(Grammar):
    grammar = (DoubleQuoteCharacter, NonDoubleQuoteCharacters,
               DoubleQuoteCharacter)


class VerbatimCommandArgumentPart(Grammar):
    grammar = OR(
        VerbatimCommandString,
        ("&", NonAmpersandCharacter),
        EXCEPT(ANY, OR("|", NewLineCharacter))
    )


class VerbatimCommandArgumentChars(Grammar):
    grammar = REPEAT(VerbatimCommandArgumentPart)


class Keyword(Grammar):

    grammar = OR(*ignore_case_literals(
        "workflow", "inlinescript", "parallel", "begin", "break", "catch",
        "class", "continue", "data", "define", "do", "dynamicparam", "elseif",
        "else", "end", "exit", "filter", "finally", "foreach", "for", "from",
        "function", "if", "in", "param", "process", "return", "switch", "var",
        "throw", "trap", "try", "until", "using", "while"
    ))


class TypeSpec(Grammar):
    grammar = (
        TypeName,
        OPTIONAL(
            ("[", OR(
                GenericTypeArguments,
                OPTIONAL(Dimension)
            ), "]"))
    )


class TypeLiteral(Grammar):
    grammar = ("[", TypeSpec, "]")


class Token(Grammar):
    grammar = OR(
        Keyword,
        Variable,
        # FIXME: Remove REF
        REF('Command'),
        CommandParameter,
        IntegerLiteral,
        RealLiteral,
        StringLiteral,
        TypeLiteral,
        OperatorOrPunctuator,
    )


class ExpandableHereStringLiteralWithSubexpr(Grammar):
    # FIXME: Remove references
    grammar = (
        ExpandableHereStringWithSubexprStart,
        OPTIONAL(StatementList),
        REF('ExpandableHereStringWithSubexprCharacters'),
        ExpandableHereStringWithSubexprEnd
    )


class ExpandableHereStringWithSubexprPart(Grammar):
    # FIXME: Remove reference
    grammar = OR(REF('SubExpression'), ExpandableHereStringPart)


class ExpandableHereStringWithSubexprCharacters(Grammar):
    grammar = REPEAT(ExpandableHereStringWithSubexprPart)


class ExpandableStringWithSubexprPart(Grammar):
    grammar = OR(REF('SubExpression'), ExpandableStringPart)


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
            ")", ExpandableHereStringWithSubexprCharacters,
            ExpandableHereStringWithSubexprEnd
        )
    )


class StringLiteralWithSubexpression(Grammar):
    # grammar is added.
    grammar = OR(
        ExpandableStringLiteralWithSubexpr,
        ExpandableHereStringLiteralWithSubexpr
    )


class MemberName(Grammar):
    grammar = OR(
        # FIXME: Remove references
        SimpleName, StringLiteral, REF('StringLiteralWithSubexpression'),
        REF('ExpressionWithUnaryOperator'), REF('Value')
    )


class RangeArgumentExpression(Grammar):
    grammar = OR(
        # FIXME: Remove references
        REF('UnaryExpression'),
        (
            REF('RangeExpression'), "..", OPTIONAL(NewLines),
            REF('UnaryExpression')
        )
    )


class FormatArgumentExpression(Grammar):
    grammar = LIST_OF(RangeArgumentExpression,
                      sep=(FormatOperator, OPTIONAL(NewLines)))


class MultiplicativeArgumentExpression(Grammar):
    grammar = LIST_OF(FormatArgumentExpression,
                      sep=(OR("*", "/", "%"), OPTIONAL(NewLines)))


class AdditiveArgumentExpression(Grammar):
    grammar = LIST_OF(MultiplicativeArgumentExpression,
                      sep=(OR("+", Dash), OPTIONAL(NewLines)))


class ComparisonArgumentExpression(Grammar):
    grammar = LIST_OF(AdditiveArgumentExpression,
                      sep=(ComparisonOperator, OPTIONAL(NewLines)))


class BitwiseArgumentExpression(Grammar):
    grammar = LIST_OF(
        ComparisonArgumentExpression,
        sep=(
            OR(RE_LITERAL("-band"),
               RE_LITERAL("-bor"),
               RE_LITERAL("-bxor")),
            OPTIONAL(NewLines)
        )
    )


class LogicalArgumentExpression(Grammar):
    grammar = LIST_OF(
        BitwiseArgumentExpression,
        sep=(
            OR(RE_LITERAL("-and"),
               RE_LITERAL("-or"),
               RE_LITERAL("-xor")),
            OPTIONAL(NewLines)
        )
    )


class ArgumentExpressionList(Grammar):
    grammar_whitespace_mode = "optional"
    grammar = LIST_OF(LogicalArgumentExpression,
                      sep=(OPTIONAL(NewLines), ",", OPTIONAL(WHITESPACE)))


class ArgumentList(Grammar):
    grammar = ("(", OPTIONAL(ArgumentExpressionList), OPTIONAL(NewLines), ")")


class InvocationExpressionPrime(Grammar):
    grammar = (OR(".", "::"), MemberName,
               ArgumentList, OPTIONAL(REF("InvocationExpressionPrime")))


class ElementAccessPrime(Grammar):
    # Use this idiom to get rid of left recursion.
    grammar = ("[", OPTIONAL(NewLines), REF("Expression"),
               OPTIONAL(NewLines), "]",
               OPTIONAL(REF("ElementAccessPrime")))


class MemberAccessPrime(Grammar):
    # Use this idiom to get rid of left recursion.
    grammar = (OR(".", "::"), Spaces,
               MemberName, OPTIONAL(REF('MemberAccessPrime')))


class PostDecrementExpressionPrime(Grammar):
    # Use this idiom to get rid of left recursion.
    grammar = (DashDash, OPTIONAL(REF("PostDecrementExpressionPrime")))


class PostIncrementExpressionPrime(Grammar):
    # Use this idiom to get rid of left recursion.
    grammar = ("++", OPTIONAL(REF("PostIncrementExpressionPrime")))


class KeyExpression(Grammar):
    # FIXME: Remove reference
    grammar = OR(
        SimpleName,
        REF('UnaryExpression')
    )


class HashEntry(Grammar):
    # FIXME: Remove reference
    grammar = (KeyExpression,
               OPTIONAL(WHITESPACE), "=",
               OPTIONAL(WHITESPACE),
               OPTIONAL(NewLines),
               REF('Statement'))


class HashLiteralBodyPrime(Grammar):
    grammar = (StatementTerminators, HashEntry,
               OPTIONAL(REF("HashLiteralBodyPrime")))


class HashLiteralBody(Grammar):
    grammar = LIST_OF(HashEntry,
                      sep=(OPTIONAL(WHITESPACE),
                           OPTIONAL(HashLiteralBodyPrime),
                           OPTIONAL(WHITESPACE)))


class HashLiteralExpression(Grammar):
    grammar = ("@{", OPTIONAL(NewLines),
               OPTIONAL(WHITESPACE),
               OPTIONAL(HashLiteralBody),
               OPTIONAL(WHITESPACE),
               OPTIONAL(NewLines), "}")


class ScriptBlockExpression(Grammar):
    grammar = ("{", OPTIONAL(NewLines), ScriptBlock,
               OPTIONAL(NewLines), "}")


class ArrayExpression(Grammar):
    # FIXME: Remove reference
    grammar_whitespace_mode = "optional"
    grammar = ("@(", OPTIONAL(StatementList), ")")


class SubExpression(Grammar):
    # FIXME: Remove reference
    grammar_whitespace_mode = "optional"
    grammar = ("$(", OPTIONAL(StatementList), ")")


class ParenthesizedExpression(Grammar):
    grammar_whitespace_mode = "optional"
    grammar = ("(", REF('Pipeline'), ")")


class Value(Grammar):
    grammar = OR(
        ParenthesizedExpression,
        SubExpression,
        ArrayExpression,
        ScriptBlockExpression,
        HashLiteralExpression,
        Literal,
        TypeLiteral,
        Variable
    )


class PrimaryExpressionPrime(Grammar):
    grammar = OR(MemberAccessPrime,
                 ElementAccessPrime,
                 InvocationExpressionPrime,
                 PostIncrementExpressionPrime,
                 PostDecrementExpressionPrime)


class PrimaryExpression(Grammar):
    grammar = (
        Value,
        OPTIONAL(OR(REPEAT(PrimaryExpressionPrime),
                    REF('PrimaryExpression'))),
    )


class UnaryExpression(Grammar):
    grammar = OR(
        PrimaryExpression,
        REF('ExpressionWithUnaryOperator'),
    )


class CastExpression(Grammar):
    grammar = (TypeLiteral, UnaryExpression)


class PreDecrementExpression(Grammar):
    grammar = (DashDash, OPTIONAL(NewLines), UnaryExpression)


class PreIncrementExpression(Grammar):
    grammar = ("++", OPTIONAL(NewLines), UnaryExpression)


class ExpressionWithUnaryOperator(Grammar):
    grammar = OR(
        (
            OR(",", RE_LITERAL("-bnot"), RE_LITERAL("-not"),
               RE_LITERAL("-split"), RE_LITERAL("-join"), "!", "+", Dash),
            Spaces, UnaryExpression
        ),
        PreIncrementExpression,
        PreDecrementExpression,
        CastExpression,
    )


class ArrayLiteralExpression(Grammar):
    grammar = LIST_OF(UnaryExpression,
                      sep=(OPTIONAL(WHITESPACE), ",", Spaces))


class RangeExpression(Grammar):
    grammar = LIST_OF(ArrayLiteralExpression,
                      sep=("..", OPTIONAL(NewLines)))


class FormatExpression(Grammar):
    grammar = LIST_OF(RangeExpression,
                      sep=(OPTIONAL(WHITESPACE),
                           FormatOperator,
                           OPTIONAL(WHITESPACE),
                           OPTIONAL(NewLines)))


class MultiplicativeExpression(Grammar):
    grammar = LIST_OF(FormatExpression,
                      sep=((OPTIONAL(WHITESPACE),
                            OR("*", "/", "%"),
                            OPTIONAL(WHITESPACE)),
                           OPTIONAL(NewLines)))


class AdditiveExpression(Grammar):
    grammar = LIST_OF(MultiplicativeExpression,
                      sep=(OPTIONAL(WHITESPACE),
                           OR("+", Dash),
                           OPTIONAL(WHITESPACE),
                           OPTIONAL(NewLines)))


class ComparisonExpression(Grammar):
    grammar = LIST_OF(AdditiveExpression,
                      sep=(OPTIONAL(WHITESPACE),
                           ComparisonOperator,
                           OPTIONAL(WHITESPACE),
                           OPTIONAL(NewLines)))


class BitwiseExpression(Grammar):
    grammar = LIST_OF(ComparisonExpression,
                      sep=(OPTIONAL(WHITESPACE),
                           OR(RE_LITERAL("-band"),
                              RE_LITERAL("-bor"),
                              RE_LITERAL("-bxor")),
                           OPTIONAL(WHITESPACE),
                           OPTIONAL(NewLines)))


class LogicalExpression(Grammar):
    grammar = LIST_OF(BitwiseExpression,
                      sep=(OPTIONAL(WHITESPACE),
                           OR(RE_LITERAL("-and"),
                              RE_LITERAL("-or"),
                              RE_LITERAL("-xor")),
                           OPTIONAL(WHITESPACE),
                           OPTIONAL(NewLines)))


class Expression(Grammar):
    grammar = LogicalExpression
# End of grammar for Expressions


# Attributes
class AttributeArgument(Grammar):
    grammar = OR(
        (OPTIONAL(NewLines), Expression),
        (
            OPTIONAL(NewLines),
            SimpleName,
            OPTIONAL(WHITESPACE), "=", OPTIONAL(Spaces),
            Expression
        )
    )


class AttributeArguments(Grammar):
    grammar = LIST_OF(AttributeArgument,
                      sep=(Spaces, ",", OPTIONAL(WHITESPACE)))


class AttributeName(Grammar):
    grammar = TypeSpec


class Attribute(Grammar):
    grammar = OR(
        ("[", AttributeName, "(", AttributeArguments, OPTIONAL(NewLines),
         ")", OPTIONAL(NewLines), "]"),
        TypeLiteral
    )


class AttributeList(Grammar):
    grammar = LIST_OF(Attribute, sep=OPTIONAL(WHITESPACE))


class ScriptParameterDefault(Grammar):
    grammar = (Spaces, "=", Spaces, Expression)


class ScriptParameter(Grammar):
    grammar = (
        OPTIONAL(NewLines),
        OPTIONAL(AttributeList), Spaces,
        Variable, OPTIONAL(ScriptParameterDefault)
    )


class ParameterListPrime(Grammar):
    grammar_whitespace_mode = "optional"
    grammar = (",", ScriptParameter, OPTIONAL(REF('ParameterListPrime')))


class ParameterList(Grammar):
    grammar = (ScriptParameter, OPTIONAL(ParameterListPrime))


class RedirectedFileName(Grammar):
    grammar = OR(CommandArgument, PrimaryExpression)


class Redirection(Grammar):
    grammar = OR(
        MergingRedirectionOperator,
        (FileRedirectionOperator, OPTIONAL(WHITESPACE), RedirectedFileName)
    )


class CommandElement(Grammar):
    grammar = OR(CommandParameter, CommandArgument, Redirection)


class CommandElements(Grammar):
    grammar = LIST_OF(CommandElement, sep=OPTIONAL(WHITESPACE))


class CommandModule(Grammar):
    grammar = PrimaryExpression


class CommandInvocationOperator(Grammar):
    grammar = OR("&", ".")


class Command(Grammar):
    grammar = OR(
        (CommandName, OPTIONAL(WHITESPACE), OPTIONAL(CommandElements)),
        (
            CommandInvocationOperator,
            OPTIONAL(WHITESPACE),
            OPTIONAL(CommandModule),
            OPTIONAL(WHITESPACE),
            CommandNameExpr,
            OPTIONAL(WHITESPACE),
            OPTIONAL(CommandElements)
        )
    )


class PipelineTail(Grammar):
    grammar = (
        Spaces, "|", Spaces,
        Command, OPTIONAL(REF('PipelineTail'))
    )


class AssignmentExpression(Grammar):
    grammar = (Expression, OPTIONAL(WHITESPACE),
               AssignmentOperator, OPTIONAL(WHITESPACE),
               REF('Statement'))


class Pipeline(Grammar):
    grammar = OR(
        AssignmentExpression,
        (Expression, OPTIONAL(Redirection), OPTIONAL(PipelineTail)),
        (Command, OPTIONAL(PipelineTail))
    )


class InlinescriptStatement(Grammar):
    grammar = (RE_LITERAL("inlinescript"), Spaces, StatementBlock)


class ParallelStatement(Grammar):
    grammar = (RE_LITERAL("parallel"), Spaces, StatementBlock)


class SequenceStatement(Grammar):
    grammar = (RE_LITERAL("sequence"), Spaces, StatementBlock)


class DataCommand(Grammar):
    grammar = CommandNameExpr


class DataCommandsList(Grammar):
    grammar = LIST_OF(DataCommand, sep=(Spaces, ",", Spaces))


class DataCommandsAllowed(Grammar):
    grammar = (RE_LITERAL("-supportedcommand"), Spaces, DataCommandsList)


class DataStatement(Grammar):
    grammar = (
        RE_LITERAL("data"), Spaces, OPTIONAL(DataCommandsAllowed),
        Spaces, StatementBlock
    )


class ElseIfClause(Grammar):
    grammar = (
        Spaces, RE_LITERAL("elseif"), Spaces,
        "(", Spaces, Pipeline, Spaces, ")", Spaces,
        StatementBlock
    )


class ElseIfClauses(Grammar):
    grammar = LIST_OF(ElseIfClause, sep=Spaces)


class ElseClause(Grammar):
    grammar = (Spaces, RE_LITERAL("else"), Spaces, StatementBlock)


class IfStatement(Grammar):
    grammar = (
        RE_LITERAL("if"),
        Spaces, "(", Spaces, Pipeline, Spaces, ")", Spaces,
        StatementBlock, OPTIONAL(ElseIfClauses),
        OPTIONAL(ElseClause)
    )


class LabelExpression(Grammar):
    grammar = OR(SimpleName, UnaryExpression)


class FinallyClause(Grammar):
    grammar = (
        OPTIONAL(NewLines),
        RE_LITERAL("finally"),
        Spaces, StatementBlock
    )


class CatchTypeList(Grammar):
    grammar_whitespace_mode = "optional"
    grammar = LIST_OF(TypeLiteral, sep=(","), whitespace_mode="optional")


class CatchClause(Grammar):
    grammar_whitespace_mode = "optional"
    grammar = (
        OPTIONAL(NewLines),
        RE_LITERAL("catch"),
        OPTIONAL(CatchTypeList),
        StatementBlock
    )


class CatchClauses(Grammar):
    grammar_whitespace_mode = "optional"
    grammar = REPEAT(CatchClause)


class TryStatement(Grammar):
    grammar_whitespace_mode = "optional"
    grammar = (
        RE_LITERAL("try"), StatementBlock,
        OR(
            (CatchClauses, FinallyClause),
            CatchClauses,
            FinallyClause
        )
    )


class TrapStatement(Grammar):
    grammar = (
        RE_LITERAL("trap"), Spaces, OPTIONAL(TypeLiteral),
        Spaces, StatementBlock
    )


class FlowControlStatement(Grammar):
    grammar = OR(
        (
            OR(RE_LITERAL("break"), RE_LITERAL("continue")),
            OPTIONAL((WHITESPACE, LabelExpression))
        ),
        (
            OR(RE_LITERAL("throw"), RE_LITERAL("return"), RE_LITERAL("exit")),
            OPTIONAL((WHITESPACE, Pipeline))
        ),
    )


class FunctionParameterDeclaration(Grammar):
    grammar = ("(", Spaces, ParameterList, Spaces, ")")


class FunctionName(Grammar):
    grammar = CommandArgument


class FunctionStatement(Grammar):
    grammar = (
        OR(
            RE_LITERAL("function"),
            RE_LITERAL("filter"),
            RE_LITERAL("workflow")
        ),
        Spaces, FunctionName, Spaces,
        OPTIONAL(FunctionParameterDeclaration), Spaces,
        "{", Spaces, ScriptBlock, Spaces, "}"
    )


class WhileCondition(Grammar):
    grammar = (OPTIONAL(NewLines), Pipeline)


class DoStatement(Grammar):
    grammar = (
        RE_LITERAL("do"),
        Spaces, StatementBlock, Spaces,
        OR(RE_LITERAL("while"), RE_LITERAL("until")),
        Spaces, "(", WhileCondition, Spaces, ")"
    )


class WhileStatement(Grammar):
    grammar = (
        RE_LITERAL("while"), Spaces, "(", Spaces, WhileCondition,
        Spaces, ")", Spaces, StatementBlock
    )


class ForInitializer(Grammar):
    grammar = Pipeline


class ForCondition(Grammar):
    grammar = Pipeline


class ForIterator(Grammar):
    grammar = Pipeline


class ForStatement(Grammar):
    grammar = OR(
        (
            RE_LITERAL("for"), Spaces, "(",
            Spaces, OPTIONAL(ForInitializer), StatementTerminator,
            Spaces, OPTIONAL(ForCondition), StatementTerminator,
            Spaces, OPTIONAL(ForIterator), Spaces, ")",
            Spaces, StatementBlock
        ),
        (
            RE_LITERAL("for"), Spaces, "(",
            Spaces, OPTIONAL(ForInitializer), StatementTerminator,
            Spaces, OPTIONAL(ForCondition), Spaces, ")",
            Spaces, StatementBlock
        ),
        (
            RE_LITERAL("for"), Spaces, "(", Spaces,
            OPTIONAL(ForInitializer), Spaces, ")",
            Spaces, StatementBlock
        ),
    )


class ForeachParameter(Grammar):
    grammar = RE_LITERAL("-parallel")


class ForeachStatement(Grammar):
    grammar = (
        RE_LITERAL("foreach"), Spaces, OPTIONAL(ForeachParameter), Spaces,
        "(", Spaces, Variable, Spaces, RE_LITERAL("in"), Spaces, Pipeline,
        Spaces, ")", Spaces, StatementBlock
    )


class SwitchClauseCondition(Grammar):
    grammar = OR(CommandArgument, PrimaryExpression)


class SwitchClause(Grammar):
    grammar = (SwitchClauseCondition, OPTIONAL(WHITESPACE), StatementBlock,
               OPTIONAL(StatementTerminators))


class SwitchClauses(Grammar):
    grammar_whitespace_mode = "optional"
    grammar = LIST_OF(SwitchClause, sep=OPTIONAL(WHITESPACE))


class SwitchBody(Grammar):
    grammar = (
        OPTIONAL(NewLines), "{", OPTIONAL(NewLines),
        OPTIONAL(WHITESPACE), SwitchClauses, OPTIONAL(WHITESPACE), "}"
    )


class SwitchFilename(Grammar):
    grammar = OR(CommandArgument, PrimaryExpression)


class SwitchCondition(Grammar):
    grammar = OR(
        ("(", OPTIONAL(NewLines), Pipeline, OPTIONAL(NewLines), ")"),
        ("-file", Spaces, SwitchFilename)
    )


class SwitchParameter(Grammar):
    grammar = OR(*ignore_case_literals(
        "-regex", "-rege", "-reg", "-re", "-r", "-wildcard", "-wildcar",
        "-wildca", "-wildc", "-wild", "-wil", "-wi", "-w", "-exact",
        "-exac", "-exa", "-ex", "-e", "-casesensitive", "-casesensitiv",
        "-casesensiti", "-casesensit", "-casesensi", "-casesens",
        "-casesen", "-casese", "-cases", "-case", "-cas", "-ca", "-c"
    ))


class SwitchParameters(Grammar):
    grammar_whitespace_mode = "optional"
    grammar = LIST_OF(SwitchParameter, sep=OPTIONAL(WHITESPACE))


class SwitchStatement(Grammar):
    grammar = (
        RE_LITERAL("switch"), OPTIONAL(NewLines), OPTIONAL(WHITESPACE),
        OPTIONAL(SwitchParameters), OPTIONAL(WHITESPACE),
        SwitchCondition, OPTIONAL(WHITESPACE), SwitchBody
    )


class LabeledStatement(Grammar):
    grammar = (
        OPTIONAL((":", SimpleName, Spaces)),
        OR(
            SwitchStatement,
            ForeachStatement,
            ForStatement,
            WhileStatement,
            DoStatement
        )
    )


class Statement(Grammar):
    grammar = OR(
        IfStatement,
        LabeledStatement,
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


class Hashes(Grammar):
    grammar = REPEAT("#")


class NotGreaterThanOrHash(Grammar):
    grammar = ANY_EXCEPT("#>", max=1)


class DelimitedCommentSection(Grammar):
    grammar = OR(">", (OPTIONAL(Hashes), NotGreaterThanOrHash))


class DelimitedCommentText(Grammar):
    grammar = REPEAT(DelimitedCommentSection)


class DelimitedComment(Grammar):
    grammar = ("<#", OPTIONAL(DelimitedCommentText), Hashes, ">")


class RequiresComment(Grammar):
    grammar = ("#requires", WHITESPACE, CommandArgument)


class SingleLineComment(Grammar):
    grammar = ("#", OPTIONAL(WHITESPACE), OPTIONAL(InputCharacters))


class Comment(Grammar):
    grammar = OR(SingleLineComment, RequiresComment, DelimitedComment)


class SignatureBegin(Grammar):
    grammar = (NewLineCharacter, "# SIG # Begin signature block",
               NewLineCharacter)


class SignatureEnd(Grammar):
    grammar = (NewLineCharacter, "# SIG # End signature block",
               NewLineCharacter)


class Signature(Grammar):
    grammar = LIST_OF(SingleLineComment, sep=NewLineCharacter)


class SignatureBlock(Grammar):
    grammar = (SignatureBegin, Signature, SignatureEnd)


class InputElement(Grammar):
    grammar = OR(WHITESPACE, Comment, Token)


class InputElements(Grammar):
    grammar = LIST_OF(InputElement, sep=NewLineCharacter)


class Input(Grammar):
    grammar = OPTIONAL(InputElements), OPTIONAL(SignatureBlock)
