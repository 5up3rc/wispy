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
    OPTIONAL, ANY, EXCEPT,
    LIST_OF, REF, WHITESPACE,
)


# Grammars without dependencies
class EscapedCharacter(Grammar):

    """An escaped character is a way to assign a special interpretation
    to a character by giving it a prefix Backtick character."""

    grammar = ("\u0060", ANY)


class Colon(Grammar):
    grammar = "\u003A"


class Dimension(Grammar):
    grammar = REPEAT(",")


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


# Grammar for comments
class Dash(Grammar):
    grammar = OR("\u002D", "\u2013", "\u2014", "\u2015")


class DashDash(Grammar):
    grammar = (Dash, Dash)


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
    grammar = ("#requires", WHITESPACE, REF('CommandArguments'))


class SingleLineComment(Grammar):
    grammar = ("#", OPTIONAL(WHITESPACE), OPTIONAL(InputCharacters))


class Comment(Grammar):
    grammar = OR(SingleLineComment, RequiresComment, DelimitedComment)
# Enf of grammar for comments


# Grammar for siganature
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
# Enf of grammar for signature


# Grammar for Operators and punctuators
class FormatOperator(Grammar):
    grammar = (Dash, "f")


class ComparisonOperator(Grammar):
    grammar = (
        Dash,
        OR(
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
    )


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
        (Dash, OR("and", "band", "bnot", "bor",
                  "bxor", "not", "or", "xor", Dash)),
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
    # FIXME: Remove reference
    grammar = LIST_OF(REF('TypeSpec'), sep=",")


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
    grammar = OR("global:", "local:", "private:", "script:",
                 VariableNamespace)


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
    grammar = OR("kb", "mb", "tb", "pb", "gb")


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


# Syntactic grammar
class ScriptBlockBody(Grammar):
    # FIXME: Remove REF
    grammar = OR(REF('NamedBlockList'), REF('StatementList'))


class ParamBlock(Grammar):
    grammar = (
        "param", Spaces, "(", Spaces,
        # FIXME: Remove REF
        OPTIONAL(LIST_OF(REF('ScriptParameter'), sep=(Spaces, ",", Spaces))),
        Spaces, ")"
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


# Grammar fo Keywords
class Keyword(Grammar):

    grammar = OR(
        "workflow", "inlinescript", "parallel", "begin", "break", "catch",
        "class", "continue", "data", "define", "do", "dynamicparam", "elseif",
        "else", "end", "exit", "filter", "finally", "foreach", "for", "from",
        "function", "if", "in", "param", "process", "return", "switch", "var",
        "throw", "trap", "try", "until", "using", "while"
    )
# End of grammar for Keywords


# Grammar for tokens
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
        # FIXME: Remove reference
        REF('TypeLiteral'),
        OperatorOrPunctuator,
    )
# End of grammar for tokens


# Grammar for input
class InputElement(Grammar):
    grammar = OR(WHITESPACE, Comment, Token)


class InputElements(Grammar):
    grammar = LIST_OF(InputElement, sep=NewLineCharacter)


class Input(Grammar):
    grammar = OPTIONAL(InputElements), OPTIONAL(SignatureBlock)
# End of grammar for Input


# Grammar for Expressions


class TypeSpec(Grammar):
    grammar = (
        TypeName,
        OPTIONAL(("[",
                  OR(REF("GenericTypeArguments"),
                     OPTIONAL(Dimension)),
                  "]"))
    )


class TypeLiteral(Grammar):
    grammar = ("[", TypeSpec, "]")


class ExpandableHereStringLiteralWithSubexpr(Grammar):
    # FIXME: Remove references
    grammar = (
        ExpandableHereStringWithSubexprStart,
        OPTIONAL(REF('StatementList')),
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
            ExpandableStringWithSubexprStart, OPTIONAL(REF('StatementList')),
            ")", ExpandableStringWithSubexprCharacters,
            ExpandableStringWithSubexprEnd
        ),
        (
            ExpandableHereStringWithSubexprStart, OPTIONAL(
                REF('StatementList')),
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
    grammar = LIST_OF(ComparisonArgumentExpression,
                      sep=(OR("-band", "-bor", "-bxor"), OPTIONAL(NewLines)))


class LogicalArgumentExpression(Grammar):
    grammar = LIST_OF(BitwiseArgumentExpression,
                      sep=(OR("-and", "-or", "-xor"), OPTIONAL(NewLines)))


class ArgumentExpressionList(Grammar):
    grammar = LIST_OF(LogicalArgumentExpression,
                      sep=(Spaces, ",", Spaces))


class ArgumentList(Grammar):
    grammar = ("(", Spaces, OPTIONAL(ArgumentExpressionList),
               Spaces, ")")


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
    grammar = ("@(", Spaces, OPTIONAL(REF('StatementList')),
               Spaces, ")")


class SubExpression(Grammar):
    # FIXME: Remove reference
    grammar = ("$(", Spaces,
               OPTIONAL(REF('StatementList')), Spaces, ")")


class ParenthesizedExpression(Grammar):
    grammar = (
        "(", Spaces, REF('Pipeline'), Spaces, ")"
    )


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


class PrimaryExpressionPrime(Grammar):
    grammar = OR(MemberAccessPrime,
                 ElementAccessPrime,
                 REF('InvocationExpressionPrime'),
                 PostIncrementExpressionPrime,
                 PostDecrementExpressionPrime)


class PrimaryExpression(Grammar):
    grammar = (
        Value,
        OPTIONAL(OR(REPEAT(PrimaryExpressionPrime),
                    REF('PrimaryExpression'))),
    )


class CastExpression(Grammar):
    grammar = (TypeLiteral, REF('UnaryExpression'))


class PreDecrementExpression(Grammar):
    # FIXME: Remove reference
    grammar = (DashDash, OPTIONAL(NewLines), REF('UnaryExpression'))


class PreIncrementExpression(Grammar):
    # FIXME: Remove reference
    grammar = ("++", OPTIONAL(NewLines), REF('UnaryExpression'))


class ExpressionWithUnaryOperator(Grammar):
    grammar = OR(
        (
            OR(",", "-bnot", "-not", "-split", "-join", "!", "+", Dash),
            Spaces, REF("UnaryExpression")
        ),
        REF("PreIncrementExpression"),
        REF("PreDecrementExpression"),
        REF("CastExpression"),
    )


class UnaryExpression(Grammar):
    grammar = OR(
        PrimaryExpression,
        REF('ExpressionWithUnaryOperator'),
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
                           OR("-band", "-bor", "-bxor"),
                           OPTIONAL(WHITESPACE),
                           OPTIONAL(NewLines)))


class LogicalExpression(Grammar):
    grammar = LIST_OF(BitwiseExpression,
                      sep=(OPTIONAL(WHITESPACE),
                           OR("-and", "-or", "-xor"),
                           OPTIONAL(WHITESPACE),
                           OPTIONAL(NewLines)))


class Expression(Grammar):
    grammar = LogicalExpression
# End of grammar for Expressions


# Syntactic grammar
class StatementList(Grammar):
    grammar = LIST_OF(REF('Statement'), sep=Spaces)


class StatementBlock(Grammar):
    grammar = (
        "{", Spaces, OPTIONAL(StatementList), Spaces,
        "}"
    )


class ScriptParameterDefault(Grammar):
    grammar = (Spaces, "=", Spaces, Expression)


class ScriptParameter(Grammar):
    grammar = (
        # FIXME: Remove REF
        OPTIONAL(NewLines),
        OPTIONAL(REF('AttributeList')), Spaces,
        Variable, OPTIONAL(ScriptParameterDefault)
    )


class ParameterListPrime(Grammar):
    grammar = (
        Spaces, ",", Spaces, ScriptParameter,
        OPTIONAL(REF('ParameterListPrime'))
    )


class ParameterList(Grammar):
    grammar = (ScriptParameter, OPTIONAL(ParameterListPrime))


class BlockName(Grammar):
    grammar = OR("dynamicparam", "begin", "process", "end")


class NamedBlock(Grammar):
    grammar = (BlockName, Spaces,
               StatementBlock,
               OPTIONAL(StatementTerminators))


class NamedBlockList(Grammar):
    grammar = LIST_OF(NamedBlock, sep=Spaces)


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
    grammar = ("inlinescript", Spaces, StatementBlock)


class ParallelStatement(Grammar):
    grammar = ("parallel", Spaces, StatementBlock)


class SequenceStatement(Grammar):
    grammar = ("sequence", Spaces, StatementBlock)


class DataCommand(Grammar):
    grammar = CommandNameExpr


class DataCommandsList(Grammar):
    grammar = LIST_OF(DataCommand, sep=(Spaces, ",", Spaces))


class DataCommandsAllowed(Grammar):
    grammar = ("-supportedcommand", Spaces, DataCommandsList)


class DataStatement(Grammar):
    grammar = (
        "data", Spaces, OPTIONAL(DataCommandsAllowed),
        Spaces, StatementBlock
    )


class ElseIfClause(Grammar):
    grammar = (
        Spaces, "elseif", Spaces, "(", Spaces, Pipeline, Spaces, ")", Spaces,
        StatementBlock
    )


class ElseIfClauses(Grammar):
    grammar = LIST_OF(ElseIfClause, sep=Spaces)


class ElseClause(Grammar):
    grammar = (Spaces, "else", Spaces, StatementBlock)


class IfStatement(Grammar):
    grammar = (
        "if",
        Spaces, "(", Spaces, Pipeline, Spaces, ")", Spaces,
        StatementBlock, OPTIONAL(ElseIfClauses),
        OPTIONAL(ElseClause)
    )


class LabelExpression(Grammar):
    grammar = OR(SimpleName, UnaryExpression)


class FinallyClause(Grammar):
    grammar = (OPTIONAL(NewLines), "finally", Spaces, StatementBlock)


class CatchTypeList(Grammar):
    grammar = LIST_OF(TypeLiteral,
                      sep=(Spaces, ",", Spaces))


class CatchClause(Grammar):
    grammar = (OPTIONAL(NewLines), "catch", Spaces, OPTIONAL(CatchTypeList),
               Spaces, StatementBlock)


class CatchClauses(Grammar):
    grammar = LIST_OF(CatchClause, sep=Spaces)


class TryStatement(Grammar):
    grammar = (
        "try", Spaces, StatementBlock,
        OR(
            (CatchClauses, FinallyClause),
            CatchClauses,
            FinallyClause
        )
    )


class TrapStatement(Grammar):
    grammar = (
        "trap", Spaces, OPTIONAL(TypeLiteral),
        Spaces, StatementBlock
    )


class FlowControlStatement(Grammar):
    grammar = OR(
        (
            OR("break", "continue"),
            OPTIONAL((WHITESPACE, LabelExpression))
        ),
        (
            OR("throw", "return", "exit"),
            OPTIONAL((WHITESPACE, Pipeline))
        ),
    )


class FunctionParameterDeclaration(Grammar):
    grammar = ("(", Spaces, ParameterList, Spaces, ")")


class FunctionName(Grammar):
    grammar = CommandArgument


class FunctionStatement(Grammar):
    grammar = (
        OR("function", "filter", "workflow"), Spaces,
        FunctionName, Spaces, OPTIONAL(FunctionParameterDeclaration), Spaces,
        "{", Spaces, ScriptBlock, Spaces, "}"
    )


class WhileCondition(Grammar):
    grammar = (OPTIONAL(NewLines), Pipeline)


class DoStatement(Grammar):
    grammar = (
        "do", Spaces, StatementBlock, Spaces, OR("while", "until"),
        Spaces, "(", WhileCondition, Spaces, ")"
    )


class WhileStatement(Grammar):
    grammar = (
        "while", Spaces, "(", Spaces, WhileCondition,
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
            "for", Spaces, "(",
            Spaces, OPTIONAL(ForInitializer), StatementTerminator,
            Spaces, OPTIONAL(ForCondition), StatementTerminator,
            Spaces, OPTIONAL(ForIterator), Spaces, ")",
            Spaces, StatementBlock
        ),
        (
            "for", Spaces, "(",
            Spaces, OPTIONAL(ForInitializer), StatementTerminator,
            Spaces, OPTIONAL(ForCondition), Spaces, ")",
            Spaces, StatementBlock
        ),
        (
            "for", Spaces, "(", Spaces,
            OPTIONAL(ForInitializer), Spaces, ")",
            Spaces, StatementBlock
        ),
    )


class ForeachParameter(Grammar):
    grammar = "-parallel"


class ForeachStatement(Grammar):
    grammar = (
        "foreach", Spaces, OPTIONAL(ForeachParameter), Spaces,
        "(", Spaces, Variable, Spaces, "in", Spaces, Pipeline,
        Spaces, ")", Spaces, StatementBlock
    )


class SwitchClauseCondition(Grammar):
    grammar = OR(CommandArgument, PrimaryExpression)


class SwitchClause(Grammar):
    grammar = (SwitchClauseCondition, OPTIONAL(WHITESPACE), StatementBlock,
               OPTIONAL(StatementTerminators))


class SwitchClauses(Grammar):
    grammar = LIST_OF(SwitchClause, sep=Spaces)


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
    grammar = OR(
        "-regex", "-rege", "-reg", "-re", "-r", "-wildcard", "-wildcar",
        "-wildca", "-wildc", "-wild", "-wil", "-wi", "-w", "-exact",
        "-exac", "-exa", "-ex", "-e", "-casesensitive", "-casesensitiv",
        "-casesensiti", "-casesensit", "-casesensi", "-casesens",
        "-casesen", "-casese", "-cases", "-case", "-cas", "-ca", "-c"
    )


class SwitchParameters(Grammar):
    grammar = LIST_OF(SwitchParameter, sep=WHITESPACE)


class SwitchStatement(Grammar):
    grammar = (
        "switch", OPTIONAL(NewLines), OPTIONAL(WHITESPACE),
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
                      sep=(Spaces, ",", OPTIONAL(Spaces)))


class AttributeName(Grammar):
    grammar = TypeSpec


class Attribute(Grammar):
    grammar = OR(
        ("[", AttributeName, "(", AttributeArguments, OPTIONAL(NewLines),
         ")", OPTIONAL(NewLines), "]"),
        TypeLiteral
    )


class AttributeList(Grammar):
    grammar = LIST_OF(Attribute, sep=Spaces)
