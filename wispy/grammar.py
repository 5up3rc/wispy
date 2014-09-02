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
    LIST_OF, REF
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
    grammar = ("#", OPTIONAL(WHITESPACE), OPTIONAL(InputCharacters))


class DelimitedCommentSection(Grammar):
    grammar = OR(">", (OPTIONAL(Hashes), NotGreaterThanOrHash))


class DelimitedCommentText(Grammar):
    grammar = REPEAT(DelimitedCommentSection)


class DelimitedComment(Grammar):
    grammar = ("<#", OPTIONAL(DelimitedCommentText), Hashes, ">")


class Comment(Grammar):
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
        (Dash, "inotmatch"), (Dash, "ireplace"), (Dash, "split"),
        (Dash, "isnot"), (Dash, "isplit"), (Dash, "join"),
        (Dash, "le"), (Dash, "like"), (Dash, "lt"),
        (Dash, "match"), (Dash, "ne"), (Dash, "notcontains"),
        (Dash, "notlike"), (Dash, "notmatch"), (Dash, "replace"),
        (Dash, "is")
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
        (Dash, "and"), (Dash, "band"), (Dash, "bnot"),
        (Dash, "bor"), (Dash, "bxor"), (Dash, "not"),
        (Dash, "or"), (Dash, "xor"),
        (Dash, Dash), Dash,
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


# Attributes
class AttributeArgument(Grammar):
    # FIXME: Remove REF after the Expression grammar is added.
    grammar = OR(
        (OPTIONAL(NewLines), REF('Expression')),
        (OPTIONAL(NewLines), SimpleName, "=", OPTIONAL(NewLines),
         REF('Expression'))
    )


class AttributeArguments(Grammar):
    grammar = OR(
        AttributeArgument,
        (AttributeArgument, OPTIONAL(NewLines), ",",
         REF('AttributeArguments'))
    )


class AttributeName(Grammar):

    """The :class `AttributeName`: is a reserved attribute type or some
    implementation-defined attribute type."""
    grammar = TypeSpec


class Attribute(Grammar):

    """An attribute consists of an :class `AttributeName`: and an optional
    list of positional and named arguments.

    The positional arguments (if any) precede the named arguments.
    A positional argument consists of a :class `SimpleName`:, followed by an
    equal sign, followed by an :class `Expression`:."""

    grammar = OR(
        ("[", AttributeName, "(", AttributeArguments, OPTIONAL(NewLines),
         ")", OPTIONAL(NewLines), "]"),
        TypeLiteral
    )


class AttributeList(Grammar):
    grammar = OR(
        Attribute,
        (REF('AttributeList'), OPTIONAL(NewLines), Attribute)
    )


# Statements
class CommandName(Grammar):
    grammar = OR(GenericToken, GenericTokenWithSubexprStart)


class CommandNameExpr(Grammar):
    # FIXME: Remove REF after the PrimaryExpression grammar is added.
    grammar = OR(CommandName, REF('PrimaryExpression'))


class CommandArgument(Grammar):
    grammar = CommandNameExpr


class CommandElement(Grammar):
    grammar = OR(CommandParameter, CommandArgument, REF('Redirections'))


class CommandElements(Grammar):
    grammar = REPEAT(CommandElement)


class RedirectedFileName(Grammar):
    # FIXME: Remove REF after the PrimaryExpression grammar is added.
    grammar = OR(CommandArgument, REF('PrimaryExpression'))


class Redirection(Grammar):
    grammar = OR(
        "2>&1", "1>&2",
        (FileRedirectionOperator, RedirectedFileName)
    )


class Redirections(Grammar):
    grammar = REPEAT(Redirection)


class CommandModule(Grammar):
    # FIXME: Remove REF after the PrimaryExpression grammar is added.
    grammar = REF('PrimaryExpression')


class CommandInvocationOperator(Grammar):
    grammar = OR("&", ".")


class Command(Grammar):
    grammar = OR(
        (CommandName, OPTIONAL(CommandElements)),
        (CommandInvocationOperator, OPTIONAL(CommandModule), CommandNameExpr,
         OPTIONAL(CommandElements))
    )


class PipelineTail(Grammar):
    grammar = OR(
        ("|", OPTIONAL(NewLines), Command),
        ("|", OPTIONAL(NewLines), Command, REF('PipelineTail'))
    )


class Pipeline(Grammar):
    # FIXME: Remove REF after the Expression grammar is added.
    grammar = OR(
        REF('AssignmentExpression'),
        (REF('Expression'), OPTIONAL(Redirections), OPTIONAL(PipelineTail)),
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
    # FIXME: Remove REF after the Expression grammar is added.
    grammar = (
        OPTIONAL(NewLines), "=", OPTIONAL(NewLines), REF('Expression')
    )


class ScriptParameter(Grammar):
    grammar = (
        OPTIONAL(NewLines), OPTIONAL(AttributeList), OPTIONAL(NewLines),
        Variable, OPTIONAL(ScriptParameterDefault)
    )


class LabelExpression(Grammar):
    # FIXME: Remove REF after the UnaryExpression grammar is added.
    grammar = OR(SimpleName, REF('UnaryExpression'))


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
    grammar = OR("-regex", "-wildcard", "-exact", "-casesensitive")


class SwitchParameters(Grammar):
    grammar = REPEAT(SwitchParameter)


class SwitchFilename(Grammar):
    # FIXME: Remove REF after the PrimaryExpression grammar is added.
    grammar = OR(CommandArgument, REF('PrimaryExpression'))


class SwitchCondition(Grammar):
    grammar = OR(
        ("(", OPTIONAL(NewLines), Pipeline, OPTIONAL(NewLines), ")"),
        ("-file", OPTIONAL(NewLines), SwitchFilename)
    )


class SwitchClauseCondition(Grammar):
    # FIXME: Remove REF after the PrimaryExpression grammar is added.
    grammar = (CommandArgument, REF('PrimaryExpression'))


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
    grammar = OR(
        (
            "do", StatementBlock, OPTIONAL(NewLines), "while",
            OPTIONAL(NewLines), "(", WhileCondition, OPTIONAL(NewLines), ")"
        ),
        (
            "do", StatementBlock, OPTIONAL(NewLines), "until",
            OPTIONAL(NewLines), "(", WhileCondition, OPTIONAL(NewLines), ")"
        )
    )


class LabeledStatement(Grammar):
    grammar = OR(
        SwitchStatement,
        ForeachStatement,
        ForStatement,
        WhileStatement,
        DoStatement
    )


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
        (Pipeline, StatementTerminator)
    )


class AssignmentExpression(Grammar):
    # FIXME: Remove REF after the Expression grammar is added.
    grammar = (REF('Expression'), AssignmentOperator, Statement)


# Expressions
class ExpandableHereStringWithSubexprPart(Grammar):
    # FIXME: Remove REF after the SubExpression grammar is added.
    grammar = OR(REF('SubExpression'), ExpandableHereStringPart)


class ExpandableHereStringWithSubexprCharacters(Grammar):
    grammar = REPEAT(ExpandableHereStringWithSubexprPart)


class ExpandableStringWithSubexprPart(Grammar):
    # FIXME: Remove REF after the SubExpression grammar is added.
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


class MemberName(Grammar):
    # FIXME: Remove REF after the Value grammar is added.
    # FIXME: Remove REF after the ExpressionWithUnaryOperator grammar is added.
    grammar = OR(
        SimpleName, StringLiteral, StringLiteralWithSubexpression,
        REF('ExpressionWithUnaryOperator'), REF('Value')
    )


class RangeArgumentExpression(Grammar):
    # FIXME: Remove REF after the Expression grammar is added.
    grammar = OR(
        REF('UnaryExpression'),
        (REF('RangeExpression'), "..", OPTIONAL(NewLines),
         REF('UnaryExpression'))
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
            REF('AdditiveArgumentExpression'), "+", OPTIONAL(NewLines),
            MultiplicativeArgumentExpression
        ),
        (
            REF('AdditiveArgumentExpression'), Dash, OPTIONAL(NewLines),
            MultiplicativeArgumentExpression
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


class Expression(Grammar):
    grammar = REF('LogicalExpression')


class LogicalExpression(Grammar):
    grammar = OR(
        REF('BitwiseExpression'),
        (
            REF('LogicalExpression'), OR("-and", "-or", "-xor"),
            OPTIONAL(NewLines), REF("BitwiseExpression")
        )
    )


class BitwiseExpression(Grammar):
    grammar = OR(
        REF('ComparisonExpression'),
        (
            REF('BitwiseExpression'), OR("-band", "-bor", "-bxor"),
            OPTIONAL(NewLines), REF("ComparisonExpression")
        )
    )


class ComparisonExpression(Grammar):
    grammar = OR(
        REF("AdditiveExpression"),

        (REF("ComparisonExpression"), REF("ComparisonOperator"),
         OPTIONAL(NewLines), REF("AdditiveExpression"))
    )


class AdditiveExpression(Grammar):
    grammar = OR(
        REF("MultiplicativeExpression"),
        (
            REF("AdditiveExpression"), OR("+", Dash), OPTIONAL(NewLines),
            REF("MultiplicativeExpression")
        )

    )


class MultiplicativeExpression(Grammar):
    grammar = OR(
        REF("FormatExpression"),
        (
            REF("MultiplicativeExpression"), OR("*", "/", "%"),
            OPTIONAL(NewLines), REF("FormatExpression")
        )
    )


class FormatExpression(Grammar):
    grammar = OR(
        REF("RangeExpression"),

        (REF("FormatExpression"), REF("FormatOperator"),
         OPTIONAL(NewLines), REF("RangeExpression"))
    )


class RangeExpression(Grammar):
    grammar = OR(
        REF("ArrayLiteralExpression"),

        (REF("RangeExpression"), "..", OPTIONAL(NewLines),
         REF("ArrayLiteralExpression"))
    )


class ArrayLiteralExpression(Grammar):
    grammar = OR(
        REF("UnaryExpression"),

        (REF("UnaryExpression"), ",", OPTIONAL(NewLines),
         REF("ArrayLiteralExpression"))
    )


class UnaryExpression(Grammar):
    grammar = OR(
        REF("PrimaryExpression"),
        REF("ExpressionWithUnaryOperator")
    )


class ExpressionWithUnaryOperator(Grammar):
    grammar = OR(
        (",", OPTIONAL(NewLines), REF("UnaryExpression")),
        ("-bnot", OPTIONAL(NewLines), REF("UnaryExpression")),
        ("-not", OPTIONAL(NewLines), REF("UnaryExpression")),
        ("-split", OPTIONAL(NewLines), REF("UnaryExpression")),
        ("-join", OPTIONAL(NewLines), REF("UnaryExpression")),
        ("!", OPTIONAL(NewLines), REF("UnaryExpression")),
        ("+", OPTIONAL(NewLines), REF("UnaryExpression")),
        (Dash, OPTIONAL(NewLines), REF("UnaryExpression")),
        REF("PreIncrementExpression"),
        REF("PreDecrementExpression"),
        REF("CastExpression"),
    )


class PreIncrementExpression(Grammar):
    grammar = ("++", OPTIONAL(NewLines), REF("UnaryExpression"))


class PreDecrementExpression(Grammar):
    grammar = (Dash, Dash, OPTIONAL(NewLines), REF("UnaryExpression"))


class CastExpression(Grammar):
    grammar = (TypeLiteral, REF("UnaryExpression"))


class PrimaryExpression(Grammar):
    grammar = OR(
        REF("Value"),
        REF("MemberAccess"),
        REF("ElementAccess"),
        REF("InvocationExpression"),
        REF("PostIncrementExpression"),
        REF("PostDecrementExpression"),
    )


class Value(Grammar):
    grammar = OR(
        REF("ParenthesizedExpression"),
        REF("SubExpression"),
        REF("ArrayExpression"),
        REF("ScriptBlockExpression"),
        REF("HashLiteralExpression"),
        REF("Literal"),
        TypeLiteral,
        Variable
    )


class ParenthesizedExpression(Grammar):
    grammar = (OPTIONAL(NewLines), Pipeline, OPTIONAL(NewLines))


class SubExpression(Grammar):
    grammar = ("$(", OPTIONAL(NewLines),
               OPTIONAL(StatementList), OPTIONAL(NewLines), ")")


class ArrayExpression(Grammar):
    grammar = ("@(", OPTIONAL(NewLines), OPTIONAL(StatementList),
               OPTIONAL(NewLines), ")")


class ScriptBlockExpression(Grammar):
    grammar = ("{", OPTIONAL(NewLines), REF("ScriptBlock"),
               OPTIONAL(NewLines), "}")


class HashLiteralExpression(Grammar):
    grammar = ("@{", OPTIONAL(NewLines), OPTIONAL(REF("HashLiteralBody")),
               OPTIONAL(NewLines), "}")


class HashLiteralBody(Grammar):
    grammar = OR(
        REF("HashEntry"),
        (REF("HashLiteralBody"), StatementTerminators, REF("HashEntry"))
    )


class HashEntry(Grammar):
    grammar = (REF("KeyExpression"), "=", OPTIONAL(NewLines), Statement)


class KeyExpression(Grammar):
    grammar = OR(
        SimpleName,
        UnaryExpression
    )


class PostIncrementExpression(Grammar):
    grammar = (PrimaryExpression, "++")


class PostDecrement(Grammar):
    grammar = (PrimaryExpression, Dash, Dash)


class MemberAccess(Grammar):
    grammar = (PrimaryExpression, OR(".", "::"), REF("MemberName"))


class ElementAccess(Grammar):
    grammar = (PrimaryExpression, "[",
               OPTIONAL(NewLines), Expression, OPTIONAL(NewLines), "]")


class InvocationExpression(Grammar):
    grammar = (
        PrimaryExpression, OR(".", "::"), REF("MemberName"),
        REF("ArgumentList")
    )


class ArgumentList(Grammar):
    grammar = ("(", OPTIONAL(REF("ArgumentExpressionList")),
               OPTIONAL(NewLines), ")")


class ArgumentExpressionList(Grammar):
    grammar = OR(
        REF("ArgumentExpression"),
        (REF("ArgumentExpression"), OPTIONAL(NewLines),
         ",", REF("ArgumentExpressionList"))
    )
