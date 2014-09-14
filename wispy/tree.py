"""
    wispy.tree
    ~~~~~~~~~~

    Contains the AST nodes defined by *wispy*.
"""
# pylint: disable=no-init, too-few-public-methods, missing-docstring
# pylint: disable=protected-access

from inspect import Parameter as SignatureParameter, Signature


def make_signature(names):
    """ Build a Signature object from a list of names. """
    return Signature(
        SignatureParameter(name,
                           SignatureParameter.POSITIONAL_OR_KEYWORD,
                           default=None)
        for name in names)


class NodeMeta(type):
    """
    Metaclass which reads the '_fields' attribute
    and builds a signature from it. This allows a class
    to be defined either as ::

       m = MyClass(a=1, b=2)

    or as ::

       m = MyClass()
       m.a = 1
       m.b = 2
    """

    def __new__(mcs, name, bases, clsdict):
        clsobj = super().__new__(mcs, name, bases, clsdict)
        sig = make_signature(clsobj._fields)
        setattr(clsobj, '__signature__', sig)
        return clsobj


class Node(metaclass=NodeMeta):
    """ Base class for all CST nodes.

    Exports a couple of attributes, which can be
    find in any CST node:

    * parent: the parent of this node or None, if the node
      doesn't have a parent, as in the case of the root node.

    * grammar: The grammar from which this node was built.

    """

    parent = None
    grammar = None
    _fields = ()

    def __init__(self, *args, **kwargs):
        # pylint: disable=no-member
        bound = self.__signature__.bind(*args, **kwargs)
        for name, val in bound.arguments.items():
            setattr(self, name, val)


class Statement(Node):
    """ Node representing a statement. """


class Expression(Node):
    """ Node representing an expression. """


class ArrayExpression(Expression):
    _fields = ('body', )


class HashExpression(Expression):
    _fields = ('keys', 'values')


class Assign(Expression):
    _fields = ('targets', 'value', 'op')


class Array(Expression):
    _fields = ('elems', )


class Range(Expression):
    _fields = ('elems', )


class BinOp(Expression):
    """ This expression can be used for MultiplicativeExpression,
    AdditiveExpression, ComparisonExpression, BitwiseExpression,
    LogicalExpression.
    """
    _fields = ('left', 'right', 'op')


class LabeledStatement(Statement):
    _fields = ('name', 'stmt')


class TrapStatement(Statement):
    _fields = ('exception', 'body')


class DoStatement(Statement):
    # TODO: type is 'while' and 'until'
    _fields = ('body', 'type', 'condition')


class ForStatement(Statement):
    _fields = ('initializer', 'condition', 'iterator', 'body')


class WhileStatement(Statement):
    _fields = ('body', 'condition')


class InlinescriptStatement(Statement):
    _fields = ('body', )


class ParallelStatement(Statement):
    _fields = ('body', )


class SequenceStatement(Statement):
    _fields = ('body', )


class FunctionStatement(Statement):
    _fields = ('type', 'name', 'params', 'body')


class ForeachStatement(Statement):
    _fields = ('body', 'parameter', 'iter', 'target')


class ScriptBlock(Node):
    _fields = ('statements', 'named_blocks')


class NamedBlock(Node):
    _fields = ('block_name', 'statements')


class IfStatement(Node):
    _fields = ('test', 'body', 'elifs', 'orelse')


class ElifClause(Node):
    _fields = ('test', 'body')


class ElseClause(Node):
    _fields = ('body', )


class FlowControl(Node):
    _fields = ('keyword', 'value')


class TryExcept(Node):
    _fields = ('body', 'handlers', 'finalbody')


class CatchHandler(Node):
    _fields = ('body', 'types')


class Parameter(Node):
    _fields = ('attributes', 'variable', 'default')


class TypeSpec(Node):
    _fields = ('name', 'types')


class Name(Node):
    _fields = ('value', )


class String(Node):
    _fields = ('value', )


class Number(Node):
    _fields = ('value', )
