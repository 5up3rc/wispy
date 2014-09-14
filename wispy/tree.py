"""
    wispy.tree
    ~~~~~~~~~~

    Contains the AST nodes defined by *wispy*.
"""
# pylint: disable=no-init, too-few-public-methods, missing-docstring

class Node:
    """ Base class for all CST nodes.

    Exports a couple of attributes, which can be
    find in any CST node:

    * parent: the parent of this node or None, if the node
      doesn't have a parent, as in the case of the root node.

    * grammar: The grammar from which this node was built.

    """

    parent = None
    grammar = None


class ScriptBlock(Node):
    _fields = ('statements', 'named_blocks')


class NamedBlock(Node):
    _fields = ('block_name', 'statements')
