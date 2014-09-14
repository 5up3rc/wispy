"""
    wispy.builder
    ~~~~~~~~~~~~~

    Contains utilities for building a CST node
    from the grammars.
"""

# pylint: disable=bad-builtin, missing-docstring
# pylint: disable=no-self-use, no-init, invalid-name

from functools import partial

from . import tree
from . import grammar

__all__ = ['build_tree', 'Builder']


def build_tree(grammar_node):
    """
    Visit the nodes from the given grammar, returning
    a new AST node.
    """
    builder = Builder()
    return builder.visit_scriptblock(grammar_node)


class Builder:
    """ An AST builder, implemented using the visitor pattern. """

    def iter_generic_visit(self, nodes, parent):
        """ Visit each nodes from the given *nodes*, by calling
        :meth:`generic_visit` for each of those nodes.
        """
        meth = partial(self.generic_visit, parent=parent)
        return list(filter(None, map(meth, nodes)))

    def generic_visit(self, node, parent):
        """ A generic visit method, which visits the given node,
        if the builder implements it. If so, it should return
        a new AST node representing the type of the node.
        """

        cls = node.__class__.__name__.replace("<", "").replace(">", "")
        visit_name = 'visit_' + cls.lower()
        visit_method = getattr(self, visit_name, None)
        if visit_method:
            return visit_method(node, parent)

    def visit_children(self, node):
        """ Visit the children of the given node, by calling
        :meth:`generic_visit` for each of them.
        Returns a list of AST nodes, representing the children.
        """
        return self.iter_generic_visit(node, node)

    def visit_scriptblock(self, node):
        """
        Visit a ScriptBlock grammar node and return the AST
        representing it.
        """
        stmts = node.find_all(grammar.Statement)
        blocks = node.find_all(grammar.NamedBlock)

        newnode = tree.ScriptBlock()
        newnode.body = self.visit_children(node)
        newnode.statements = self.iter_generic_visit(stmts, newnode)
        newnode.named_blocks = self.iter_generic_visit(blocks, newnode)
        newnode.grammar = node
        return newnode

    def visit_namedblock(self, node, parent):
        """ Visit a NamedBlock. """
        stmts = node.find_all(grammar.Statement)
        newnode = tree.NamedBlock()
        newnode.parent = parent
        newnode.block_name = tree.Name(value=node[0].string.lower())
        newnode.statements = self.iter_generic_visit(stmts, newnode)
        return newnode

    def visit_statement(self, node, parent):
        return self.generic_visit(node[0], parent)

    def visit_functionstatement(self, node, parent):
        newnode = tree.FunctionStatement()
        newnode.parent = parent
        newnode.type = tree.Name(value=node[0].string.lower())
        newnode.name = tree.Name(value=node[2].string)
        newnode.body = self.visit_scriptblock(node[8])
        newnode.body.parent = newnode
        newnode.params = self.generic_visit(node[4], newnode)
        return newnode

    def visit_ifstatement(self, node, parent):
        stmts = node.find_all(grammar.Statement)
        elifs = node.find_all(grammar.ElseIfClause)
        orelse = node.find_all(grammar.ElseClause)

        newnode = tree.IfStatement()
        newnode.parent = parent
        newnode.test = self.generic_visit(node[4], newnode)
        newnode.body = self.iter_generic_visit(stmts, newnode)
        newnode.elifs = self.iter_generic_visit(elifs, newnode)
        newnode.orelse = self.iter_generic_visit(orelse, newnode)
        return newnode

    def visit_elseclause(self, node, parent):
        # TODO
        pass

    def visit_elseifclause(self, node, parent):
        # TODO
        pass

    def visit_functionparameterdeclaration(self, node, parent):
        script_params = node[2].find_all(grammar.ScriptParameter)
        return self.iter_generic_visit(script_params, parent)

    def visit_scriptparameter(self, node, parent):
        newnode = tree.Parameter()
        attributes = node.find_all(grammar.Attribute)
        newnode.parent = parent
        newnode.variable = self.generic_visit(node[3], newnode)
        newnode.default = self.generic_visit(node[4], newnode)
        newnode.attributes = self.iter_generic_visit(
            attributes, newnode.variable)
        return newnode

    def visit_typespec(self, node, parent):
        newnode = tree.TypeSpec()
        newnode.parent = parent
        newnode.name = tree.Name(value=node[0].string)
        specs = node[1].find_all(grammar.TypeSpec) or []
        newnode.types = self.iter_generic_visit(specs, newnode)
        return newnode
