"""
    wispy.builder
    ~~~~~~~~~~~~~

    Contains utilities for building a CST node
    from the grammars.
"""

# pylint: disable=bad-builtin, missing-docstring
# pylint: disable=no-self-use, no-init, invalid-name

from functools import partial

from modgrammar import Literal

from . import tree
from . import grammar

__all__ = ['build_tree', 'Builder']


def to_underscore(string):
    new_string = []
    for char in string:
        if char.isupper():
            char = char.lower()
            new_string.append("_")
        new_string.append(char)
    return "".join(new_string).strip("_")


def build_tree(grammar_node):
    """
    Visit the nodes from the given grammar, returning
    a new AST node.
    """
    builder = Builder()
    return builder.visit_script_block(grammar_node)


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
        if node is None:
            return
        cls = node.__class__.__name__.replace("<", "").replace(">", "")
        visit_name = 'visit_' + to_underscore(cls)
        visit_method = getattr(self, visit_name, None)
        if visit_method:
            ast = visit_method(node, parent)
            if ast:
                if isinstance(ast, list):
                    for child in ast:
                        child.parent = parent
                        child.grammar = node
                else:
                    ast.parent = parent
                    ast.grammar = node
            return ast

    def visit_children(self, node):
        """ Visit the children of the given node, by calling
        :meth:`generic_visit` for each of them.
        Returns a list of AST nodes, representing the children.
        """
        return self.iter_generic_visit(node, node)

    def visit_variable(self, node, parent):
        scope = None
        if isinstance(node[0], Literal):
            name = node[0].string
        else:
            if isinstance(node[0][1], grammar.VariableScope):
                scope = node[0][1].string.strip(":")
            name = node[0][0].string + node[0][2].string
        return tree.Variable(scope=scope, name=name)

    def visit_script_block(self, node):
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

    def visit_named_block(self, node, parent):
        """ Visit a NamedBlock. """
        stmts = node.find_all(grammar.Statement)
        newnode = tree.NamedBlock()
        newnode.block_name = tree.Name(value=node[0].string.lower())
        newnode.statements = self.iter_generic_visit(stmts, newnode)
        return newnode

    def visit_statement(self, node, parent):
        if node[0].grammar_desc == '<GRAMMAR>':
            child = node[0][0]
        else:
            child = node[0]
        return self.generic_visit(child, parent)

    def visit_function_statement(self, node, parent):
        stmts = node[8].find_all(grammar.Statement)
        newnode = tree.FunctionStatement()
        newnode.type = tree.Name(value=node[0].string.lower())
        newnode.name = tree.Name(value=node[2].string)
        newnode.body = self.iter_generic_visit(stmts, newnode)
        newnode.params = self.generic_visit(node[4], newnode)
        return newnode

    def visit_if_statement(self, node, parent):
        stmts = node.find_all(grammar.Statement)
        elifs = node.find_all(grammar.ElseIfClause)
        orelse = node.find_all(grammar.ElseClause)

        newnode = tree.IfStatement()
        # TODO: 4?
        newnode.test = self.generic_visit(node[4], newnode)
        newnode.body = self.iter_generic_visit(stmts, newnode)
        newnode.elifs = self.iter_generic_visit(elifs, newnode)
        newnode.orelse = self.iter_generic_visit(orelse, newnode)
        return newnode

    def visit_else_clause(self, node, parent):
        # TODO
        pass

    def visit_elseif_clause(self, node, parent):
        # TODO
        pass

    def visit_function_parameter_declaration(self, node, parent):
        # TODO: 2
        script_params = node[2].find_all(grammar.ScriptParameter)
        return self.iter_generic_visit(script_params, parent)

    def visit_script_parameter(self, node, parent):
        newnode = tree.Parameter()
        attributes = node.find_all(grammar.Attribute)
        # TODO: 3, 4?
        newnode.variable = self.generic_visit(node[3], newnode)
        newnode.default = self.generic_visit(node[4], newnode)
        newnode.attributes = self.iter_generic_visit(
            attributes, newnode.variable)
        return newnode

    def visit_type_spec(self, node, parent):
        newnode = tree.TypeSpec()
        newnode.name = tree.Name(value=node[0].string)
        specs = node[1].find_all(grammar.TypeSpec) or []
        newnode.types = self.iter_generic_visit(specs, newnode)
        return newnode

    def visit_while_statement(self, node, parent):
        newnode = tree.WhileStatement()
        condition = node.find(grammar.WhileCondition)
        newnode.condition = self.generic_visit(condition, newnode)
        statements = node.find_all(grammar.Statement)
        newnode.body = self.iter_generic_visit(statements, newnode)
        return newnode

    def visit_inlinescript_statement(self, node, parent):
        newnode = tree.InlinescriptStatement()
        statements = node.find_all(grammar.Statement)
        newnode.body = self.iter_generic_visit(statements, newnode)
        return newnode

    def visit_parallel_statement(self, node, parent):
        newnode = tree.ParallelStatement()
        statements = node.find_all(grammar.Statement)
        newnode.body = self.iter_generic_visit(statements, newnode)
        return newnode

    def visit_sequence_statement(self, node, parent):
        newnode = tree.SequenceStatement()
        statements = node.find_all(grammar.Statement)
        newnode.body = self.iter_generic_visit(statements, newnode)
        return newnode

    def visit_foreach_statement(self, node, parent):
        statements = node.find_all(grammar.Statement)
        parameter = node[2].string
        newnode = tree.ForeachStatement()
        newnode.body = self.iter_generic_visit(statements, newnode)
        newnode.parameter = tree.Name(value=parameter) if parameter else None
        newnode.iter = self.generic_visit(node[7], newnode)
        newnode.target = self.generic_visit(node[11], newnode)
        return newnode

    def visit_switch_condition(self, node, parent):
        filename = node.find(grammar.SwitchFilename)
        newnode = tree.SwitchCondition()
        newnode.file = tree.Name(value=filename.string) if filename else None
        condition = node.find(grammar.Pipeline)
        newnode.condition = node.generic_visit(condition, newnode)
        return newnode

    def visit_switch_clause(self, node, parent):
        newnode = tree.SwitchClause()
        statements = node.find_all(grammar.Statement)
        newnode.body = self.iter_generic_visit(statements, newnode)
        newnode.clause = self.iter_generic_visit(node[0], newnode)
        return newnode