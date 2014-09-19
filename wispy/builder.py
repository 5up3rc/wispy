"""
    wispy.builder
    ~~~~~~~~~~~~~

    Contains utilities for building a CST node
    from the grammars.
"""

# pylint: disable=bad-builtin, missing-docstring
# pylint: disable=no-self-use, no-init, invalid-name
# pylint: disable=too-many-public-methods, unused-argument

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


def _set_info(ast, grammar_node, parent):
    ast.parent = parent
    ast.grammar = grammar_node


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
                        _set_info(child, node, parent)
                else:
                    _set_info(ast, node, parent)
            return ast

    def visit_children(self, node):
        """ Visit the children of the given node, by calling
        :meth:`generic_visit` for each of them.
        Returns a list of AST nodes, representing the children.
        """
        return self.iter_generic_visit(node, node)

    def visit_statement(self, node, parent):
        if node[0].grammar_desc == '<GRAMMAR>':
            child = node[0][0]
        else:
            child = node[0]
        return self.generic_visit(child, parent)

    # TODO: add visit_expression(self, node, parent)
    # TODO: add visit_array_expression(self, node, parent)
    # TODO: add visit_hash_expression(self, node, parent)
    # TODO: add visit_assign(self, node, parent)
    # TODO: add visit_array(self, node, parent)
    # TODO: add visit_range(self, node, parent)
    # TODO: add visit_unary_op(self, node, parent)
    # TODO: add visit_bin_op(self, node, parent)
    # TODO: add visit_for_statement(self, node, parent)

    def visit_trap_statement(self, node, parent):
        newnode = tree.TrapStatement()
        statements = node.find_all(grammar.Statement)
        newnode.body = self.iter_generic_visit(statements, newnode)
        if node[2]:
            newnode.exception = self.generic_visit(node[2], newnode)
        return newnode

    def visit_while_statement(self, node, parent):
        newnode = tree.WhileStatement()
        condition = node.find(grammar.WhileCondition)
        newnode.condition = self.generic_visit(condition, newnode)
        statements = node.find_all(grammar.Statement)
        newnode.body = self.iter_generic_visit(statements, newnode)
        return newnode

    def visit_labeled_statement(self, node, parent):
        newnode = tree.LabeledStatement()
        newnode.stmt = self.generic_visit(node[1], newnode)
        newnode.label = tree.Name(value=node[0]) if node[0] else None
        return newnode

    def visit_do_statement(self, node, parent):
        newnode = tree.DoStatement()
        condition = node.find(grammar.WhileCondition)
        statements = node.find_all(grammar.Statement)
        newnode.condition = self.generic_visit(condition, newnode)
        newnode.body = self.iter_generic_visit(statements, newnode)
        # TODO: Name or simple string?
        newnode.type = node[4].string.lower()
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

    def visit_function_statement(self, node, parent):
        stmts = node[8].find_all(grammar.Statement)
        newnode = tree.FunctionStatement()
        newnode.type = tree.Name(value=node[0].string.lower())
        newnode.name = tree.Name(value=node[2].string)
        newnode.body = self.iter_generic_visit(stmts, newnode)
        newnode.params = self.generic_visit(node[4], newnode)
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

    def visit_switch_statement(self, node, parent):
        newnode = tree.SwitchStatement()
        newnode.params = self.iter_generic_visit(node[3], newnode)
        newnode.condition = self.iter_generic_visit(node[5], newnode)
        newnode.body = self.iter_generic_visit(node[7], newnode)
        return newnode

    def visit_script_block(self, node):
        """
        Visit a ScriptBlock grammar node and return the AST
        representing it.
        """
        stmts = node.find_all(grammar.Statement)
        blocks = node.find_all(grammar.NamedBlock)

        newnode = tree.ScriptBlock()
        newnode.body = self.iter_generic_visit(stmts, newnode)
        newnode.named_blocks = self.iter_generic_visit(blocks, newnode)
        newnode.grammar = node
        return newnode

    def visit_named_block(self, node, parent):
        """ Visit a NamedBlock. """
        stmts = node.find_all(grammar.Statement)
        newnode = tree.NamedBlock()
        newnode.block_name = tree.Name(value=node[0].string.lower())
        newnode.body = self.iter_generic_visit(stmts, newnode)
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

    # TODO: add visit_data_statement(self, node, parent)
    def visit_elseif_clause(self, node, parent):
        newnode = tree.ElifClause()
        newnode.test = self.iter_generic_visit(node[5], newnode)
        newnode.body = self.iter_generic_visit(node[9], newnode)
        return newnode

    def visit_else_clause(self, node, parent):
        newnode = tree.ElseClause()
        statements = node.find_all(grammar.Statement)
        newnode.body = self.iter_generic_visit(statements, newnode)
        return newnode

    def visit_flow_control(self, node, parent):
        newnode = tree.FlowControl()
        newnode.keyword = tree.Name(value=node[0].string)
        value = node.find(grammar.LabelExpression)
        if not value:
            value = node.find(grammar.Pipeline)
        newnode.value = self.iter_generic_visit(value, newnode)
        return newnode

    def visit_try_except(self, node, parent):
        statements = node.find_all(grammar.Statement)
        handlers = node.find_all(grammar.CatchClauses)
        final = node.find(grammar.FinallyClause)
        newnode = tree.TryExcept()
        newnode.body = self.iter_generic_visit(statements, newnode)
        newnode.handlers = self.iter_generic_visit(handlers, newnode)
        newnode.finalbody = self.iter_generic_visit(final, newnode)
        return newnode

    def visit_catch_handler(self, node, parent):
        statements = node.find_all(grammar.Statement)
        newnode = tree.CatchHandler()
        newnode.body = self.iter_generic_visit(statements, newnode)
        newnode.types = self.iter_generic_visit(node[2], newnode)
        return newnode

    # TODO: add visit_parameter(self, node, parent)
    def visit_type_spec(self, node, parent):
        newnode = tree.TypeSpec()
        newnode.name = tree.Name(value=node[0].string)
        specs = node[1].find_all(grammar.TypeSpec) or []
        newnode.types = self.iter_generic_visit(specs, newnode)
        return newnode

    # TODO: add visit_name
    # TODO: add visit_string
    # TODO: add visit_number
    # TODO: add visit_argument
    # TODO: add visit_subscript

    def visit_variable(self, node, parent):
        scope = None
        if isinstance(node[0], Literal):
            name = node[0].string
        else:
            if isinstance(node[0][1], grammar.VariableScope):
                scope = node[0][1].string.strip(":")
            name = node[0][0].string + node[0][2].string
        return tree.Variable(scope=scope, name=name)

    # TODO: add_visit_getattr

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
