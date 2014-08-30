#! /usr/bin/env python
# -*- coding: UTF-8 -*-
"""Wispy - AST parser and inference engine for PowerShell language."""


try:
    from setuptools import setup
except ImportError:
    from distutils.core import setup

with open("README.rst") as file_handler:
    DESCRIPTION = file_handler.read()

setup(
    name="wispy",
    version="0.1a",
    description="AST parser and inference engine for PowerShell language.",
    long_description=DESCRIPTION,
    author="Alexandru Coman, Claudiu Popa, Cosmin Poieană",
    author_email=("Alexandru Coman <alex@ropython.org"
                  "Claudiu Popa <claudiu@ropython.org>"
                  "Cosmin Poieană <cmin@ropython.org>"),
    url="https://github.com/RoPython/wispy",
    packages=["wispy"],
    scripts=[],
    requires=["modgrammar"]
)
