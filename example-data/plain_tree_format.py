# -*- coding: utf-8 -*-
#
# Copyright (c) 2015 Jonathan M. Lange <jml@mumak.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Simple Python file that we can use as an example.
#
# This one is the same as tree_format.py, but has had its unicode literals
# stripped, in order to work-around
# https://github.com/bjpop/language-python/pull/25

"""Library for formatting trees."""

import itertools


FORK = '\u251c'
LAST = '\u2514'
VERTICAL = '\u2502'
HORIZONTAL = '\u2500'


def _format_tree(node, format_node, get_children, prefix=''):
    children = get_children(node)
    next_prefix = ''.join([prefix, VERTICAL, '   '])
    for child in children[:-1]:
        yield ''.join([prefix, FORK, HORIZONTAL, HORIZONTAL, ' ', format_node(child)])
        for result in _format_tree(child, format_node, get_children, next_prefix):
            yield result
    if children:
        last_prefix = ''.join([prefix, '    '])
        yield ''.join([prefix, LAST, HORIZONTAL, HORIZONTAL, ' ', format_node(children[-1])])
        for result in _format_tree(children[-1], format_node, get_children, last_prefix):
            yield result


def format_tree(node, format_node, get_children):
    lines = itertools.chain(
        [format_node(node)],
        _format_tree(node, format_node, get_children),
        [''],
    )
    return '\n'.join(lines)


def print_tree(node, format_node, get_children):
    print format_tree(node, format_node, get_children)
