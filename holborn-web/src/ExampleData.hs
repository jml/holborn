
module ExampleData (
  examplePython,
  ) where

import BasicPrelude


examplePython :: Text
examplePython = unlines (
  [ "# -*- coding: utf-8 -*-"
  , "#"
  , "# Copyright (c) 2015 Jonathan M. Lange <jml@mumak.net>"
  , "#"
  , "# Licensed under the Apache License, Version 2.0 (the \"License\");"
  , "# you may not use this file except in compliance with the License."
  , "# You may obtain a copy of the License at"
  , "#"
  , "#     http://www.apache.org/licenses/LICENSE-2.0"
  , "#"
  , "# Unless required by applicable law or agreed to in writing, software"
  , "# distributed under the License is distributed on an \"AS IS\" BASIS,"
  , "# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied."
  , "# See the License for the specific language governing permissions and"
  , "# limitations under the License."
  , ""
  , ""
  , "\"\"\"Library for formatting trees.\"\"\""
  , ""
  , "import itertools"
  , ""
  , ""
  , "FORK = u'\\u251c'"
  , "LAST = u'\\u2514'"
  , "VERTICAL = u'\\u2502'"
  , "HORIZONTAL = u'\\u2500'"
  , ""
  , ""
  , "def _format_tree(node, format_node, get_children, prefix=''):"
  , "    children = get_children(node)"
  , "    next_prefix = u''.join([prefix, VERTICAL, u'   '])"
  , "    for child in children[:-1]:"
  , "        yield u''.join([prefix, FORK, HORIZONTAL, HORIZONTAL, u' ', format_node(child)])"
  , "        for result in _format_tree(child, format_node, get_children, next_prefix):"
  , "            yield result"
  , "    if children:"
  , "        last_prefix = u''.join([prefix, u'    '])"
  , "        yield u''.join([prefix, LAST, HORIZONTAL, HORIZONTAL, u' ', format_node(children[-1])])"
  , "        for result in _format_tree(children[-1], format_node, get_children, last_prefix):"
  , "            yield result"
  , ""
  , ""
  , "def format_tree(node, format_node, get_children):"
  , "    lines = itertools.chain("
  , "        [format_node(node)],"
  , "        _format_tree(node, format_node, get_children),"
  , "        [u''],"
  , "    )"
  , "    return u'\\n'.join(lines)"
  , ""
  , ""
  , "def print_tree(node, format_node, get_children):"
  , "    print format_tree(node, format_tree, get_children)"
  ])

exampleAST :: [[Text]]
exampleAST = [["token", "FORK", "492057e481cfda05e6cb098c7b039886"], ["token", "LAST", "a7496ecada53f1fefdde09d941fd050a"], ["token", "VERTICAL", "a82605b0a1e91e9efbc37f9104f89f2c"], ["token", "HORIZONTAL", "c98d4868e769d2acd916a8b64747f2df"], ["token", "node", "ab8e13b4f60d1a3334a747c0ce727d05"], ["token", "format_node", "070541633cbe5a008747528bd6660075"], ["token", "get_children", "66769cfe421620d543a185dc05b993db"], ["token", "prefix", "40f17850faa6431023d9d4dae957ec3d"], ["token", "children", "d0b3be899a89b0447da666c4b947d518"], ["token", "get_children", "d353c65c33383c03c0fb00b700962af3"], ["token", "node", "07cf4fcb1cb6c304ee6cce4e5040d58e"], ["token", "next_prefix", "0c5ca292897e914d2f0e017ada70f8df"], ["token", "prefix", "a8d368170daa380544e7371b99035d50"], ["token", "VERTICAL", "e926802a74f79fa879191fad336cbe68"], ["token", "child", "c6329af50572b22f9a437396fe13a731"], ["token", "children", "d8d4396e16175aef25afbe0f9a7006d3"], ["token", "prefix", "3d45d47b2fcc3c33bc697a0631fbbaac"], ["token", "FORK", "41fefe312464c5b6680c6e96504984ce"], ["token", "HORIZONTAL", "a89817e279613cc35ae52ebc9de488bf"], ["token", "HORIZONTAL", "a89817e279613cc35ae52ebc9de488bf"], ["token", "format_node", "328e4f3c9e1d915ff828307a04d0a603"], ["token", "child", "c35d9b865c9eba51d8238bccc77c0e92"], ["token", "result", "a73f9beabadfd218aba9ad6312ea96ad"], ["token", "_format_tree", "e37c3ad464a5eb3e5af2c1c0db0eba4c"], ["token", "child", "a264a6256860b67b907e048de6294e15"], ["token", "format_node", "cd1d787e977a40c2dff6b924b0c25d92"], ["token", "get_children", "99818c021486785c47b47b98d0980106"], ["token", "next_prefix", "79e286c8accc1cf57beed7214abda258"], ["token", "result", "63427ab5fbf7664c54a5532962ea50ce"], ["token", "children", "d0b3be899a89b0447da666c4b947d518"], ["token", "last_prefix", "2030fab53ff6324b72008d73b2a4d9ac"], ["token", "prefix", "d33897ef8f2d6600a823d7f6b7168df6"], ["token", "prefix", "3d45d47b2fcc3c33bc697a0631fbbaac"], ["token", "LAST", "c970cb0fc5f3e409f052cdf5193df741"], ["token", "HORIZONTAL", "a89817e279613cc35ae52ebc9de488bf"], ["token", "HORIZONTAL", "a89817e279613cc35ae52ebc9de488bf"], ["token", "format_node", "328e4f3c9e1d915ff828307a04d0a603"], ["token", "children", "008fda851d63ed8887f090ee2e0f8361"], ["token", "result", "a73f9beabadfd218aba9ad6312ea96ad"], ["token", "_format_tree", "e37c3ad464a5eb3e5af2c1c0db0eba4c"], ["token", "children", "910a43a75de20bd8295c79409227b9a7"], ["token", "format_node", "cd1d787e977a40c2dff6b924b0c25d92"], ["token", "get_children", "99818c021486785c47b47b98d0980106"], ["token", "last_prefix", "5a1f0dcefab20fc4f44c73d628281f90"], ["token", "result", "63427ab5fbf7664c54a5532962ea50ce"], ["token", "node", "ab8e13b4f60d1a3334a747c0ce727d05"], ["token", "format_node", "070541633cbe5a008747528bd6660075"], ["token", "get_children", "66769cfe421620d543a185dc05b993db"], ["token", "lines", "4a85938f0fcb423d6e6adfb2408e5232"], ["token", "itertools", "ab06ddfd61402c384a94c922e54589d9"], ["token", "format_node", "731b85bbd74800d5cb013714907da445"], ["token", "node", "0107e4cf479bc28b66dd4cbdb86c9c55"], ["token", "_format_tree", "e37c3ad464a5eb3e5af2c1c0db0eba4c"], ["token", "node", "ed700877cc1124320ebe5bfb2a641c47"], ["token", "format_node", "cd1d787e977a40c2dff6b924b0c25d92"], ["token", "get_children", "99818c021486785c47b47b98d0980106"], ["token", "lines", "d64dc81be92f90f4aa1f3bc5c442e4d5"], ["token", "node", "ab8e13b4f60d1a3334a747c0ce727d05"], ["token", "format_node", "070541633cbe5a008747528bd6660075"], ["token", "get_children", "66769cfe421620d543a185dc05b993db"], ["token", "format_tree", "eb328baf84b0d245d17478861975d951"], ["token", "node", "07cf4fcb1cb6c304ee6cce4e5040d58e"], ["token", "format_tree", "eb328baf84b0d245d17478861975d951"], ["token", "get_children", "d353c65c33383c03c0fb00b700962af3"]]
