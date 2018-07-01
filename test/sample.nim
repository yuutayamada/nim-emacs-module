#[

License:

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

]#

# `plugin_is_GPL_compatible` indicates that its code is
# released under the GPL or compatible license; Emacs will refuse to
# load modules that don't export such a symbol.
{.emit:"int plugin_is_GPL_compatible;".}

import emacs_module             # primitive wrapper for emacs_module.h

init(emacs)

# intern
emacs.defun(mod_test_return_t, 1):
  env.intern(env, "t".cstring)

# extract_integer
emacs.defun(mod_test_sum, 2):
  assert(nargs == 2)
  let
    a = env.extract_integer(env, args[0])
    b = env.extract_integer(env, args[1])
    s = a + b

  env.make_integer(env, s)

# vec_size
emacs.defun(mod_test_vector_fill, 2):
  var
    vec = args[0]
    val = args[1]
    size = env.vec_size(env, vec)

  for i in 0 ..< size:
    env.vec_set(env, vec, i, val)
  result = env.intern(env, "t")

# vec_get
emacs.defun(mod_test_vector_eq, 2):
  var
    vec = args[0]
    val = args[1]
    size = env.vec_size(env, vec)
  for i in 0 ..< size:
    if not env.eq(env, env.vec_get(env, vec, i), val):
      result = env.intern(env, "nil")
  result = env.intern(env, "t")

# non_local_exit_signal
emacs.defun(mod_test_signal, 0):
  assert(env.non_local_exit_check(env) == emacs_funcall_exit_return)
  env.non_local_exit_signal(env, env.intern(env, "error"),
                            env.make_integer(env, 100))

emacs.defun(mod_test_throw, 0):
  assert(env.non_local_exit_check(env) == emacs_funcall_exit_return)
  env.non_local_exit_throw(env, env.intern(env, "tag"),
                           env.make_integer(env, 42))
  result = env.intern(env, "nil")

# copy_string_contents
emacs.defun(mod_test_return_uname_cmd, 1):
  var l: ptrdiff_t
  if (env.copy_string_contents(env, args[0], nil, addr l)):
    var buf1 = newString(l-1)
    if (env.copy_string_contents(env, args[0], addr buf1[0], addr l)):
      var res = "uname " & $buf1
      result = env.make_string(env, addr res[0], res.len)

from osproc import execCmdEx
from strutils import strip
emacs.defun(mod_test_return_uname, 1):
  var l: ptrdiff_t
  if (env.copy_string_contents(env, args[0], nil, addr l)):
    var buf1 = newString(l-1)
    if (env.copy_string_contents(env, args[0], addr buf1[0], addr l)):
      var (res, _) = execCmdEx("uname " & $buf1 )
      res = res.strip()
      result = env.make_string(env, addr res[0], res.len)

emacs.provide()
