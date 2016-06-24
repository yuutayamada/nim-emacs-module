# `plugin_is_GPL_compatible` indicates that its code is
# released under the GPL or compatible license; Emacs will refuse to
# load modules that don't export such a symbol.
{.emit:"int plugin_is_GPL_compatible;".}

import emacs_module # primitive wrapper for emacs_module.h
import emextra      # helper library

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
  var len: ptrdiff_t
  if (env.copy_string_contents(env, args[0], nil, addr len)):
    var buf1 = newString(len)
    if (env.copy_string_contents(env, args[0], addr buf1[0], addr len)):
      var res = "uname " & $buf1
      result = env.make_string(env, addr res[0], res.len - 1 )

from osproc import execCmdEx
emacs.defun(mod_test_return_uname, 1):
  var len: ptrdiff_t
  if (env.copy_string_contents(env, args[0], nil, addr len)):
    var buf1 = newString(len)
    if (env.copy_string_contents(env, args[0], addr buf1[0], addr len)):
      var (res, _) = execCmdEx("uname " & $buf1 )
      result = env.make_string(env, addr res[0], res.len - 1)

emacs.provide("sample")