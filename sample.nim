# `plugin_is_GPL_compatible' indicates that its code is
# released under the GPL or compatible license; Emacs will refuse to
# load modules that don't export such a symbol.
{.emit:"int plugin_is_GPL_compatible;".}

import strutils, emacs_module
import emextra as emacs
from osproc import execCmdEx

emacs.addFunc(Fmod_test_return_t, 0):
  env.intern(env, "t".cstring)

emacs.addFunc(Fmod_test_return_uname_cmd, 1):
  var len: ptrdiff_t
  if (env.copy_string_contents(env, args[0], nil, addr len)):
    var buf1 = newString(len)
    if (env.copy_string_contents(env, args[0], addr buf1[0], addr len)):
      var res = "uname " & $buf1
      result = env.make_string(env, addr res[0], res.len - 1 )

emacs.addFunc(Fmod_test_return_uname, 1):
  var len: ptrdiff_t
  if (env.copy_string_contents(env, args[0], nil, addr len)):
    var buf1 = newString(len)
    if (env.copy_string_contents(env, args[0], addr buf1[0], addr len)):
      var (res, errC) = execCmdEx("uname " & $buf1 )
      result = env.make_string(env, addr res[0], res.len - 1)

emacs.addFunc(Fmod_test_sum, 1):
  assert(nargs == 2)
  let
    a = env.extract_integer(env, args[0])
    b = env.extract_integer(env, args[1])
    s = a + b

  env.make_integer(env, s)

emacs.addFunc(Fmod_test_vector_fill, 2):
  var
    vec = args[0]
    val = args[1]
    size = env.vec_size(env, vec)

  for i in 0 ..< size:
    env.vec_set(env, vec, i, val)
  result = env.intern(env, "t")


emacs.addFunc(Fmod_test_vector_eq, 2):
  var
    vec = args[0]
    val = args[1]
    size = env.vec_size(env, vec)
  for i in 0 ..< size:
    if not env.eq(env, env.vec_get(env, vec, i), val):
      result = env.intern(env, "nil")
  result = env.intern(env, "t")

emacs.defuns("libsample", """
DEFUN ("mod-test-return-t", Fmod_test_return_t, 1, 1, NULL, NULL);
DEFUN ("mod-test-return-uname", Fmod_test_return_uname, 1, 1, NULL, NULL);
DEFUN ("mod-test-return-uname-cmd", Fmod_test_return_uname_cmd, 1, 1, NULL, NULL);
DEFUN ("mod-test-sum", Fmod_test_sum, 2, 2, NULL, NULL);
DEFUN ("mod-test-vector-fill", Fmod_test_vector_fill, 2, 2, NULL, NULL);
DEFUN ("mod-test-vector-eq", Fmod_test_vector_eq, 2, 2, NULL, NULL);
""")
