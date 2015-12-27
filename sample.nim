# `plugin_is_GPL_compatible' indicates that its code is
# released under the GPL or compatible license; Emacs will refuse to
# load modules that don't export such a symbol.
{.emit:"int plugin_is_GPL_compatible;".}

import strutils
import emacs_module as emacs

emacs.addFunc(Fmod_test_return_t):
  env.intern(env, "t".cstring)

emacs.defuns("libsample", """
DEFUN ("mod-test-return-t", Fmod_test_return_t, 1, 1, NULL, NULL);
""")
