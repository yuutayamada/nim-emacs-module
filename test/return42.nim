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

##### END OF BOILERPLATE CODE #####


# Return 42
# This sample Nim program mimics the example in
# http://diobla.info/blog-archive/modules-tut.html.

#[ C code from http://diobla.info/blog-archive/modules-tut.html

    static emacs_value
    Fmymod_test (emacs_env *env, int nargs, emacs_value args[], void *data)
    {
      return env->make_integer (env, 42);
    }
]#
# This will be accessed as (return42-return42) from Emacs.
# For functions in foo.nim, all Emacs-side references of those get
# prepended with "foo-".
emacs.defun(return42, 0):
  env.make_integer(env, 42)


### You always need the below. If this file's name is foo.nim, below
### acts like the Elisp (provide 'foo).
emacs.provide()
