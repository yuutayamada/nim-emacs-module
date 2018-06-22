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
import emacs_module/helpers     # helper library

init(emacs)

#[
struct emacs_env_26
{

  /* Structure size (for version checking).  */
  ptrdiff_t size;

  /* Private data; users should not touch this.  */
  struct emacs_env_private *private_members;
]#

#[
  /* Memory management.  */

  emacs_value (*make_global_ref) (emacs_env *env,
				  emacs_value any_reference)
    EMACS_ATTRIBUTE_NONNULL(1);

  void (*free_global_ref) (emacs_env *env,
			   emacs_value global_reference)
    EMACS_ATTRIBUTE_NONNULL(1);
]#

#[
  /* Non-local exit handling.  */

  #[
      Exit status enum:

      /* Function has returned normally.  */
      emacs_funcall_exit_return = 0,

      /* Function has signaled an error using `signal'.  */
      emacs_funcall_exit_signal = 1,

      /* Function has exit using `throw'.  */
      emacs_funcall_exit_throw = 2,
  ]#

  enum emacs_funcall_exit (*non_local_exit_check) (emacs_env *env)
    EMACS_ATTRIBUTE_NONNULL(1);

  void (*non_local_exit_clear) (emacs_env *env)
    EMACS_ATTRIBUTE_NONNULL(1);

  enum emacs_funcall_exit (*non_local_exit_get)
    (emacs_env *env,
     emacs_value *non_local_exit_symbol_out,
     emacs_value *non_local_exit_data_out)
    EMACS_ATTRIBUTE_NONNULL(1, 2, 3);

  void (*non_local_exit_signal) (emacs_env *env,
				 emacs_value non_local_exit_symbol,
				 emacs_value non_local_exit_data)
    EMACS_ATTRIBUTE_NONNULL(1);

  void (*non_local_exit_throw) (emacs_env *env,
				emacs_value tag,
				emacs_value value)
    EMACS_ATTRIBUTE_NONNULL(1);
]#

# non_local_exit_check, non_local_exit_signal
emacs.defun(signal, 0):
  assert(env.non_local_exit_check(env) == emacs_funcall_exit_return)
  env.non_local_exit_signal(env, env.intern(env, "error"),
                            env.make_integer(env, 100))

# non_local_exit_check, non_local_exit_throw
emacs.defun(throw, 0):
  assert(env.non_local_exit_check(env) == emacs_funcall_exit_return)
  env.non_local_exit_throw(env, env.intern(env, "tag"),
                           env.make_integer(env, 42))
  result = env.intern(env, "nil")

#[
  /* Function registration.  */

  emacs_value (*make_function) (emacs_env *env,
				ptrdiff_t min_arity,
				ptrdiff_t max_arity,
				emacs_value (*function) (emacs_env *env,
							 ptrdiff_t nargs,
							 emacs_value args[],
							 void *)
				  EMACS_NOEXCEPT
                                  EMACS_ATTRIBUTE_NONNULL(1),
				const char *documentation,
				void *data)
    EMACS_ATTRIBUTE_NONNULL(1, 4);

  emacs_value (*funcall) (emacs_env *env,
                          emacs_value function,
                          ptrdiff_t nargs,
                          emacs_value args[])
    EMACS_ATTRIBUTE_NONNULL(1);

  emacs_value (*intern) (emacs_env *env,
                         const char *symbol_name)
    EMACS_ATTRIBUTE_NONNULL(1, 2);
]#

emacs.defun(return_t, 1):
  env.intern(env, "t".cstring)

#[
  /* Type conversion.  */

  emacs_value (*type_of) (emacs_env *env,
			  emacs_value value)
    EMACS_ATTRIBUTE_NONNULL(1);

  bool (*is_not_nil) (emacs_env *env, emacs_value value)
    EMACS_ATTRIBUTE_NONNULL(1);

  bool (*eq) (emacs_env *env, emacs_value a, emacs_value b)
    EMACS_ATTRIBUTE_NONNULL(1);

  intmax_t (*extract_integer) (emacs_env *env, emacs_value value)
    EMACS_ATTRIBUTE_NONNULL(1);

  emacs_value (*make_integer) (emacs_env *env, intmax_t value)
    EMACS_ATTRIBUTE_NONNULL(1);

  double (*extract_float) (emacs_env *env, emacs_value value)
    EMACS_ATTRIBUTE_NONNULL(1);

  emacs_value (*make_float) (emacs_env *env, double value)
    EMACS_ATTRIBUTE_NONNULL(1);
]#

emacs.defun(get_type, 1):
  ## Returns the Emacs-Lisp symbol of the argument type.
  env.type_of(env, args[0])

emacs.defun(is_true, 1):
  ## Returns ``t`` if argument is non-nil, else returns ``nil``.
  if env.is_not_nil(env, args[0]):
    env.intern(env, "t")
  else:
    env.intern(env, "nil")

emacs.defun(sum, 2):
  ## Returns the sum of two integers.
  assert(nargs == 2)
  let
    a = env.extract_integer(env, args[0])
    b = env.extract_integer(env, args[1])
  env.make_integer(env, a + b)

#[
  /* Create a Lisp string from a utf8 encoded string.  */
  emacs_value (*make_string) (emacs_env *env,
			      const char *contents, ptrdiff_t length)
    EMACS_ATTRIBUTE_NONNULL(1, 2);
]#

# make_string
emacs.defun(lazy, 0):
  ## Returns a string constant.
  var str = "The quick brown fox jumped over the lazy dog."
  return env.make_string(env, addr str[0], str.len)

#[
  /* Copy the content of the Lisp string VALUE to BUFFER as an utf8
     null-terminated string.

     SIZE must point to the total size of the buffer.  If BUFFER is
     NULL or if SIZE is not big enough, write the required buffer size
     to SIZE and return true.

     Note that SIZE must include the last null byte (e.g. "abc" needs
     a buffer of size 4).

     Return true if the string was successfully copied.  */

  bool (*copy_string_contents) (emacs_env *env,
                                emacs_value value,
                                char *buffer,
                                ptrdiff_t *size_inout)
    EMACS_ATTRIBUTE_NONNULL(1, 4);
]#

# copy_string_contents, make_string
emacs.defun(hello, 1):
  ## Returns "Hello " prefixed to the passed string argument.
  var l: ptrdiff_t
  if (env.copy_string_contents(env, args[0], nil, addr l)):
    var name = newString(l-1)
    if (env.copy_string_contents(env, args[0], addr name[0], addr l)):
      var res = "Hello " & name
      result = env.make_string(env, addr res[0], res.len)

from osproc import execCmdEx
from strutils import strip
emacs.defun(uname, 1):
  ## Returns the output of the ``uname`` command run the passed string argument.
  var l: ptrdiff_t
  if (env.copy_string_contents(env, args[0], nil, addr l)):
    var unameArg = newString(l-1)
    if (env.copy_string_contents(env, args[0], addr unameArg[0], addr l)):
      var (res, _) = execCmdEx("uname " & unameArg)
      res = res.strip()
      result = env.make_string(env, addr res[0], res.len)

#[
  /* Embedded pointer type.  */
  emacs_value (*make_user_ptr) (emacs_env *env,
				void (*fin) (void *) EMACS_NOEXCEPT,
				void *ptr)
    EMACS_ATTRIBUTE_NONNULL(1);

  void *(*get_user_ptr) (emacs_env *env, emacs_value uptr)
    EMACS_ATTRIBUTE_NONNULL(1);
  void (*set_user_ptr) (emacs_env *env, emacs_value uptr, void *ptr)
    EMACS_ATTRIBUTE_NONNULL(1);

  void (*(*get_user_finalizer) (emacs_env *env, emacs_value uptr))
    (void *) EMACS_NOEXCEPT EMACS_ATTRIBUTE_NONNULL(1);
  void (*set_user_finalizer) (emacs_env *env,
			      emacs_value uptr,
			      void (*fin) (void *) EMACS_NOEXCEPT)
    EMACS_ATTRIBUTE_NONNULL(1);
]#

#[
  /* Vector functions.  */
  emacs_value (*vec_get) (emacs_env *env, emacs_value vec, ptrdiff_t i)
    EMACS_ATTRIBUTE_NONNULL(1);

  void (*vec_set) (emacs_env *env, emacs_value vec, ptrdiff_t i,
		   emacs_value val)
    EMACS_ATTRIBUTE_NONNULL(1);

  ptrdiff_t (*vec_size) (emacs_env *env, emacs_value vec)
    EMACS_ATTRIBUTE_NONNULL(1);
]#

# vec_get
emacs.defun(vector_eq, 2):
  var
    vec = args[0]
    val = args[1]
    size = env.vec_size(env, vec)
  for i in 0 ..< size:
    if not env.eq(env, env.vec_get(env, vec, i), val):
      result = env.intern(env, "nil")
  result = env.intern(env, "t")

# vec_size
emacs.defun(vector_fill, 2):
  var
    vec = args[0]
    val = args[1]
    size = env.vec_size(env, vec)

  for i in 0 ..< size:
    env.vec_set(env, vec, i, val)
  result = env.intern(env, "t")

#[
  /* Returns whether a quit is pending.  */
  bool (*should_quit) (emacs_env *env)
    EMACS_ATTRIBUTE_NONNULL(1);
};
]#


emacs.provide()                 # (provide 'modtest)
