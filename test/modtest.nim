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

# Official test file for Emacs Modules:
#   http://git.savannah.gnu.org/cgit/emacs.git/tree/test/data/emacs-module/mod-test.c

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

emacs.defun(globref_make, 0):
  ## Make a big string and make it global.
  var str: string = ""
  for i in 0 ..< (26 * 100):
    str.add(char('a'.ord + (i mod 26)))
  let lispStr = env.make_string(env, addr str[0], str.len)
  return env.make_global_ref(env, lispStr)

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

# non_local_exit_get, non_local_exit_clear
emacs.defun(non_local_exit_funcall, 1):
  ## Call argument function (which takes 0 arguments), catch all
  ## non-local exists and return either normal result or a list
  ## describing the non-local exit.
  let elispFuncallRet = env.funcall(env, args[0], 0, nil)
  var
    non_local_exit_symbol, non_local_exit_data: emacs_value
  let exitCode: emacs_funcall_exit =
    env.non_local_exit_get(env,
                           addr non_local_exit_symbol,
                           addr non_local_exit_data)

  case exitCode
  of emacs_funcall_exit_return:
    return elispFuncallRet
  of emacs_funcall_exit_signal:
    env.non_local_exit_clear(env)
    let
      Flist = env.intern(env, "list")
      listArgs: array[3, emacs_value] = [env.intern(env, "signal"),
                                         non_local_exit_symbol,
                                         non_local_exit_data]
    # TODO: Need to understand why unsafeAddr needs to be used below
    # instead of addr. Using addr gives this error: expression has no
    # address; maybe use 'unsafeAddr'. Though.. with unsafeAddr, this
    # function seems to work fine.
    return env.funcall(env, Flist, 3, unsafeAddr listArgs[0])
  of emacs_funcall_exit_throw:
    env.non_local_exit_clear(env)
    let
      Flist = env.intern(env, "list")
      listArgs: array[3, emacs_value] = [env.intern(env, "throw"),
                                         non_local_exit_symbol,
                                         non_local_exit_data]
    return env.funcall(env, Flist, 3, unsafeAddr listArgs[0])

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

emacs.defun(make_string, 2):
  ## Returns string created by Emacs-Lisp ``make-string``.
  let
    fSymbol = env.intern(env, "make-string") # Get 'make-string
  return env.funcall(env, fSymbol, nargs, addr args[0]) # Return the (funcall make-string ..) returned elisp string

emacs.defun(return_t, 1):
  env.intern(env, "t")

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
  ## See http://git.savannah.gnu.org/cgit/emacs.git/tree/src/data.c?id=5583e6460c38c5d613e732934b066421349a5259#n204.
  env.type_of(env, args[0])

emacs.defun(is_true, 1):
  ## Returns ``t`` if argument is non-nil, else returns ``nil``.
  if env.is_not_nil(env, args[0]):
    env.intern(env, "t")
  else:
    env.intern(env, "nil")

emacs.defun(eq, 2):
  ## Returns ``t`` if both arguments are the same Lisp object, else returns ``nil``.
  ## Note that this returns the value of Emacs-Lisp ``eq``, not ``equal``.
  if env.eq(env, args[0], args[1]):
    env.intern(env, "t")
  else:
    env.intern(env, "nil")

emacs.defun(sum, 2):
  ## Returns the sum of two integers.
  # assert(nargs == 2)
  # The above assert statement is never reached! Emacs throws the
  # "wrong-number-of-arguments" error signal before the execution
  # reaches this assert statement.
  let
    a = env.extract_integer(env, args[0])
    b = env.extract_integer(env, args[1])
  env.make_integer(env, a + b)

emacs.defun(sum_float, 2):
  ## Returns the sum of two floats.
  let
    a = env.extract_float(env, args[0])
    b = env.extract_float(env, args[1])
  env.make_float(env, a + b)

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
  if (env.copy_string_contents(env, args[0], nil, addr l)): # Get the length of the elisp string args[0] (it's num chars + 1).
    var name = newString(l-1) # So the actual string length is l-1. Allocate that much space for the name string in Nim.
    if (env.copy_string_contents(env, args[0], addr name[0], addr l)): # *Now* copy the elisp string args[0] to Nim string name.
      var res = "Hello " & name # Create a new Nim string res using the name string.
      return env.make_string(env, addr res[0], res.len) # Convert the Nim string res to elisp string before returning.

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

# /* Return a copy of the argument string where every 'a' is replaced
#    with 'b'.  */
# static emacs_value
# Fmod_test_string_a_to_b (emacs_env *env, ptrdiff_t nargs, emacs_value args[],
# 			 void *data)
# {
#   emacs_value lisp_str = args[0];
#   ptrdiff_t size = 0;
#   char * buf = NULL;

#   env->copy_string_contents (env, lisp_str, buf, &size);
#   buf = malloc (size);
#   env->copy_string_contents (env, lisp_str, buf, &size);

#   for (ptrdiff_t i = 0; i + 1 < size; i++)
#     if (buf[i] == 'a')
#       buf[i] = 'b';

#   return env->make_string (env, buf, size - 1);
# }


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
]#

# /* Embedded pointers in lisp objects. */

# /* C struct (pointer to) that will be embedded.  */
# struct super_struct
# {
#   int amazing_int;
#   char large_unused_buffer[512];
# };

# /* Return a new user-pointer to a super_struct, with amazing_int set
#    to the passed parameter.  */
# static emacs_value
# Fmod_test_userptr_make (emacs_env *env, ptrdiff_t nargs, emacs_value args[],
# 			void *data)
# {
#   struct super_struct *p = calloc (1, sizeof *p);
#   p->amazing_int = env->extract_integer (env, args[0]);
#   return env->make_user_ptr (env, free, p);
# }

# /* Return the amazing_int of a passed 'user-pointer to a super_struct'.  */
# static emacs_value
# Fmod_test_userptr_get (emacs_env *env, ptrdiff_t nargs, emacs_value args[],
# 		       void *data)
# {
#   struct super_struct *p = env->get_user_ptr (env, args[0]);
#   return env->make_integer (env, p->amazing_int);
# }

# static emacs_value invalid_stored_value;

# /* The next two functions perform a possibly-invalid operation: they
#    store a value in a static variable and load it.  This causes
#    undefined behavior if the environment that the value was created
#    from is no longer live.  The module assertions check for this
#    error.  */

# static emacs_value
# Fmod_test_invalid_store (emacs_env *env, ptrdiff_t nargs, emacs_value *args,
#                          void *data)
# {
#   return invalid_stored_value = env->make_integer (env, 123);
# }

# static emacs_value
# Fmod_test_invalid_load (emacs_env *env, ptrdiff_t nargs, emacs_value *args,
#                         void *data)
# {
#   return invalid_stored_value;
# }

# /* An invalid finalizer: Finalizers are run during garbage collection,
#    where Lisp code can’t be executed.  -module-assertions tests for
#    this case.  */

# static emacs_env *current_env;

# static void
# invalid_finalizer (void *ptr)
# {
#   current_env->intern (current_env, "nil");
# }

# static emacs_value
# Fmod_test_invalid_finalizer (emacs_env *env, ptrdiff_t nargs, emacs_value *args,
#                              void *data)
# {
#   current_env = env;
#   env->make_user_ptr (env, invalid_finalizer, NULL);
#   return env->funcall (env, env->intern (env, "garbage-collect"), 0, NULL);
# }

#[
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

# vec_size, vec_get
emacs.defun(vector_eq, 2):
  var
    vec = args[0]
    val = args[1]
    size = env.vec_size(env, vec)
  for i in 0 ..< size:
    if not env.eq(env, env.vec_get(env, vec, i), val):
      result = env.intern(env, "nil")
  result = env.intern(env, "t")

# vec_size, vec_set
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
