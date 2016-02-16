# Copyright (C) 2015 by Yuta Yamada

# This program is a wrapper for emacs-module.h to use Nim's functions
# from Emacs using Emacs' dynamic module feature, which is supported
# from Emacs 25.1.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Comment from Emacs' NEWS
#
# A dynamic Emacs module is a shared library that provides additional
# functionality for use in Emacs Lisp programs, just like a package
# written in Emacs Lisp would.  The functions `load', `require',
# `load-file', etc. were extended to load such modules, as they do with
# Emacs Lisp packages.  The new variable `module-file-suffix' holds the
# system-dependent value of the file-name extension (`.so' on Posix
# hosts) of the module files.

# If a module needs to call Emacs functions, it should do so through the
# API defined and documented in the header file `emacs-module.h'.  Note
# that any module that provides Lisp-callable functions will have to use
# Emacs functions such as `fset' and `funcall', in order to register its
# functions with the Emacs Lisp interpreter.

# Modules can create `user-ptr' Lisp objects that embed pointers to C
# struct's defined by the module.  This is useful for keeping around
# complex data structures created by a module, to be passed back to the
# module's functions.  User-ptr objects can also have associated
# "finalizers" -- functions to be run when the object is GC'ed; this is
# useful for freeing any resources allocated for the underlying data
# structure, such as memory, open file descriptors, etc.  A new
# predicate `user-ptrp' returns non-nil if its argument is a `user-ptr'
# object.

# A module should export a C-callable function named
# `emacs_module_init', which Emacs will call as part of the call to
# `load' or `require' which loads the module.  It should also export a
# symbol named `plugin_is_GPL_compatible' to indicate that its code is
# released under the GPL or compatible license; Emacs will refuse to
# load modules that don't export such a symbol.

# Loadable modules in Emacs are an experimental feature, and subject to
# change in future releases.  For that reason, their support is disabled
# by default, and must be enabled by using the `--with-modules' option
# at configure time.

type
  intmax_t* {.importc: "intmax_t", header: "<inttypes.h>".} = clonglong
  ptrdiff_t* {.importc: "ptrdiff_t", header: "<stddef.h>".} = int

type
  emacs_runtime_private {.importc: "struct emacs_runtime_private",
                          header: "<emacs-module.h>".} = object
  emacs_env_private {.importc: "struct emacs_env_private",
                      header: "<emacs-module.h>".} = object

type
  ## Function prototype for the module init function.
  emacs_init_function* = proc (ert: ptr emacs_runtime): cint {.cdecl.}

  ## Function prototype for the module Lisp functions.
  emacs_subr* = proc(env: ptr emacs_env, nargs: ptrdiff_t,
                     args: ptr emacs_value, data: pointer): emacs_value {.cdecl.}
  emacs_runtime* {.importc: "struct emacs_runtime",
                   header: "<emacs-module.h>".} = object ## \
    ## Struct passed to a module init function (emacs_module_init).
    size: ptrdiff_t ## Structure size (for version checking).
    private_members: ptr emacs_runtime_private ## \
      ## Private data; users should not touch this.
    get_environment: proc(ert: ptr emacs_runtime): ptr emacs_env {.cdecl.}

  emacs_value* {.importc: "struct emacs_value_tag",
                 header: "<emacs-module.h>".} = pointer

  emacs_funcall_exit* = enum
    emacs_funcall_exit_return = 0, # Function has returned normally.
    emacs_funcall_exit_signal = 1, # Function has signaled an error using `signal'.
    emacs_funcall_exit_throw  = 2  # Function has exit using `throw'.

  emacs_env* {.importc: "struct emacs_env_25",
               header: "<emacs-module.h>".} = object
    size: ptrdiff_t
    private_members*: ptr emacs_env_private
    make_global_ref*: proc (env: ptr emacs_env;
                            any_reference: emacs_value): emacs_value {.cdecl.}
    free_global_ref*: proc (env: ptr emacs_env;
                            global_reference: emacs_value) {.cdecl.}
    non_local_exit_check*: proc (env: ptr emacs_env): emacs_funcall_exit {.cdecl.}
    non_local_exit_clear*: proc (env: ptr emacs_env) {.cdecl.}
    non_local_exit_get*: proc (env: ptr emacs_env;
                               non_local_exit_symbol_out: ptr emacs_value;
                               non_local_exit_data_out: ptr emacs_value
                              ): emacs_funcall_exit {.cdecl.}
    non_local_exit_signal*: proc (env: ptr emacs_env;
                                  non_local_exit_symbol: emacs_value;
                                  non_local_exit_data: emacs_value) {.cdecl.}
    non_local_exit_throw*: proc (env: ptr emacs_env; tag: emacs_value;
                                 value: emacs_value) {.cdecl.}

    # Function registration
    make_function*: proc (env: ptr emacs_env,
                          min_arity: ptrdiff_t;
                          max_arity: ptrdiff_t;
                          function: proc (env: ptr emacs_env;
                                          nargs: ptrdiff_t;
                                          args: ptr emacs_value;
                                          a5: pointer): emacs_value {.cdecl.},
                          documentation: cstring;
                          data: pointer): emacs_value {.cdecl.}
    funcall*: proc (env: ptr emacs_env; function: emacs_value; nargs: ptrdiff_t;
                    args: ptr emacs_value): emacs_value {.cdecl.}
    intern*: proc (env: ptr emacs_env;
                   symbol_name: cstring): emacs_value {.cdecl.}

    # Type Conversion
    type_of*: proc (env: ptr emacs_env;
                    value: emacs_value): emacs_value {.cdecl.}
    is_not_nil*: proc (env: ptr emacs_env;
                       value: emacs_value): bool {.cdecl.}
    eq*: proc (env: ptr emacs_env;
               a: emacs_value; b: emacs_value): bool {.cdecl.}
    extract_integer*: proc (env: ptr emacs_env;
                            value: emacs_value): intmax_t {.cdecl.}
    make_integer*: proc (env: ptr emacs_env;
                         value: intmax_t): emacs_value {.cdecl.}
    extract_float*: proc (env: ptr emacs_env;
                          value: emacs_value): cdouble {.cdecl.}
    make_float*: proc (env: ptr emacs_env;
                       value: cdouble): emacs_value {.cdecl.}

    # String manipulation
    copy_string_contents*: proc (env: ptr emacs_env;
                                 value: emacs_value;
                                 buffer: cstring;
                                 size_inout: ptr ptrdiff_t): bool {.cdecl.}

    # Create a Lisp string from a utf8 encoded string.
    make_string*: proc (env: ptr emacs_env;
                        contents: cstring;
                        length: ptrdiff_t): emacs_value {.cdecl.}

    # Embedded pointer type.
    make_user_ptr*: proc (env: ptr emacs_env;
                          fin: proc (a2: pointer) {.cdecl.},
                          `ptr`: pointer): emacs_value {.cdecl.}
    get_user_ptr*: proc (env: ptr emacs_env;
                         uptr: emacs_value): pointer {.cdecl.}
    set_user_ptr*: proc (env: ptr emacs_env;
                         uptr: emacs_value; `ptr`: pointer) {.cdecl.}

    set_user_finalizer*: proc (env: ptr emacs_env; uptr: emacs_value;
                               fin: proc (a2: pointer) {.cdecl.}) {.cdecl.}

    # Vector
    vec_get*: proc (env: ptr emacs_env;
                    vec: emacs_value; i: ptrdiff_t): emacs_value {.cdecl.}
    vec_set*: proc (env: ptr emacs_env;
                    vec: emacs_value; i: ptrdiff_t; val: emacs_value) {.cdecl.}
    vec_size*: proc (env: ptr emacs_env; vec: emacs_value): ptrdiff_t {.cdecl.}

proc emacs_module_init*(ert: ptr emacs_runtime): cint
    {.importc: "emacs_module_init", header: "<emacs_module.h>".} ## \
      ## Every module should define a function as follows.
