import strutils, emacs_module

# my memo:
#   immediate, and dirty: http://forum.nim-lang.org/t/1100
#   expr, stmt, typed, and untyped: http://forum.nim-lang.org/t/2025
template addFunc*(function_name, max_args, body: typed): typed
    {.immediate, dirty.} = ## \
      ## emacs_func(env: ptr emacs_env, nargs: ptrdiff_t,
      ## args: ptr array[0..max_args, emacs_value], data: pointer):
      ## emacs_value {.exportc.}
  proc function_name*(env: ptr emacs_env, nargs: ptrdiff_t,
                      args: ptr array[0..max_args, emacs_value],
                      data: pointer): emacs_value {.exportc.} =
    body

template defuns* (package_name, defuns: typed): typed =
    {.emit:"""
/* Lisp utilities for easier readability (simple wrappers).  */

/* Provide FEATURE to Emacs.  */
static void
provide (emacs_env *env, const char *feature)
{
  emacs_value Qfeat = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall (env, Qprovide, 1, args);
}

/* Bind NAME to FUN.  */
static void
bind_function (emacs_env *env, const char *name, emacs_value Sfun)
{
  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, name);
  emacs_value args[] = { Qsym, Sfun };

  env->funcall (env, Qfset, 2, args);
}

/* Module init function.  */
int
emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment (ert);
  NimMain(); // <- Nim executes this in `main` function
#define DEFUN(lsym, csym, amin, amax, doc, data) \
  bind_function (env, lsym, \
     env->make_function (env, amin, amax, csym, doc, data))
  $1

#undef DEFUN

  provide (env, "$2");
  return 0;

}
""".format(defuns, package_name).}
