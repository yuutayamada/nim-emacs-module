import strutils as su
import emacs_module
import macros as m

type Emacs* = object
  functions*: string
  libName*: string
  defunTemplate*: string

proc pushFunction*(self: var Emacs, fn: string, max_args: int) =
  ## Push function name `fn` to `functions` object.
  ## This variable is used later by `provide` proc.
  let
    emacs_func = self.libName & "-" & su.replace(fn, "_", "-")
    nim_func = "nimEmacs" & fn

  self.functions.add(
    su.format("""DEFUN ("$1", $2, $3, $4, NULL, NULL);""",
              emacs_func, nim_func, max_args, max_args)
  )

# my memo:
#   immediate, and dirty: http://forum.nim-lang.org/t/1100
#   expr, stmt, typed, and untyped: http://forum.nim-lang.org/t/2025
#   http://forum.nim-lang.org/t/2228
template defun*(self, fsym, max_args, body: untyped) {.dirty.} = ## \
  ## emacs_func(env: ptr emacs_env, nargs: ptrdiff_t,
  ## args: ptr array[0..max_args, emacs_value], data: pointer):
  ## emacs_value {.exportc.}
  ## The `fsym` is registered as the name in emacs and also
  ## be registered in Nim with nimEmacs prefix.
  ## If you include "_" in the function name, it will be converted "-"
  ## in Emacs.
  static:
    self.pushFunction(astToStr(fsym), max_args)

  proc `nimEmacs fsym`*(env: ptr emacs_env, nargs: ptrdiff_t,
                        args: ptr array[0..max_args, emacs_value],
                        data: pointer): emacs_value {.exportc.} =
     body


proc provideString* (self: Emacs): string =
  su.format("""
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
""", self.functions, self.libName)


template provide*(self: typed): typed {.dirty.} =
  static:
    const temp = self.provideString()
  {.emit: temp.}


template init*(sym: untyped): untyped {.dirty.} =
  from os import splitFile

  static:
    var sym = Emacs()
    let info = instantiationInfo()
    sym.functions = ""
    sym.libName = splitFile(info.filename).name

