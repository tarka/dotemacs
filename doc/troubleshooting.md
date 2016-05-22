In case you run into issues here are a few tips that can help you diagnose the
problem.

Generally, it's not a bad idea to configure Emacs to spit the backtrace on error
(instead of just logging the error in the `*Messages*` buffer. You can toggle
this behavior by using <kbd>M-x</kbd> `toggle-debug-on-error`.

## Debugging CIDER commands

Emacs features a super powerful built-in
[Emacs Lisp debugger](http://www.gnu.org/software/emacs/manual/html_node/elisp/Edebug.html)
and using it is the best way to diagnose problems of any kind.

Here's a [great crash course](https://www.youtube.com/watch?v=odkYXXYOxpo) on
using the debugger.

To debug some command you need to do the following:

* Figure out the name of the command you want to debug (e.g. by using <kbd>C-h k</kbd>
to see which command is associated with some keybinding)
* Find the source of the command (e.g. by using <kbd>M-x</kbd> `find-function`
  <kbd>RET</kbd> `function-name`)
* Press <kbd>C-u C-M-x</kbd> while in the body of the function
* Run the command again

At this point you'll be dropped in the debugger and you can step forward until
you find the problem.

## REPL not starting

Make sure that your CIDER version matches your `cider-nrepl` version. Check
the contents of the `*Messages*` buffer for CIDER-related errors. You should
also check the nREPL messages passed between CIDER and nREPL in
`*nrepl-messages*`. If you don't see anything useful there it's time to bring
out the big guns.

### Debugging the REPL init

To debug CIDER's REPL initialization it's a good idea to hook into one of its
entry points. Add a breakpoint to `cider-make-repl` (<kbd>C-u C-M-x</kbd>, while
in its body). Next time you start CIDER you'll be dropped in the debugger and
you can step forward until you find the problem.

## Missing `*nrepl-messages*` buffer

Check the value of `nrepl-log-messages`. It should be non-nil.

## `cider-debug` complains that it “failed to instrument ...”

In the REPL buffer, issue the following.

    your.namespace> (ns cider.nrepl.middleware.util.instrument)
    cider.nrepl.middleware.util.instrument> (def verbose-debug true)

This will cause cider to print extensive information on the REPL buffer when you
try to debug an expression (e.g., with <kbd>C-u
C-M-x</kbd>). [File an issue](https://github.com/clojure-emacs/cider-repl/issues/new)
and copy this information.

## Debugging freezes & lock ups

Sometimes a CIDER command might hang for a while (e.g. due to a bug or a
configuration issue). Such problems are super annoying, but are relatively easy
to debug. Here are a few steps you can take in such situations:

* Do <kbd>M-x</kbd> `toggle-debug-on-quit`
* Reproduce the problem
* Hit <kbd>C-g</kbd> around 10 seconds into the hang

This will bring up a backtrace with the entire function stack, including
function arguments. So you should be able to figure out what's going on (or at
least what's being required).

## Warning saying you have to use nREPL 0.2.12+

CIDER currently requires at least nREPL 0.2.12 to work properly (there were some
nasty bugs in older version and no support tracking where some var was defined
in the source code). Leiningen users can add this to their `profiles.clj` to
force the proper dependency:

```clojure
{:repl {:dependencies [[org.clojure/tools.nrepl "0.2.12"]]}}
```

Make sure you add the newer nREPL dependency to the `:dependencies` key instead
of `:plugins` (where `cider-nrepl` Lein plugin resides). That's a pretty common
mistake.

Generally you're advised to use the newest nREPL with CIDER, as bugs get fixed
in pretty much every release.

Note, that running `cider-jack-in` from outside the scope of a project will
result in the **older (0.2.6) nREPL dependency being used** (at least on Leiningen
2.5.1). This is likely a Leiningen bug.

## Missing clojure-... function after CIDER update

Most likely you've updated CIDER, without updating `clojure-mode` as well.

CIDER depends on `clojure-mode` and you should always update them together, as
the latest CIDER version might depend on functionality present only in the latest
`clojure-mode` version.

## I upgraded CIDER using `package.el` and it broke

The built-in package manager isn't perfect and sometimes it messes up.  If you
just updated and encountered an error you should try the following before
opening an issue: Go into the `.emacs.d/elpa` directory, delete any folders
related to CIDER, restart Emacs and then re-install the missing packages.  Note
that the order here matters.

## I upgraded CIDER using `package.el` and nothing changed

Emacs doesn't load the new files, it only installs them on disk.  To see the
effect of changes you have to restart Emacs.

## CIDER injects its dependencies but I still get warnings when I use `cider-jack-in`

Injecting the depencies should override the old settings in the
`~/lein/profiles.clj` file. However, this works only if those settings were placed
in the `:repl` profile. Configuration placed in the `:user` profile will not be
overridden. To fix this issue stop nREPL (and CIDER), remove the `cider-nrepl` and `tools.nrepl`
dependencies from your `~/.lein/profiles.clj` and start nREPL again.
