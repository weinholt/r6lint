# r6lint

[![CircleCI](https://circleci.com/gh/weinholt/r6lint.svg?style=svg)](https://circleci.com/gh/weinholt/r6lint)

r6lint is an R6RS syntax and style checker.

It is currently somewhat rudimentary and at a PoC level. It shows the
location of lexical errors and most of the time the location of
syntactical errors (all invalid Scheme forms, as determined by
psyntax). There are some style checks for issues like hanging braces,
but it doesn't know yet about all exceptions to the rules. Finally it
also prints the location of unused variables, which can commonly
indicate bugs.

## Setup

Set your Scheme library path (e.g. `CHEZSCHEMELIBDIRS`) to include the
directory where `r6lint` is checked out (i.e `r6lint/..`). To get a
better experience you can compile the linter's libraries. With Chez
Scheme this can be done with `scheme --compile-imported-libraries
--program bin/r6lint`.

Support for more Schemes will be forthcoming. They just need an
`(r6lint psyntax compat)` library.

## Usage with emacs

First [install Flycheck](http://www.flycheck.org/en/latest/user/installation.html).
Then install flycheck-r6lint with:

```bash
emacs --batch -l package -f package-initialize --eval '(package-install-file "flycheck-r6lint.el")'
```

Add this to your `~/.emacs` file:

```elisp
(eval-after-load 'flycheck '(flycheck-r6lint-setup))
```

Enable flycheck with `M-x global-flycheck-mode RET`. The r6lint program
is probably not in the path so you'll also need to do
`M-x customize-variable RET flycheck-r6lint-executable RET`.
Open a Scheme source file and use
`M-x flycheck-verify-checker RET r6lint RET` to
see why it's not working.

Create the file `.r6lintrc` in your home or project directory. This
file contains an alist with configuration items. This is one way to
set it up, assuming you keep Scheme projects under `~/code/<project>`
and the library names start with `<project>`:

```bash
cat > $HOME/.r6lintrc << EOF
((library-path . ("$HOME/code")))
EOF

```
