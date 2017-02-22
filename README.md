# r6lint

[![Build Status](https://travis-ci.org/weinholt/r6lint.svg?branch=master)](https://travis-ci.org/weinholt/r6lint)

r6lint is an R6RS syntax and style checker.

It is currently *extremely rudimentary* and in early development, with
basically the only useful check being to show the location of unbound
variables (and just one at a time).

## Setup

Only Chez Scheme is supported at the moment. Set the environment
variable `CHEZSCHEMELIBDIRS` to include the directory where `r6lint`
is checked out (i.e `r6lint/..`). To get a better experience you can
compile the linter's libraries with
`scheme --compile-imported-libraries --program bin/r6lint`.

Support for more Schemes will be forthcoming. They just need an
`(r6lint psyntax compat)` library.

## Usage with emacs

First install [flycheck](https://github.com/flycheck/flycheck) 
with `M-x package-refresh-contents RET` followed by
`M-x package-install RET flycheck RET`. Then install flycheck-r6lint with:

```bash
emacs --batch -l package -f package-initialize --eval '(package-install-file "flycheck-r6lint.el")'
```

Add this to your `~/.emacs` file:

```elisp
(eval-after-load 'flycheck '(flycheck-r6lint-setup))
```

Create the file `.r6lintrc` in your home or project directory. This
file contains an alist with configuration items. This is one way to
set it up, assuming you keep Scheme projects under `~/code/<project>`
and the library names start with `<project>`:

```bash
cat > $HOME/.r6lintrc << EOF
((library-path . ("$HOME/code")))
EOF

```
