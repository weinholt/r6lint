# r6lint

r6lint is an R6RS syntax and style checker.

It is currently extremely rudimentary and in early development, with
basically the only useful check being to show the location of unbound
variables.

## Usage with emacs

First install flycheck and activate flycheck-r6lint with:

```bash
emacs --batch -l package -f package-initialize --eval '(package-install-file "flycheck-r6lint.el")'
```

Add this to your `~/.emacs` file:

```elisp
(eval-after-load 'flycheck '(flycheck-r6lint-setup))
```
