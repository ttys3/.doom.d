# .doom.d
my doom emacs private config

## install

### enable 24bit color under terminal

add below to .zshrc

``` shell
alias em='TERM=xterm-direct emacs -nw'
```

### external tools

``` shell
# go tools
go get -u github.com/cweill/gotests/...
go get -u github.com/motemen/gore/cmd/gore
go get -u github.com/mdempsky/gocode

# https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/markdown/README.org#prerequisites
npm install -g marked

# Fedora
# https://github.com/koalaman/shellcheck
sudo dnf install -y ShellCheck

# https://github.com/Andersbakken/rtags
sudo dnf install -y rtags


# Ubuntu
sudo apt install --no-install-recommends -y shellcheck

# use `--no-install-recommends` to avoid apt install recommended packages:
# emacs-bin-common emacs-common emacs-el dh-elpa-helper elpa-rtags emacs emacs-gtk
sudo apt install --no-install-recommends -y rtags

# https://github.com/MaskRay/ccls
sudo apt install --no-install-recommends -y ccls
```



## refs

https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#modules

https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#file-structure

https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#configel
Use after! or use-package! to configure packages.

**Doom Emacs does not use package.el** (the package manager built into Emacs). Instead, it uses its own declarative package manager built on top of straight.el.
Packages are declared in packages.el files. You’ll find one in your DOOMDIR and in many of Doom’s modules. Read on to learn how to use this system to install your own packages.

https://github.com/hlissner/doom-emacs#install


https://github.com/hlissner/doom-emacs/blob/develop/init.example.el

https://github.com/emacs-lsp/lsp-mode

https://github.com/Emacs-lsp/lsp-ui

https://emacs-lsp.github.io/lsp-ui/

Ivy https://oremacs.com/swiper/

## articles

- Helm or Ivy ?

https://archive.casouri.cat/note/2018/ivy-to-helm/index.html

<https://github.com/lujun9972/emacs-document/blob/master/emacs-common/%E4%BB%8EHelm%E5%88%B0Ivy.org>

<https://github.com/abo-abo/swiper/issues/3>

- LSP

https://phenix3443.github.io/notebook/emacs/modes/company-lsp.html

<https://phenix3443.github.io/notebook/emacs/modes/lsp-mode.html>

<https://phenix3443.github.io/notebook/emacs/modes/lsp-ui.html>

## FAQ

Is Company different from Helm and Ivy ? https://www.reddit.com/r/emacs/comments/6x7ph2/is_company_different_from_helm_and_ivy/


## Peformance

how to profile Doom startup time ?  https://github.com/hlissner/doom-emacs/issues/4498

```shell
TERM=xterm-direct emacs -nw --debug-init main.go
```

A guide on disabling/enabling lsp-mode features https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/

http://blog.binchen.org/posts/how-to-speed-up-lsp-mode/


## Troubleshoot

Project root won't be guessed correctly https://github.com/hlissner/doom-emacs/issues/3024

lsp-auto-guess-root problem https://github.com/hlissner/doom-emacs/issues/1928
