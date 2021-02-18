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
