# .doom.d
my doom emacs private config

## install

please ref to <https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#install> for detailed guide

### Doom Emacs installation

```shell
git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install
```

add below to your zshrc / bashrc:

```shell
# So we don't have to write ~/.emacs.d/bin/doom every time
PATH="$HOME/.emacs.d/bin:$PATH"
```

### Private config install

```shell
mv ~/.doom.d{,.bak}
git clone https://github.com/ttys3/.doom.d.git ~/.doom.d
~/.emacs.d/bin/doom sync

~/.emacs.d/bin/doom doctor
```

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

# css lint https://stylelint.io/user-guide/get-started
npm i -g stylelint stylelint-config-standard

# html https://github.com/htacg/tidy-html5
sudo apt install tidy

# js-beautify https://github.com/beautify-web/js-beautify
npm -g install js-beautify

# markdown
# https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/markdown/README.org#prerequisites
npm install -g marked

# css
# https://github.com/stylelint/stylelint
# https://stylelint.io/user-guide/get-started
npm i -g stylelint stylelint-config-standard

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


## How to build emacs from source

```shell
git clone --depth 1 https://github.com/emacs-mirror/emacs.git


# prepare depends
apt install -y libjansson-dev
apt install -y libmagickwand-6.q16-dev
sudo apt install libotf-dev libxft-dev libgpm-dev

# configure
git clean -dxf  ## cleans up old files
./autogen.sh

./configure \
--without-mailutils --without-pop \
--enable-link-time-optimization \
--with-x-toolkit=gtk3 \
--with-xpm \
--with-jpeg \
--with-tiff \
--with-gif \
--with-png \
--with-rsvg \
--with-lcms2 \
--with-libsystemd \
--with-cairo \
--with-xml2 \
--with-imagemagick \
--with-json \
--with-xft \
--with-harfbuzz \
--with-libotf \
--with-toolkit-scroll-bars \
--with-gpm \
--with-dbus \
--with-gsettings \
--with-selinux \
--with-gnutls \
--with-zlib \
--with-modules \
--with-threads \
--with-file-notification=yes \
--with-libgmp
```

example configure output:

```shell
Configured for 'x86_64-pc-linux-gnu'.

  Where should the build process find the source code?    .
  What compiler should emacs be built with?               gcc -g3 -O2 -flto=6 -ffat-lto-objects
  Should Emacs use the GNU version of malloc?             no
    (The GNU allocators don't work with this system configuration.)
  Should Emacs use a relocating allocator for buffers?    no
  Should Emacs use mmap(2) for buffer allocation?         no
  What window system should Emacs use?                    x11
  What toolkit should Emacs use?                          GTK3
  Where do we find X Windows header files?                Standard dirs
  Where do we find X Windows libraries?                   Standard dirs
  Does Emacs use -lXaw3d?                                 no
  Does Emacs use -lXpm?                                   yes
  Does Emacs use -ljpeg?                                  yes
  Does Emacs use -ltiff?                                  yes
  Does Emacs use a gif library?                           yes -lgif
  Does Emacs use a png library?                           yes -lpng16 -lz
  Does Emacs use -lrsvg-2?                                yes
  Does Emacs use cairo?                                   yes
  Does Emacs use -llcms2?                                 yes
  Does Emacs use imagemagick?                             yes
  Does Emacs support sound?                               yes
  Does Emacs use -lgpm?                                   yes
  Does Emacs use -ldbus?                                  yes
  Does Emacs use -lgconf?                                 no
  Does Emacs use GSettings?                               yes
  Does Emacs use a file notification library?             yes -lglibc (inotify)
  Does Emacs use access control lists?                    yes -lacl
  Does Emacs use -lselinux?                               yes
  Does Emacs use -lgnutls?                                yes
  Does Emacs use -lxml2?                                  yes
  Does Emacs use -lfreetype?                              yes
  Does Emacs use HarfBuzz?                                yes
  Does Emacs use -lm17n-flt?                              no
  Does Emacs use -lotf?                                   yes
  Does Emacs use -lxft?                                   no
  Does Emacs use -lsystemd?                               yes
  Does Emacs use -ljansson?                               yes
  Does Emacs use -lgmp?                                   yes
  Does Emacs directly use zlib?                           yes
  Does Emacs have dynamic modules support?                yes
  Does Emacs use toolkit scroll bars?                     yes
  Does Emacs support Xwidgets (requires gtk3)?            no
  Does Emacs have threading support in lisp?              yes
  Does Emacs support the portable dumper?                 yes
  Does Emacs support legacy unexec dumping?               no
  Which dumping strategy does Emacs use?                  pdumper
```

check and ensure that `Does Emacs use -ljansson?` is `yes`, this will help improve JSON encode/decode performance.

support native json-serialize json-parse-string function for JSON serialization/deserialization

you can also verify this after compile:

```shell
❯ readelf -d src/emacs | grep jansson
 0x0000000000000001 (NEEDED)             Shared library: [libjansson.so.4]
```

```shell
# now build and install it
#make bootstrap
make -j14

sudo make install
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

true color:  https://github.com/hlissner/doom-emacs/blob/develop/modules/os/tty/README.org#true-color-and-italic-support

https://gist.github.com/XVilka/8346728

Update TERMINFO capabilities for 24-bit color support in emacs. #1141
https://github.com/kovidgoyal/kitty/issues/1141


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
