# doom emacs troubleshooting

## error doom-after-init-hook doom-modeline-mode no such file compat

"Cannot open load file" "No such file or directory" "compat"

try `doom sync` can not fix this

solution:

fd -uuu compat

```shell
cd ~/.emacs.d
rm -rf .local/straight/build-29.4/compat/
rm -rf .local/straight/repos/compat/
```

seems no files under `.local/straight/build-29.4/compat/`

so no `*.elc*` files builded. `sync` only check git repo, not build

