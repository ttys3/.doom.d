;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; TERM=xterm-direct emacs -nw
;; fixup 24bit color for tmux TERM=screen-256color
;; seems not work, commnet out currently
;; (add-to-list 'term-file-aliases
;;     '("screen-256color" ."xterm-direct"))
;; (add-to-list 'term-file-aliases
;;     '("tmux-256color" ."xterm-direct"))
;; (add-to-list 'term-file-aliases
;;     '("xterm-256color" ."xterm-direct"))

;; evil leader key
(setq evil-snipe-override-evil-repeat-keys nil)
(setq doom-leader-key ",")
(setq doom-localleader-key ",")

;; xterm mouse support
(xterm-mouse-mode t)

;; func for calc first monitor width
;; geometry: position of the top-left corner of the monitor’s screen and its size, in pixels, as `(x y width height)`
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Multiple-Terminals.html#index-display_002dmonitor_002dattributes_002dlist
(defun my/first-monitor-width ()
(nth 3 (assq 'geometry (car (display-monitor-attributes-list))))
)

(defun my/first-monitor-height ()
(nth 4 (assq 'geometry (car (display-monitor-attributes-list))))
)

;; Set initial frame size and position for GUI emacs
(defun my/set-initial-frame ()
  (let* ((base-factor 0.70)
	(a-width (* (my/first-monitor-width) base-factor))
        (a-height (* (my/first-monitor-height) base-factor))
        (a-left (truncate (/ (- (my/first-monitor-width) a-width) 2)))
	(a-top (truncate (/ (- (my/first-monitor-height) a-height) 2))))
    (set-frame-position (selected-frame) a-left a-top)
    (set-frame-size (selected-frame) (truncate a-width)  (truncate a-height) t)))

;; Use of the `window-system` variable as a boolean is deprecated. Instead,
;; use `display-graphic-p' or any of the other `display-*-p'
;; predicates which report frame's specific UI-related capabilities.
;; see http://doc.endlessparentheses.com/Var/window-system.html
;; and https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Systems.html#index-window_002dsystem
(if (display-graphic-p)
   (funcall (lambda ()
        (setq frame-resize-pixelwise t)
        (my/set-initial-frame)))
)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "荒野無燈"
      user-mail-address "nobody@ttys3.dev")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; font size for GUI emacs, here the size 16 is equal to font size 12.0 under Linux
 (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 16 :weight 'normal)
       doom-variable-pitch-font (font-spec :family "Noto Sans CJK SC" :size 15))

;; font size for GUI emacs on HiDPI screen
;; display-pixel-width can not handle multi monitor correctly
(if (and (display-graphic-p) (>= (my/first-monitor-width) 3840))
   (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 28 :weight 'normal)
       doom-variable-pitch-font (font-spec :family "Noto Sans CJK SC" :size 28)))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-material)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; ----------------- doom bundled package config --------------------

;; add global company-backend
;; https://github.com/hlissner/doom-emacs/issues/1269#issuecomment-473573906
(after! company
(setq company-backends '(company-tabnine company-capf)))

;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(after! lsp-mode
(setq lsp-log-io nil) ; if set to true can cause a performance hit
(setq lsp-print-performance t)
(setq lsp-auto-guess-root t) ; auto detect workspace and start lang server
(setq lsp-eldoc-render-all t) ; display all of the info returned by document/onHover on bottom, only the symbol if nil.

;; disable some lsp clients packages for speedup
;; List of the clients to be automatically required.
;; see https://github.com/emacs-lsp/lsp-mode/blob/587c042044769862691ef8ca10b081ef8a3b6892/lsp-mode.el#L170
(setq lsp-client-packages '(ccls lsp-bash lsp-clangd lsp-cmake
         lsp-css lsp-dockerfile lsp-eslint lsp-go
         lsp-javascript lsp-json
         lsp-lua lsp-php
         lsp-python-ms lsp-rust
         lsp-vala lsp-vimscript lsp-xml
         lsp-yaml lsp-sqls))
;; disable some lsp clients for speedup
; (setq lsp-disabled-clients '(jedi pyls))
;; https://github.com/emacs-lsp/lsp-mode/blob/master/docs/lsp-clients.json
;; grep lsp doo-emacs-lsp-slow.md | awk '{print $1}' | grep '^lsp' | grep -v '^ui' | sed "s|lsp-|(add-to-list 'lsp-disabled-clients '|"
(add-to-list 'lsp-disabled-clients 'sqls)
(add-to-list 'lsp-disabled-clients 'protocol)
(add-to-list 'lsp-disabled-clients 'svelte)
(add-to-list 'lsp-disabled-clients 'yaml)
(add-to-list 'lsp-disabled-clients 'xml)
(add-to-list 'lsp-disabled-clients 'vetur)
; (add-to-list 'lsp-disabled-clients 'html)
; (add-to-list 'lsp-disabled-clients 'rust)
(add-to-list 'lsp-disabled-clients 'solargraph)
(add-to-list 'lsp-disabled-clients 'rf)
(add-to-list 'lsp-disabled-clients 'pyls)
(add-to-list 'lsp-disabled-clients 'pwsh)
; (add-to-list 'lsp-disabled-clients 'php)
; (add-to-list 'lsp-disabled-clients 'lua)
(add-to-list 'lsp-disabled-clients 'elm)
(add-to-list 'lsp-disabled-clients 'perl)
(add-to-list 'lsp-disabled-clients 'kotlin)
(add-to-list 'lsp-disabled-clients 'haxe)
(add-to-list 'lsp-disabled-clients 'fsharp)
; (add-to-list 'lsp-disabled-clients 'eslint)
; (add-to-list 'lsp-disabled-clients 'clangd)
(add-to-list 'lsp-disabled-clients 'steep)
; (add-to-list 'lsp-disabled-clients 'vimscript)
(add-to-list 'lsp-disabled-clients 'vhdl)
(add-to-list 'lsp-disabled-clients 'verilog)
; (add-to-list 'lsp-disabled-clients 'vala)
(add-to-list 'lsp-disabled-clients 'terraform)
(add-to-list 'lsp-disabled-clients 'tex)
(add-to-list 'lsp-disabled-clients 'sorbet)
(add-to-list 'lsp-disabled-clients 'r)
(add-to-list 'lsp-disabled-clients 'purescript)
(add-to-list 'lsp-disabled-clients 'ocaml)
(add-to-list 'lsp-disabled-clients 'nix)
(add-to-list 'lsp-disabled-clients 'nim)
; (add-to-list 'lsp-disabled-clients 'json)
; (add-to-list 'lsp-disabled-clients 'javascript)
(add-to-list 'lsp-disabled-clients 'groovy)
(add-to-list 'lsp-disabled-clients 'hack)
; (add-to-list 'lsp-disabled-clients 'go)
(add-to-list 'lsp-disabled-clients 'gdscript)
(add-to-list 'lsp-disabled-clients 'fortran)
(add-to-list 'lsp-disabled-clients 'erlang)
(add-to-list 'lsp-disabled-clients 'elixir)
; (add-to-list 'lsp-disabled-clients 'dockerfile)
(add-to-list 'lsp-disabled-clients 'dhall)
; (add-to-list 'lsp-disabled-clients 'css)
(add-to-list 'lsp-disabled-clients 'csharp)
(add-to-list 'lsp-disabled-clients 'crystal)
; (add-to-list 'lsp-disabled-clients 'cmake)
(add-to-list 'lsp-disabled-clients 'clojure)
; (add-to-list 'lsp-disabled-clients 'bash)
(add-to-list 'lsp-disabled-clients 'angular)
(add-to-list 'lsp-disabled-clients 'ada)
(add-to-list 'lsp-disabled-clients 'actionscript)

(setq lsp-intelephense-licence-key "TTYS3"))

(after! lsp-ui
  (setq lsp-ui-doc-max-height 13 ; recover default value, ref https://github.com/emacs-lsp/lsp-ui/blob/78e0a41c9b6b0a166c6c86d2541f310883d15b68/lsp-ui-doc.el#L106
        lsp-ui-doc-max-width 150
        ;; https://github.com/emacs-lsp/lsp-ui/blob/master/lsp-ui-doc.el
        ;; doom author: lsp-ui-doc is redundant with and more invasive than `+lookup/documentation'
        lsp-ui-doc-enable nil ; set to t to enable lsp ui doc
        lsp-ui-doc-delay 0.3 ; default is 0.2
        ;; lsp-ui-doc-position 'at-point ; on terminal seems forced to 'top
        lsp-ui-sideline-show-code-actions t))

(after! treemacs
        ; (setq treemacs-no-png-images t)
        ; (treemacs-load-theme "all-the-icons")
        (treemacs-display-current-project-exclusively))

; (after! treemacs-all-the-icons
;         (treemacs-load-theme "all-the-icons"))

(after! doom-themes
        (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
              doom-themes-enable-italic t) ; if nil, italics is universally disabled

        ;; Enable flashing mode-line on errors
        (doom-themes-visual-bell-config)

        (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
        (doom-themes-treemacs-config))

;; recover max face count limit to 9 by doom emacs in core/core-ui.el
;; and also add rainbow delimiters for all langs (doom only enabled it in few langs)
;; https://github.com/Fanael/rainbow-delimiters

(use-package! rainbow-delimiters
  :config
  (progn
    (defun @-enable-rainbow-delimiters ()
      (rainbow-delimiters-mode t))
    (setq rainbow-delimiters-max-face-count 9)
    (add-hook 'prog-mode-hook '@-enable-rainbow-delimiters)))

;; enable responsive guides
;; responsive guides allow you to visualize not only the indentation itself, but your place in it.
;; see https://github.com/DarthFennec/highlight-indent-guides/blob/master/README.md#responsive-guides
;; can be nil (default) / top / stack
(after! highlight-indent-guides
  (setq highlight-indent-guides-responsive "top"))

;; enable fill-column-indicator (since emacs 27.1) in prog mode
;; see https://www.gnu.org/software/emacs/manual/html_node/emacs/Displaying-Boundaries.html
(add-hook 'prog-mode-hook (lambda ()
        (setq fill-column 120)
        (setq display-fill-column-indicator t)
        (setq display-fill-column-indicator-column t)
        ;; (display-fill-column-indicator-mode)
        (global-display-fill-column-indicator-mode)
))

;; ----------------- user packages config ----------------------

(use-package! evil-matchit
    :config
    (global-evil-matchit-mode 1))

(use-package! easy-hugo
   :config
(progn
        (setq easy-hugo-basedir "~/repo/blog/ttys3.dev")
        (setq easy-hugo-url "https://ttys3.dev")
        (setq easy-hugo-previewtime "300")
        (map! :leader
              :desc "easy Hugo blog"
              :nv "e h" #'easy-hugo)
     ))
