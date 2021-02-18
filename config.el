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

 (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 12 :weight 'semi-light)
       doom-variable-pitch-font (font-spec :family "Noto Sans CJK SC" :size 13))

(if (equal (display-pixel-width) 3840)
   (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 28 :weight 'semi-light)
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

(after! lsp-mode
(setq lsp-log-io t)
(setq lsp-auto-guess-root t)
(setq lsp-intelephense-licence-key "TTYS3")
)

(after! neotree
    (setq neo-vc-integration '(char face))
)

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


;; user packages config

(use-package! rainbow-mode
  :config
  (progn
    (defun @-enable-rainbow ()
      (rainbow-mode t))
    (add-hook 'prog-mode-hook '@-enable-rainbow)
))

(use-package! rainbow-delimiters
  :config
  (progn
    (defun @-enable-rainbow-delimiters ()
      (rainbow-delimiters-mode t))
    (add-hook 'prog-mode-hook '@-enable-rainbow-delimiters)))

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
