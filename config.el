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
;; the battole of lead key
;; see https://www.reddit.com/r/vim/comments/1sdkg2/dae_use_spacebar_as_their_leader/
;; https://www.slant.co/topics/7423/~which-key-works-best-as-vim-leader
;; avoid "," conflicts with evil-snipe (evil-snipe-repeat-reverse)
;; see https://github.com/hlissner/evil-snipe#default-keybindings
;; and https://github.com/hlissner/doom-emacs/issues/4242
;; (setq evil-snipe-override-evil-repeat-keys nil) ; not in an after! block, it needs to be set before the package loads
;; use , (comma) as leader key
;; (setq doom-leader-key ",")
;; (setq doom-localleader-key ",")

;; https://github.com/doomemacs/doomemacs/issues/2688#issuecomment-596684817
(setq confirm-kill-emacs nil)

;; xterm mouse support
(xterm-mouse-mode t)


;; credit: yorickvP on Github
;; https://www.emacswiki.org/emacs/CopyAndPaste
;; pacman -S wl-clipboard
(when (getenv "WAYLAND_DISPLAY")
  (setq wl-copy-process nil)
  (defun wl-copy (text)
    (setq wl-copy-process (make-process :name "wl-copy"
                                        :buffer nil
                                        :command '("wl-copy" "-f" "-n")
                                        :connection-type 'pipe))
    (process-send-string wl-copy-process text)
    (process-send-eof wl-copy-process))
  (defun wl-paste ()
    (if (and wl-copy-process (process-live-p wl-copy-process))
        nil ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r")))
  (setq interprogram-cut-function 'wl-copy)
  (setq interprogram-paste-function 'wl-paste))

;; func for calc first monitor width
;; geometry: position of the top-left corner of the monitor’s screen and its size, in pixels, as `(x y width height)`
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Multiple-Terminals.html#index-display_002dmonitor_002dattributes_002dlist
(defun my/first-monitor-width ()
  (nth 3 (assq 'geometry (car (display-monitor-attributes-list)))))

(defun my/first-monitor-height ()
  (nth 4 (assq 'geometry (car (display-monitor-attributes-list)))))

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
               (my/set-initial-frame))))

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
(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 16 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Noto Sans CJK SC" :size 15))

;; font size for GUI emacs on HiDPI screen
;; display-pixel-width can not handle multi monitor correctly
(if (and (display-graphic-p) (>= (my/first-monitor-width) 3840))
    (setq doom-font (font-spec :family "Iosevka Nerd Font" :size 14 :weight 'normal)
          doom-variable-pitch-font (font-spec :family "Noto Sans CJK SC" :size 14)))

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

(map!
 :leader
 :nv "t" #'+vterm/toggle ; toggle vterm popup
 :nv "s" #'save-buffer ; save file
 :nv "q" #'save-buffers-kill-terminal ; save and quit
 :nv "x" #'evil-quit-all-with-error-code ; quit without saving
 )

;; decrease which-key delay
(after! which-key
  (setq which-key-idle-delay 0.1
        which-key-idle-secondary-delay 0.01))

;; 2-char searching ala vim-sneak & vim-seek, for evil-mode
;; https://github.com/hlissner/evil-snipe#customization
(use-package! evil-snipe
  :commands (evil-snipe-mode
             evil-snipe-override-mode
             evil-snipe-local-mode
             evil-snipe-override-local-mode)
  :after-call pre-command-hook
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'buffer ; doom default: line
        evil-snipe-repeat-scope 'buffer ; doom default: whole-line
        evil-snipe-char-fold t)
  :config
  (pushnew! evil-snipe-disabled-modes 'Info-mode 'calc-mode 'treemacs-mode)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(after! magit
  ;; It seems evil-snipe-override-mode causes problems in Magit buffers, to fix this:
  ;; https://github.com/hlissner/evil-snipe#conflicts-with-other-plugins
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))


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

  ;; lua
  ;; https://emacs-lsp.github.io/lsp-mode/page/lsp-lua-language-server/
  (setq lsp-clients-lua-language-server-install-dir (f-join (getenv "HOME") ".local/share/lua-language-server/"); Default: ~/.emacs.d/.cache/lsp/lua-language-server/
        lsp-clients-lua-language-server-bin (f-join lsp-clients-lua-language-server-install-dir "bin/Linux/lua-language-server")
        lsp-clients-lua-language-server-main-location (f-join lsp-clients-lua-language-server-install-dir "main.lua")
        lsp-lua-workspace-max-preload 2048 ; Default: 300, Max preloaded files
        lsp-lua-workspace-preload-file-size 1024; Default: 100, Skip files larger than this value (KB) when preloading.
        )


  (setq lsp-intelephense-licence-key "TTYS3"))

                                        ; config cc module
                                        ; +lsp Disables irony+rtags and replaces them with LSP (ccls by default). This requires the :tools lsp module.
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/cc/README.org
(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"
                                "--header-insertion-decorators=0"))
                                        ; This will both set your clangd flags and choose clangd as the default LSP server everywhere clangd can be used.
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

(after! lsp-ui
  (setq lsp-ui-doc-max-height 13 ; recover default value, ref https://github.com/emacs-lsp/lsp-ui/blob/78e0a41c9b6b0a166c6c86d2541f310883d15b68/lsp-ui-doc.el#L106
        lsp-ui-doc-max-width 150
        ;; https://github.com/emacs-lsp/lsp-ui/blob/master/lsp-ui-doc.el
        ;; doom author: lsp-ui-doc is redundant with and more invasive than `+lookup/documentation'
        lsp-ui-doc-enable nil ; set to t to enable lsp ui doc
        lsp-ui-doc-delay 0.3 ; default is 0.2
        ;; lsp-ui-doc-position 'at-point ; on terminal seems forced to 'top
        lsp-ui-sideline-show-code-actions t))

;; @TODO fixup
;; error in a Doom startup hook: +doom-dashboard-init-h, (error "Window is dedicated to ' *Treemacs-Scoped-Buffer-Perspsctive main*'")
;; (use-package! treemacs
;;     :config
;;         (map! [f4] #'treemacs)
;;         (treemacs-display-current-project-exclusively)
;;         )

                                        ; (after! treemacs-all-the-icons
                                        ;         (treemacs-load-theme "all-the-icons"))


(after! doom-themes
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  ;; (doom-themes-neotree-config)
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; https://github.com/sebastiencs/sidebar.el
(map! [f4] #'neotree-toggle)

(use-package! neotree
  :config
  (setq neo-theme 'icons))

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
                            (setq fill-column 160)
                            (setq display-fill-column-indicator t)
                            (setq display-fill-column-indicator-column t)
                            ;; (display-fill-column-indicator-mode)
                            (global-display-fill-column-indicator-mode)


                            (defun eglot-rename (newname)
                              "Rename the current symbol to NEWNAME."
                              (interactive
                               (list (read-from-minibuffer
                                      (format "Rename `%s' to: " (or (thing-at-point 'symbol t) "unknown symbol"))
                                      (thing-at-point 'symbol t) nil nil nil
                                      (symbol-name (symbol-at-point)))))
                              (unless (eglot--server-capable :renameProvider)
                                (eglot--error "Server can't rename!"))
                              (eglot--apply-workspace-edit
                               (jsonrpc-request (eglot--current-server-or-lose)
                                                :textDocument/rename `(,@(eglot--TextDocumentPositionParams)
                                                                       :newName ,newname))
                               current-prefix-arg))


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

(use-package! org-superstar
  :init
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

;; (use-package! tree-sitter
;;               :config
;;               (global-tree-sitter-mode)
;; (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
;; )

;; (use-package! tree-sitter-langs)
