;;;; Emacs configuration for Eric James Michael Ritz
;;;;     <ejmr@plutono.com>

(server-start)

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(eval-when-compile
  (require 'use-package))

(setq inhibit-startup-message t)
(setq c-default-style "linux")
(transient-mark-mode t)
(delete-selection-mode t)
(column-number-mode t)
(show-paren-mode t)
(global-hi-lock-mode 1)
(which-function-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-auto-revert-mode 1)

(use-package yasnippet
  :config
  (progn
    (defhydra hydra-yasnippet (:color blue :hint nil)
      "
              YASnippets
--------------------------------------------
  ^Modes:^    ^Load/Visit:^    ^Actions:^

  _g_lobal    _d_irectory      _i_nsert
  _m_inor     _f_ile           _t_ryout
  _e_xtra     _a_ll            _n_ew
"
      ("d" yas-load-directory)
      ("e" yas-activate-extra-mode)
      ("i" yas-insert-snippet)
      ("f" yas-visit-snippet-file :color blue)
      ("n" yas-new-snippet)
      ("t" yas-tryout-snippet)
      ("g" yas/global-mode)
      ("m" yas/minor-mode)
      ("a" yas-reload-all))
    (global-set-key (kbd "C-c y") 'hydra-yasnippet/body)
    (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))))

(add-to-list 'load-path "/home/eric/.emacs.d/lisp")
(autoload 'forth-mode "gforth.el")
(autoload 'forth-block-mode "gforth.el")
(add-to-list 'auto-mode-alist '("\\.fs" . forth-mode))
(add-to-list 'auto-mode-alist '("\\.fb" . forth-block-mode))

(use-package lua-mode
  :mode "\\.lnvl$")

(use-package recentf
  :config (recentf-mode 1))
(use-package yaml-mode)
(use-package tomatinho
  :config (global-set-key (kbd "<f12>") 'tomatinho))
(use-package hydra)
(use-package highlight-parentheses)
(use-package helm-config)
(use-package helm-git-grep)
(use-package helm-ls-git)
(use-package helm-ag)
(use-package helm-j-cheatsheet)
(use-package helm-make)
(use-package aggressive-indent)
(use-package pastebin)
(use-package bbcode-mode)
(use-package flycheck)
(use-package anyins)
(use-package firestarter)
(use-package highlight-blocks)
(use-package tup-mode)
(use-package love-minor-mode)
(use-package page-break-lines)
(use-package focus)
(use-package define-word)

(use-package helm-flyspell
  :config (global-set-key (kbd "M-$") #'helm-flyspell-correct))

(use-package adoc-mode
  :mode "\\.adoc$")

(use-package textile-mode
  :mode "\\.textile$")

(use-package haskell-mode
  :config
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)))

(add-to-list 'load-path "/home/eric/.emacs.d/lisp/qwe/src")
(add-to-list 'load-path "/home/eric/.emacs.d/lisp/qwe/ext")
(use-package qwe)

(global-page-break-lines-mode t)
(add-hook 'after-init-hook #'global-flycheck-mode)

(define-key helm-find-files-map (kbd "C-j") 'helm-select-action)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

(use-package helm-flycheck)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

(use-package flycheck-pyflakes
  :config
  (progn
    (add-to-list 'flycheck-disabled-checkers 'python-flake8)
    (add-to-list 'flycheck-disabled-checkers 'python-pylint)))

(use-package simple-mode-line
  :config (add-hook 'emacs-startup-hook 'activate-simple-mode-line))

(defhydra hydra-yank-pop (:idle 2.0)
  "yank"
  ("C-y" yank nil)
  ("M-y" yank-pop nil)
  ("y" (yank-pop 1) "next")
  ("Y" (yank-pop -1) "prev")
  ("l" helm-show-kill-ring "list" :color blue))

(global-set-key (kbd "C-y") #'hydra-yank-pop/yank)
(global-set-key (kbd "M-y") #'hydra-pank-pop/yank-pop)

(use-package peep-dired
  :config (define-key dired-mode-map (kbd "M-p") #'peep-dired))

(use-package change-inner
  :config
  (progn
    (global-set-key (kbd "M-i") 'change-inner)
    (global-set-key (kbd "M-o") 'change-outer)))

(use-package tiny
  :config (global-set-key (kbd "C-;") 'tiny-expand))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)

(global-highlight-parentheses-mode t)

(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-list-file-name nil)
(setq auto-save-default nil)

(global-set-key [remap mark-sexp] 'easy-mark)

(use-package anzu
  :config
  (progn
    (global-anzu-mode 1)
    (global-set-key (kbd "M-%") 'anzu-query-replace)
    (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
    (global-set-key (kbd "C-c %") 'anzu-replace-at-cursor-thing)))

(use-package olivetti)

(defun ejmr/toggle-writing-mode ()
  "Toggle a distraction-free environment for writing."
  (interactive)
  (cond ((bound-and-true-p olivetti-mode)
         (olivetti-mode -1)
         (olivetti-toggle-hide-modeline)
         (toggle-frame-fullscreen)
         (menu-bar-mode 1))
        (t
         (olivetti-mode 1)
         (olivetti-toggle-hide-modeline)
         (toggle-frame-fullscreen)
         (menu-bar-mode -1))))

(use-package artbollocks-mode
  :config (add-hook 'text-mode-hook 'artbollocks-mode))

(defhydra hydra-text (:hint nil :idle 2.0)
  "
^Modes^           ^Commands^            ^Rectangles^
----------------------------------------------------------------------
_w_ritting mode    _a_lign              _k_ill
_f_ill mode        s_o_rt               _y_ank
_l_ine mode        _D_efine word        o_p_en
fly_s_pell mode      (_d_ at point)     _c_lear
_t_ypo mode        _i_spell buffer      _n_umber
art_b_ollocks mode                    _r_eplace
                                    _I_nsert
"
  ("w" ejmr/toggle-writing-mode)
  ("b" artbollocks-mode)
  ("r" string-rectangle :color blue)
  ("k" kill-rectangle :color blue)
  ("y" yank-rectangle :color blue)
  ("c" clear-rectangle :color blue)
  ("I" string-insert-rectangle :color blue)
  ("n" rectangle-number-lines :color blue)
  ("p" open-rectangle :color blue)
  ("f" auto-fill-mode)
  ("a" align-regexp)
  ("i" ispell-buffer :color blue)
  ("o" sort-lines)
  ("l" visual-line-mode)
  ("s" flyspell-mode)
  ("d" define-word-at-point :color blue)
  ("D" define-word :color blue)
  ("t" typo-mode))

(global-set-key (kbd "C-c t") 'hydra-text/body)

(defhydra hydra-desktop (:color blue :idle 2.0)
  "desktop"
  ("c" desktop-clear "clear")
  ("s" desktop-save "save")
  ("r" desktop-revert "revert")
  ("d" desktop-change-dir "dir"))

(global-set-key (kbd "C-c d") 'hydra-desktop/body)

(defhydra hydra-major (:color blue :idle 2.0)
  "major-mode"
  ("t" text-mode "text")
  ("d" diff-mode "diff")
  ("l" lua-mode "lua")
  ("p" php-mode "php")
  ("a" mail-mode "mail")
  ("m" markdown-mode "markdown"))

(global-set-key (kbd "C-c m") 'hydra-major/body)

(defun ejmr/insert-email-signature ()
  "Insert my email signature into the current buffer."
  (interactive)
  (goto-char (point-max))
  (insert-file-contents "/home/eric/.signature"))

(use-package markdown-mode
  :config (define-key markdown-mode-map (kbd "C-c C-w") 'ejmr/insert-email-signature))

(use-package corral
  :config
  (progn
    (defhydra hydra-corral ()
      "corral"
      ("(" corral-parentheses-backward "Back")
      (")" corral-parentheses-forward "Forward")
      ("[" corral-brackets-backward "Back")
      ("]" corral-brackets-forward "Forward")
      ("{" corral-braces-backward "Back")
      ("}" corral-braces-forward "Forward")
      ("." hydra-repeat "Repeat"))
    (global-set-key (kbd "C-c c") #'hydra-corral/body)))

(defun ejmr/enable-mail-settings ()
  "Enable settings I like when writing mail."
  (interactive)
  (visual-line-mode 1)
  (flyspell-mode 1)
  (typo-mode 1))

(defun ejmr/possibly-enable-mail-settings ()
  "Enable my mail settings when visiting `/tmp/mail.md'."
  (if (string= "/tmp/mail.md" (buffer-file-name))
      (ejmr/enable-mail-settings)))

(add-hook 'markdown-mode-hook #'ejmr/possibly-enable-mail-settings)

(use-package git-blame)

(defhydra hydra-minor ()
  "minor-mode"
  ("i" global-aggressive-indent-mode "indent")
  ("f" global-flycheck-mode "flycheck")
  ("c" focus-mode "focus")
  ("b" highlight-blocks-mode "hl-blocks")
  ("g" git-blame-mode "git-blame")
  ("v" view-mode "view")
  ("s" firestarter-mode "firestarter")
  ("y" yas-minor-mode "yasnippet"))

(global-set-key (kbd "C-c n") 'hydra-minor/body)

(use-package quickrun
  :config
  (progn
    (defhydra hydra-quickrun (:color blue)
      "quickrun"
      ("q" quickrun "run")
      ("r" quickrun-region "region")
      ("w" quickrun-with-arg "with-arg")
      ("s" quickrun-shell "shell")
      ("c" quickrun-compile-only "compile")
      ("p" quickrun-replace-region "replace"))
    (global-set-key (kbd "C-c q") 'hydra-quickrun/body)))

(defhydra hydra-command (:color blue :idle 2.0)
  "command"
  ("w" whitespace-cleanup "whitespace")
  ("i" helm-imenu "imenu")
  ("g" helm-git-grep "git-grep")
  ("l" helm-ls-git-ls "git-ls")
  ("n" gnus "news")
  ("v" visit-tags-table "visit-tags")
  ("t" find-temp-file "temp"))

(global-set-key (kbd "C-c x") 'hydra-command/body)

(global-set-key (kbd "<f8>") 'helm-recentf)
(global-set-key (kbd "C-h C-m") 'discover-my-major)

(use-package swoop
  :config
  (progn
    (defhydra hydra-swoop (:color blue)
      "swoop"
      ("s" swoop)
      ("m" swoop-multi "multi")
      ("p" swoop-pcre-regexp "pcre")
      ("b" swoop-back-to-last-position "back"))
    (global-set-key (kbd "C-c s") 'hydra-swoop/body)))

(progn
  (setq find-temp-file-directory "/tmp")
  (setq find-temp-template-default "%M/%N-%T.%E"))

(global-set-key (kbd "<RET>") 'newline-and-indent)
(global-set-key (kbd "<C-return>") 'newline)
(global-set-key (kbd "<M-return>") 'indent-new-comment-line)

(add-hook 'prog-mode-hook 'fic-mode)

(use-package autopair
  :config (autopair-global-mode))

(use-package key-chord
  :config
  (progn
    (key-chord-mode 1)
    (setq key-chord-two-keys-delay 0.5)

    (defhydra hydra-chord (:color blue :hint nil)
      "
^Windows^    ^Files and Buffers^
-------------------------------
_o_ther       _k_ill buffer
_0_ delete    _s_ave buffer
_1_ only      _l_ist buffers
_4_ kill      _f_ind files
"
      ("o" other-window)
      ("0" delete-window)
      ("1" delete-other-windows)
      ("4" kill-buffer-and-window)
      ("k" kill-buffer)
      ("s" save-buffer)
      ("l" helm-buffers-list)
      ("f" helm-find-files))

    (key-chord-define-global "qq" 'hydra-chord/body)
    (key-chord-define-global "qn" 'operate-on-number-at-point)
    (key-chord-define-global "qd" #'duplicate-thing)
    (key-chord-define-global "qw" #'easy-kill)))

(use-package omni-kill
  :config
  (progn
    (defhydra hydra-omni-kill (:color blue :hint nil)
      "
Omni Kill
--------------------------------------------------
_d_efun      _n_umber        _S_exp
_e_mail      se_n_tence      _u_rl
_f_ilename   _P_page         _w_ord
_l_ine       _s_ymbol        _W_hitespace
_L_ist
"
      ("d" omni-kill-defun)
      ("e" omni-kill-email)
      ("f" omni-kill-filename)
      ("l" omni-kill-line)
      ("L" omni-kill-list)
      ("n" omni-kill-number)
      ("P" omni-kill-page)
      ("n" omni-kill-sentence)
      ("S" omni-kill-sexp)
      ("s" omni-kill-symbol)
      ("u" omni-kill-url)
      ("W" omni-kill-whitespace)
      ("w" omni-kill-word))
    (global-set-key (kbd "C-c k") #'hydra-omni-kill/body)))

(use-package expand-region
  :config (global-set-key (kbd "C-=") 'er/expand-region))

(add-to-list 'load-path "/home/eric/Software/recutils/etc/")
(use-package rec-mode)

(use-package fold-dwim
  :config
  (progn
    (defhydra hydra-fold (:pre (hs-minor-mode 1))
      "fold"
      ("t" fold-dwim-toggle "toggle")
      ("h" fold-dwim-hide-all "hide-all")
      ("s" fold-dwim-show-all "show-all"))

    (global-set-key (kbd "C-c f") 'hydra-fold/body)))

(use-package eproject
  :init (setq eproject-keybind-prefix "C-c e")
  :config
  (progn
    (use-package eproject-extras)
    (use-package eproject-tags
      :config (setq eproject-tags-etags "etags"))
    (use-package eproject-compile)
    (global-set-key (kbd "C-c e f") #'eproject-find-file)
    (global-set-key (kbd "C-c e k") #'eproject-kill-project-buffers)
    (global-set-key (kbd "C-c e v") #'eproject-revisit-project)
    (global-set-key (kbd "C-c e a") #'eproject-open-all-project-files)
    (global-set-key (kbd "C-c e g") #'eproject-grep)
    (global-set-key (kbd "C-c e t") #'eproject-tags)
    (global-set-key (kbd "C-c e T") #'eproject-todo)
    (global-set-key (kbd "C-c e c") #'eproject-compile)
    (global-set-key (kbd "C-c e s") #'eproject-multi-isearch-buffers)

    (defun eproject-helm-ag ()
      "Run helm-ag on the project's root directory."
      (interactive)
      (helm-ag (eproject-root)))

    (global-set-key (kbd "C-c e G") #'eproject-helm-ag)

    (define-project-type generic-tup (generic)
      (look-for "Tupfile")
      :common-compiles ("tup"))

    (define-project-type love (generic)
      (look-for "main.lua")
      :relevant-files ("\\.lua$" "\\.md$" "\\.sh$" "\\.org$" "Makefile" "Tupfile")
      :common-compiles ("make")
      :main-file "main.lua")))

(use-package bts
  :config
  (progn
    (defhydra hydra-bts (:color blue :hint nil)
"
    Bug Tracking System
------------------------------------------------------------
^Tickets:^        ^Projects:^        ^Query:^

_n_ew             [_pn_] new         [_qn_] new
_s_ummary         [_pu_] update      [_qu_] update
                [_pd_] delete      [_qd_] delete
                [_pD_] delete all  [_qD_] delete all
"
      ("n" bts:ticket-new)
      ("s" bts:summary-open)
      ("pn" bts:project-new)
      ("pu" bts:project-update)
      ("pd" bts:project-remove)
      ("pD" bts:project-remove-all)
      ("qn" bts:query-new)
      ("qu" bts:query-update)
      ("qd" bts:query-remove)
      ("qD" bts:query-remove-all))
    (global-set-key (kbd "C-c b") #'hydra-bts/body)))

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . diff-mode))

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

(set-register ?e '(file . "/home/eric/.emacs.d/init.el"))
(set-register ?t '(file . "/home/eric/Documents/Todo.org"))
(set-register ?m '(file . "/tmp/mail.md"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(ansi-term-color-vector
   [unspecified "#FAFAFA" "#FF1744" "#66BB6A" "#FFA000" "#42A5F5" "#7E57C2" "#0097A7" "#546E7A"])
 '(browse-url-browser-function (quote browse-url-conkeror))
 '(browse-url-generic-program "conkeror")
 '(browse-url-text-browser "eww")
 '(custom-enabled-themes (quote (minimal-light)))
 '(custom-safe-themes
   (quote
    ("c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "fb7b5b24d459ccf40f44659506a111ff0df9a07117a00aaee4999952a92f056a" "b9183de9666c3a16a7ffa7faaa8e9941b8d0ab50f9aaba1ca49f2f3aec7e3be9" "4af6fad34321a1ce23d8ab3486c662de122e8c6c1de97baed3aa4c10fe55e060" default)))
 '(evil-emacs-state-cursor (quote ("#D50000" bar)))
 '(evil-insert-state-cursor (quote ("#D50000" hbar)))
 '(evil-normal-state-cursor (quote ("#FFA000" box)))
 '(evil-visual-state-cursor (quote ("#66BB6A" box)))
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#d9d9d9" t)
 '(highlight-symbol-colors
   (quote
    ("#FFA000" "#66BB6A" "#0097A7" "#42A5F5" "#7E57C2" "#D84315")))
 '(highlight-symbol-foreground-color "#546E7A")
 '(highlight-tail-colors
   (if
       (eq
        (quote light)
        (quote light))
       (quote
        (("#FF5722" . 0)
         ("#FFA000" . 10)
         ("#FFD600" . 30)
         ("#f2f2f2" . 60)
         ("#FAFAFA" . 80)))
     (quote
      (("#F8BBD0" . 0)
       ("#EC407A" . 10)
       ("#B388FF" . 30)
       ("#f2f2f2" . 60)
       ("#FAFAFA" . 80)))))
 '(indent-tabs-mode nil)
 '(lua-indent-level 4)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("org" . "http://orgmode.org/elpa/")
     ("melpa" . "http://melpa.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(package-selected-packages
   (quote
    (helm-ls-git git-blame faff-theme use-package emoji-cheat-sheet-plus bts-github bts artbollocks-mode corral textile-mode omni-kill flycheck-pyflakes helm-j-cheatsheet helm-flyspell helm-ag define-word chronos omni-scratch eproject helm-gitignore totd csharp-mode ocodo-svg-modelines python-docstring autumn-light-theme peep-dired haskell-snippets fixmee flycheck-hdevtools flycheck-haskell haskell-emacs yaml-mode runner helm-dired-recent-dirs dired-toggle dired-details ix quack guile-scheme geiser chicken-scheme fasm-mode iasm-mode darkroom swiper page-break-lines glsl-mode md-readme dpaste emmet-mode lentic swoop helm-swoop discover-my-major fold-dwim love-minor-mode m-buffer helm-flycheck helm-git-grep flymake-lua olivetti fountain-mode tup-mode simple-mode-line ag ggtags flx-ido operate-on-number unfill highlight-blocks firestarter xkcd sqlplus sqlite edbi js3-mode iterator helm-ack esqlite-helm esqlite erlang epoch-view emoji-display emacsql golden-ratio elwm dummy-h-mode aproject anzu anyins anchored-transpose anaphora adoc-mode gitconfig-mode bbcode-mode whole-line-or-region yasnippet yaxception rainbow-identifiers pandoc-mode change-inner pastebin expand-region ert-expectations erefactor cerbere all-ext all @ aggressive-indent j-mode helm-make key-chord helm fish-mode duplicate-thing doremi-mac doremi-cmd doremi dired-efap lispy tiny form-feed flycheck-pos-tip flycheck-rust rust-mode dokuwiki-mode typo find-temp-file fic-mode recentf-ext tomatinho autopair electric-case haskell-mode go-mode flycheck-package flycheck php-mode hydra seq quickrun pomodoro org moe-theme markdown-mode lua-mode bubbleberry-theme basic-theme atom-dark-theme alect-themes)))
 '(pastebin-default-domain "pastebin.com/")
 '(pastebin-domain-versions
   (quote
    (("pastebin.com" "/api_public.php")
     ("pastbin.com" "/api")
     ("pastebin.example.com" "/pastebin.php"))))
 '(pos-tip-background-color "#ffffff")
 '(pos-tip-foreground-color "#78909C")
 '(tabbar-background-color "#ffffff"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-mode-line ((t (:foreground "green" :weight bold))))
 '(js3-external-variable-face ((t (:foreground "black" :weight bold)))))
