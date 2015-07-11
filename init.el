;;; init.el --- Emacs Settings
;;
;;; Commentary:
;;
;; Emacs configuration for Eric James Michael Ritz
;;     <ejmr@plutono.com>
;;
;;
;;
;;; Code:

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

(add-to-list 'load-path "/home/eric/.cask")
(require 'cask)


(use-package page-break-lines
  :config (page-break-lines-mode 1))


(use-package bury-successful-compilation
  :config (bury-successful-compilation 1))


(use-package pandoc-mode)


(use-package org
  :config
  (defhydra hydra-org (:color red :hint nil)
    "
^Navigation^
--------------------------------------------------
_n_ext heading
_p_rev heading
_N_ext heading at same level
_P_rev heading at same level
_u_p   heading
_g_o to"
    ("n" outline-next-visible-heading)
    ("p" outline-previous-visible-heading)
    ("N" org-forward-heading-same-level)
    ("P" org-backward-heading-same-level)
    ("u" outline-up-heading)
    ("g" org-goto :exit t))
  (global-set-key (kbd "C-c o") #'hydra-org/body)
  (defun ejmr/enable-org-mode-settings ()
    (auto-fill-mode 1))
  (add-hook 'org-mode-hook #'ejmr/enable-org-mode-settings))


(use-package shrink-whitespace
  :config (global-set-key (kbd "M-\\") #'shrink-whitespace))


(use-package js3-mode
  :mode ("\\.js$" . js3-mode))

(use-package conkeror-minor-mode
  :config
  (defun ejmr/maybe-enable-conkeror-minor-mode ()
    (when (string-match "conkeror" (buffer-file-name))
      (conkeror-minor-mode 1)))
  (add-hook 'js3-mode-hook #'ejmr/maybe-enable-conkeror-minor-mode))


(progn
  (use-package rainbow-blocks)
  (use-package ranibow-identifiers)
  (use-package rainbow-delimiters)
  (defhydra hydra-rainbow ()
    "rainbow"
    ("b" rainbow-blocks-mode "blocks")
    ("i" rainbow-identifiers-mode "identifiers")
    ("d" rainbow-delimiters-mode "delimiters"))
  (global-set-key (kbd "C-c r") #'hydra-rainbow/body))


(use-package yasnippet
  :config
  (use-package auto-yasnippet)
  (defhydra hydra-yasnippet (:color red :hint nil)
      "
              YASnippets
--------------------------------------------
  ^Modes:^    ^Load/Visit:^    ^Actions:^

  _g_lobal    _d_irectory      _i_nsert
  _m_inor     _f_ile           _t_ryout
  _e_xtra     _a_ll            _n_ew
                      auto _c_reate
                      auto e_x_pand
                      auto _o_pen
"
      ("d" yas-load-directory)
      ("e" yas-activate-extra-mode)
      ("i" yas-insert-snippet)
      ("f" yas-visit-snippet-file :color blue)
      ("n" yas-new-snippet)
      ("t" yas-tryout-snippet)
      ("g" yas/global-mode)
      ("m" yas/minor-mode)
      ("a" yas-reload-all)
      ("c" aya-create)
      ("x" aya-expand)
      ("o" aya-open-line))
  (global-set-key (kbd "C-c y") 'hydra-yasnippet/body)
  (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt)))


(use-package forth-mode
  :load-path "/home/eric/.emacs.d/lisp/"
  :mode ("\\.fs$" . forth-mode))


(use-package picolisp-mode
  :mode ("\\.l$" . picolisp-mode))


(use-package lua-mode
  :mode "\\.lnvl$")


(use-package recursive-narrow
  :config
  (global-set-key (kbd "C-x n n") #'recursive-narrow-or-widen-dwim)
  (global-set-key (kbd "C-x n w") #'recursive-widen-dwim))


(use-package recentf
  :config (recentf-mode 1))
(use-package yaml-mode)
(use-package tomatinho
  :config (global-set-key (kbd "<f12>") 'tomatinho))
(use-package hydra
  :config (global-set-key (kbd "C-s-h") #'hydra-pause-resume))
(use-package highlight-parentheses)
(use-package helm-config)
(use-package helm-git-grep)
(use-package helm-ls-git)
(use-package helm-ag)
(use-package helm-j-cheatsheet)
(use-package helm-make)
(use-package swiper-helm
  :config
  (global-set-key (kbd "C-s") #'swiper-helm)
  (global-set-key (kbd "C-r") #'swiper-helm))
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


(use-package helm-mode-manager
  :config
  (defhydra hydra-helm-mode-manager (:color blue)
    "Modes"
    ("m" helm-switch-major-mode "major")
    ("n" helm-enable-minor-mode "minor on")
    ("N" helm-disable-minor-mode "minor off"))
  (global-set-key (kbd "C-c h") #'hydra-helm-mode-manager/body))


(use-package adoc-mode
  :mode (("\\.adoc$" . adoc-mode)
         ("\\.asciidoc$" . adoc-mode))
  :config
  (defun ejmr/enable-adoc-mode-settings ()
    (auto-fill-mode 1)
    (flyspell-mode 1)
    (typo-mode 1))
  (add-hook 'adoc-mode-hook #'ejmr/enable-adoc-mode-settings))


(use-package textile-mode
  :mode "\\.textile$")


(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))


(add-to-list 'load-path "/home/eric/.emacs.d/lisp/qwe/src")
(add-to-list 'load-path "/home/eric/.emacs.d/lisp/qwe/ext")
(use-package qwe)


(use-package smalltalk-mode
  :load-path "/home/eric/Software/GNU Smalltalk/"
  :mode "\\.st$")


(global-page-break-lines-mode t)
(add-hook 'after-init-hook #'global-flycheck-mode)


(define-key helm-find-files-map (kbd "C-j") 'helm-select-action)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)


(use-package helm-flycheck)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

(use-package flycheck-pyflakes
  :config
  (add-to-list 'flycheck-disabled-checkers 'python-flake8)
  (add-to-list 'flycheck-disabled-checkers 'python-pylint))


(defhydra hydra-yank-pop ()
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
  (global-set-key (kbd "M-i") 'change-inner)
  (global-set-key (kbd "M-o") 'change-outer))


(use-package tiny
  :config
  (global-set-key (kbd "C-s-t") 'tiny-expand))


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
  (global-anzu-mode 1)
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
  (global-set-key (kbd "C-c %") 'anzu-replace-at-cursor-thing))


(use-package darkroom
  :config
  (defun ejmr/toggle-writing-mode ()
    "Toggle a distraction-free environment for writing."
    (interactive)
    (cond ((bound-and-true-p darkroom-mode)
           (darkroom-mode -1)
           (menu-bar-mode 1))
          (t
           (darkroom-mode 1)
           (menu-bar-mode -1)))))

(use-package artbollocks-mode
  :config (add-hook 'text-mode-hook 'artbollocks-mode))

(defhydra hydra-text (:hint nil)
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


(use-package string-inflection
  :config (global-set-key (kbd "C-\"") #'string-inflection-all-cycle))


(use-package unicode-enbox)


(use-package fix-word
  :config
  (global-set-key (kbd "M-u") #'fix-word-upcase)
  (global-set-key (kbd "M-l") #'fix-word-downcase)
  (global-set-key (kbd "M-c") #'fix-word-capitalize))


(defhydra hydra-desktop (:color blue)
  "desktop"
  ("c" desktop-clear "clear")
  ("s" desktop-save "save")
  ("r" desktop-revert "revert")
  ("d" desktop-change-dir "dir"))

(global-set-key (kbd "C-c d") 'hydra-desktop/body)


(use-package poly-markdown)


(defun ejmr/insert-email-signature ()
  "Insert my email signature into the current buffer."
  (interactive)
  (goto-char (point-max))
  (insert-file-contents "/home/eric/.signature"))

(use-package markdown-mode
  :config (define-key markdown-mode-map (kbd "C-c C-w") 'ejmr/insert-email-signature))


(use-package avy
  :config
  (defhydra hydra-avy (:color blue)
    "avy-goto"
    ("c" avy-goto-char "char")
    ("C" avy-goto-char-2 "char-2")
    ("l" avy-goto-line "line")
    ("w" avy-goto-word-1 "word")
    ("W" avy-goto-word-0 "word-0"))
  (global-set-key (kbd "C-c a") #'hydra-avy/body))


(use-package corral
  :config
  (defhydra hydra-corral ()
    "corral"
    ("(" corral-parentheses-backward "Back")
    (")" corral-parentheses-forward "Forward")
    ("[" corral-brackets-backward "Back")
    ("]" corral-brackets-forward "Forward")
    ("{" corral-braces-backward "Back")
    ("}" corral-braces-forward "Forward")
    ("." hydra-repeat "Repeat"))
  (global-set-key (kbd "C-c c") #'hydra-corral/body))


(use-package racket-mode)


(defun ejmr/enable-mail-settings ()
  "Enable settings I like when writing mail."
  (interactive)
  (visual-line-mode 1)
  (flyspell-mode 1)
  (typo-mode 1))

(defun ejmr/possibly-enable-mail-settings ()
  "Enable my mail settings when visiting my draft mail file."
  (if (string= "/home/eric/Documents/Mail.md" (buffer-file-name))
      (ejmr/enable-mail-settings)))

(add-hook 'markdown-mode-hook #'ejmr/possibly-enable-mail-settings)


(use-package git-blame)
(use-package git-messenger
  :config (global-set-key (kbd "C-x v p") #'git-messenger:popup-message))


(use-package imenu-list)
(use-package annotate)


(use-package solid-mode
  :load-path "/home/eric/Software/Solid/etc/")


(use-package quickrun
  :config
  
  (quickrun-add-command
   "solid"
   '((:command . "solid")
     (:exec . "%c %s"))
   :default "solid"
   :mode 'solid-mode)
  
  (defhydra hydra-quickrun (:color blue)
    "quickrun"
    ("q" quickrun "run")
    ("r" quickrun-region "region")
    ("w" quickrun-with-arg "with-arg")
    ("s" quickrun-shell "shell")
    ("c" quickrun-compile-only "compile")
    ("p" quickrun-replace-region "replace"))
  (global-set-key (kbd "C-c q") 'hydra-quickrun/body))


(use-package fish-mode :mode "\\.fish$")


(use-package find-temp-file)

(defhydra hydra-command (:color blue)
  "command"
  ("w" whitespace-cleanup "whitespace")
  ("i" helm-imenu "imenu")
  ("g" helm-git-grep "git-grep")
  ("l" helm-ls-git-ls "git-ls")
  ("v" visit-tags-table "visit-tags")
  ("t" find-temp-file "temp"))

(global-set-key (kbd "C-c x") 'hydra-command/body)


(global-set-key (kbd "<f8>") 'helm-recentf)
(global-set-key (kbd "C-h C-m") 'discover-my-major)


(use-package swoop
  :config
  (defhydra hydra-swoop (:color blue)
    "swoop"
    ("s" swoop)
    ("m" swoop-multi "multi")
    ("p" swoop-pcre-regexp "pcre")
    ("b" swoop-back-to-last-position "back"))
  (global-set-key (kbd "C-c s") 'hydra-swoop/body))


(progn
  (setq find-temp-file-directory "/tmp")
  (setq find-temp-template-default "%M/%N-%T.%E"))


(global-set-key (kbd "<RET>") 'newline-and-indent)
(global-set-key (kbd "<C-return>") 'newline)
(global-set-key (kbd "<M-return>") 'indent-new-comment-line)


(add-hook 'prog-mode-hook 'fic-mode)


(use-package autopair
  :config (autopair-global-mode))


(progn
  (defhydra hydra-window (:hint nil)
    "
          Split: _v_ert  _s_:horz
         Delete: _c_lose  _o_nly
  Switch Window: _h_:left  _j_:down  _k_:up  _l_:right
        Buffers: _p_revious  _n_ext  _b_:select  _f_ind-file  _F_projectile
         Winner: _u_ndo  _r_edo
         Resize: _H_:splitter left  _J_:splitter down  _K_:splitter up  _L_:splitter right
           Move: _a_:up  _z_:down  _i_menu"
    ("z" scroll-up-line)
    ("a" scroll-down-line)
    ("i" idomenu)
    ("u" winner-undo)
    ("r" winner-redo)
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)
    ("p" previous-buffer)
    ("n" next-buffer)
    ("b" ido-switch-buffer) 
    ("f" ido-find-file)
    ("F" projectile-find-file)
    ("s" split-window-below)
    ("v" split-window-right)
    ("c" delete-window)
    ("o" delete-other-windows)
    ("H" hydra-move-splitter-left)
    ("J" hydra-move-splitter-down)
    ("K" hydra-move-splitter-up)
    ("L" hydra-move-splitter-right)
    ("q" nil))
  (global-set-key (kbd "C-c w") #'hydra-window/body))


(use-package key-chord
  :config
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
  (key-chord-define-global "qr" #'read-only-mode)
  (key-chord-define-global "qn" 'operate-on-number-at-point)
  (key-chord-define-global "qd" #'duplicate-thing)
  (key-chord-define-global "qw" #'easy-kill))


(use-package omni-kill
  :disabled t
  :config
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
  (global-set-key (kbd "C-c k") #'hydra-omni-kill/body))


(use-package expand-region
  :config (global-set-key (kbd "C-=") 'er/expand-region))


(add-to-list 'load-path "/home/eric/Software/recutils/etc/")
(use-package rec-mode)


(use-package fold-dwim
  :config
  (defhydra hydra-fold (:pre (hs-minor-mode 1))
    "fold"
    ("t" fold-dwim-toggle "toggle")
    ("h" fold-dwim-hide-all "hide-all")
    ("s" fold-dwim-show-all "show-all"))

  (global-set-key (kbd "C-c f") 'hydra-fold/body))


(use-package ert-modeline)


(use-package lispy
  :config
  (defun ejmr/enable-scheme-mode-settings ()
    (autopair-mode -1)
    (lispy-mode 1)
    (geiser-mode -1))
  (add-hook 'scheme-mode-hook #'ejmr/enable-scheme-mode-settings))

(define-skeleton ejmr/scsh-skeleton
  "Inserts the basic structure for a Scheme Shell script."
  nil
  "#!/usr/local/bin/scsh \\\n"
  "-e main -s\n"
  "!#\n\n"
  > "(define (main arguments)\n"
  > _ ")")


(use-package eproject
  :init (setq eproject-keybind-prefix "C-c e")
  :config
  (use-package eproject-extras)
  (use-package eproject-tags
    :config (setq eproject-tags-etags "etags"))
  (use-package eproject-compile)
  (use-package helm-eproject)
  (use-package eproject-tasks)
  (use-package eproject-python)

  (defun eproject-helm-ag ()
    "Run helm-ag on the project's root directory."
    (interactive)
    (helm-ag (eproject-root)))

  (global-set-key (kbd "C-c e f") #'eproject-find-file)
  (global-set-key (kbd "C-c e h") #'helm-eproject)
  (global-set-key (kbd "C-c e k") #'eproject-kill-project-buffers)
  (global-set-key (kbd "C-c e r") #'eproject-revisit-project)
  (global-set-key (kbd "C-c e a") #'eproject-open-all-project-files)
  (global-set-key (kbd "C-c e g") #'eproject-grep)
  (global-set-key (kbd "C-c e t") #'eproject-tags)
  (global-set-key (kbd "C-c e T") #'eproject-todo)
  (global-set-key (kbd "C-c e c") #'eproject-compile)
  (global-set-key (kbd "C-c e s") #'eproject-multi-isearch-buffers)
  (global-set-key (kbd "C-c e G") #'eproject-helm-ag)
  (global-set-key (kbd "C-c e S") #'helm-eproject-tasks)

  (define-project-type elisp (generic)
    (look-for "*.el")
    :relevant-files ("\\.el$" ".ert-runner" "Cask")
    :irrelevant-files ("\\.elc$"))

  (define-project-type haskell (generic)
    (or (look-for "*.cabal")
        (look-for "Main.hs")
        (look-for "Setup.hs"))
    :main-file "Main.hs"
    :relevant-files ("\\.hs" "\\.lhs")
    :common-compiles ("cabal build"))

  (define-project-type lua (generic)
    (or (look-for "*.rockspec")
        (look-for ".busted")
        (look-for "main.lua"))
    :relevant-files ("\\.lua$"))

  (define-project-type scheme (generic)
    (look-for "*.scm")
    :relevant-files ("\\.scm$"))

  (define-project-type bash (generic)
    (look-for "*.sh")
    :relevant-files ("\\.sh$"))

  (define-project-type travis-ci (generic)
    (look-for ".travis.yml")
    :main-file ".travis.yml")

  (define-project-type common-lisp (generic)
    (or (look-for "*.lisp")
        (look-for "*.asd"))
    :relevant-files ("\\.lisp"))

  (define-project-type java (generic)
    (or (look-for "pom.xml")
        (look-for "build.xml"))
    :relevant-files ("\\.java" "\\.xml")
    :irrelevant-files ("\\.class")
    :common-compiles ("ant" "maven"))

  (define-project-type php (generic)
    (look-for "composer.json")
    :relevant-files ("\\.php")
    :common-compiles ("composer install"))

  (define-project-type ruby (generic)
    (or (look-for "Gemfile")
        (look-for "*.gemspec")
        (look-for "Rakefile"))
    :relevant-files ("\\.rb"))

  (define-project-type love (lua)
    (or (look-for "*.love")
        (look-for "conf.lua")
        (look-for "main.lua"))
    :tasks (("play" :shell "love *.love"))
    :main-file "main.lua")

  (define-project-type tup (generic)
    (or (look-for "Tupfile")
        (look-for "Tupfile.lua"))
    :common-compiles ("tup"))

  (define-project-type make (generic)
    (look-for "Makefile")
    :common-compiles ("make")
    :main-file "Makefile")

  (define-project-type documentation (generic)
    (or (look-for "*.md")
        (look-for "*.org")
        (look-for "*.adoc")
        (look-for "*.textile")
        (look-for "README"))
    :relevant-files ("\\.md$" "\\.org$" "README" "\\.adoc$" "\\.textile")))


(use-package undo-tree
  :config (global-undo-tree-mode))


(use-package bts
  :config
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
  (global-set-key (kbd "C-c b") #'hydra-bts/body))


(use-package llvm-mode
  :config
  (use-package autodisass-llvm-bitcode))


(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . diff-mode))


(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;;; init.el ends here
