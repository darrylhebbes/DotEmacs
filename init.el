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

(require 'use-package)
(require 'bind-key)

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

(defun ejmr/create-initial-buffer ()
  (bookmark-bmenu-list)
  (switch-to-buffer "*Bookmark List*"))

(add-to-list 'load-path "/home/eric/.cask")
(require 'cask)

(global-set-key (kbd "<C-prior>") #'backward-page)
(global-set-key (kbd "<C-next>") #'forward-page)


(use-package pc-bufsw
  :config (pc-bufsw-default-keybindings))


(use-package bookmark+)


(use-package nameless
  :config (add-hook 'emacs-lisp-mode-hook #'nameless-mode))


(use-package page-break-lines
  :config (global-page-break-lines-mode 1))


(use-package eyebrowse
  :config (eyebrowse-mode t))


(use-package electric-operator)


(use-package ws-butler
  :diminish ws-butler-mode
  :config (add-hook 'prog-mode-hook 'ws-butler-mode))


(use-package bury-successful-compilation
  :config (bury-successful-compilation 1))


(use-package pandoc-mode)


(use-package toml-mode)


(use-package logmode)


(use-package org
  :config
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN PROGRESS(p)" "|" "DONE(d)")
          (sequence "REPORT(r)" "BUG(b)" "TESTING(e)" "|" "FIXED(f)")
          (sequence "|" "CANCELED(c)")))
  (defhydra hydra-org (:color red :columns 3)
    "Org Mode Movements"
    ("n" outline-next-visible-heading "next heading")
    ("p" outline-previous-visible-heading "prev heading")
    ("N" org-forward-heading-same-level "next heading at same level")
    ("P" org-backward-heading-same-level "prev heading at same level")
    ("u" outline-up-heading "up heading")
    ("g" org-goto "goto" :exit t))
  (global-set-key (kbd "C-c o") #'hydra-org/body)
  (defun ejmr/enable-org-mode-settings ()
    (auto-fill-mode 1))
  (add-hook 'org-mode-hook #'ejmr/enable-org-mode-settings))


(use-package shrink-whitespace
  :config (global-set-key (kbd "M-\\") #'shrink-whitespace))


(use-package guide-key
  :disabled t
  :config
  (setq guide-key/recursive-key-sequence-flag t)
  (setq guide-key/guide-key-sequence
        '("C-s-n"
          "M-s-n"))
  (guide-key-mode 1))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
  (setq which-key-use-C-h-for-paging t
        which-key-prevent-C-h-from-cycling t))

(use-package helm-descbinds
  :config (helm-descbinds-mode))


(use-package nasm-mode)


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
  (defhydra hydra-yasnippet (:color red :columns 3)
      "YASnippet"
      ("g" yas/global-mode "global mode")
      ("m" yas/minor-mode "minor mode")
      ("e" yas-activate-extra-mode "extra mode")
      ("d" yas-load-directory "load directory")
      ("f" yas-visit-snippet-file "load file" :color blue)
      ("a" yas-reload-all "load all")
      ("i" yas-insert-snippet "insert")
      ("t" yas-tryout-snippet "tryout")
      ("n" yas-new-snippet "new")
      ("c" aya-create "aya-create")
      ("x" aya-expand "aya-expand")
      ("o" aya-open-line "aya-open"))
  (global-set-key (kbd "C-c y") 'hydra-yasnippet/body)
  (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt)))


(use-package forth-mode
  :load-path "/home/eric/.emacs.d/lisp/"
  :mode ("\\.fs$" . forth-mode))


(use-package picolisp-mode
  :mode ("\\.l$" . picolisp-mode))


(use-package lua-mode
  :mode (("\\.lnvl$" . lua-mode)
         ("\\.spec\\.lua$" . fundamental-mode)
         (".luacheckrc" . lua-mode)))


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
(use-package highlight-parentheses :diminish highlight-parentheses-mode)
(use-package highlight-thing
  :config (global-highlight-thing-mode)
  :diminish highlight-thing-mode)
(use-package helm-config)
(use-package helm-git-grep)
(use-package helm-themes)
(use-package helm-hoogle)
(use-package helm-pydoc)
(use-package helm-perldoc)
(use-package helm-ls-git)
(use-package helm-ag)
(use-package helm-j-cheatsheet)
(use-package helm-make)
(use-package helm-pages
  :config
  (global-set-key (kbd "C-x [") #'helm-pages)
  (global-set-key (kbd "C-x ]") #'helm-pages))
(use-package swiper-helm
  :disabled t
  :config
  (global-set-key (kbd "C-s") #'swiper-helm)
  (global-set-key (kbd "C-r") #'swiper-helm))
(use-package aggressive-indent)
(use-package pastebin)
(use-package bbcode-mode)
(use-package flycheck
  :config (use-package flycheck-tip
            :config (flycheck-tip-use-timer 'verbose)))
(use-package anyins)
(use-package firestarter)
(use-package highlight-blocks)
(use-package tup-mode)
(use-package love-minor-mode)
(use-package page-break-lines :diminish page-break-lines-mode)
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
  :diminish anzu-mode
  :config
  (global-anzu-mode 1)
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
  (global-set-key (kbd "C-c %") 'anzu-replace-at-cursor-thing))


(use-package langtool
  :config
  (setq langtool-language-tool-jar "/home/eric/Software/LanguageTool/languagetool-commandline.jar")
  (defhydra hydra-langtool ()
    "langtool"
    ("c" langtool-check "check")
    ("b" langtool-correct-buffer "correct-buffer")
    ("s" langtool-show-message-at-point "show-at-point")
    ("d" langtool-check-done "done" :color blue))
  (global-set-key (kbd "C-c l") #'hydra-langtool/body))

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

(use-package fountain-mode
  :mode ("\\.fountain$" . fountain-mode))

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
  :config
  (define-key markdown-mode-map (kbd "C-c w") 'ejmr/insert-email-signature)
  (defhydra hydra-markdown (:hint nil)
    "
Formatting        C-c C-s    _s_: bold          _e_: italic     _b_: blockquote   _p_: pre-formatted    _c_: code
Headings          C-c C-t    _h_: automatic     _1_: h1         _2_: h2           _3_: h3               _4_: h4
Lists             C-c C-x    _m_: insert item
Demote/Promote    C-c C-x    _l_: promote       _r_: demote     _u_: move up      _d_: move down
Links, footnotes  C-c C-a    _L_: link          _U_: uri        _F_: footnote     _W_: wiki-link      _R_: reference
"
    ("s" markdown-insert-bold)
    ("e" markdown-insert-italic)
    ("b" markdown-insert-blockquote :color blue)
    ("p" markdown-insert-pre :color blue)
    ("c" markdown-insert-code)
    ("h" markdown-insert-header-dwim)
    ("1" markdown-insert-header-atx-1)
    ("2" markdown-insert-header-atx-2)
    ("3" markdown-insert-header-atx-3)
    ("4" markdown-insert-header-atx-4)
    ("m" markdown-insert-list-item)
    ("l" markdown-promote)
    ("r" markdown-demote)
    ("d" markdown-move-down)
    ("u" markdown-move-up)
    ("L" markdown-insert-link :color blue)
    ("U" markdown-insert-uri :color blue)
    ("F" markdown-insert-footnote :color blue)
    ("W" markdown-insert-wiki-link :color blue)
    ("R" markdown-insert-reference-link-dwim :color blue))
  (define-key markdown-mode-map (kbd "C-s-k") #'hydra-markdown/body))


(use-package avy
  :config
  (use-package link-hint)
  (global-set-key (kbd "M-g g") #'avy-goto-line)
  (defhydra hydra-avy (:color blue)
    "avy-goto"
    ("c" avy-goto-char "char")
    ("C" avy-goto-char-2 "char-2")
    ("l" avy-goto-line "line")
    ("w" avy-goto-word-1 "word")
    ("W" avy-goto-word-0 "word-0")
    ("s" avy-goto-subword-1 "subword")
    ("S" avy-goto-subword-0 "subword-0")
    ("u" link-hint-open-link "open-URI")
    ("U" link-hint-copy-link "copy-URI"))
  (global-set-key (kbd "C-c a") #'hydra-avy/body))


(use-package corral
  :config
  (defhydra hydra-corral (:columns 4)
    "Corral"
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
   "scheme/guile"
   '((:command . "guile")
     (:exec . "%c %s"))
   :default "scheme"
   :mode 'scheme-mode)

  (quickrun-add-command
   "eslisp/javascript"
   '((:command . "eslc")
     (:exec . "%c %s"))
   :mode 'prog-mode)

  (quickrun-add-command
   "solid"
   '((:command . "solid")
     (:exec . "%c %s"))
   :default "solid"
   :mode 'solid-mode)
  
  (defhydra hydra-quickrun (:color blue :columns 2)
    "Quickrun"
    ("h" helm-quickrun "helm")
    ("q" quickrun "run")
    ("r" quickrun-region "region")
    ("w" quickrun-with-arg "with-arg")
    ("s" quickrun-shell "shell")
    ("c" quickrun-compile-only "compile")
    ("p" quickrun-replace-region "replace"))
  (global-set-key (kbd "C-c q") 'hydra-quickrun/body))


(use-package fish-mode :mode "\\.fish$")


(use-package find-temp-file)
(use-package lively)
(use-package keyword-search)

(defhydra hydra-command (:color blue :columns 2)
  "Command"
  ("w" whitespace-cleanup "whitespace")
  ("i" helm-imenu "imenu")
  ("g" helm-git-grep "git-grep")
  ("l" helm-ls-git-ls "git-ls")
  ("k" keyword-search "keyword-search")
  ("L" lively "lively")
  ("S" lively-stop "lively-stop")
  ("r" revert-buffer "revert-buffer")
  ("v" visit-tags-table "visit-tags")
  ("t" find-temp-file "temp"))

(global-set-key (kbd "C-c x") 'hydra-command/body)


(defhydra hydra-remember (:columns 2)
  ("r" remember "remember")
  ("R" remember-region "region")
  ("c" remember-clipboard "clipboard")
  ("f" remember-finalize "finalize" :color blue)
  ("d" remember-destroy "destroy" :color blue)
  ("n" remember-notes "notes" :color blue))

(global-set-key (kbd "<f7>") #'hydra-remember/body)


(use-package bm
  :config
  (global-set-key (kbd "<f9>") #'bm-toggle)
  (global-set-key (kbd "<M-f9>") #'bm-previous)
  (global-set-key (kbd "<C-f9>") #'bm-next))


(global-set-key (kbd "<f8>") 'helm-recentf)


(use-package discover-my-major
  :config
  (global-set-key (kbd "C-h C-m") #'discover-my-major))


(use-package swoop
  :init (use-package helm-swoop)
  :config
  (defhydra hydra-swoop (:color blue)
    "swoop"
    ("h" helm-swoop "helm")
    ("s" swoop "swoop")
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


(use-package fic-mode :diminish fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)


(use-package autopair
  :diminish autopair-mode
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

  (defhydra hydra-main-chords (:color blue :hint nil)
      "
^Windows^    ^Files and Buffers^
-------------------------------
_o_ther       _k_ill buffer
_0_ delete    _s_ave buffer
_1_ only      _l_ist buffers
_4_ kill      _f_ind files
            _r_evert buffer
"
      ("o" other-window)
      ("0" delete-window)
      ("1" delete-other-windows)
      ("4" kill-buffer-and-window)
      ("k" kill-buffer)
      ("s" save-buffer)
      ("l" helm-buffers-list)
      ("r" revert-buffer)
      ("f" helm-find-files))

  (defhydra hydra-occur-chords (:color blue)
    "occur"
    ("o" occur "occur")
    ("k" keep-lines "keep-lines")
    ("f" flush-lunes "flush-lines")
    ("h" highlight-phrase "highlight"))

  (use-package diffview)
  (defhydra hydra-diffview-chords (:color blue)
    "diffview"
    ("c" diffview-current "current")
    ("r" diffview-region "region")
    ("m" diffview-message "message"))

  (use-package helm-open-github
    :config
    (defhydra hydra-helm-open-github (:color blue)
      "helm-open-github"
      ("c" helm-open-github-from-commit "commit")
      ("f" helm-open-github-from-file "file")
      ("i" helm-open-github-from-issues "issue")
      ("p" helm-open-github-from-pull-requests "pull-request"))
    (key-chord-define-global "qg" #'hydra-helm-open-github/body))

  (key-chord-define-global "qv" 'hydra-diffview-chords/body)
  (key-chord-define-global "qq" 'hydra-main-chords/body)
  (key-chord-define-global "qo" 'hydra-occur-chords/body)
  (key-chord-define-global "qt" #'global-highlight-thing-mode)
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

(use-package eno
  :config
  (bind-keys
    ("C-s-n a". eno-word-goto)
    ("C-s-n b". eno-word-copy)
    ("C-s-n c". eno-word-cut)
    ("C-s-n d". eno-word-paste)
    ("C-s-n e". eno-symbol-goto)
    ("C-s-n f". eno-symbol-copy)
    ("C-s-n g". eno-symbol-cut)
    ("C-s-n h". eno-symbol-paste)
    ("C-s-n i". eno-str-goto)
    ("C-s-n j". eno-str-copy)
    ("C-s-n k". eno-str-cut)
    ("C-s-n l". eno-str-paste)
    ("C-s-n m". eno-line-goto)
    ("C-s-n n". eno-line-copy)
    ("C-s-n o". eno-line-cut)
    ("C-s-n p". eno-line-paste)
    ("C-s-n q". eno-paren-goto)
    ("C-s-n r". eno-paren-copy)
    ("C-s-n s". eno-paren-cut)
    ("C-s-n t". eno-paren-paste)
    ("M-s-n a". eno-symbol-copy-to)
    ("M-s-n b". eno-symbol-cut-to)
    ("M-s-n c". eno-symbol-paste-to)
    ("M-s-n d". eno-line-copy-to)
    ("M-s-n e". eno-line-cut-to)
    ("M-s-n f". eno-line-paste-to)
    ("M-s-n g". eno-line-comment-to)
    ("M-s-n h". eno-symbol-copy-from-to)
    ("M-s-n i". eno-symbol-cut-from-to)
    ("M-s-n j". eno-symbol-paste-from-to)
    ("M-s-n k". eno-line-copy-from-to)
    ("M-s-n l". eno-line-cut-from-to)
    ("M-s-n m". eno-line-paste-from-to)
    ("M-s-n n". eno-line-comment-from-to)
    ("M-s-n o". eno-word-goto-inline)
    ("M-s-n p". eno-word-copy-to-inline)
    ("M-s-n q". eno-word-cut-to-inline)
    ("M-s-n r". eno-word-paste-to-inline)
    ("M-s-n s". eno-url-open)
    ("M-s-n t". eno-clear-overlay)))


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
  :diminish lispy-mode
  :config
  (define-key lispy-mode-map (kbd "V") #'eproject-find-file)
  (define-key lispy-mode-map (kbd "T") #'ert)
  (defun ejmr/toggle-lispy-mode ()
    (interactive)
    (if lispy-mode
        (progn (lispy-mode -1)
               (autopair-mode 1))
      (progn (lispy-mode 1)
             (autopair-mode -1))))
  (define-key emacs-lisp-mode-map (kbd "C-c C-l") #'ejmr/toggle-lispy-mode))

(define-skeleton ejmr/scsh-skeleton
  "Inserts the basic structure for a Scheme Shell script."
  nil
  "#!/usr/local/bin/scsh \\\n"
  "-e main -s\n"
  "!#\n\n"
  > "(define (main arguments)\n"
  > _ ")")


(use-package eproject
  :diminish eproject-mode
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

  (define-project-type eslisp (generic)
    (look-for "*.esl")
    :relevant-files ("\\.esl$")
    :common-compiles ("eslc"))

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

  (define-project-type rust (generic)
    (look-for "Cargo.toml")
    :common-compiles ("cargo")
    :main-file "main.rs"
    :relevant-files ("Cargo.toml" "\\.rs$"))

  (define-project-type documentation (generic)
    (or (look-for "*.md")
        (look-for "*.org")
        (look-for "*.adoc")
        (look-for "*.textile")
        (look-for "README"))
    :relevant-files ("\\.md$" "\\.org$" "README" "\\.adoc$" "\\.textile")))


(use-package jdee)


(use-package undo-tree
  :diminish undo-tree-mode
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


(use-package comment-dwim-2
  :config (global-set-key (kbd "M-;") #'comment-dwim-2))


(use-package general-close
  :config (global-set-key (kbd "C-s-c") #'general-close))


(use-package origami
  :config
  (defhydra hydra-origami (:color red :hint nil)
    "
  _o_pen node    _n_ext fold       toggle _f_orward
  _c_lose node   _p_revious fold   toggle _a_ll
                               toggle _r_ecursively
  "
    ("o" origami-open-node)
    ("c" origami-close-node)
    ("n" origami-next-fold)
    ("p" origami-previous-fold)
    ("f" origami-forward-toggle-node)
    ("r" origami-recursively-toggle-node)
    ("a" origami-toggle-all-nodes))
  (global-set-key (kbd "C-c g") #'hydra-origami/body))


(use-package c0-mode
  :mode "\\.[ch]0$")


(use-package modalka
  :config
  (global-set-key (kbd "<f6>") #'modalka-global-mode)
  (modalka-define-kbd "a" "C-a")
  (modalka-define-kbd "e" "C-e")
  (modalka-define-kbd "A" "C-M-a")
  (modalka-define-kbd "E" "C-M-e")
  (modalka-define-kbd "f" "C-f")
  (modalka-define-kbd "F" "M-f")
  (modalka-define-kbd "b" "C-b")
  (modalka-define-kbd "B" "M-b")
  (modalka-define-kbd "p" "C-p")
  (modalka-define-kbd "n" "C-n")
  (modalka-define-kbd "v" "C-v")
  (modalka-define-kbd "V" "M-v")
  (modalka-define-kbd "k" "C-k")
  (modalka-define-kbd "K" "M-k")
  (modalka-define-kbd "w" "C-w")
  (modalka-define-kbd "y" "C-y")
  (modalka-define-kbd "W" "M-w")
  (modalka-define-kbd "Y" "M-y")
  (modalka-define-kbd "d" "C-d")
  (modalka-define-kbd "D" "M-d")
  (modalka-define-kbd "SPC" "C-SPC")
  (modalka-define-kbd "h" "M-h")
  (modalka-define-kbd "t" "C-t")
  (modalka-define-kbd "T" "M-t")
  (modalka-define-kbd "q" "M-q")
  (modalka-define-kbd ";" "M-;")
  (modalka-define-kbd "/" "C-/")
  (modalka-define-kbd "=" "C-=")
  (modalka-define-kbd "o" "C-o")
  (modalka-define-kbd "s" "C-s")
  (modalka-define-kbd "r" "C-r")
  (modalka-define-kbd "x b" "C-x C-b")
  (modalka-define-kbd "x f" "C-x C-f")
  (modalka-define-kbd "x r" "C-x C-r")
  (modalka-define-kbd "x k" "C-x k")
  (modalka-define-kbd "x s" "C-x C-s")
  (modalka-define-kbd "x t" "C-x C-t")
  (modalka-define-kbd "x 0" "C-x 0")
  (modalka-define-kbd "x 1" "C-x 1")
  (modalka-define-kbd "x 2" "C-x 2")
  (modalka-define-kbd "x o" "C-x o"))


(use-package dynamic-ruler
  :config
  (global-set-key (kbd "<f5>") #'dynamic-ruler)
  (global-set-key (kbd "<M-f5>") #'dynamic-ruler-vertical))


(use-package 0blayout
  :config
  (0blayout-add-keybindings-with-prefix "C-c C-0"))


(use-package shader-mode)


(use-package powerline
  :config (powerline-default-theme))


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;;; init.el ends here
