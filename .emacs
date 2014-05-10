;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; GNU Emacs Configuration by Eric James Michael Ritz
;;;;
;;;;     http://ericjmritz.name/
;;;;     https://github.com/ejmr/DotEmacs
;;;;
;;;; Conventions for key-bindings:
;;;;
;;;;     - C-c t:    Text commands.
;;;;     - C-c s:    Swoop commands.
;;;;     - C-c d:    Desktop commands.
;;;;     - C-c e:    Engine searching commands.
;;;;     - C-c m:    Major modes.
;;;;     - C-c n:    Minor modes.
;;;;     - C-c x:    General commands.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; First, setup my load path before anything else so that Emacs can
;;; find everything.

(add-to-list 'load-path "/home/eric/.emacs.d/lisp/")

;;; I often alias 'emacs' to 'emacsclient' in my shell, so the server
;;; needs to be running.

(server-start)

;;; Now comes a long section of general, global settings, minor modes,
;;; display configuration, and so on.

(setq inhibit-startup-message t
      make-backup-files nil
      auto-save-default t
      auto-save-interval 50
      auto-save-timeout 5
      delete-auto-save-files t
      case-fold-search t
      tooltip-delay 1
      major-mode 'text-mode
      imenu-sort-function 'imenu--sort-by-name
      kill-read-only-ok t
      show-trailing-whitespace t
      size-indication-mode t
      read-quoted-char-radix 16
      line-move-visual nil
      initial-scratch-message nil
      delete-by-moving-to-trash t
      visible-bell nil
      save-interprogram-paste-before-kill t
      history-length 250
      tab-always-indent 'complete
      save-abbrevs nil
      select-active-region t
      shift-select-mode nil
      x-select-enable-clipboard t
      auto-hscroll-mode t
      delete-active-region 'kill)

(setq scroll-preserve-screen-position 'always
      scroll-conservatively           most-positive-fixnum
      scroll-step                     0)

(setq-default truncate-lines t)
(setq-default abbrev-mode 1)
(setq-default indent-tabs-mode nil)

(transient-mark-mode t)
(delete-selection-mode t)
(column-number-mode t)
(show-paren-mode t)
(global-hi-lock-mode 1)
(which-function-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-auto-revert-mode 1)
(blink-cursor-mode 0)
(tooltip-mode 1)
(electric-pair-mode 0)

(setq c-default-style "linux")

(put 'narrow-to-page 'disabled nil)

(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-14"))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/local/bin/conkeror")

(setq custom-file "/home/eric/.emacs.d/lisp/.emacs-custom.el")
(load custom-file 'noerror)

;;; Setup registers for files I commonly edit.

(set-register ?b '(file . "/home/eric/Documents/Personal/Brainstorming.org"))
(set-register ?e '(file . "/home/eric/Projects/DotEmacs/.emacs"))
(set-register ?m '(file . "/home/eric/Temp/mail.md"))
(set-register ?t '(file . "/home/eric/.todo.org"))

;;; Load use-package and bind-key before anything else so that I can
;;; use those for loading all other packages.

(add-to-list 'load-path "/home/eric/.emacs.d/lisp/use-package/")
(require 'use-package)
(require 'bind-key)

;;; Setup my basic, global key-bindings.

(bind-key "<RET>" 'newline-and-indent)
(bind-key "<C-return>" 'newline)
(bind-key "<M-return>" 'indent-new-comment-line)
(bind-key "M-/" 'hippie-expand)

;;; These packages provide functions that others rely on so I want to
;;; load them early.

(use-package epl :load-path "lisp/epl/")
(use-package dash :load-path "lisp/dash.el/")
(use-package pkg-info :load-path "lisp/pkg-info/")
(use-package s)
(use-package f)
(use-package ido :config (ido-mode 1))
(use-package popup :load-path "lisp/popup-el/")
(use-package async :load-path "lisp/emacs-async/")
(use-package ht :load-path "lisp/ht.el/")
(use-package noflet :load-path "lisp/emacs-noflet")

;;; A utility to help manage minor modes:

(use-package manage-minor-mode
  :load-path "lisp/manage-minor-mode/"
  :commands manage-minor-mode
  :bind ("C-c x n" . manage-minor-mode))

;;; Use Undo Tree instead of the Emacs default:

(use-package undo-tree :init (global-undo-tree-mode))

;;; Replace 'C-x C-b' with Ibuffer:

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config (setq ibuffer-default-sorting-mode 'major-mode))

;;; More buffer-related configuration.

(use-package swoop
  :load-path "lisp/emacs-swoop/"
  :bind (("C-c s s" . swoop)
         ("C-c s m" . swoop-multi)
         ("C-c s r" . swoop-pcre-regexp)
         ("C-c s b" . swoop-back-to-last-position)))

;;; These are some personal editing commands that I use everywhere.

(defun ejmr/move-line-up ()
  "Move the current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun ejmr/move-line-down ()
  "Move the current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(bind-key "<C-up>" 'ejmr/move-line-up)
(bind-key "<C-down>" 'ejmr/move-line-down)

;;; These are commands that I mostly use for text editing, or more
;;; specifically not for programming.  So they use the 'C-c t' prefix
;;; for 'text'.

(bind-key "C-c m t" 'text-mode)
(bind-key "C-c t a" 'align-regexp)
(bind-key "C-c t c" 'flyspell-auto-correct-word)
(bind-key "C-c t f" 'toggle-text-mode-auto-fill)
(bind-key "C-c t s" 'sort-lines)

;;; These keys are for commands I often use and use the 'C-c x' prefix
;;; as an association with 'M-x'.

(defun ejmr/open-project-log-file (project)
  "Opens a log file for a project."
  (interactive
   (list
    (completing-read "Project: " (directory-files "/home/eric/Documents/Logs/"))))
  (find-file (concat "/home/eric/Documents/Logs/" project)))

(bind-key "C-c x i" 'imenu)
(bind-key "C-c x l" 'ejmr/open-project-log-file)
(bind-key "C-c x v" 'visit-tags-table)
(bind-key "C-c x w" 'whitespace-cleanup)

;;; Desktop managament:

(bind-key "C-c d c" 'desktop-clear)
(bind-key "C-c d d" 'desktop-change-dir)
(bind-key "C-c d s" 'desktop-save)

;;; Easily visit recently opened files.

(use-package recentf-mode
  :init (recentf-mode 1)
  :bind ("<f8>" . recentf-open-files))

;;; Configure the key chords I use globally.

(use-package key-chord
  :bind ("C-c n k" . key-chord-mode)
  :init (key-chord-mode 1)
  :config
  (progn
    (key-chord-define-global "##" 'server-edit)
    (key-chord-define-global "VV" 'other-window)
    (key-chord-define-global "KK" 'ido-kill-buffer)
    (key-chord-define-global "$$" 'ispell-buffer)
    (key-chord-define-global "BB" 'ido-switch-buffer)))

;;; I use these packages to navigate and edit text in semantic terms,
;;; with the Expand Region package being the foundation for the rest.

(use-package expand-region
  :load-path "lisp/expand-region.el/"
  :bind ("C-=" . er/expand-region)
  :config
  (progn
    (use-package change-inner
      :load-path "lisp/change-inner.el/"
      :bind (("M-i" . change-inner)
             ("M-o" . change-outer)))))

;;; These packages also help navigate through text but are more
;;; focused on jumping to specific characters or fixed positions.

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(use-package ace-link
  :load-path "lisp/ace-link/"
  :commands ace-link-setup-default
  :config (ace-link-setup-default))

;;; Use a mode-line that looks better and is more compact.

(use-package smart-mode-line
  :load-path "lisp/smart-mode-line/"
  :disabled t
  :config
  (progn
    (setq sml/theme 'respectful
          sml/shorten-directory t
          sml/shorten-modes t
          sml/name-width 40
          sml/mode-width 'full)
    (add-to-list 'sml/replacer-regexp-list '("^~/Projects/" ":Pro:"))
    (add-to-list 'sml/replacer-regexp-list '("^~/Projects/LAVO/" ":LAVO:"))
    (add-to-list 'sml/replacer-regexp-list '("^~/Documents/To Read/" ":To-Read:"))
    (sml/setup)))

;;; Load snippets that I use in a variety of major modes.

(use-package yasnippet
  :load-path "lisp/yasnippet/"
  :idle
  (progn
    (setq yas-snippet-dirs
          (list"/home/eric/.emacs.d/lisp/yasnippet/snippets"
               "/home/eric/Projects/DotEmacs/snippets"))
    (yas-global-mode)))

;;; These are packages I use for plain text in general.

(use-package simple
  :commands visual-line-mode
  :bind ("C-c n l" . visual-line-mode)
  :config (add-hook 'text-mode-hook 'typo-mode))

(use-package view-mode
  :commands view-mode
  :bind ("C-c n v" . view-mode))

(use-package autopair
  :load-path "lisp/autopair/"
  :idle (autopair-global-mode))

(use-package anchored-transpose
  :bind ("C-c t t" . anchored-transpose))

(use-package typo
  :load-path "lisp/typoel/"
  :commands typo-mode
  :bind ("C-c n t" . typo-mode))

(use-package writegood-mode
  :load-path "lisp/writegood-mode/"
  :commands writegood-mode
  :bind ("C-c n g" . writegood-mode))

(use-package flyspell
  :bind (("C-c n s" . flyspell-mode)
         ("C-c n c" . flyspell-prog-mode))
  :config (flyspell-mode 1))

(use-package writeroom-mode
  :load-path "lisp/writeroom-mode/")

(use-package screenwriter
  :load-path "lisp/screenwriter"
  :commands screenwriter-mode)

(use-package unipoint
  :load-path "lisp/unipoint/"
  ;; The package rebinds C-\ by default, which I use.  So I restore
  ;; the original binding.
  :bind (("C-c \\" . unipoint-insert)
         ("C-\\" . toggle-input-method)))

(use-package charmap :load-path "lisp/charmap/")

(use-package wiki-nav
  :load-path "lisp/button-lock/"
  :bind ("C-c n w" . wiki-nav-mode))

(use-package on-screen
  :load-path "lisp/on-screen.el/"
  :commands on-screen-global-mode
  :bind ("C-c n o" . on-screen-global-mode)
  :config (on-screen-global-mode 1))

(use-package fancy-narrow
  :load-path "lisp/fancy-narrow/"
  :disabled t
  :commands fancy-narrow-mode
  :config (fancy-narrow-mode 1))

(use-package ido-at-point
  :load-path "lisp/ido-at-point/"
  :disabled t
  :commands ido-at-point-mode
  :config (ido-at-point-mode 1)
  :bind ("M-/" . completion-at-point))

(use-package tex-mode
  :init
  (progn
    (add-hook 'tex-mode-hook (lambda () (typo-mode -1)))
    (add-hook 'tex-mode-hook (lambda () (flycheck-mode -1)))))

;;; Share a region or buffer on <http://dpaste.com/>.
(use-package dpaste
  :load-path "lisp/dpaste.el/"
  :bind ("C-c x d" . dpaste-region-or-buffer)
  :init
  (progn
    (setq dpaste-poster "Eric James Michael Ritz")))

;;; My Pomodoro timer of choice.

(use-package tomatinho
  :load-path "lisp/tomatinho/"
  :bind ("<f12>" . tomatinho))

;;; I use Flycheck in many programming modes by default.

(use-package flycheck
  :commands global-flycheck-mode
  :load-path "lisp/flycheck/"
  :bind ("C-c n f" . flycheck-mode)
  :config
  (progn
    (global-flycheck-mode 1)
    (use-package flycheck-tip
      :commands flycheck-tip-cycle
      :load-path "lisp/flycheck-tip/"
      :bind ("C-c ! t" . flycheck-tip-cycle))))

;;; Make it easy to run `M-x compile` when saving source files:

(use-package recompile-on-save
  :load-path "lisp/recompile-on-save.el/"
  :commands recompile-on-save)

;;; Git:

(use-package conf-mode
  :mode ((".gitignore" . conf-mode)
         (".gitconfig" . conf-mode)))

(use-package diff-mode
  :mode ("COMMIT_EDITMSG" . diff-mode))

(use-package git-messenger
  :load-path "lisp/emacs-git-messenger/"
  :bind ("C-c x g" . git-messenger:popup-message))

;;; Tup:

(use-package tup-mode)

;;; Lua:

(use-package lua-mode
  :commands lua-mode
  :bind ("C-c m l" . lua-mode)
  :config
  (progn
    (setq lua-indent-level 4)
    (use-package lua-block
      :commands lua-block-mode
      :config (lua-block-mode t)))
  :mode (("\\.lua" . lua-mode)
         ("\\.rockspec" . lua-mode)
         ("\\.busted" . lua-mode)
         ("\\.spec.lua" . fundamental-mode)
         ("\\.slua" . lua-mode)))

;;; Rust:

(use-package rust-mode
  :load-path "/home/eric/Software/Rust/src/etc/emacs/"
  :mode ("\\.rs" . rust-mode))

;;; Python:

(use-package python-mode
  :mode ("\\.py" . python-mode))

;;; Emacs Lisp:

(defun ejmr/byte-compile-current-elisp-file ()
  (interactive)
  (byte-compile-file (buffer-file-name) t))

(bind-key "C-c l" 'ejmr/byte-compile-current-elisp-file emacs-lisp-mode-map)

(use-package litable
  :load-path "lisp/litable/"
  :config (bind-key "C-c t" 'litable-mode emacs-lisp-mode-map))

;;; Racket:

(use-package racket-mode
  :load-path "lisp/racket-mode/"
  :commands racket-mode
  :mode ("\\.rkt" . racket-mode))

;;; Perl:

(use-package perl-mode
  :init (defalias 'perl-mode 'cperl-mode)
  :config
  (progn
    (use-package perl-find-library)
    (setq cperl-invalid-face nil
          cperl-close-paren-offset -4
          cperl-continued-statement-offset 0
          cperl-indent-level 4
          cperl-indent-parens-as-block t)))

;;; JavaScript:

(use-package js3-mode
  :load-path "lisp/js3-mode/"
  :mode (("\\.json" . js3-mode)
         ("\\.js" . js3-mode))
  :init (defalias 'js-mode 'js3-mode)
  :interpreter "node"
  :config
  (use-package json-reformat :load-path "lisp/json-reformat/"))

;;; PHP:

(use-package php-mode
  :load-path "/home/eric/Projects/php-mode"
  :config
  (progn
    (use-package phpunit
      :load-path "lisp/phpunit.el/"
      :commands (phpunit-current-project
                 phpunit-current-class))))

;;; These packages affect my modeline.

(use-package anzu
  :load-path "lisp/emacs-anzu/"
  :config (global-anzu-mode 1))

;;; Twitter:

(use-package twittering-mode
  :load-path "lisp/twittering-mode/"
  :bind ("C-c x t" . twit)
  :config
  (progn
    (setq twittering-use-master-password t)
    (setq twittering-number-of-tweets-on-retrieval 50)))

;;; BBCode:

(use-package bbcode-mode)

;;; Markdown:

(defun ejmr/insert-mail-signature ()
  (interactive)
  (when (string= (buffer-file-name) "/home/eric/Temp/mail.md")
    (mail-signature)))

(use-package markdown-mode
  :load-path "lisp/markdown-mode/"
  :bind ("C-c m k" . markdown-mode)
  :mode (("\\.md" . markdown-mode)
         ("\\.markdown" . markdown-mode))
  :config
  (progn
    (add-hook 'markdown-mode-hook 'typo-mode)

    (use-package pandoc-mode
      :load-path "lisp/pandoc-mode/"
      :config (add-hook 'markdown-mode-hook 'turn-on-pandoc))

    (defun ejmr/toggle-markdown-mode-wrapping ()
      (interactive)
      (let ((normal-settings (and auto-fill-function (not word-wrap))))
        (cond (normal-settings
               (auto-fill-mode -1)
               (visual-line-mode 1)
               (message "Using email settings"))
              (t
               (auto-fill-mode 1)
               (visual-line-mode -1)
               (message "Using normal settings")))))

    (bind-key "C-c w" 'ejmr/toggle-markdown-mode-wrapping markdown-mode-map)
    (bind-key "C-c s" 'ejmr/insert-mail-signature markdown-mode-map)))

;;; Mail:

(use-package sendmail
  :bind ("C-c m a" . mail-mode))

;;; Org Mode:

(use-package org-install
  :load-path "lisp/org-mode/lisp/"
  :idle
  (progn
    (add-hook 'org-mode-hook 'typo-mode)
    (setq org-todo-keywords '((sequence "TODO(t)"
                                        "REVIEW(r)"
                                        "TESTING(e)"
                                        "FEEDBACK(f)"
                                        "|"
                                        "DONE(d)"
                                        "ABORTED(a)"))
          org-drawers '("PROPERTIES" "CLOCK" "NOTES" "LOGBOOK")
          org-log-done 'time)))

;;; Forth:

(use-package forth-mode
  :commands forth-mode
  :bind ("C-c m f" . forth-mode)
  :mode (("\\.forth" . forth-mode)
         ("\\.fth" . forth-mode)
         ("\\.fs" . forth-mode))
  :interpreter ("gforth" . forth-mode))

;;; Zencoding and Emmet

(use-package emmet-mode
  :load-path "lisp/emmet-mode/"
  :commands emmet-mode
  :bind ("C-c n e" . emmet-mode)
  :config
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'html-mode-hook 'emmet-mode)
    (add-hook 'nxml-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook 'emmet-mode)))

;;; fic-mode:

(use-package fic-mode
  :load-path "lisp/fic-mode/"
  :config
  (progn (add-hook 'prog-mode-hook 'fic-mode)))

;;; howdoi:

(use-package howdoi
  :load-path "lisp/howdoi-emacs/"
  :bind ("C-c x h" . howdoi))

;;; Edit filenames at-point in dired:

(use-package dired-efap
  :load-path "lisp/dired-efap/"
  :config (bind-key "<f2>" 'dired-efap dired-mode-map))

;;; God Mode:

(use-package god-mode
  :load-path "lisp/god-mode/"
  :bind ("<f9>" . god-mode-all))

;;; Wand:

(use-package wand
  :load-path "lisp/wand/"
  :bind ("C-c RET" . wand:execute)
  :config
  (progn
    (setq wand:*rules*
          (list
           (wand:create-rule :match "https?://"
                             :capture :whole
                             :action browse-url-generic)
           (wand:create-rule :match "file://"
                             :capture :after
                             :action find-file-other-window)))))

;;; GLSL Mode:

(use-package glsl-mode
  :load-path "lisp/glsl-mode/"
  :mode ("\\.glsl" . glsl-mode))

;;; GNU APL

(use-package gnu-apl-mode :load-path "lisp/gnu-apl-mode/")

;;; Golang:

(use-package go-mode
  :load-path "lisp/go-mode/"
  :mode ("\\.go" . go-mode))

;;; Interface for GNU Global:

(use-package ggtags-mode
  :load-path "lisp/ggtags/"
  :commands ggtags-mode)

;;; Perl and Emacs Regular Expression Utilities:

(use-package pcre2el
  :load-path "lisp/pcre2el/"
  :commands rxt-global-mode
  :bind ("C-c n r" . rxt-global-mode))

;;; Nimrod:

(use-package nimrod-mode
  :load-path "lisp/nimrod-mode/"
  :mode ("\\.nim" . nimrod-mode)
  :commands nimrod-mode)

;;; Engine Mode:

(use-package engine-mode
  :load-path "lisp/engine-mode/"
  :commands (engine-mode defengine)
  :init (engine-mode t)
  :config
  (progn
    (defengine duckduckgo
      "https://duckduckgo.com/?q=%s"
      "C-c e d")
    (defengine github
      "https://github.com/search?ref=simplesearch&q=%s"
      "C-c e g")
    (defengine wikipedia
      "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
      "C-c e w")))

;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
