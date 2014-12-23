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
;;;;     - C-c m:    Major modes.
;;;;     - C-c n:    Minor modes.
;;;;     - C-c q:    Quickrun commands.
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
      auto-hscroll-mode nil
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

(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(add-to-list 'default-frame-alist '(font . "Cousine-14"))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/local/bin/conkeror")

(setq custom-file "/home/eric/.emacs.d/lisp/.emacs-custom.el")
(load custom-file 'noerror)

;;; Setup registers for files I commonly edit.

(set-register ?e '(file . "/home/eric/Projects/DotEmacs/.emacs"))
(set-register ?j '(file . "/home/eric/Temp/Entry.jrnl.txt"))
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
(global-unset-key (kbd "C-z"))

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
(use-package makey :load-path "lisp/makey/")
(use-package request :load-path "lisp/emacs-request/")
(use-package syntactic-sugar :load-path "lisp/syntactic-sugar/")
(use-package ample-regexps :load-path "lisp/ample-regexps.el/")
(use-package logito :load-path "lisp/logito/")
(use-package pcache :load-path "lisp/pcache/")
(use-package gh :load-path "lisp/gh.el/")
(use-package truthy :load-path "lisp/truthy/")
(use-package grizzl :load-path "lisp/grizzl/")
(use-package ws-butler :load-path "lisp/ws-butler/")
(use-package deferred :load-path "lisp/emacs-deferred/")
(use-package nav-flash :load-path "lisp/nav-flash/")
(use-package m-buffer :load-path "lisp/m-buffer-el/")
(use-package diminish)
(use-package popwin
  :load-path "lisp/popwin-el/"
  :init (popwin-mode 1))

;;; A utility to help manage minor modes:

(use-package manage-minor-mode
  :load-path "lisp/manage-minor-mode/"
  :commands manage-minor-mode
  :bind ("C-c x n" . manage-minor-mode))

;;; Show key-bindings for the current major mode:

(use-package discover-my-major
  :load-path "lisp/discover-my-major/"
  :commands discover-my-major
  :bind ("C-h C-m" . discover-my-major))

;;; More buffer-related configuration.

(use-package swoop
  :load-path "lisp/emacs-swoop/"
  :bind (("C-c s s" . swoop)
         ("C-c s m" . swoop-multi)
         ("C-c s r" . swoop-pcre-regexp)
         ("C-c s b" . swoop-back-to-last-position))
  :config (setq swoop-window-split-direction: 'split-window-vertically))

(use-package buffer-move
  :bind (("<M-down>" . buf-move-down)
         ("<M-up>" . buf-move-up)
         ("<M-left>" . buf-move-left)
         ("<M-right>" . buf-move-right)))

(use-package lentic
  :load-path "lisp/lentic/")

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

;;; Desktop management:

(bind-key "C-c d c" 'desktop-clear)
(bind-key "C-c d d" 'desktop-change-dir)
(bind-key "C-c d s" 'desktop-save)

;;; Easily visit recently opened files.

(use-package recentf-mode
  :init
  (progn
    (recentf-mode 1)
    (add-hook 'emacs-startup-hook 'recentf-open-files))
  :bind ("<f8>" . recentf-open-files))

;;; Configure the key chords I use globally.

(use-package key-chord
  :diminish key-chord-mode
  :bind ("C-c n k" . key-chord-mode)
  :init (key-chord-mode 1)
  :config
  (progn
    (setq key-chord-two-keys-delay 0.5)
    (key-chord-define-global "##" 'server-edit)
    (key-chord-define-global "VV" 'other-window)
    (key-chord-define-global "$$" 'ispell-buffer)
    ;; Pretty much everything in Enlish word beginning with 'q' is
    ;; follewed the vowel 'u'.  These chords take advantage of that.
    (key-chord-define-global "qq" 'read-only-mode)
    (key-chord-define-global "qs" 'save-buffer)
    (key-chord-define-global "q0" 'delete-window)
    (key-chord-define-global "qv" 'vc-next-action)
    (key-chord-define-global "qm" 'mark-whole-buffer)
    (key-chord-define-global "ql" 'ido-kill-buffer)))

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

(use-package jump-char
  :load-path "lisp/jump-char/"
  :commands jump-char-forward
  :init
  (progn
    (key-chord-define-global "qj" 'jump-char-forward)))

;;; These are packages I use for plain text in general.

(use-package simple
  :commands visual-line-mode
  :bind ("C-c n l" . visual-line-mode)
  :config (add-hook 'text-mode-hook 'typo-mode))

(use-package easy-kill
  :load-path "lisp/easy-kill/"
  :config (key-chord-define-global "qw" 'easy-kill))

(use-package browse-kill-ring
  :load-path "lisp/browse-kill-ring/"
  :commands browse-kill-ring
  :config (key-chord-define-global "qy" 'browse-kill-ring))

(use-package operate-on-number
  :load-path "lisp/operate-on-number.el/"
  :commands operate-on-number-at-point
  :init (key-chord-define-global "NN" 'operate-on-number-at-point))

(use-package view-mode
  :commands view-mode
  :bind ("C-c n v" . view-mode))

(use-package autopair
  :load-path "lisp/autopair/"
  :diminish autopair-mode
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
  :diminish flyspell-mode
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

(use-package ids-edit
  :load-path "lisp/ids-edit/")

(use-package wiki-nav
  :load-path "lisp/button-lock/"
  :bind ("C-c n w" . wiki-nav-mode))

(use-package tex-mode
  :init
  (progn
    (add-hook 'tex-mode-hook (lambda () (typo-mode -1)))
    (add-hook 'tex-mode-hook (lambda () (flycheck-mode -1)))))

;;; HTML, XML, CSS:

(use-package emmet-mode
  :load-path "lisp/emmet-mode/"
  :bind ("C-c m e" . emmet-mode))

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
  :load-path "lisp/flycheck/"
  :commands global-flycheck-mode
  :diminish flycheck-mode
  :bind ("C-c n f" . global-flycheck-mode)
  :init
  (use-package flycheck-package
    :load-path "lisp/flycheck-package/"
    :commands flycheck-package-setup
    :init (eval-after-load 'flycheck '(flycheck-package-setup)))
  (use-package flycheck-pos-tip
    :load-path "lisp/flycheck-pos-tip/")
  :config
  (progn
    (global-flycheck-mode 1)
    (use-package flycheck-tip
      :disabled t
      :commands flycheck-tip-cycle
      :load-path "lisp/flycheck-tip/"
      :bind ("C-c ! t" . flycheck-tip-cycle))))

;;; Make it easy to run `M-x compile` when saving source files:

(use-package recompile-on-save
  :load-path "lisp/recompile-on-save.el/"
  :commands recompile-on-save)

;;; C and C++:

(defun ejmr/enable-c-mode-preferences ()
  (c-set-style "linux"))

(add-hook 'c-mode-hook 'ejmr/enable-c-mode-preferences)

;;; Git:

(use-package conf-mode
  :mode ((".gitignore" . conf-mode)
         (".gitconfig" . conf-mode)))

(use-package diff-mode
  :mode ("COMMIT_EDITMSG" . diff-mode))

;;; Auto Indent:

(use-package auto-indent-mode
  :load-path "lisp/auto-indent-mode.el/"
  :diminish auto-indent-mode
  :config (auto-indent-global-mode))

;;; Tup:

(use-package tup-mode)

;;; Lua:

(use-package lua-mode
  :commands lua-mode
  :init (bind-key "C-c m l" 'lua-mode)
  :config
  (progn
    (setq lua-indent-level 4)
    (add-hook 'lua-mode-hook 'ws-butler-mode)
    (use-package lua-block
      :commands lua-block-mode
      :config (lua-block-mode t)))
  :mode (("\\.lua$" . lua-mode)
         ("\\.rockspec$" . lua-mode)
         ("\\.busted$" . lua-mode)
         ("\\.spec.lua$" . fundamental-mode)
         ("\\.slua$" . lua-mode)))

;;; Rust:

(use-package rust-mode
  :load-path "/home/eric/Software/Rust/src/etc/emacs/"
  :mode ("\\.rs" . rust-mode))

;;; Python:

(use-package python-mode
  :mode ("\\.py" . python-mode)
  :config
  (progn
    (use-package py-autopep8
      :load-path "lisp/py-autopep8/"
      :config (add-hook 'before-save-hook 'py-autopep8-before-save))))

;;; Emacs Lisp:

(defun ejmr/byte-compile-current-elisp-file ()
  (interactive)
  (byte-compile-file (buffer-file-name) t))

(bind-key "C-c l" 'ejmr/byte-compile-current-elisp-file emacs-lisp-mode-map)

(use-package bump-version :load-path "lisp/emacs-bump-version/")

(use-package macrostep
  :load-path "lisp/macrostep/"
  :config (bind-key "C-c C-e" 'macrostep-expand emacs-lisp-mode-map))

(use-package paredit-everywhere
  :load-path "lisp/paredit-everywhere/"
  :commands paredit-everywhere-mode
  :diminish paredit-everywhere-mode
  :bind ("C-c n p" . paredit-everywhere-mode)
  :init (add-hook 'prog-mode-hook 'paredit-everywhere-mode))

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
  :commands js3-mode
  :mode (("\\.json" . js3-mode)
         ("\\.js" . js3-mode))
  :init (defalias 'js-mode 'js3-mode)
  :interpreter "node"
  :config
  (use-package json-reformat :load-path "lisp/json-reformat/"))

(use-package coffee-mode :load-path "lisp/coffee-mode/")

;;; Java:

(defun ejmr/enable-java-mode-settings ()
  (c-set-style "java"))

(add-hook 'java-mode-hook 'ejmr/enable-java-mode-settings)

;;; PHP:

(use-package php-mode
  :load-path "/home/eric/Projects/php-mode"
  :disabled t
  :config
  (progn
    (use-package php-refactor-mode
      :load-path "lisp/php-refactor-mode.el/"
      :config (php-refactor-mode))
    (use-package phpunit
      :load-path "lisp/phpunit.el/"
      :commands (phpunit-current-project
                 phpunit-current-class))))

;;; BBCode:

(use-package bbcode-mode)

;;; Markdown:

(defun ejmr/insert-mail-signature ()
  (interactive)
  (when (string= (buffer-file-name) "/home/eric/Temp/mail.md")
    (mail-signature)))

(use-package markdown-mode
  :load-path "lisp/markdown-mode/"
  :init (bind-key "C-c m k" 'markdown-mode)
  :mode (("\\.md" . markdown-mode)
         ("\\.markdown" . markdown-mode))
  :config
  (progn
    (add-hook 'markdown-mode-hook 'typo-mode)

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

;;; Pandoc:

(use-package pandoc-mode
  :load-path "lisp/pandoc-mode/"
  :config
  (progn
    (add-hook 'markdown-mode-hook 'turn-on-pandoc)
    (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)))

;;; Mail:

(use-package sendmail
  :init (bind-key "C-c m a" 'mail-mode))

;;; Org Mode:

(use-package org-install
  :load-path "lisp/org-mode/lisp/"
  :idle
  (progn
    (use-package org-trello :load-path "lisp/org-trello/")
    (add-hook 'org-mode-hook 'typo-mode)
    (setq org-todo-keywords '((sequence "TODO(t)"
                                        "REVIEW(r)"
                                        "TESTING(e)"
                                        "FEEDBACK(f)"
                                        "|"
                                        "DONE(d)"
                                        "ABORTED(a)"
                                        "ON-HOLD(h)"
                                        "DELEGATED(g)"))
          org-drawers '("PROPERTIES" "CLOCK" "NOTES" "LOGBOOK")
          org-log-done 'time)
    (bind-key "C-c o a" 'org-agenda)
    (bind-key "C-c o c" 'org-capture)
    (bind-key "C-c o l" 'org-store-link)
    (bind-key "C-c o b" 'org-iswitchb)))

;;; Edit filenames at-point in dired:

(use-package dired-efap
  :load-path "lisp/dired-efap/"
  :config (bind-key "<f2>" 'dired-efap dired-mode-map))

;;; God Mode:

(use-package god-mode
  :load-path "lisp/god-mode/"
  :bind ("C-z" . god-local-mode))

;;; GLSL Mode:

(use-package glsl-mode
  :load-path "lisp/glsl-mode/"
  :mode ("\\.glsl" . glsl-mode))

;;; Perl and Emacs Regular Expression Utilities:

(use-package pcre2el
  :load-path "lisp/pcre2el/"
  :commands rxt-global-mode
  :bind ("C-c n r" . rxt-global-mode))

;;; YAML Mode:

(use-package yaml-mode
  :load-path "lisp/yaml-mode/"
  :commands yaml-mode
  :mode ("\\.yml" . yaml-mode))

;;; Toggle tests:

(use-package toggle-test
  :load-path "lisp/toggle-test")

;;; Manipulate Regions:

(use-package wrap-region
  :load-path "lisp/wrap-region/")

(use-package whole-line-or-region
  :load-path "lisp/whole-line-or-region/"
  :config (whole-line-or-region-mode 1))

(use-package duplicate-thing
  :config (key-chord-define-global "qd" 'duplicate-thing))

;;; SQL

(use-package sqlup-mode
  :load-path "lisp/sqlup-mode.el/")

;;; Dokuwiki:

(use-package dokuwiki-mode
  :load-path "lisp/dokuwiki-mode.el/")

;;; Quickrun:

(use-package quickrun
  :load-path "lisp/emacs-quickrun/"
  :commands ((quickrun
              quickrun-region
              quickrun-shell
              quickrun-compile-only
              quickrun-replace-region
              quickrun-add-command))
  :bind (("C-c q q" . quickrun)
         ("C-c q r" . quickrun-region)
         ("C-c q s" . quickrun-shell)
         ("C-c q c" . quickrun-compile-only)
         ("C-c q g" . quickrun-replace-region))
  :config
  (progn
    (quickrun-add-command "jrnl"
                          '((:command . "jrnl")
                            (:exec . ("%c < %s"))
                            (:default-directory . "/tmp")))
    (add-to-list 'quickrun-file-alist '("\\.jrnl.txt$" . "jrnl"))))

;;; Haskell:

(use-package haskell-mode-autoloads
  :load-path "lisp/haskell-mode/"
  :init (add-to-list 'Info-default-directory-list "~/.emacs.d/lisp/haskell-mode/")
  :config
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
    (bind-key "C-," 'haskell-move-nested-left haskell-mode-map)
    (bind-key "C-." 'haskell-move-nested-right haskell-mode-map)
    (bind-key "C-c C-c" 'haskell-compile haskell-mode-map)
    (add-to-list 'which-func-modes 'haskell-mode)))

;;; Powerline:

(use-package powerline
  :load-path "lisp/emacs-powerline/")

;;; REST Client:

(use-package restclient
  :load-path "lisp/restclient.el/")

;;; Shelltest Mode:

(use-package shelltest-mode
  :load-path "lisp/shelltest-mode/"
  :commands shelltest-mode
  :mode ("\\.shelltest" . shelltest-mode))
