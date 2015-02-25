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
;;;;     - C-c a:    Anzu commands.
;;;;     - C-c r:    Rainbow commands.
;;;;     - C-c x:    General commands.
;;;;
;;;; All key chords consist of repeat a character twice, e.g 'NN', or
;;;; the letter 'q' followed by anything except 'u', e.g. 'qs'.
;;;;
;;;; C-z toggles Evil mode on and off in buffers. The keys 'C-c n e'
;;;; toggle it globally.
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

;;; By default these commands are disabled, but I use them.

(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; Default Font:

(add-to-list 'default-frame-alist '(font . "Cousine-14"))

;;; Browser:

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/local/bin/conkeror")

;;; This sets the location of my custom file, i.e. all of the settings
;;; I change via commands like `customize-group'.

(setq custom-file "/home/eric/.emacs.d/lisp/.emacs-custom.el")
(load custom-file 'noerror)

;;; Setup registers for files I commonly edit.

(set-register ?e '(file . "/home/eric/.emacs"))
(set-register ?m '(file . "/home/eric/Temp/mail.md"))
(set-register ?t '(file . "/home/eric/.todo.org"))
(set-register ?j '(file . "/tmp/Entry.jrnl.txt"))

;;; Load use-package and bind-key before anything else so that I can
;;; use those for loading all other packages.

(add-to-list 'load-path "/home/eric/.emacs.d/lisp/use-package/")
(require 'use-package)
(require 'bind-key)

;;; Setup my basic, global key-bindings.

(bind-key "<RET>" 'newline-and-indent)
(bind-key "<C-return>" 'newline)
(bind-key "<M-return>" 'indent-new-comment-line)
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
(use-package ctable :load-path "lisp/emacs-ctable/"
  :config (use-package epc :load-path "lisp/emacs-epc/"))
(use-package nav-flash :load-path "lisp/nav-flash/")
(use-package m-buffer :load-path "lisp/m-buffer-el/")
(use-package alert :load-path "lisp/alert/")
(use-package diminish)
(use-package popwin
  :load-path "lisp/popwin-el/"
  :init (popwin-mode 1))
(use-package hydra :load-path "lisp/hydra/")

;;; A utility to help manage minor modes.  It gives me a menu where I
;;; can enable or disable each minor mode.  And it shows me which
;;; minor modes are active.

(use-package manage-minor-mode
  :load-path "lisp/manage-minor-mode/"
  :bind ("C-c x n" . manage-minor-mode))

;;; Show key-bindings for the current major mode:

(use-package discover-my-major
  :load-path "lisp/discover-my-major/"
  :bind ("C-h C-m" . discover-my-major))

;;; Swoop lets me search buffers.  It is more useful than the standard
;;; C-s because I can search multiple buffers at once and search using
;;; Perl-compatible regular expressions, which I am more comfortable
;;; with than Elisp's.

(defhydra hydra-swoop (global-map "C-c")
  "swoop"
  ("s s" swoop)
  ("s m" swoop-multi "multi")
  ("s r" swoop-pcre "pcre")
  ("s b" swoop-back-to-last-position "back"))

;;; Commands to move the location of buffers on screen, i.e. moving
;;; around their windows.

(use-package buffer-move
  :bind (("<M-down>" . buf-move-down)
         ("<M-up>" . buf-move-up)
         ("<M-left>" . buf-move-left)
         ("<M-right>" . buf-move-right)))

;;; Lentic allows me to view the same file in more than one buffer
;;; with the benefit that each buffer can use a separate major mode.
;;; This is very useful for writing documents with code, where I may
;;; have one buffer in Markdown Mode and the other in the appropriate
;;; programming language mode.

(use-package lentic
  :load-path "lisp/lentic/")

;;; Highlight the current block of code in which the point resides.

(use-package highlight-blocks
  :load-path "lisp/highlight-blocks"
  :bind ("C-c n b" . highlight-blocks-mode))

;;; Company mode for auto-completion:

(use-package company
  :load-path "lisp/company-mode/"
  :diminish company-mode
  :bind (("M-/" . company-complete)
         ("C-c n m" . global-company-mode))
  :init (add-hook 'after-init-hook 'global-company-mode))

;;; Drag text around.

(use-package drag-stuff
  :load-path "lisp/drag-stuff.el/"
  :diminish drag-stuff-mode
  :config (progn
            (drag-stuff-global-mode 1)
            (setq drag-stuff-modifier '(meta shift))))

;;; These are commands that I mostly use for text editing, or more
;;; specifically not for programming.  So they use the 'C-c t' prefix
;;; for 'text'.

(bind-key "C-c m t" 'text-mode)

(defhydra hydra-text (global-map "C-c")
  "text"
  ("t a" align-regexp "align")
  ("t c" flyspell-auto-correct-word "flyspell")
  ("t f" toggle-text-mode-auto-fill "auto-fill")
  ("t s" sort-lines "sort"))

;;; Improve Japanese word movement.

(use-package tinysegmenter
  :load-path "lisp/tinysegmenter.el/"
  :config (use-package jaword :load-path "lisp/jaword/"))

;;; These keys are for commands I often use and use the 'C-c x' prefix
;;; as an association with 'M-x'.

(defun ejmr/open-project-log-file (project)
  "Opens a log file for a project."
  (interactive
   (list
    (completing-read "Project: " (directory-files "/home/eric/Documents/Logs/"))))
  (find-file (concat "/home/eric/Documents/Logs/" project)))

(defhydra hydra-commands (global-map "C-c")
  "commands"
  ("x i" imenu)
  ("x l" ejmr/open-project-log-file "log")
  ("x v" visit-tags-table "tags")
  ("x w" whitespace-cleanup "whitespace"))

;;; Desktop management:

(defhydra hydra-desktop (global-map "C-c")
  "desktop"
  ("d c" desktop-clear "clear")
  ("d d" desktop-change-dir "change")
  ("d s" desktop-save "save"))

;;; Easily visit recently opened files.

(use-package recentf-mode
  :init
  (progn
    (recentf-mode 1)
    (add-hook 'emacs-startup-hook 'recentf-open-files))
  :bind ("<f8>" . recentf-open-files))

;;; Find temporary files:

(use-package find-temp-file
  :load-path "lisp/find-temp-file/"
  :bind ("C-c x t" . find-temp-file)
  :config
  (progn
    (setq find-temp-file-directory "/tmp")
    (setq find-temp-template-default "%M/%N-%T.%E")))

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
  :bind (("C-=" . er/expand-region)
         ;; These next two bindings are because I use Dvorak and they
         ;; are easy to reach.
         ("C-0" . whole-line-or-region-kill-ring-save)
         ("C-9" . whole-line-or-region-kill-region))
  :config
  (progn
    (use-package change-inner
      :load-path "lisp/change-inner.el/"
      :bind (("M-i" . change-inner)
             ("M-o" . change-outer)))))

;;; Unfill:

(use-package unfill :load-path "lisp/unfill/")

;;; These packages also help navigate through text but are more
;;; focused on jumping to specific characters or fixed positions.

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

;;; Generates classic Lorem Ipsum filler content.

(use-package lorem-ipsum
  :load-path "lisp/emacs-lorem-ipsum/"
  :config (lorem-ipsum-use-default-bindings))

(use-package jump-char
  :load-path "lisp/jump-char/"
  :init
  (progn
    (key-chord-define-global "qj" 'jump-char-forward)
    (key-chord-define-global "q;" 'jump-char-backward)))

;;; These are packages I use for plain text in general.

(use-package simple
  :bind ("C-c n l" . visual-line-mode)
  :config (add-hook 'text-mode-hook 'typo-mode))

;;; This will center the contents of a window if I only have one
;;; window open.  It makes better use of screen space.

(use-package centered-window-mode
  :disabled t
  :load-path "lisp/centered-window-mode/"
  :config (centered-window-mode 1))

;;; Resize windows more easily.

(use-package windsize
  :load-path "lisp/windsize/"
  :config (windsize-default-keybindings))

;;; Faster navigation of the mark ring.

(use-package mark-tools
  :load-path "lisp/emacs-mark-tools/")

;;; A semantic kill command which lets me do such things as kill a
;;; sexp or filename at point.

(use-package easy-kill
  :load-path "lisp/easy-kill/"
  :config (key-chord-define-global "qw" 'easy-kill))

(use-package browse-kill-ring
  :load-path "lisp/browse-kill-ring/"
  :bind ("C-c x k" . browse-kill-ring)
  :config
  (use-package bbyac
    :load-path "lisp/bbyac/"
    :diminish bbyac-mode
    :config (bbyac-global-mode 1)))

;;; Switches between double and single quotes.

(use-package toggle-quotes
  :load-path "lisp/toggle-quotes.el/"
  :bind ("C-c t q" . toggle-quotes))

;;; Insert content from the kill ring or from the output of shell
;;; commands.

(use-package anyins
  :load-path "lisp/anyins/"
  :bind ("C-c m i" . anyins-mode))

;;; Increases the number at point.

(use-package operate-on-number
  :load-path "lisp/operate-on-number.el/"
  :init (key-chord-define-global "NN" 'operate-on-number-at-point))

;;; This package provides a shorthand for enumerating lists.  See the
;;; README at <https://github.com/abo-abo/tiny> for examples.

(use-package tiny
  :load-path "lisp/tiny/"
  :config
  (progn
    (tiny-setup-default)
    (key-chord-define-global "qt" 'tiny-expand)))

(use-package view-mode
  :bind ("C-c n v" . view-mode))

(use-package autopair
  :load-path "lisp/autopair/"
  :diminish autopair-mode
  :idle (autopair-global-mode))

(use-package anchored-transpose
  :bind ("C-c t t" . anchored-transpose))

;;; Inserts fancy characters like true ellipses, en and em-dashes,
;;; ligatures, and some mathematical symbols.

(use-package typo
  :load-path "lisp/typoel/"
  :bind ("C-c n t" . typo-mode)
  :config (typo-global-mode 1))

;;; Highlights poor English writing.

(use-package writegood-mode
  :load-path "lisp/writegood-mode/"
  :bind ("C-c n g" . writegood-mode))

(use-package flyspell
  :diminish flyspell-mode
  :bind (("C-c n s" . flyspell-mode)
         ("C-c n c" . flyspell-prog-mode))
  :config (flyspell-mode 1))

;;; Highlights the current sentence at point, which I find helps me
;;; read documents more easily.

(use-package hl-sentence
  :load-path "lisp/hl-sentence/"
  :config (hl-sentence-mode 1))

;;; Mode for distraction-free writing.

(use-package writeroom-mode
  :disabled t
  :load-path "lisp/writeroom-mode/")

;;; Allows entering a Unicode character by its name.

(use-package unipoint
  :load-path "lisp/unipoint/"
  ;; The package rebinds C-\ by default, which I use.  So I restore
  ;; the original binding.
  :bind (("C-c \\" . unipoint-insert)
         ("C-\\" . toggle-input-method)))

;;; Displays entire Unicode blocks.

(use-package charmap :load-path "lisp/charmap/")

;;; A minor mode which recognizes [[wiki-style]] double-bracketed
;;; navigation links in any type of file, providing the ability to
;;; jump between sections, between files, or open external links.

(use-package wiki-nav
  :load-path "lisp/button-lock/"
  :diminish (button-lock-mode wiki-nav-mode)
  :bind ("C-c n w" . wiki-nav-mode)
  :config (wiki-nav-mode 1))

;;; Shows horizontal lines where there are page breaks.

(use-package page-break-lines
  :load-path "lisp/page-break-lines/"
  :diminish page-break-lines-mode
  :config (global-page-break-lines-mode 1))

(use-package tex-mode
  :init
  (progn
    (add-hook 'tex-mode-hook (lambda () (typo-mode -1)))
    (add-hook 'tex-mode-hook (lambda () (flycheck-mode -1)))))

;;; Shorthand notation for HTML, XML, and CSS.

(use-package emmet-mode
  :load-path "lisp/emmet-mode/"
  :bind ("C-c m e" . emmet-mode)
  :init (progn
          (add-hook 'sgml-mode-hook 'emmet-mode)
          (add-hook 'css-mode-hook  'emmet-mode)))

;;; Share a region or buffer with different sites.

(use-package pastebin)

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
  :diminish flycheck-mode
  :bind ("C-c n f" . global-flycheck-mode)
  :init
  (use-package flycheck-package
    :load-path "lisp/flycheck-package/"
    :init (eval-after-load 'flycheck '(flycheck-package-setup)))
  (use-package flycheck-pos-tip
    :load-path "lisp/flycheck-pos-tip/")
  :config
  (progn
    (global-flycheck-mode 1)
    (use-package flycheck-tip
      :disabled t
      :load-path "lisp/flycheck-tip/"
      :bind ("C-c ! t" . flycheck-tip-cycle))))

;;; Make it easy to run `M-x compile` when saving source files:

(use-package recompile-on-save
  :load-path "lisp/recompile-on-save.el/")

;;; C and C++:

(use-package delim-kill
  :bind ("C-c n d" . delim-kill))

(defun ejmr/enable-c-mode-preferences ()
  (c-set-style "linux"))

(add-hook 'c-mode-hook 'ejmr/enable-c-mode-preferences)

;;; Git:

(use-package conf-mode
  :mode ((".gitignore" . conf-mode)
         (".gitconfig" . conf-mode)))

;;; I use the verbose flag with git-commit, which includes a diff of
;;; what I'm about to commit, and so I use this mode so I can easily
;;; read that diff.

(use-package diff-mode
  :mode ("COMMIT_EDITMSG" . diff-mode))

;;; Auto Indent:

(use-package auto-indent-mode
  :load-path "lisp/auto-indent-mode.el/"
  :diminish auto-indent-mode
  :config (auto-indent-global-mode))

;;; Tup build tool:

(use-package tup-mode)

;;; Lua:

(use-package lua-mode
  :init (bind-key "C-c m l" 'lua-mode)
  :config
  (progn
    (setq lua-indent-level 4))
  :mode (("\\.lua$" . lua-mode)
         ("\\.rockspec$" . lua-mode)
         ("\\.busted$" . lua-mode)
         ("\\.lnvl$" . lua-mode)
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

;;; Emacs Lisp

(defun ejmr/byte-compile-current-elisp-file ()
  (interactive)
  (byte-compile-file (buffer-file-name) t))

(bind-key "C-c l" 'ejmr/byte-compile-current-elisp-file emacs-lisp-mode-map)

;;; Extract the header comment of an Elisp file into a Markdown
;;; document suitable for a README, particularly on GitHub.

(use-package md-readme :load-path "lisp/md-readme/")

(use-package bump-version :load-path "lisp/emacs-bump-version/")
  
;;; Steps through macro expansion.

(use-package macrostep
  :load-path "lisp/macrostep/"
  :config (bind-key "C-c C-e" 'macrostep-expand emacs-lisp-mode-map))

;;; Indicate on the mode-line if a variable has lexical binding or is
;;; a dynamic variable.

(use-package lexbind-mode
  :load-path "lisp/lexbind-mode/"
  :config (add-hook 'emacs-lisp-mode-hook 'lexbind-mode))

;;; Make paredit commands available in all programming modes.

(use-package paredit-everywhere
  :load-path "lisp/paredit-everywhere/"
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
  :mode (("\\.json" . js3-mode)
         ("\\.js" . js3-mode))
  :init (defalias 'js-mode 'js3-mode)
  :interpreter "node"
  :config
  (use-package json-reformat :load-path "lisp/json-reformat/"))

;;; CoffeeScript

(use-package coffee-mode :load-path "lisp/coffee-mode/")

;;; Java:

(defun ejmr/enable-java-mode-settings ()
  (c-set-style "java"))

(add-hook 'java-mode-hook 'ejmr/enable-java-mode-settings)

;;; PHP:

(use-package php-mode
  :load-path "/home/eric/Projects/php-mode"
  :config
  (progn
    (use-package php-refactor-mode
      :load-path "lisp/php-refactor-mode.el/"
      :disabled t
      :config (php-refactor-mode))
    (use-package phpunit
      :load-path "lisp/phpunit.el/"
      :disabled t)))

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

    ;; When writing email I do not want to use auto-fill-mode because
    ;; I do not want arbitrary line-breaks in the message.  This
    ;; function toggles between the settings I prefer for writing
    ;; email and those I prefer for everything else.
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
    (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)))

;;; Mail:

(use-package sendmail
  :init (bind-key "C-c m a" 'mail-mode))

;;; Org Mode:

(use-package org-install
  :load-path "lisp/org-8.2.10/lisp/"
  :config
  (progn
    (use-package org-trello :load-path "lisp/org-trello/" :disabled t)
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
    ;; My key-bindings for moving around buffers shadow those for
    ;; promoting and demoting headers and trees.
    (bind-key "C-c <left>" 'org-do-promote org-mode-map)
    (bind-key "C-c <S-left>" 'org-promote-subtree org-mode-map)
    (bind-key "C-c <right>" 'org-do-demote org-mode-map)
    (bind-key "C-c <S-right>" 'org-demote-subtree org-mode-map)
    (bind-key "C-c a" 'org-agenda org-mode-map)))

;;; Edit filenames at-point in dired:

(use-package dired-efap
  :load-path "lisp/dired-efap/"
  :config (bind-key "<f2>" 'dired-efap dired-mode-map))

;;; God Mode:

(use-package god-mode
  :load-path "lisp/god-mode/"
  :disabled t
  :bind ("C-z" . god-local-mode))

;;; GLSL Mode:

(use-package glsl-mode
  :load-path "lisp/glsl-mode/"
  :mode ("\\.glsl" . glsl-mode))

;;; Perl and Emacs Regular Expression Utilities:

(use-package pcre2el
  :load-path "lisp/pcre2el/"
  :bind ("C-c n r" . rxt-global-mode))

;;; YAML Mode:

(use-package yaml-mode
  :load-path "lisp/yaml-mode/"
  :mode ("\\.yml" . yaml-mode))

;;; A mode for toggling between source files and tests.  I have not
;;; yet configured this because you must make the library aware of
;;; each project and where it can find the unit tests.

(use-package toggle-test
  :load-path "lisp/toggle-test/")

;;; Packages to manipulate regions.

(use-package wrap-region
  :load-path "lisp/wrap-region.el/"
  :diminish wrap-region-mode
  :config (wrap-region-mode t))

(use-package whole-line-or-region
  :load-path "lisp/whole-line-or-region/"
  :diminish whole-line-or-region-mode
  :config (whole-line-or-region-mode 1))

(use-package duplicate-thing
  :config (key-chord-define-global "qd" 'duplicate-thing))

;;; SQL

(use-package sqlup-mode
  :load-path "lisp/sqlup-mode.el/")

(use-package edbi
  :load-path "lisp/emacs-edbi/")

;;; DokuWiki:

(use-package dokuwiki-mode
  :load-path "lisp/dokuwiki-mode.el/")

;;; Quickrun:

(use-package quickrun
  :load-path "lisp/emacs-quickrun/"
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
    (quickrun-add-command "org/doku"
                          '((:command . "pandoc")
                            (:exec . "%c --from=org --to=dokuwiki %o %s %a")
                            (:default-directory . "/tmp"))
                          :mode 'org-mode)
    (quickrun-add-command "markdown/doku"
                          '((:command . "pandoc")
                            (:exec . "%c --from=markdown --to=dokuwiki %o %s %a")
                            (:default-directory . "/tmp"))
                          :mode 'markdown-mode)
    (quickrun-add-command "xml/fmt"
                          '((:command . "xml-fmt")
                            (:exec . "%c %s")
                            (:default-directory . "/tmp"))
                          :mode 'nxml-mode)

    (quickrun-set-default "markdown" "markdown/doku")
    (add-to-list 'quickrun-file-alist '("\\.xml$" . "xml/fmt"))
    (add-to-list 'quickrun-file-alist '("\\.org$" . "org/doku"))
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
  :load-path "lisp/powerline/"
  :config
  (progn
    (powerline-nano-theme)))

;;; REST Client:

(use-package restclient
  :load-path "lisp/restclient.el/")

;;; Increases the amount of screen space for the current window.

(use-package toggle-window
  :load-path "lisp/toggle-window/"
  :bind ("<f9>" . toggle-window-hide-show-window)
  :config (setq window-min-height 5))

;;; Interface with Travis CI:

(use-package travis
  :load-path "lisp/emacs-travis/")

;;; Word Count Mode.  It keeps track of the number of characters,
;;; words, and lines entered, and allows me to set goals so that I can
;;; motivate myself to write 'X' amount of material at a time.

(use-package wc-goal-mode
  :load-path "lisp/wc-goal-mode/"
  :bind ("C-c n M-w" . wc-goal-mode))

;;; Folding:

(use-package yafolding
  :load-path "lisp/yafolding.el/"
  :bind ("C-c x y" . yafolding-toggle-all))

;;; Better navigation for selecting open files and buffers.  The
;;; commands begin with 'C-c l' as a mnemonic for 'lusty'.

(use-package lusty-explorer
  :load-path "lisp/lusty-emacs/"
  :bind (("C-c l f" . lusty-file-explorer)
         ("C-c l b" . lusty-buffer-explorer)
         ("C-c l o" . lusty-open-this)))

;;; Evil

(use-package evil
  :load-path "lisp/evil/"
  :bind ("C-c n e" . evil-mode)
  :config
  (progn
    ;; I use 'q' for many key-chords so I do not want Evil using it to
    ;; record macros or anything else.
    (define-key evil-normal-state-map (kbd "q") nil)
    (use-package evil-numbers
      :load-path "lisp/evil-numbers/"
      :config (progn
                (global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
                (global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)))
    (use-package evil-surround
      :load-path "lisp/evil-surround/"
      :config (global-evil-surround-mode 1))
    (use-package evil-matchit
      :load-path "lisp/evil-matchit/"
      :config (global-evil-matchit-mode 1))
    (use-package evil-args
      :load-path "lisp/evil-args/"
      :config (progn
                (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
                (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
                (define-key evil-normal-state-map "L" 'evil-forward-arg)
                (define-key evil-normal-state-map "H" 'evil-backward-arg)
                (define-key evil-motion-state-map "L" 'evil-forward-arg)
                (define-key evil-motion-state-map "H" 'evil-backward-arg)
                (define-key evil-normal-state-map "K" 'evil-jump-out-args)))
    (use-package evil-snipe
      :load-path "lisp/evil-snipe"
      :config (global-evil-snipe-mode 1))
    (use-package evil-commentary
      :load-path "lisp/evil-commentary/"
      :diminish evil-commentary-mode
      :config (evil-commentary-mode))
    (use-package evil-exchange
      :load-path "lisp/evil-exchange/"
      :config (evil-exchange-install))
    (use-package anzu
      :load-path "lisp/emacs-anzu/"
      :diminish anzu-mode
      :bind ("C-c a c" . anzu-replace-at-cursor-thing)
      :config
      (progn
        (global-anzu-mode 1)
        (use-package evil-anzu
          :load-path "lisp/emacs-evil-anzu/")))
    (use-package evil-easymotion
      :load-path "lisp/evil-easymotion"
      :config (evilem-default-keybindings "SPC"))))

;;; Rainbow Modes

(use-package rainbow-identifiers
  :load-path "lisp/rainbow-identifiers/"
  :bind ("C-c r i" . rainbow-identifiers-mode)
  :config (setq rainbow-identifiers-choose-face-function
                #'rainbow-identifiers-cie-l*a*b*-choose-face))

(use-package rainbow-delimiters
  :load-path "lisp/rainbow-delimiters/"
  :bind ("C-c r d" . rainbow-delimiters-mode))

(use-package rainbow-blocks
  :load-path "lisp/rainbow-blocks/"
  :bind ("C-c r b" . rainbow-blocks-mode))

;;; Discover:

(use-package discover :load-path "lisp/discover.el/"
  :config (global-discover-mode 1))

;;; Highlight FIXME, TODO, and similar words:

(use-package fic-mode
  :config
  (progn
    (add-hook 'prog-mode-hook 'turn-on-fic-mode)))

;;; Generate passwords:

(use-package password-generator
  :load-path "lisp/emacs-password-genarator/")
