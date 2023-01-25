;;; jet.el --- Emacs integration for jet Clojure tool -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Eric Dallo
;;
;; Author: Eric Dallo <ercdll1337@gmail.com>
;; Maintainer: Eric Dallo <ercdll1337@gmail.com>
;; Created: january 24, 2023
;; Version: 0.1.0
;; Keywords: tools
;; Homepage: https://github.com/ericdallo/jet.el
;; Package-Requires: ((emacs "27.1") (transient "0.3.7"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Emacs integration for jet Clojure tool
;;
;;; Code:

(require 'transient)

(defcustom jet-command "jet"
  "The jet command name to run."
  :group 'jet
  :type 'string)

(defvar jet-version-string "0.1.0")

(defun jet--assert-jet-on-path ()
  "Asserts if jet is on path"
  (unless (= 0 (shell-command (concat jet-command " --version")))
    (error "jet not found on emacs PATH.")))

(defun jet--run (command &rest args)
  "Run jet command with ARGS"
  (jet--assert-jet-on-path)
  (shell-command (string-join (append (list command) args) " ")))

(defun jet--thing-at-point ()
  "Return the active region or the thing at point."
  (string-trim
   (if (use-region-p)
       (buffer-substring-no-properties (region-beginning) (region-end))
     (thing-at-point 'line t))))

(defun jet-menu--suffixes->args (suffixes)
  (string-join
   (thread-last
     suffixes
     (seq-filter (lambda (obj)
                   (cl-typep obj 'transient-option)))
     (seq-map (lambda (obj)
                (when-let (arg (oref obj value))
                  (concat (oref obj argument) (oref obj value))))))
   " "))

(defun jet-menu--command ()
  "Return the command description for the jet-menu transient."
  (let* ((command (oref transient--prefix scope))
         (args (jet-menu--suffixes->args transient--suffixes)))
    (concat (propertize "Jet interative command builder" 'face 'transient-heading)
            "\n\n"
            (propertize (concat command " " args) 'face 'transient-inactive-argument) "\n")))

(defun jet-menu--run (command &optional args)
  "Run the specified args"
  (interactive (list (or (and transient-current-prefix (oref transient-current-prefix scope))
                         jet-command)
                     (transient-args transient-current-command)))
  (apply #'jet--run command args))

;; (defclass jet-string-value (transient-option)
;;   ((multi-value :initarg :multi-value :initform t)))

(transient-define-infix jet-menu--from ()
  :argument "--from="
  :class 'transient-option
  :choices '("edn" "json" "transit" "yaml")
  :description "From what transform"
  :key "i")

(transient-define-infix jet-menu--to ()
  :argument "--to="
  :class 'transient-option
  :choices '("edn" "json" "transit" "yaml")
  :description "To what transform"
  :key "o")

(transient-define-infix jet-menu--keywordize ()
  :argument "--keywordize"
  :class 'transient-switch
  :description "if present, keywordizes JSON keys using `keyword` function"
  :key "k")

(transient-define-infix jet-menu--no-pretty ()
  :argument "--no-pretty"
  :class 'transient-switch
  :description "Whether to not pretty-print the result"
  :key "n")

(transient-define-infix jet-menu--colors ()
  :argument "--colors="
  :class 'transient-option
  :choices '("auto" "true" "false")
  :description "Use colored output while pretty-printing"
  :key "c")

(transient-define-infix jet-menu--edn-reader-opts ()
  :argument "--edn-reader-opts="
  :class 'transient-option
  :description "Use colored output while pretty-printing"
  :key "e")

(transient-define-infix jet-menu--func ()
  :argument "--func="
  :class 'transient-option
  :description "A single-arg Clojure function, or a path to a file that contains a function, that transforms input"
  :key "f")

(transient-define-infix jet-menu--thread-last ()
  :argument "--thread-last="
  :class 'transient-option
  :description "Implicit thread last"
  :key "t")

(transient-define-infix jet-menu--thread-first ()
  :argument "--thread-first="
  :class 'transient-option
  :description "Implicit thread first"
  :key "T")

(transient-define-infix jet-menu--query ()
  :argument "--query="
  :class 'transient-option
  :description "Given a jet-lang query, transforms input"
  :key "q")

(transient-define-infix jet-menu--collect ()
  :argument "--collect"
  :class 'transient-switch
  :description "Given separate values, collects them in a vector"
  :key "C")

;; Public API

;;;###autoload (autoload 'jet-menu "jet-menu" "Run jet for COMMAND." t)
(transient-define-prefix jet-menu (command)
  "Run jet for THING."
  [:description
   jet-menu--command
   "\nCommon Options"
   (jet-menu--from)
   (jet-menu--to)
   (jet-menu--keywordize)
   (jet-menu--no-pretty)
   (jet-menu--edn-reader-opts)
   "\nAdvanced Options"
   (jet-menu--func)
   (jet-menu--thread-last)
   (jet-menu--thread-first)
   (jet-menu--query)
   (jet-menu--collect)]
  ["Actions"
   ("x" "Execute" jet-menu--run)]
  (interactive (list jet-command))
  (transient-setup 'jet-menu nil nil :scope command))

;;;###autoload
(defun jet ()
  (interactive)
  (jet-menu jet-command))

;;;###autoload
(defun jet-print-version ()
  "Print the jet version"
  (interactive)
  (jet--run "--version"))

;;;###autoload
(defun jet-pretty ()
  (interactive)
  (shell-command-on-region
   (region-beginning)
   (region-end)
   "jet --pretty --edn-reader-opts '{:default tagged-literal}'"
   (current-buffer)
   t
   "*jet error buffer*"
   t))

(provide 'jet)
;;; jet.el ends here
