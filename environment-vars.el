;;; environment-vars.el --- create setenv commands from Windows environment variables

;; Copyright (C) 2017 Jeff Spaulding

;; Maintainer: sarnet@gmail.com

;; This file is NOT part of GNU Emacs.

;; This is the ISC license.

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;; Commentary:

;; These functions are used to create a library file that can be used to
;; set environment variables for use with Microsoft Visual Studio.  They
;; do so by using the "vcvarsall.bat" file that is included with Visual
;; Studio.

;; To use, require this library and then run the function jds/create-library
;; from the M-x prompt.  The function will search for all occurences of
;; the vcvarsall.bat file in subdirectories of the directory named in
;; the "ProgramFiles(x86)" environment variable, compare those to the
;; existing environment, and then generate a buffer containing functions
;; to set those environment variables.  Once this buffer is generated,
;; check it over and then save it somewhere in your load-path.

;; To use the generated library, require it via
;; (require 'windows-dev-environment) and then call the function that sets
;; up the environment you with to use.  Function names are in the
;; following format:

;; setup-ms-dev-environment-<version>-<configuration>

;; where <version> is the version of visual studio given by the
;; "VisualStudioVersion" environment variable and the configuration is one
;; of the strings listed in the jds/vc-configs constant below.

;; Note that this only sets up environment variables.  Calling one of these
;; functions is the equivalent of opening a regular cmd prompt and calling
;; the vcvarsall.bat file with the appropriate configuration argument.
;; This library does not
;; do any other setup for making VC++ development any easier; that is an
;; exercise left to the reader.

;; This file (the one you're reading now) only needs to be required when
;; you want to regenerate your files (for instance, if you have installed
;; a new version of Visual Studio) or if a new version of this file is
;; released with fixes or new features.

;; Just a note: the file that's generated has nil in place of the empty
;; list for the defun argument list.  It looks weird, but since '() and
;; nil are identical in elisp, it works fine.

;;; Code:

;; Yes, I use the old-style slash instead of the dash for namespaces.  I think
;; it's prettier.  Comments are welcome at mailto:root@localhost.

(defconst jds/vc-configs
  (list "x86" "arm" "x86_amd64" "x86_arm" "amd64" "amd64_x86" "amd64_arm")
  "List of possible configurations for the vcvarsall.bat script.")

(defun jds/case-insensitive-string= (s1 s2)
  "Compares uppercase versions of two strings."
  (string= (upcase s1) (upcase s2)))

(defun jds/case-insensitive-member (string list)
  "Search for a string in a list of strings, ignoring case.
Returns t if it is found, nil otherwise."
  (delq nil
	(mapcar (lambda (list-item)
		  (jds/case-insensitive-string= string list-item))
		list)))

;; I had a beautiful recursive scheme-like version of this function.
;; Unfortunately, it would have required dash, cl-lib or seq and I didn't
;; feel like dealing with compatability issues (or reimplementing the wheel).
;; For a LISP, elisp doesn't have a very good default set of list-mangling
;; functions.
(defun jds/locate-file-without-puking (basedir filename)
  "Locates matching filenames recursively, skipping errors."
  (let* ((basedir (file-name-as-directory basedir))
	 (dir-listing (delete "." (delete ".." (condition-case nil
						   (directory-files basedir)
						 (error nil)))))
	 (found-files
	  (if (jds/case-insensitive-member filename dir-listing) 
	      (list (concat basedir filename))
	    nil)))
    (dolist (dir-entry dir-listing)
      (when (file-directory-p (concat basedir dir-entry))
	(let ((recursive-results (jds/locate-file-without-puking
				  (concat basedir dir-entry)
				  filename)))
	  (when recursive-results
	    (setq found-files (append found-files recursive-results nil))))))
    found-files))

(defun jds/determine-vs-version (filename)
  "Given the path to vcvarsall.bat, return the version of Visual Studio."
  (with-temp-buffer
    (insert-file-contents filename)
    (re-search-forward "^set VisualStudioVersion=")
    (buffer-substring (point) (line-end-position))))

(defun jds/envlist-to-command (envlist)
  "Given a list of key-value lists, create a LISP command to setenv them."
  (append '(defun setup-dev-environment ())
	  (mapcar (lambda (e) (cons 'setenv e))
		  envlist)))

(defun jds/buffer-to-environment-list (buffer)
  "Read a buffer containing the output of `set' and return a list of key-value lists.
The lists returned are true lists, not alists."
  (with-current-buffer buffer
    (mapcar (lambda (line)
	      (when (string-match "=" line)
		 (split-string line "=")))
	    (split-string (buffer-string) "\n" t))))

(defun jds/remove-env-duplicates (env1 env2)
  "Given two environment lists, remove duplicates from env2 and return it."
  (mapc (lambda (env-element)
	  (setq env2 (remove env-element env2)))
	env1)
  env2)

(defun jds/get-base-environment ()
  "Read the base environment emacs lives in and return a list of key-value lists."
  (with-temp-buffer
    (call-process "cmd" nil t nil "/c" "set")
    (jds/buffer-to-environment-list (current-buffer))))

(defun jds/get-environment-for-config (batch-file config base-env)
  "Return an environment list from a particular batch file and configuration.
base-env should be from jds/vc-configs.
If the configuration is unsupported, return nil."
  (with-temp-buffer
    (call-process "cmd" nil t nil "/c" batch-file config "&&" "set")
    (if (re-search-forward
	 "^The specified configuration type is missing" nil t)
	nil
      (jds/remove-env-duplicates
       base-env
       (jds/buffer-to-environment-list (current-buffer))))))

(defun jds/get-all-configs-for-batch-file (batch-file base-env)
  "Return a list of configs and config lists for the given batch file.
Unsupported configs will be omitted.

The output from this command will be a list starting with the version
of visual studio the batch file belongs to, then one list for each config.
Each config starts with the name of the config (think `arm' or `amd64'
here) and is followed by a series of lists for the environment variables.

The environment variables are regular lists, the first entry being the
name of the variable and the second entry being the value."
  (let ((config-list
	 (mapcar (lambda (config)
		   (cons
		    config
		    (jds/get-environment-for-config
		     batch-file config base-env)))
		 jds/vc-configs)))
    (cons (jds/determine-vs-version batch-file)
	  (delq nil (mapcar (lambda (list)
			      (if (cadr list)
				  list
				nil))
			    config-list)))))

(defun jds/insert-config (config version)
  "Takes a single config and inserts it into the current buffer as a function."
  (let ((funcname (concat "setup-ms-dev-environment-"
			  (replace-regexp-in-string "\\." "-" version)
			  "-"
			  (car config))))
    (insert
     (pp (append (list 'defun (make-symbol funcname) nil '(interactive))
		 (mapcar (lambda (env-var)
			   (list 'setenv (car env-var) (cadr env-var)))
			 (cdr config)))))))

(defun jds/insert-all-configs (configlist)
  "Takes the entire list of configs and inserts each config as a function.
The configlist should contain one list for each batch file that is processed
by jds/get-all-configs-for-batch-file."
  (mapc (lambda (config-with-version)
	  (let ((version (car config-with-version)))
	    (mapc (lambda (config)
		    (jds/insert-config config version)
		    (newline))
		  (cdr config-with-version))))
	configlist))

(defun jds/create-library ()
  (interactive)
  (let* ((batch-files (jds/locate-file-without-puking
		       (getenv "ProgramFiles(x86)") "vcvarsall.bat"))
	 (base-env (jds/get-base-environment))
	 (master-config-list
	  (mapcar (lambda (batfile)
		    (jds/get-all-configs-for-batch-file batfile base-env))
		  batch-files)))
    (with-current-buffer (get-buffer-create "windows-dev-environment.el")
      (kill-region (point-min) (point-max))
      (switch-to-buffer (current-buffer))
      (insert ";;; windows-dev-environment.el --- Set up Windows ")
      (insert "development environment")
      (newline)
      (newline)
      (insert ";; This is an automatically generated file.")
      (newline)
      (newline)
      (insert ";; Copyright should not apply to this file.  In countries ")
      (insert "where")
      (newline)
      (insert ";; copyright is required, the copyright should be 2017 by")
      (newline)
      (insert ";; Jeff Spaulding and the license Creative Commons CC0.")
      (newline)
      (newline)
      (insert ";;; Commentary:")
      (newline)
      (newline)
      (insert ";; This file generated by environment-vars.el")
      (insert ";; See that file for more information.")
      (newline)
      (newline)
      (insert ";;; Code:")
      (newline)
      (newline)
      (jds/insert-all-configs master-config-list)
      (newline)
      (newline)
      (insert "(provide 'windows-dev-environment)")
      (newline)
      (newline)
      (insert ";;; windows-dev-environment.el ends here")
      (newline))))


;;; environment-vars.el ends here
