;;; font-lock-test.el --- Test org-superstar-mode's fontification for various settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  David Williams

;; Author: David Williams <d.williams@posteo.net>
;; Package-Requires: ((faceup "0.0.5"))

;;; Commentary:

;; The tests in this file are best used in batch mode.  A simple
;; "make font-lock" or "make font-lock-inlinetask" (for tests only
;; making sense when org-inlinetask is loaded)

;;; Code:

(require 'cl-macs)
(require 'subr-x)
(require 'faceup)

(defvar org-superstar/test-dir (faceup-this-file-directory)
  "Base directory where sample files are located in.")

(defvar org-superstar/quiet-test t
  "If t, do not log individual calls to ‘compose-region’.")

(defun org-superstar/get-path (filename)
  "Provide the correct path to FILENAME."
  (concat org-superstar/test-dir filename))

(defun org-superstar/test-file (org-file ref-file)
  "Test that ORG-FILE is fontified according to REF-FILE."
  (faceup-test-font-lock-file
   'org-mode
   (org-superstar/get-path org-file)
   (org-superstar/get-path ref-file)))

(faceup-defexplainer org-superstar/test-file)

(ert-deftest org-superstar/test-font-lock-defaults ()
  "Test default fontification facilities of ‘\\[org-superstar-mode]’."
  (message "Looking for needed packages..")
  (skip-unless (and (featurep 'faceup)
                    (not (featurep 'org-inlinetask))))
  (unwind-protect
      (let ((org-mode-hook
             (list (lambda () (org-superstar-mode 1)))))
        (message "Setting up listeners..")
        (unless org-superstar/quiet-test
          (org-superstar/start-listening))
        (message "Testing item bullets (default)..")
        (should (org-superstar/test-file
                 "sample-items.org" "items.faceup"))
        (message "Testing deep headlines (default)..")
        (should (org-superstar/test-file
                 "sample-inlinetask.org" "noinline.faceup"))
        (message "Testing regular headlines (default)..")
        (should (org-superstar/test-file
                 "sample-headlines.org" "headlines-leading.faceup"))
        (message "Testing fake item bullets (default)..")
        (should (org-superstar/test-file
                 "sample-nonitem.org" "nonitem.faceup")))
    (org-superstar/dismiss-listening)))

(ert-deftest org-superstar/test-font-lock-custom ()
  "Test customized fontification facilities of ‘\\[org-superstar-mode]’."
  (message "Looking for needed packages..")
  (skip-unless (and (featurep 'faceup)
                    (not (featurep 'org-inlinetask))))
  (unwind-protect
      (let ((org-mode-hook
             (list (lambda () (org-superstar-mode 1)))))
        (message "Setting up listeners..")
        (unless org-superstar/quiet-test
          (org-superstar/start-listening))
        (message "Testing disabled item bullets..")
        (let ((org-superstar-prettify-item-bullets nil))
          (should (org-superstar/test-file
                   "sample-items.org" "disabled-items.faceup")))
        (let ((org-hide-leading-stars t))
          (message "Testing headlines (hidden stars)..")
          (should (org-superstar/test-file
                   "sample-headlines.org" "headlines-noleading.faceup"))))
    (org-superstar/dismiss-listening)))

(ert-deftest org-superstar/test-font-lock-inlinetask ()
  "Test fontification of ‘\\[org-superstar-mode]’ with ‘org-inlinetask’."
  (message "Looking for needed packages..")
  (skip-unless (and (featurep 'faceup) (featurep 'org-inlinetask)))
  (unwind-protect
      (let ((org-mode-hook
             (list (lambda () (org-superstar-mode 1)))))
        (message "Load org-inlinetask..")
        (require 'org-inlinetask)
        (message "Setting up listeners..")
        (unless org-superstar/quiet-test
          (org-superstar/start-listening))
        (message "Testing standard inlinetask..")
        (should (org-superstar/test-file
                 "sample-inlinetask.org" "inline.faceup"))
        (let ((org-hide-leading-stars t))
          (message "Testing inline task with hidden stars..")
          (should (org-superstar/test-file
                   "sample-inlinetask.org" "inline-noleading.faceup"))))
    (org-superstar/dismiss-listening)))


(provide 'font-lock-test)
;;; font-lock-test.el ends here
