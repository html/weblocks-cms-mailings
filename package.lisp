;;;; package.lisp

(defpackage #:weblocks-cms-mailings
  (:use #:cl #:weblocks)
  (:export #:*email-model* #:email-model-email-address #:*default-email-from* #:*email-target* #:*spellcheck-enabled-p* #:*available-backends* #:send-with-backend))

