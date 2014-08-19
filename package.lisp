;;;; package.lisp

(defpackage #:weblocks-cms-mailings
  (:use #:cl #:weblocks)
  (:export #:*email-model* 
           #:email-model-email-address 
           #:email-model-display-in-grid-p
           #:*default-email-from* 
           #:*email-target* 
           #:*spellcheck-enabled-p* 
           #:*available-backends* 
           #:send-with-backend 
           #:send-messages))

