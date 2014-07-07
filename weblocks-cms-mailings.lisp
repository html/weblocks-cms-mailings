;;;; weblocks-cms-mailings.lisp

(in-package #:weblocks-cms-mailings)

(defvar *email-model* nil)
(defvar *default-email-from* nil)
(defvar *email-target* nil)
(defvar *mail-transport* :sendmail)

(defgeneric email-model-email-address (obj)
  (:documentation "Used to extract emails from email model")
  (:method ((obj t))
   (error "email-model-email-address should be implemented for model with type ~A" (type-of obj))))

(defun send-messages ()
  (mapcar 
    #'send-message 
    (weblocks-utils:find-by-values 'weblocks-cms::message :status :ready-to-send)))

(cl-cron:make-cron-job 'send-messages)
