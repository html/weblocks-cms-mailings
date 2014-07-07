;;;; weblocks-cms-mailings.asd

(asdf:defsystem #:weblocks-cms-mailings
  :serial t
  :description "Mailings interface for Weblocks CMS"
  :author "Olexiy Zamkoviy <olexiy.z@gmail.com>"
  :version (:read-from-file "version.lisp-expr")
  :license "LLGPL"
  :depends-on (#:weblocks-cms #:weblocks-filtering-widget #:arnesi #:weblocks-cms-pages #:weblocks-bootstrap-typeahead-presentation #:cl-cron #:cl-sendmail #:flexi-streams #:weblocks-utils)
  :components 
  ((:file "package")
   (:file "spellcheck")
   (:file "weblocks-cms-mailings")
   (:file "model")
   (:file "message-grid-widget")
   (:file "weblocks-cms-integration")))

