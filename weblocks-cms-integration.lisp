(in-package :weblocks-cms-mailings)

(defun email-from-address-fields (type description model-description-list)
  (case type
    (:table nil)
    (:form (list 
             (list 
               (weblocks-cms::keyword->symbol (getf description :name))
               :label (getf description :title)
               :reader (lambda (item)
                         (if (object-id item)
                           (weblocks-cms::message-email-from-address item)
                           *default-email-from*)))))))

(defun email-address-fields (type description model-description-list)
  (case type
    (:table 
      (list 
        (list 
          (weblocks-cms::keyword->symbol (getf description :name))
          :label (getf description :title))))
    (:form (list 
             (list 
               (weblocks-cms::keyword->symbol (getf description :name))
               :label (getf description :title)
               :present-as (list 'weblocks-bootstrap-typeahead-presentation::bootstrap-typeahead
                                 :choices (lambda (&rest args) 
                                            (loop for i in (weblocks-utils:all-of *email-model*) 
                                                  if (email-model-email-address i)
                                                  collect (email-model-email-address i)))))))))

(defun updated-at-fields (type description model-description-list)
  (case type
    (:table 
      (list 
        (list 
          (weblocks-cms::keyword->symbol (getf description :name))
          :label (getf description :title)
          :present-as (list 'weblocks::date :format "%Y-%m-%d %H:%I"))))
    (:form (list 
             (list 
               (weblocks-cms::keyword->symbol (getf description :name))
               :label (getf description :title)
               :present-as 'hidden
               :writer (lambda (value item)
                         (setf (weblocks-cms::message-updated-at item) (get-universal-time))))))))

(defmethod weblocks-cms::make-widget-for-model-description ((model (eql :message)) description)
  (make-instance 
    'message-grid 
    :data-class (weblocks-cms::keyword->symbol (getf description :name))
    :item-form-view (weblocks-cms::get-model-form-view (getf description :name))
    :view (weblocks-cms::get-model-table-view (getf description :name))))

