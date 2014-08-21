(in-package :weblocks-cms-mailings)

(defun nl2br (text)
  "Replaces string #\Newline symbols with html br elements"
  (cl-ppcre:regex-replace-all #\newline text "<br/>"))


(defwidget message-grid (weblocks-cms::popover-gridedit)
  ())

(defgeneric email-model-display-in-grid-p (obj message-type)
  (:documentation "After message type choosen all email model records should be filtered using this method.
                   For example to filter objects without email or phone number, we don't need mailing for such records")
  (:method ((obj t) message-type)
   (error "email-model-display-in-grid-p should be implemented for model with type ~A and message-type ~A" (type-of obj) message-type)))

(defmethod mass-mailing-flow ((grid message-grid) sel)
  (let* ((email-model-keyword (intern (string-upcase *email-model*) "KEYWORD"))
         (result-widget))

    (with-flow (dataedit-item-widget grid) 

      (setf result-widget 
            (weblocks-utils:make-form 
              :fields `((message-type :present-as (radio :choices (quote ,*available-backends*))
                                      :requiredp t))
              :buttons '(:submit :cancel)
              :on-success (lambda (form obj)
                            (answer result-widget 
                                    (alexandria:make-keyword 
                                      (string-upcase (slot-value obj 'message-type)))))
              :on-cancel (lambda (obj)
                           (declare (ignore obj))
                           (answer result-widget nil))))

      (let* ((result (yield result-widget))
             (emails-grid)
             (filtering-widget))

        (when result 

          (setf emails-grid (make-instance 'weblocks-cms::popover-gridedit 
                                           :data-class (or *email-model* (error "Please set weblocks-cms-mailings:*email-model* "))
                                           :item-form-view (weblocks-cms::get-model-form-view email-model-keyword)
                                           :view (weblocks-cms::get-model-table-view email-model-keyword)))

          (setf filtering-widget 
                (make-instance 'weblocks-filtering-widget:filtering-widget 
                               :dataseq-instance emails-grid 
                               :hidden-filter-lambda (lambda (item)
                                                       (email-model-display-in-grid-p item result))
                               :form-fields 
                               (list* 
                                 (list :id :id
                                       :caption "Identifier"
                                       :accessor #'object-id)
                                 (loop for i in (getf (weblocks-cms::get-model-description email-model-keyword) :fields)
                                       collect (list :id (getf i :name)
                                                     :caption (getf i :title)
                                                     :slot (intern (string-upcase (getf i :name)) "WEBLOCKS-CMS"))))))

          (setf result-widget 
                (make-instance 
                  'widget 
                  :children
                  (list 
                    (lambda (&rest args)
                      (with-html 
                        (render-link
                          (lambda (&rest args)
                            (answer result-widget t))
                          "Create a letter for all filtered objects" 
                          :class "btn btn-primary")
                        (:span " when you've done filtering or ")
                        (render-link
                          (lambda/cc (&rest args)
                            (answer result-widget nil))
                          "Cancel" 
                          :class "btn")))
                    filtering-widget
                    emails-grid)))

          (when (yield result-widget)
            (let ((view (dataedit-item-form-view grid))
                  (data (make-instance (dataseq-data-form-class grid) :email-from-address *default-email-from*))
                  (email-targets 
                    (weblocks-utils:find-by 
                      *email-model*
                      (lambda (item)
                        (weblocks-filtering-widget::item-matches-widget-filters-p item filtering-widget))))
                  (email-objects ))

              (setf view (defview nil (:type form :persistp nil :inherit-from view)
                                  (weblocks-cms::email-address :hidep t)
                                  (weblocks-cms::status :hidep t)
                                  (weblocks-cms::message-type :hidep t)
                                  (emails-to-write 
                                    :label "Emails for the letter" 
                                    :present-as html
                                    :reader (lambda (item)
                                              (format nil "~{~A~^, ~}" (mapcar #'email-model-email-address (get 'emails item)))))
                                  (variables 
                                    :label "Variables assigned to letter body and subject"
                                    :present-as html 
                                    :reader (lambda (item)
                                              (weblocks-cms-pages::get-variables-descriptions 
                                                (weblocks-cms-pages::append-default-variables :mailings-mail nil))))))

              (setf (get 'emails data) email-targets)

              (setf result-widget
                    (make-instance 'dataform
                                   :data  data
                                   :class-store (dataseq-class-store grid)
                                   :ui-state :form
                                   :on-cancel (lambda (obj)
                                                (declare (ignore obj))
                                                (answer result-widget nil))
                                   :on-success (lambda (obj)
                                                 (answer result-widget t))
                                   :data-view (dataedit-item-data-view grid)
                                   :form-view view))

              (when (yield result-widget)

                (setf result-widget 
                      (make-widget 
                        (lambda (&rest args)
                          (with-html 
                            (:h1 "Preview emails")

                            (setf email-objects nil)
                            (loop for i in email-targets do 
                                  (let ((*email-target* i)
                                        (variables-values))

                                    (setf variables-values 
                                          (loop for (first second) 
                                                on (weblocks-cms-pages::append-default-variables :mailings-mail nil)
                                                by #'cddr
                                                collect (cons first (car second))))

                                    (push (make-instance 
                                            'weblocks-cms::message 
                                            :email-from-address (weblocks-cms::message-email-from-address data)
                                            :email-address (email-model-email-address i)
                                            :updated-at (get-universal-time)
                                            :status :created
                                            :subject (mustache:mustache-render-to-string 
                                                       (weblocks-cms::message-subject data)
                                                       variables-values)
                                            :content (mustache:mustache-render-to-string 
                                                       (weblocks-cms::message-content data)
                                                       variables-values)
                                            :type :sendmail-mail) email-objects)))

                            (loop for i in email-objects do 
                                  (cl-who:htm 
                                    (:dl :class "dl-horizontal"
                                     (:dt "From:")
                                     (:dd (cl-who:esc (weblocks-cms::message-email-from-address data)))
                                     (:dt "To:")
                                     (:dd 
                                       (if (weblocks-cms::message-email-address i)
                                         (cl-who:esc (weblocks-cms::message-email-address i) )
                                         (cl-who:htm (:b (:i :class "text-error"  "< empty email >")))))
                                     (:dt :class (unless (check-text (weblocks-cms::message-subject i))
                                                   "text-error")
                                      "Subject:")
                                     (:dd (if (weblocks-cms::message-subject i)
                                            (cl-who:htm (cl-who:str (add-spelling-info (weblocks-cms::message-subject i))))
                                            (cl-who:htm (:b (:i :class "text-error" "< empty subject >")))))
                                     (:dt :class (unless (check-text (weblocks-cms::message-content i))
                                                   "text-error")
                                      "Content:")
                                     (:dd (cl-who:str 
                                            (add-spelling-info 
                                              (nl2br (arnesi:escape-as-html (weblocks-cms::message-content i)))))))
                                    (:hr)))

                            (:div :class "row"
                             (:div :class "span2")
                             (:div :class "span4"
                              (render-link 
                                (lambda (&rest args)
                                  (loop for i in email-objects do 
                                        (weblocks-stores:persist-object (dataseq-class-store grid) i))
                                  (answer result-widget)
                                  (answer (dataedit-item-widget grid)))
                                "Create emails"
                                :class "btn btn-primary")
                              (cl-who:str "&nbsp;")
                              (render-link 
                                (lambda (&rest args)
                                  (answer result-widget)
                                  (answer (dataedit-item-widget grid)))
                                "Cancel"
                                :class "btn")))
                            (:br)))))

                (yield result-widget)))))))))

(defmethod dataedit-update-operations ((obj message-grid) &key
                                                          (delete-fn #'dataedit-delete-items-flow)
                                                          (add-fn #'dataedit-add-items-flow))
  (call-next-method)

  (setf (dataseq-item-ops obj)
        (remove "Mass mailing" (dataseq-item-ops obj)
                :key #'car :test #'string-equal))

  (pushnew (cons "Mass mailing" #'mass-mailing-flow)
           (dataseq-common-ops obj)
           :key #'car))

(defmethod dataedit-create-drilldown-widget ((grid message-grid) item)
  (make-instance 'weblocks:dataform
                 :data item
                 :class-store (dataseq-class-store grid)
                 :ui-state (if (equal (weblocks-cms::message-status item) :sent)
                             :data 
                             :form)
                 :on-success (lambda (obj)
                               (declare (ignore obj))
                               (flash-message (dataseq-flash grid)
                                              (format nil (weblocks::translate "Modified ~A." :preceding-gender (determine-gender (humanize-name (dataseq-data-class grid))))
                                                      (weblocks::translate (humanize-name (dataseq-data-class grid)))))
                               (answer (dataedit-item-widget grid) t))
                 :on-cancel (when (eql (gridedit-drilldown-type grid) :edit)
                              (lambda (obj)
                                (declare (ignore obj))
                                (answer (dataedit-item-widget grid) nil)))
                 :on-close (lambda (obj)
                             (declare (ignore obj))
                             (answer (dataedit-item-widget grid) nil))
                 :data-view (weblocks:dataedit-item-data-view grid)
                 :form-view (weblocks:dataedit-item-form-view grid)))
