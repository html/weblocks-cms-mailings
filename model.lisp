(in-package :weblocks-cms-mailings)

(defvar *available-backends* (list :sendmail-mail))

(weblocks-cms:def-additional-schema 
  :message
  `((:TITLE "Message" :NAME :MESSAGE :FIELDS
     ((:TITLE "Content" :NAME :CONTENT :TYPE :EDITOR-TEXTAREA :OPTIONS NIL)
      (:TITLE "Email Address" :NAME :EMAIL-ADDRESS :TYPE :CUSTOM :OPTIONS
       "weblocks-cms-mailings::email-address-fields")
      (:TITLE "Email From" :NAME :EMAIL-FROM-ADDRESS :TYPE :CUSTOM :OPTIONS
       "weblocks-cms-mailings::email-from-address-fields")
      (:TITLE "Status" :NAME :STATUS :TYPE :SINGLE-CHOICE :OPTIONS "created
ready-to-send
sent
error-sending")
      (:TITLE "Subject" :NAME :SUBJECT :TYPE :STRING :OPTIONS NIL)
      (:TITLE "Updated at" :NAME :UPDATED-AT :TYPE :CUSTOM :OPTIONS
      "weblocks-cms-mailings::updated-at-fields")
      (:TITLE "Variables" :NAME :VARIABLES :TYPE :CUSTOM :OPTIONS NIL)
      (:TITLE "Message type" :NAME :TYPE :TYPE :SINGLE-CHOICE :OPTIONS "sendmail-mail")))))

(defmethod send-email (&key (from *default-email-from*) subject text recipient recipients (transport (eql :sendmail)))
  "Sends utf-8 email via sendmail"
  (if recipient 
    (setf recipients (list recipient)))

  (setf text (remove #\numero_sign text))
  (setf subject (remove #\numero_sign subject))
  (setf text (substitute #\- #\soft_hyphen text))
  (setf subject (substitute #\- #\soft_hyphen subject))

  (sendmail:with-email 
    (out recipients
         :subject subject
         :from from 
         :reply-to from
         :type "multipart"
         :subtype "alternative"
         :attachments (list 
                        (make-instance 
                          'cl-mime:text-mime
                          :encoding :base64 
                          :charset :utf-8
                          :content (flexi-streams:string-to-octets text :external-format :utf-8 ))
                        (make-instance 
                          'cl-mime:text-mime
                          :encoding :quoted-printable 
                          :charset :utf-8
                          :subtype "html"
                          :content (flexi-streams:string-to-octets text :external-format :utf-8)))))
  t)

(defgeneric send-with-backend (obj backend)
  (:method ((obj weblocks-cms::message) backend)
   (error "Define 'weblocks-cms::send-with-backend method for backend ~A" (write-to-string backend))))

(defmethod send-with-backend ((obj weblocks-cms::message) (backend (eql :sendmail-mail)))
  (send-email :subject (weblocks-cms::message-subject obj)
              :text (weblocks-cms::message-content obj)
              :recipient (weblocks-cms::message-email-address obj)
              :transport *mail-transport*))

(defmethod send-message ((obj weblocks-cms::message))
  (if (ignore-errors (send-with-backend obj (weblocks-cms::message-type obj)))
    (setf (weblocks-cms::message-status obj) :sent)
    (setf (weblocks-cms::message-status obj) :error-sending))
  (setf (weblocks-cms::message-updated-at obj) (get-universal-time)))
