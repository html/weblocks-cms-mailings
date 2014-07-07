(in-package :weblocks-cms-mailings)

(defvar *spellcheck-enabled-p* nil)

(defun check-text (text)
  "Checks text using yandex api"
  (unless *spellcheck-enabled-p* 
    (return-from check-text t))
  (let* ((drakma:*drakma-default-external-format* :utf-8)
         (json-check-url "http://speller.yandex.net/services/spellservice.json/checkText")
         (options "128")
         (result (json:decode-json-from-string 
                   (flexi-streams:octets-to-string 
                     (drakma:http-request json-check-url 
                                          :parameters `(("text" . ,text)
                                                        ("options" . ,options))
                                          :force-binary t)
                     :external-format :utf-8))))

    (if result 
      (values nil (loop for i in result 
                        collect (list :start (cdr (assoc :pos i))
                                      :length (cdr (assoc :len i))
                                      :suggestions (cdr (assoc :s i)))))
      t)))

(defun add-spelling-info (text)
  "Adds html tags to show spelling errors on some words"
  (unless *spellcheck-enabled-p* 
    (return-from add-spelling-info text))

  (multiple-value-bind (success errors) (check-text text)
    (if success 
      text 
      (let ((result-text text))
        (loop for i in (reverse errors) do 
              (setf result-text 
                    (concatenate 
                      'string 
                      (subseq result-text 0 (getf i :start))
                      (weblocks::with-html-to-string
                        (:span :class "text-error"
                         :data-toggle "tooltip" 
                         :title (if (getf i :suggestions)
                                  (format nil "Suggestions: ~{~A~^, ~}" (getf i :suggestions))
                                  "No suggestions for this case")
                         (cl-who:str (subseq result-text (getf i :start) (+ (getf i :start) (getf i :length))))))
                      (subseq result-text (+ (getf i :start) (getf i :length))))))
        result-text))))
