(in-package :rfc2388.test)

;; We don't really need a lock in our case, because there will every be only
;; one outstaning response, which will either be waited for, or being
;; generated.  If/when we want to scale to more parallelism, we'll also need
;; some means of identifying individual posts (for instance by posting them
;; to unique paths); and then we'd need the lock, of course.
(defstruct exchange
  (semaphore (make-semaphore))
  (response nil))

(defun post-response (exchange data)
  (assert (null (exchange-response exchange))
          (exchange)
          "~S has an unretrieved response!" exchange)
  (setf (exchange-response exchange) data)
  (signal-semaphore (exchange-semaphore exchange))
  data)

(defun retrieve-response (exchange)
  (unless (wait-on-semaphore (exchange-semaphore exchange) 10)
    (error "Tired of waiting for a response on ~S" exchange))
  (shiftf (exchange-response exchange) nil))

;;; Only strings for now...
(defun read-file-contents (path &key (external-format :utf-8))
  (alexandria:read-file-into-string path :external-format external-format))

(defun process (post)
  (loop for entry in post
        for (name . value) = entry
        if (consp value)
          collect (destructuring-bind (path filename content-type)
                      value
                    `(,name . (,(read-file-contents path)
                              ,@(when content-type
                                  (list :content-type content-type))
                              ,@(when filename
                                  (list :filename filename)))))
        else
          collect entry))

(defun post-data (data &key port)
  (assert (integerp port)
          (port)
          ":port parameter to POST-DATA is not actually optional")
  (drakma:http-request (format nil "http://localhost:~D/" port)
                       :method :post
                       :external-format-out :utf-8
                       :external-format-in :utf-8
                       :parameters data))

(defvar *exchange* (make-exchange))

(defun response-handler (post-data)
  (post-response *exchange* (process post-data))
  "success")

(defun %contents (x)
  (etypecase x
    (string x)
    ((or function symbol)
     (with-output-to-string (out)
       (funcall x out)))
    (pathname (read-file-contents x))))

(defun compare-fields (a b)
  (cond ((and (stringp a)
              (stringp b))
         (string= a b))
        ((and (listp a)
              (listp b))
         (and (equal (rest a) (rest b))
              (string= (%contents (first a))
                       (%contents (first b)))))))

(defun compare-responses (a b)
  (loop for (key . value) in a
        always (compare-fields value (cdr (assoc key b :test #'string=)))))

(defun generate-ascii (stream)
  (write-string "ascii, duh!" stream))

(defun generate-utf-8 (stream)
  (when (typep stream 'flexi-streams:flexi-stream)
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8))
  (write-string "the one non-latin character: λ" stream))

(defun run-tests (&aux (port 4242))
  (unless *test-server*
    (start-test-server 'response-handler :port port))

  (loop for (name fn content-type)
        in '((ascii generate-ascii "text/plain")
             (utf-8 generate-utf-8 "text/plain"))
        collect (list name
                      (let ((values `(("simple-field" . "simple value")
                                      ("a-file" ,fn
                                                :content-type ,content-type
                                                :filename "test.txt"))))
                        (post-data values :port port)
                        (compare-responses values
                                           (retrieve-response *exchange*))))))
