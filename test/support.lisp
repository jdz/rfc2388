(defpackage :rfc2388.test
  (:use :common-lisp))

(in-package rfc2388.test)

(defvar *test-server* nil)

(defclass test-acceptor (hunchentoot:acceptor)
  ((post-handler
    :initarg :post-handler
    :accessor post-handler-of))
  (:default-initargs
   :access-log-destination "/tmp/rfc2388-access.log"
   :message-log-destination "/tmp/rfc2388-message.log"))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor test-acceptor)
                                                  (request t))
  (declare (ignorable acceptor))
  (test-dispatcher request))

(defun test-dispatcher (request)
  (let ((path (hunchentoot:script-name request))
        (method (hunchentoot:request-method request)))
    (cond ((and (eq :post method)
                (string= "/" path))
           (let ((parameters (hunchentoot:post-parameters request)))
             (funcall (post-handler-of hunchentoot:*acceptor*)
                      parameters)))
          (t
           (warn "Unexpected request: ~A ~A" method path)))))

(defun start-test-server (handler &key (port 4242))
  (when *test-server*
    (stop-test-server *test-server*))
  (setf *test-server* (make-instance 'test-acceptor
                                     :post-handler handler
                                     :port port))
  (setf hunchentoot:*hunchentoot-default-external-format* hunchentoot::+utf-8+)
  (hunchentoot:start *test-server*)
  *test-server*)

(defun stop-test-server (&optional (server *test-server*))
  (hunchentoot:stop server))

#+sbcl
(progn
  (defun make-semaphore ()
    (sb-thread:make-semaphore :count 0))

  (defun signal-semaphore (semaphore)
    (sb-thread:signal-semaphore semaphore))

  (defun wait-on-semaphore (semaphore &optional timeout)
    (sb-thread:wait-on-semaphore semaphore :timeout timeout)))

#+ccl
(progn
  (defun make-semaphore ()
    (ccl:make-semaphore))

  (defun signal-semaphore (semaphore)
    (ccl:signal-semaphore semaphore))

  (defun wait-on-semaphore (semaphore &optional timeout)
    (if timeout
        (ccl:timed-wait-on-semaphore semaphore timeout)
        ;; `ccl:wait-on-semaphore' does not return a useful value.
        (progn (ccl:wait-on-semaphore semaphore) t))))
