;;; -*- mode: LISP; package: RFC2388 -*-

;;;; Copyright (c) 2003 Janis Dzerins
;;;; 
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;; 1. Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;; 2. Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defpackage :rfc2388
  (:use :common-lisp)
  (:export
   ;;#:read-until-next-boundary

   #:parse-header
   #:header
   #:header-name
   #:header-value
   #:header-parameters

   #:parse-mime
   #:mime-part
   #:mime-part-contents
   #:mime-part-headers))


(in-package :rfc2388)



(defun lwsp-char-p (char)
  "Returns true if CHAR is a linear-whitespace-char (LWSP-char).  Either
   space or tab, in short."
  (or (char= char #\space)
      (char= char #\tab)))




;;; *** This actually belongs to RFC2046
;;; 
(defun read-until-next-boundary (stream boundary &optional discard)
  "Reads from STREAM up to the next boundary.  Returns two values: read
   data (nil if DISCARD is true), and true if the boundary is not last
   (i.e., there's more data)."
  
  ;; Read until [CRLF]--boundary[--][transport-padding]CRLF
  ;; States:     1 2  345        67  8                 9 10
  ;;
  ;; *** This will WARN like crazy on some bad input -- should only do each
  ;; warning once.
  
  (let ((length (length boundary)))
    (unless (<= 1 length 70)
      (warn "Boundary has invalid length -- must be between 1 and 70, but is: ~S" length))
    (let ((last-char (schar boundary (1- length))))
      (when (or (char= last-char #\space)
                (char= last-char #\tab))
        (warn "Boundary has trailing whitespace: ~S" boundary))))
  
  (flet ((run (result)
           "This one writes everything up to a boundary to RESULT stream,
            and returns false if the closing delimiter has been read, and
            true otherwise."
           (let ((state 1)
                 (boundary-index 0)
                 (boundary-length (length boundary))
                 (closed nil)
                 (queued-chars (make-string 4))
                 (queue-index 0)
                 char
                 (leave-char nil))
             
             (flet ((write-queued-chars ()
                      (dotimes (i queue-index)
                        (write-char (schar queued-chars i) result))
                      (setf queue-index 0))
                    
                    (enqueue-char ()
                      (setf (schar queued-chars queue-index) char)
                      (incf queue-index)))
               
               (loop
                 
                 (if leave-char
                     (setq leave-char nil)
                     (setq char (read-char stream nil nil)))
                 
                 (unless char
                   (setq closed t)
                   (return))

                 #-(and)
                 (format t "~&S:~D BI:~2,'0D CH:~:[~;*~]~S~%"
                         state boundary-index leave-char char)

                 (case state
                   (1 ;; optional starting CR
                    (cond ((char= char #\return)
                           (enqueue-char)
                           (setq state 2))
                          ((char= char #\-)
                           (setq leave-char t
                                 state 3))
                          (t
                           (write-char char result))))

                   (2 ;; optional starting LF
                    (cond ((char= char #\linefeed)
                           (enqueue-char)
                           (setq state 3))
                          (t
                           (write-queued-chars)
                           (write-char char result)
                           (setq state 1))))

                   (3 ;; first dash in dash-boundary
                    (cond ((char= char #\-)
                           (enqueue-char)
                           (setq state 4))
                          (t
                           (write-queued-chars)
                           (write-char char result)
                           (setq state 1))))

                   (4 ;; second dash in dash-boundary
                    (cond ((char= char #\-)
                           (enqueue-char)
                           (setq state 5))
                          (t
                           (write-queued-chars)
                           (write-char char result)
                           (setq state 1))))

                   (5 ;; boundary
                    (cond ((char= char (schar boundary boundary-index))
                           (incf boundary-index)
                           (when (= boundary-index boundary-length)
                             (setq state 6)))
                          (t
                           (write-queued-chars)
                           (write-sequence boundary result :end boundary-index)
                           (write-char char result)
                           (setq boundary-index 0
                                 state 1))))

                   (6 ;; first dash in close-delimiter
                    (cond ((char= char #\-)
                           (setq state 7))
                          (t
                           (setq leave-char t)
                           (setq state 8))))

                   (7 ;; second dash in close-delimiter
                    (cond ((char= char #\-)
                           (setq closed t
                                 state 8))
                          (t
                           ;; this is a strange situation -- only two dashes, linear
                           ;; whitespace or CR is allowed after boundary, but there was
                           ;; a single dash...  One thing is clear -- this is not a
                           ;; close-delimiter.  Hence this is garbage what we're looking
                           ;; at!
                           (warn "Garbage where expecting close-delimiter!")
                           (setq leave-char t)
                           (setq state 8))))

                   (8 ;; transport-padding (LWSP* == [#\space #\tab]*)
                    (cond ((lwsp-char-p char)
                           ;; ignore these
                           )
                          (t
                           (setq leave-char t)
                           (setq state 9))))

                   (9 ;; CR
                    (cond ((char= char #\return)
                           (setq state 10))
                          (t
                           (warn "Garbage where expecting CR!"))))

                   (10 ;; LF
                    (cond ((char= char #\linefeed)
                           ;; the end
                           (return))
                          (t
                           (warn "Garbage where expecting LF!")))))))
             (not closed))))

    (if discard
        (let ((stream (make-broadcast-stream)))
          (values nil (run stream)))
        (let* ((stream (make-string-output-stream))
               (closed (run stream)))
          (values (get-output-stream-string stream)
                  closed)))))



;;; Header parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 


(defstruct (header (:type list)
                   (:constructor make-header (name value parameters)))
  name
  value
  parameters)



(defun skip-linear-whitespace (string &key start end)
  "Returns the position of first non-linear-whitespace character in STRING
   bound by START and END."
  (position-if-not #'lwsp-char-p string :start start :end end))



(defmethod parse-header ((source string) &optional (start-state :name))
  (with-input-from-string (in source)
    (parse-header in start-state)))




;;; *** I don't like this parser -- it will have to be rewritten when I
;;; make my state-machine parser-generator macro!
;;; 
(defmethod parse-header ((stream stream) &optional (start-state :name))
  "Returns a MIME part header, or NIL, if there is no header.  Header is
   terminated by CRLF."
  (let ((state (ecase start-state
                 (:name 1)
                 (:value 2)
                 (:parameters 3)))
        (result (make-string-output-stream))
        char
        (leave-char nil)

        name
        value
        parameter-name
        parameters)
    
    (labels ((skip-lwsp (next-state)
               (loop
                 do (setq char (read-char stream nil nil))
                 while (and char (lwsp-char-p char)))
               (setq leave-char t
                     state next-state))

             (collect-parameter ()
               (push (cons parameter-name
                           (get-output-stream-string result))
                     parameters)
               (setq parameter-name nil)
               (skip-lwsp 3))

             (token-end-char-p (char)
               (or (char= char #\;)
                   (lwsp-char-p char))))

      (loop
        
        (if leave-char
            (setq leave-char nil)
            (setq char (read-char stream nil nil)))
        
        ;; end of stream
        (unless char
          (return))
        
        (when (char= #\return char)
          (setq char (read-char stream nil nil))
          (cond ((or (null char)
                     (char= #\linefeed char))
                 ;; CRLF ends the input
                 (return))
                (t
                 (warn "LINEFEED without RETURN in header.")
                 (write-char #\return result)
                 (setq leave-char t))))
        
        #-(and)
        (format t "~&S:~D,'0D CH:~:[~;*~]~S~%"
                state leave-char char)
        
        (ecase state
          (1 ;; NAME
           (cond ((char= char #\:)
                  ;; end of name
                  (setq name (get-output-stream-string result))
                  (skip-lwsp 2))
                 (t
                  (write-char char result))))
          
          (2 ;; VALUE
           (cond ((token-end-char-p char)
                  (setq value (get-output-stream-string result))
                  (skip-lwsp 3))
                 (t
                  (write-char char result))))
          
          (3 ;; PARAMETER name
           (cond ((char= #\= char)
                  (setq parameter-name (get-output-stream-string result)
                        state 4))
                 (t
                  (write-char char result))))
          
          (4 ;; PARAMETER value start
           (cond ((char= #\" char)
                  (setq state 5))
                 (t
                  (setq leave-char t
                        state 7))))
          
          (5 ;; Quoted PARAMETER value
           (cond ((char= #\" char)
                  (setq state 6))
                 (t
                  (write-char char result))))

          (6 ;; End of quoted PARAMETER value
           (cond ((token-end-char-p char)
                  (collect-parameter))
                 (t
                  ;; no space or semicolon after quoted parameter value
                  (setq leave-char t
                        state 3))))
          
          (7 ;; Unquoted PARAMETER value
           (cond ((token-end-char-p char)
                  (collect-parameter))
                 (t
                  (write-char char result))))))

      (case state
        (1
         (setq name (get-output-stream-string result)))
        (2
         (setq value (get-output-stream-string result)))
        ((3 4)
         (let ((name (get-output-stream-string result)))
           (unless (zerop (length name))
             (warn "Parameter without value in header.")
             (push (cons name nil) parameters))))
        ((5 6 7)
         (push (cons parameter-name (get-output-stream-string result)) parameters))))

    (if (and (or (null name)
                 (zerop (length name)))
             (null value)
             (null parameters))
        nil
        (make-header name value parameters))))



(defgeneric parse-mime (source boundary)
  (:documentation
   "Parses MIME entities, returning them as a list.  Each element in the
    list is of form: (body . header*), where BODY is the contents of MIME
    part, and HEADERS are all headers for that part.  BOUNDARY is a string
    used to separate MIME entities."))



(defstruct (mime-part (:type list)
                      (:constructor make-mime-part (contents headers)))
  contents
  headers)



(defmethod parse-mime ((input string) separator)
  (with-input-from-string (stream input)
    (parse-mime stream separator)))




(defmethod parse-mime ((input stream) boundary)

  ;; Find the first boundary.  Return immediately if it is also the last
  ;; one.
  (unless (nth-value 1 (read-until-next-boundary input boundary t))
    (return-from parse-mime nil))
  
  (let ((result ()))
    (loop
      (let ((headers (loop
                       for header = (parse-header input)
                       while header
                       collect header)))
        (multiple-value-bind (text more)
            (read-until-next-boundary input boundary)
          (push (make-mime-part text headers) result)
          (when (not more)
            (return)))))
    (nreverse result)))

