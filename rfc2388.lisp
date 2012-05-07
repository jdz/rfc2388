;;;; -*- mode: LISP; package: RFC2388 -*-
;;;; Copyright (c) 2003 Janis Dzerins
;;;; Modifications for TBNL Copyright (c) 2004 Michael Weber and Dr. Edmund Weitz
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

#+xcvb (module (:depends-on ("packages")))

(in-package :rfc2388)



;;; Utility functions


(defun lwsp-char-p (char)
  "Returns true if CHAR is a linear-whitespace-char (LWSP-char).  Either
   space or tab, in short."
  (or (char= char #\space)
      (char= char #\tab)))


;;; *** This actually belongs to RFC2046
;;;
(defun read-until-next-boundary (stream boundary out-stream)
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
    (when (lwsp-char-p (schar boundary (1- length)))
      (warn "Boundary has trailing whitespace: ~S" boundary)))

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
                     (setq char (read-char stream)))

                  #-(and)
                  (format t "~&S:~D QI:~D BI:~2,'0D CH:~:[~;*~]~S~%"
                          state queue-index boundary-index leave-char char)

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
                           (setq leave-char t
                                 state 1))))

                   (3 ;; first dash in dash-boundary
                    (cond ((char= char #\-)
                           (enqueue-char)
                           (setq state 4))
                          (t
                           (write-queued-chars)
                           (setq leave-char t
                                 state 1))))

                   (4 ;; second dash in dash-boundary
                    (cond ((char= char #\-)
                           (enqueue-char)
                           (setq state 5))
                          (t
                           (write-queued-chars)
                           (setq leave-char t
                                 state 1))))

                   (5 ;; boundary
                    (cond ((char= char (schar boundary boundary-index))
                           (incf boundary-index)
                           (when (= boundary-index boundary-length)
                             (setq state 6)))
                          (t
                           (write-queued-chars)
                           (write-sequence boundary result :end boundary-index)
                           (setq boundary-index 0
                                 leave-char t
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

    (run out-stream)))


(defun make-tmp-file-name ()
  (if (find-package :tbnl)
      (funcall (find-symbol #.(string '#:make-tmp-file-name) :tbnl))
      (error "WRITE-CONTENT-TO-FILE keyword argument to PARSE-MIME is supported in TBNL only at the moment.")))



;;; Header parsing


(defstruct (header (:type list)
                   (:constructor make-header (name value parameters)))
  name
  value
  parameters)


(defun skip-linear-whitespace (string &key (start 0) end)
  "Returns the position of first non-linear-whitespace character in STRING
   bound by START and END."
  (position-if-not #'lwsp-char-p string :start start :end end))


(defgeneric parse-header (source &optional start-state)
  (:documentation "Parses SOURCE and returs a single MIME header.

Header is a list of the form (NAME VALUE PARAMETERS), PARAMETERS
is a list of (NAME . VALUE)"))


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
        (format t "~&S:~,'0D CH:~:[~;*~]~S~%"
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



;;; _The_ MIME parsing


(defgeneric parse-mime (source boundary &key write-content-to-file)
  (:documentation
   "Parses MIME entities, returning them as a list.

Each element in the list is of form: (body headers), where BODY is the
contents of MIME part (value depends on WRITE-CONTENT-TO-FILE, see
below), and HEADERS are all headers for that part.  BOUNDARY is a
string used to separate MIME entities.

WRITE-CONTENT-TO-FILE can have one of the following values:

  NIL: MIME part is read into a string, and returned as is.

  T: A temporary file is created, MIME part is written to it, and the
  file path is returned.

  A function designator: function is called, and should return either
  an open [file] stream or a pathname designator, where the MIME part
  content should be written.  In both cases the MIME part is written
  to the file, and its pathname is returned."))

(defstruct (mime-part (:type list)
                      (:constructor make-mime-part (contents headers)))
  contents
  headers)


(defmethod parse-mime ((input string) separator &key (write-content-to-file t))
  (with-input-from-string (stream input)
    (parse-mime stream separator :write-content-to-file write-content-to-file)))

(defun parse-headers (input)
  (loop for header = (parse-header input)
        while header
        collect header))

(defmethod parse-mime ((input stream) boundary &key (write-content-to-file t))
  (check-type write-content-to-file
              (or (eql t) null symbol function))
  ;; Find the first boundary.  Return immediately if it is also the last
  ;; one.
  (unless (read-until-next-boundary input boundary (make-broadcast-stream))
    (return-from parse-mime nil))

  (let ((result ()))
    (loop
      (let* ((headers (parse-headers input))
             (content-disposition (find-header "Content-Disposition" headers))
             (disposition-parameters (header-parameters content-disposition))
             (field-name (cdr (find-parameter "name" disposition-parameters)))
             (file-name (cdr (find-parameter "filename"
                                             disposition-parameters)))
             ;; XXX Content type header parameters are not used.
             (content-type (let ((header (find-header "Content-Type" headers)))
                             (when header (header-value header))))
             more)
        (assert (string-equal "form-data" (header-value content-disposition)))
        (cond ((and file-name
                    (zerop (length file-name)))
               ;; File name is empty.  This usually means that the
               ;; file upload field is empty.  In this case we ignore
               ;; this field (e.g., it will not be included in the result).
               (setf more (read-until-next-boundary input boundary
                                                    (make-broadcast-stream))))

              ((and file-name
                    write-content-to-file)
               (let ((destination
                       (cond ((eq 't write-content-to-file)
                              ;; Old behaviour.
                              (ensure-directories-exist (make-tmp-file-name)))
                             ((or (symbolp write-content-to-file)
                                  (functionp write-content-to-file))
                              (funcall write-content-to-file
                                       :field-name field-name
                                       :file-name file-name
                                       :content-type content-type
                                       :allow-other-keys t)))))

                 (flet ((process-part (stream)
                          (setf more (read-until-next-boundary input
                                                               boundary
                                                               stream))
                          (push (make-mime-part (pathname stream) headers)
                                result)))
                   (if (and (streamp destination)
                            (open-stream-p destination))
                       (let ((abort t))
                         (unwind-protect
                              (prog1 (process-part destination)
                                (setf abort nil))
                           (close destination :abort abort)))
                       (with-open-file
                           (out-file destination
                                     :direction :output
                                     ;; external format for faithful I/O see
                                     ;; <http://cl-cookbook.sourceforge.net/io.html#faith>
                                     #+(or sbcl lispworks allegro openmcl ccl)
                                     :external-format
                                     #+sbcl :latin-1
                                     #+lispworks '(:latin-1 :eol-style :lf)
                                     #+allegro (excl:crlf-base-ef :latin1)
                                     #+(or openmcl ccl) '(:character-encoding :iso-8859-1
                                                          :line-termination :unix))
                         (process-part out-file))))))
              (t
               (let ((stream (make-string-output-stream)))
                 (setf more (read-until-next-boundary input boundary stream))
                 (push (make-mime-part (get-output-stream-string stream)
                                       headers)
                       result))))
        (unless more
          (return))))
    (nreverse result)))


(defun find-header (label headers)
  "Find header by label from set of headers."
  (find label headers :key #'rfc2388:header-name :test #'string-equal))


(defun find-parameter (name params)
  "Find header parameter by name from set of parameters."
  (assoc name params :test #'string-equal))


;;; XXX This is not quite right: content-type also has parameters, and
;;; if AS-STRING is true, only the type/subtype will be returned (and
;;; parameters dropped).
;;;
;;; XXX Remove this and following two functions altogether when
;;; Hunchentoot is updated.
(defun content-type (part &key as-string)
  "Returns the Content-Type header of mime-part PART."
  (let ((header (find-header "CONTENT-TYPE" (mime-part-headers part))))
    (if as-string
        (if header
            (header-value header)
            "")
        header)))


(defun find-content-disposition-header (headers)
  (find-if (lambda (header)
             (and (string-equal "CONTENT-DISPOSITION"
                                (rfc2388:header-name header))
                  (string-equal "FORM-DATA"
                                (rfc2388:header-value header))))
           headers))


(defun get-file-name (headers)
  (cdr (find-parameter "FILENAME"
                       (header-parameters (find-content-disposition-header headers)))))
