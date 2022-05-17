(in-package :simple-ipc-server)

(defrequest :end-connection ()
  (declare (special *end-connection*))
  (funcall *end-connection*))

(defrequest :kill-server ()
  (declare (special *kill-server*))
  (format stream "Killing server")
  (funcall *kill-server*))

(defrequest :change-package (package &key reset)
  (let ((pkg (find-package package)))
    (if pkg
        (progn (setf *package* pkg)
               (unless reset
                 (setf *read-in-package* pkg)))
        (format stream "^[^1[ERROR]^] Package ~A not found~%" package))))

(defrequest :bind-special-variables (bindings &key eval-values)
  (with-gensyms (unbound-canary)
    (loop for cell in bindings
          for var = (car cell)
          for val = (cadr cell)
          for oldval = (handler-case (symbol-value var)
                         (unbound-variable (c)
                           (format stream "^[^1[ERROR]^] Cannot establish bindings; unbound variable ~S~&"
                                   (cell-error-name c))
                           unbound-canary))
          unless (eql oldval unbound-canary)
            collect oldval into old-values
            and collect cell into to-bind
          finally (with-connection-cleanup
                      (loop for (var val) in to-bind
                            do (setf (symbol-value var)
                                     (cond ((and eval-values *allow-evaluation*)
                                            (eval val))
                                           (eval-values
                                            (format stream "Evaluation is not allowed on this connection~&")
                                            val)
                                           (t val))))
                    (mapc (lambda (el old-val)
                            (setf (symbol-value (car el)) old-val))
                          to-bind
                          old-values)))))

(defun write-back-stumpwm-messages (stream screen)
  (let ((revs (nreverse (stumpwm::screen-last-msg screen))))
    (format stream "~{~{~A~%~}~^~%~}" revs)
    (setf (stumpwm::screen-last-msg screen) nil
          (stumpwm::screen-last-msg-highlights screen) nil)
    (finish-output stream)))

(defrequest (:swm-command :stream-name stream) (&rest rest)
  (declare (special *swm-screen*))
  (format t "~A ~S~&" (type-of stream) stream)
  (with-keyargs (commands :bind (interactivep write-back-immediately)) rest
    (mapc (lambda (c)
            (unless (string= c "")
              (stumpwm::eval-command c interactivep)
              (when write-back-immediately
                (write-back-stumpwm-messages stream *swm-screen*))))
          commands)
    (unless write-back-immediately
      (write-back-stumpwm-messages stream *swm-screen*))))

(defrequest :evaluate-form (form &key writeback)
  (if *allow-evaluation*
      (multiple-value-bind (res str)
          (handler-case (let ((*standard-output* (make-string-output-stream)))
                          (values (eval form)
                                  (get-output-stream-string *standard-output*)))
            (error (c)
              (format stream "^[^1[ERROR]^] ~A~&" (type-of c))
              (princ c stream)
              (values c nil)))
        (when writeback
          (cond ((typep res 'condition)
                 (format stream "~&Evaluation Aborted~&"))
                ((string= str "")
                 (format stream "~S~&" res))
                (t (format stream "~A~&~S~&" str res)))))
      (format stream "^[^1[ERROR]^] Evaluation is not allowed on this connection~&")))

(defrequest :information-request (information-code &rest rest
                                  &key (delimiter (code-char 3)))
  (with-keyargs (other-keys :remove (list :delimiter))
      rest
    (apply 'send-information stream information-code delimiter other-keys)))

(defgeneric send-information (stream code delimiter &key &allow-other-keys))

(defmethod send-information (stream (code (eql :swm-command-list)) delimiter
                             &key &allow-other-keys)
  (let ((commands (stumpwm::all-commands nil)))
    (format stream (format nil "~~{~~A~~^~A~~}" delimiter)
            commands)))
