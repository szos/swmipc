;;;; simple-ipc-server.lisp

(in-package #:simple-ipc-server)

(defmacro with-gensyms ((&rest syms) &body body)
  `(let ,(mapcar (lambda (s) (list s `(gensym ,(symbol-name s)))) syms)
     ,@body))

(defun remove-keyargs (list &key remove-only)
  (flet ((remkey-if (key)
           (if remove-only
               (member key remove-only)
               (keywordp key))))
    (let ((ac nil)
          (ac-tail nil)
          (keys-list))
      (do ((r list (cdr r)))
          ((null r) (values ac keys-list))
        (cond ((remkey-if (car r))
               (push (cadr r) keys-list)
               (push (car r) keys-list)
               (setf r (cdr r)))
              ((null ac)
               (let ((cell (list (car r))))
                 (setf ac cell
                       ac-tail cell)))
              (t (setf (cdr ac-tail) (list (car r))
                       ac-tail (cdr ac-tail))))))))

(defmacro with-keyargs ((nonkey-var &key bind (remove t)) list &body body)
  (with-gensyms (keys)
    `(multiple-value-bind (,nonkey-var ,keys)
         (remove-keyargs ,list
                         :remove-only ,(cond ((consp remove)
                                              remove)
                                             (remove
                                              nil)
                                             (t (mapcar
                                                 (lambda (el)
                                                   (intern
                                                    (symbol-name 
                                                     (cond ((atom el)
                                                            el)
                                                           ((consp el)
                                                            (car el))))
                                                    (find-package :keyword)))
                                                 bind))))
       ,@(cond (bind
                `((destructuring-bind (&key ,@bind &allow-other-keys)
                      ,keys
                    ,@body)))
               ((and (consp (car body))
                     (eql (caar body) 'declare))
                `((declare (ignore ,keys)
                           ,@(cdar body))
                  ,@(cdr body)))
               (t `((declare (ignore ,keys))
                    ,@body))))))

(defvar *allow-evaluation* nil)
(defvar *read-in-package* (find-package :simple-ipc-server))

(defun read-and-dispatch-request (stream)
  (handler-case 
      (loop
        (let ((res (read stream)))
          (format t "handling request: ~S~&" res)
          (finish-output)
          (unwind-protect 
               (handler-case (apply 'handle-request (cons stream res))
                 (error (c)
                   (format stream "[ERROR] ~S~&" c)))
            (format stream "~C" (code-char 4)))
          (ignore-errors (finish-output stream))))
    (end-of-file ()
      (warn "Improperly terminated connection, use :end-connection ipc request"))))

(defun read-from-socket (sock)
  (let ((stream (sb-bsd-sockets:socket-make-stream
                 (sb-bsd-sockets:socket-accept sock)
                 :input t
                 :output t
                 :buffering :full))
        (*read-eval* nil)
        (*connection-cleanup-actions* '())
        (*package* *read-in-package*)
        (*end-connection* (lambda ()
                            (return-from read-from-socket nil))))
    (declare (special *end-connection*
                      *connection-cleanup-actions*))
    (unwind-protect (read-and-dispatch-request stream)
      (cleanup-after-connection)
      (format stream "~C" #\ETB)
      (ignore-errors (finish-output stream))
      (format t "Ending connection~&")
      (finish-output))))

(defun start-server (&key (socket "/tmp/simple-ipc.socket")
                          thread-name)
  (declare (ignorable thread-name))
  (sb-thread:make-thread
   (lambda ()
     (block start-server-block
       (let ((sock (make-instance 'sb-bsd-sockets:local-socket :type :stream))
             (*kill-server* (lambda ()
                              (format t "Killing simple ipc server on ~A" socket)
                              (force-output)
                              (return-from start-server-block nil))))
         (declare (special *kill-server*))
         (sb-bsd-sockets:socket-bind sock socket)
         (sb-bsd-sockets:socket-listen sock 20)
         (unwind-protect (loop (read-from-socket sock))
           (sb-posix:unlink socket)
           (sb-bsd-sockets:socket-close sock)))))
   :name (or thread-name
             (format nil "Simple IPC Server at ~A" socket))))

(defun cleanup-after-connection ()
  (declare (special *connection-cleanup-actions*))
  (loop for cleanup-action in *connection-cleanup-actions*
        do (funcall cleanup-action)))

(defun add-connection-cleanup-function (thunk)
  (declare (special *connection-cleanup-actions*))
  (push thunk *connection-cleanup-actions*))

(defmacro add-connection-cleanup (&body body)
  `(add-connection-cleanup-function (lambda () ,@body)))

(defun call-with-connection-cleanup (thunk cleanup-thunk)
  (add-connection-cleanup-function cleanup-thunk)
  (funcall thunk))

(defmacro with-connection-cleanup (form &body cleanup-forms)
  (with-gensyms (fn cleanup)
    `(flet ((,fn () ,form)
            (,cleanup () ,@cleanup-forms))
       (declare (dynamic-extent #',fn))
       (call-with-connection-cleanup #',fn #',cleanup))))

(defgeneric handle-request (stream request &rest rest))

(defmethod handle-request :around (s r &rest rest)
  (declare (ignore s r rest))
  (let* ((*swm-screen* (stumpwm::current-screen))
         (msgs (stumpwm::screen-last-msg *swm-screen*))
         (hlts (stumpwm::screen-last-msg-highlights *swm-screen*)))
    (declare (special *swm-screen*))
    (unwind-protect
         (progn (setf (stumpwm::screen-last-msg *swm-screen*) nil
                      (stumpwm::screen-last-msg-highlights *swm-screen*) nil)
                (call-next-method))
      (setf (stumpwm::screen-last-msg *swm-screen*) msgs
            (stumpwm::screen-last-msg-highlights *swm-screen*) hlts))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun defrequest-verbose (type stream-variable-name argslist body
                             &key stream-variable-ignorable-p)
      
    (with-gensyms (rest request)
      `(defmethod handle-request
           (,stream-variable-name (,request (eql ,type)) &rest ,rest)
         (declare (ignore ,request)
                  ,@(when stream-variable-ignorable-p
                      `((ignorable ,stream-variable-name))))
         (apply (lambda ,argslist ,@body) ,rest)))))

(defmacro defrequest (type (&rest argslist) &body body)
  (destructuring-bind (request-type
                       &key (stream-name 'stream stream-name-provided-p))
      (if (consp type)
          type
          (list type))
    (defrequest-verbose request-type stream-name argslist body
      :stream-variable-ignorable-p stream-name-provided-p)))
