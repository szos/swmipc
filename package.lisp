;;;; package.lisp

(defpackage #:simple-ipc-server
  (:use #:cl)
  (:export #:start-server
           #:*allow-evaluation*
           #:*read-in-package*
           #:defrequest))
