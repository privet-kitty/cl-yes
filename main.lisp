(in-package :cl-user)
(defpackage :cl-yes
  (:use :cl)
  (:export :yes))
(in-package :cl-yes)

(defun make-buffer (s &optional (times 10000))
  (with-output-to-string (out)
    (dotimes (n times)
      (write-line s out))))

(defun coerce* (object type)
  "Returns NIL if COERCE failed."
  (handler-bind ((type-error (lambda (c)
                               (declare (ignore c))
                               (return-from coerce* nil))))
    (coerce object type)))

(defmacro with-dispatching-on-string (var &body body)
  (let ((maybe-coerced (gensym)))
    `(uiop:if-let ((,maybe-coerced (coerce* ,var 'simple-base-string)))
       (let ((,var ,maybe-coerced))
         (declare (simple-base-string ,var))
         ,@body)
       (progn ,@body))))

(defun yes (out &optional (s "y"))
  (let ((buf (make-buffer s)))
    (with-dispatching-on-string buf
      (loop
        (princ buf out)))))
