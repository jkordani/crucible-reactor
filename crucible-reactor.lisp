;;;; crucible-reactor.lisp

(in-package #:crucible-reactor)

(defparameter *auth-token* nil)

(defparameter *base-url* nil)

(defun login (username password base-url)
  (setf *base-url* base-url)
  (setf *auth-token*
        (request-endpoint "/rest-service-fecru/auth/login" :method :post
                                                           :params `(("userName" . ,username)
                                                                     ("password" . ,password)))))

(defun request-endpoint (endpoint &key (method :get) (params nil))
  (if (null *base-url*)
      (error "Not Logged in")
      (multiple-value-bind (response return-code headers)
          (drakma:http-request
           (format nil "~a/~a"
                   (string-right-trim '(#\/) *base-url*)
                   (string-left-trim '(#\/) endpoint))
           :method method
           :force-binary t
           :content-type "application/json"
           :parameters (append params
                               (if (not (null *auth-token*))
                                   (list (list* "FEAUTH" (rest (first *auth-token*))))
                                   nil)))

        (if (/= return-code 200)
            (error "Request failed ~a" return-code))

        (values (json:decode-json-from-string
                 (flexi-streams:octets-to-string response))
                return-code (cdr (assoc :content-type headers))))))

(defun get-users ()
  (request-endpoint "/rest-service-fecru/admin/users/"))
