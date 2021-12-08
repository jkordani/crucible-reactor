;;;; crucible-reactor.lisp

(in-package #:crucible-reactor)

(defparameter *auth-token* nil)

(defparameter *base-url* nil)

(defun make-review (title project-key repository revision-list)
  (list
   `(:review-data
     (:project-key . ,project-key)
     (:name . ,title)
     (:STATE . "Draft")
     (:TYPE . "REVIEW")
     (:ALLOW-REVIEWERS-TO-JOIN . T))
   (list
    :changesets
    `(:repository . ,repository)
    (list
     :changeset-data
     (mapcar (lambda (rev) (list* :id rev))
             revision-list)))))

(defun login (username password base-url)
  (setf *base-url* base-url)
  (setf *auth-token*
        (request-endpoint "/rest-service-fecru/auth/login" :method :post
                                                           :params `(("userName" . ,username)
                                                                     ("password" . ,password)))))

(defun request-endpoint (endpoint &key (method :get) (params nil) (content nil))
  (if (null *base-url*)
      (error "Not Logged in")
      (multiple-value-bind (response return-code headers)
          (drakma:http-request
           (format nil "~a/~a"
                   ;; this is to allow for users forgetting or adding too many trailing and leading slashes
                   ;; and to ensure a trailing slash for the final path
                   (string-right-trim '(#\/) *base-url*)
                   (format nil "~a/" (string-trim '(#\/) endpoint)))
           :method method
           :force-binary t
           :accept "application/json"
           :content-type "application/json"
           :content content
           :parameters (append params
                               (if (not (null *auth-token*))
                                   (list (list* "FEAUTH" (rest (first *auth-token*))))
                                   nil))
           )

        (if (not (< 199 return-code 300))
            (error "Request failed ~a" return-code))

        (values (json:decode-json-from-string
                 (flexi-streams:octets-to-string response))
                return-code (cdr (assoc :content-type headers))))))

(defun get-users ()
  (request-endpoint "/rest-service-fecru/admin/users/"))

(defun create-review (title project-key repo-name revision-list)
  (let ((new-review
          (make-review title project-key repo-name revision-list)))
    (request-endpoint "/rest-service/reviews-v1"
                      :method :post
                      :content (json:encode-json-to-string new-review))))
