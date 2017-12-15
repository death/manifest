(in-package :cl-user)

(defpackage :manifest
  (:use :closer-common-lisp
        :toot
        :com.gigamonkeys.utilities)
  (:import-from :yaclml :with-yaclml-stream)
  (:export :start
           :stop))
