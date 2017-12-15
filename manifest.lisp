(in-package :manifest)

(defvar *manifest-server* nil)

(defparameter *possible-readme-types* '(nil "txt" "md" "TXT"))

(defparameter *inverting-readtable*
    (let ((rt (copy-readtable nil)))
      (setf (readtable-case rt) :invert)
      rt))

(defparameter *categories* '(:function :macro :generic-function :slot-accessor :variable :class :condition :constant))

(defun start (&key (port 8080))
  "Start the manifest server and return the URL to browse. By default
picks a random unused port or you can specify a port with the :port
keyword argument."
  (cond
    ((and *manifest-server* (not (toot::shutdown-p *manifest-server*)))
     (warn "Manifest server already running."))
    (*manifest-server*
     (start-acceptor *manifest-server*))
    (t
     (setf *manifest-server*
           (start-server :handler (make-handler) :port port :access-logger nil :message-logger nil))))
  (format nil "http://localhost:~d/" (port *manifest-server*)))

(defun stop (&optional (server *manifest-server*))
  "Stop the manifest server, defaulting to *manifest-server*."
  (stop-acceptor server))

(defun case-invert-name (name)
  "Invert case of names so we can use nice lowercase names in URLs in
a true Common Lisp while still working in Allegro's mlisp."
  (let ((*readtable* *inverting-readtable*)
        (*package* (find-package :keyword)))
    (symbol-name (read-from-string name))))

(defun make-handler (&optional (root-dir (asdf:system-relative-pathname :manifest nil)))
  (let ((static-files (make-instance 'static-file-handler :root root-dir)))
    (lambda (request)
      (let ((*default-pathname-defaults* root-dir))
        (or (manifest request) (handle-request static-files request))))))

(defun manifest (request)
  (cond
    ((string= (request-path request) "/") (index-page request))
    #+quicklisp((string= (request-path request) "/quicklisp") (quicklisp-page request))
    #+quicklisp((starts-with-subseq "/quicklisp/install/" (request-path request)) (quicklisp-install request))
    ((starts-with-subseq "/package/" (request-path request)) (package-page request))))


(defun split-at (delimiter string)
  "Split the supplied string in two based on the delimiter; returns
the two parts as values.  If no delimiter was found, returns the
string and an empty string."
  (let ((pos (search delimiter string)))
    (if (null pos)
        (values string "")
        (values (subseq string 0 pos)
                (subseq string (+ pos (length delimiter)))))))

(defun order (sequence predicate &rest args)
  "A nondestructive SORT."
  (apply #'sort (copy-seq sequence) predicate args))

(defun package-link (package)
  "Return an absolute link to the package's page."
  (etypecase package
    (package
     (package-link (package-name package)))
    (string
     (format nil "/package/~(~A~)" package))))

(defun package-page (request)
  (multiple-value-bind (prefix package-name)
      (split-at "/" (subseq (request-path request) 1))
    (assert (string= prefix "package"))

    (when-let (package (find-package (case-invert-name package-name)))
      (let ((some-docs-p nil))
        (with-response-body (s request)
          (with-yaclml-stream s
            (<:html
              (<:head
               (<:title (<:format "Package: ~A" (package-name package)))
               (<:stylesheet "/manifest.css"))
              (<:body
               (<:h5 (<:a :href "/" "MANIFEST"))

               (let ((prev-package (package-name (package-before package))))
                 (<:a :class "prev-package"
                      :href (package-link prev-package)
                      (<:ai "&larr; ") (<:ah prev-package)))

               (let ((next-package (package-name (package-after package))))
                 (<:a :class "next-package"
                      :href (package-link next-package)
                      (<:ah next-package) (<:ai " &rarr;")))

               (<:h1 (<:ah (package-name package)))

               (when (package-nicknames package)
                 (<:p :class "nicknames"
                      (<:format "Nicknames: ~{~a~^, ~}" (package-nicknames package))))

               (when (documentation package t)
                 (<:p :class "package-desc"
                      (<:ah (documentation package t))))

               (let ((readme (readme-text package-name)))
                 (when readme
                   (setf some-docs-p t)
                   (<:pre (<:ai readme))))

               (loop for what in *categories*
                     for names = (names package what)
                     when names
                     do (setf some-docs-p t)
                        (<:h2 (<:format "~:(~a~)" (pluralization what)))
                        (<:table
                         (dolist (sym names)
                           (<:tr :class (format nil "~:[not-documented~;~]" (docs-for sym what))
                                 (<:td :class "symbol" (<:ah (princ-to-string sym)))
                                 (<:td :class "docs" (<:ah (or (docs-for sym what) "NO DOCS!")))))))

               (let ((used-by (order (package-used-by-list package) #'string< :key #'package-name)))
                 (when used-by
                   (<:h2 "Used by:")
                   (<:ul :class "packages"
                    (loop for p in used-by
                          do (<:li (<:a :href (package-link p) (<:ah (package-name p))))))))

               (let ((uses (order (package-use-list package) #'string< :key #'package-name)))
                 (when uses
                   (<:h2 "Uses:")
                   (<:ul :class "packages"
                         (loop for p in uses
                               do (<:li (<:a :href (package-link p) (<:ah (package-name p))))))))


                (unless some-docs-p
                  (<:p "Uh oh! No docs at all."))))))))))

(defun index-page (request)
  (with-response-body (s request)
    (with-yaclml-stream s
      (<:html
        (<:head
         (<:title "MANIFEST: all packages")
         (<:stylesheet "/manifest.css"))
        (<:body
         (<:h1 "MANIFEST")
         (<:h2 "All Packages")
         (<:ul :class "packages"
               (loop for pkg in (sort (mapcar #'package-name (public-packages)) #'string<)
                     do (<:li (<:a :class "package"
                                   :href (package-link pkg)
                                   (<:ah pkg))))))))))

(defun package-before (package)
  (let* ((all (sort (mapcar #'package-name (public-packages)) #'string>))
         (mem (member (package-name package) all :test #'equal)))
    (or (cadr mem) (car all))))

(defun package-after (package)
  (let* ((all (sort (mapcar #'package-name (public-packages)) #'string<))
         (mem (member (package-name package) all :test #'equal)))
    (or (cadr mem) (car all))))

(defun public-packages ()
  (loop for p in (list-all-packages)
     when (and (has-exported-symbols-p p) (not (eql p (find-package :keyword))))
     collect p))

(defun has-exported-symbols-p (package)
  (do-external-symbols (sym package)
    (declare (ignore sym))
    (return-from has-exported-symbols-p t))
  nil)

(defun needs-documentation (package)
  (loop for what in *categories*
     for names = (names package what)
     when names nconc
       (loop for sym in names unless (docs-for sym what) collect (list sym what))))

(defun documentation-templates (package)
  (loop for (name what) in (needs-documentation package)
       collect
       (ecase what
         (:slot-accessor
          `(setf (documentation #',name t) "WRITE ME!"))
         (:class
          `(setf (documentation (find-class ',name) t) "WRITE ME!")))))

(defun readme-text (package-name)
  (when-let ((file (find-readme package-name)))
    (with-open-file (in file :if-does-not-exist nil)
      (when in
        (with-output-to-string (s)
          (loop for line = (read-line in nil nil)
             while line do (write-line line s)))))))

(defun find-readme (package-name)
  (let ((dir (ignore-errors (asdf:system-relative-pathname package-name nil))))
    (when dir
      (loop for type in *possible-readme-types*
         when (probe-file (merge-pathnames (make-pathname :name "README" :type type) dir))
         return it))))

(defun names (package what)
  (sort
   (loop for sym being the external-symbols of package
      when (is sym what) collect sym
      when (is `(setf ,sym) what) collect `(setf ,sym))
   #'name<))

(defun name< (n1 n2)
  (cond
    ((and (symbolp n1) (symbolp n2))
     (string< n1 n2))
    ((and (symbolp n1) (listp n2))
     (cond
       ((string< n1 (second n2)) t)
       ((string< (second n2) n1) nil)
       (t t)))
    ((and (listp n1) (symbolp n2))
     (cond
       ((string< (second n1) n2) t)
       ((string< n2 (second n1)) nil)
       (t nil)))
    ((and (listp n1) (listp n2))
     (string< (second n1) (second n2)))))


(defgeneric is (symbol what))
(defgeneric docs-for (symbol what))
(defgeneric pluralization (what))

(defmethod pluralization (what) (format nil "~as" what))

(defmacro define-category (name (symbol what) &body body)
  (let ((is-test (cdr (assoc :is body)))
        (get-docs (cdr (assoc :docs body)))
        (pluralization (cdr (assoc :pluralization body))))
    `(progn
       (defmethod is (,symbol (,what (eql ',name))) ,@is-test)
       (defmethod docs-for (,symbol (,what (eql ',name))) ,@get-docs)
       ,@(when pluralization
               `((defmethod pluralization ((,what (eql ',name)))
                   ,@pluralization))))))

(defun function-p (name)
  (ignore-errors (fdefinition name)))

(defun macro-p (name)
  (and (symbolp name) (macro-function name)))

(defun generic-function-p (name)
  (and (function-p name)
       (typep (fdefinition name) 'generic-function)))

(defun variable-p (name)
  (ignore-errors (boundp name)))

(defun automatic-p (docstring)
  (member docstring '("automatically generated reader method" "automatically generated writer method") :test #'string-equal))

(defun gf-docs (name)
  (let ((simple (documentation (fdefinition name) t))
        (from-setf (and (consp name) (documentation (fdefinition (second name)) t))))

    (or
     (and simple (not (automatic-p simple)) (format nil "The ~a" simple))
     (and from-setf (not (automatic-p from-setf)) (format nil "Set the ~a" from-setf))
     (first (remove-if #'automatic-p (remove nil (mapcar
                         (lambda (m) (documentation m t))
                         (generic-function-methods (fdefinition name)))))))))


(define-category :function (symbol what)
  (:is (and (function-p symbol)
            (not (or (is symbol :macro)
                     (is symbol :generic-function)
                     (is symbol :slot-accessor)))))
  (:docs (documentation symbol 'function)))

(define-category :macro (symbol what)
  (:is (macro-p symbol))
  (:docs (documentation symbol 'function)))

(define-category :generic-function (symbol what)
  (:is (and (generic-function-p symbol)
            (not (is symbol :slot-accessor))))
  (:docs (documentation symbol 'function)))

(define-category :class (symbol what)
  (:is (and (find-class symbol nil) (not (is symbol :condition))))
  (:docs (documentation (find-class symbol) t))
  (:pluralization (format nil "~aes" what)))

(define-category :condition (symbol what)
  (:is (and (find-class symbol nil) (subtypep (find-class symbol nil) 'condition)))
  (:docs (documentation (find-class symbol) t)))

(define-category :variable (symbol what)
  (:is (and (variable-p symbol) (not (is symbol :constant))))
  (:docs   (documentation symbol 'variable)))

(define-category :constant (symbol what)
  (:is (and (variable-p symbol) (constantp symbol)))
  (:docs (documentation symbol 'variable)))

(define-category :slot-accessor (symbol what)
  (:is (and (generic-function-p symbol)
            (some (lambda (m)
                    (or (eql (class-of m) (find-class 'standard-reader-method))
                        (eql (class-of m) (find-class 'standard-writer-method))))
                  (generic-function-methods (fdefinition symbol)))))
  (:docs (gf-docs symbol)))
