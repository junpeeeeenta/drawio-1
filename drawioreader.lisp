;;;
;;;
(defpackage :drawio
  (:nicknames :dio)
  (:use :common-lisp :xmlreader)
  (:shadow #:stream #:with-open-file #:read-line #:read #:Array #:make-Array)
  (:export #:stream #:with-open-file #:read-line #:read
           #:read-xml-prolog #:read-mxfile #:query-path
           #:mxfile #:diagram #:mxGraphModel #:root
           #:starts-with-subseq #:split-string
           #:mxCell-id #:mxCell-style #:mxCell-value #:mxCell-source #:mxCell-target #:mxCell-geometry
           #:mxCell-parent
           #:mxGeometry #:mxGeometry-points #:mxGeometry-as #:mxGeometry-x #:mxGeometry-y #:mxGeometry-width #:mxGeometry-height
           #:mxPoint #:mxPoint-as #:mxPoint-x #:mxPoint-y))

(in-package :dio)

(defstruct drawio xml-prolog mxfile diagram mxGraphModel root-body)
(defstruct mxfile host modified agent etag compressed version type diagram)
(defstruct diagram id name mxGraphModel)
(defstruct mxGraphModel dx dy grid gridSize guides tooltips connect arrows fold page pageScale pageWidth pageHeight math shadow root)
(defstruct mxCell id value style align verticalAlign resizable points edge parent connectable vertex source target geometry)
(defstruct mxGeometry x y width height relative as points)
(defstruct mxPoint x y as)
(defstruct Array as points)
(defstruct mxRectangle x y width height as)
#|
:cd
:cd ./common-lisp/drawio/
(line:with-open-file (stream "PD3例題標準プロセスモデル_プラントエンジニア_説明用.xml" :external-format :utf-8)
  (loop for line = (line:read-line stream nil nil)
      while line
      do (format t "~2D;~3D: ~A~%" (line:line-count stream) (line:line-position stream) line )))
                             
:cd ./allegro-projects/drawio/
(line:with-open-file (stream "FD3-goto-test2.drawio" :external-format :utf-8)
  (loop for line = (line:read-line stream nil nil)
      while line
      do (format t "~2D;~3D: ~A~%" (line:line-count stream) (line:line-position stream) line )))
|#
(defun read-xml-prolog (stream)
  (line:skipbl stream)
  ;; at the first line in stream
  (let ((pos (file-position stream)))
    (cond ((string= "<?xml" (line:next-token stream))
           (line:skipbl stream)
           (xmlreader:read-XMLDecl stream))
          (t (file-position stream pos)
             (values nil nil nil)))))

(defun read-mxPoint (stream)
  (line:skipbl stream)
  (let* ((pos (file-position stream))
         (token (line:next-token stream)))
;;;    (format t "~%Next Token ~S in line ~S" token (line:expose-one-line-buf stream))
    (cond ((string= "<mxPoint" token)
           (let ((mxPoint-obj (make-mxPoint)))
             (loop while (progn (line:skipbl stream) (not (char= #\/ (peek-char nil stream  nil nil)))) do
                   (ecase (intern (line:next-token stream) :keyword)
                     (:|x| (read-Eq stream)
                         (setf (mxPoint-x mxPoint-obj) (line:read-string stream)))
                     (:|y| (read-Eq stream)
                         (setf (mxPoint-y mxPoint-obj) (line:read-string stream)))
                     (:|as| (read-Eq stream)
                          (setf (mxPoint-as mxPoint-obj) (line:read-string stream)))))
             (read-char stream)   ; discard '/'
             (assert (char= #\> (read-char stream)))
             mxPoint-obj))
          ((string= "<Array" token)
           (let ((Array-obj (make-Array)))
             (loop while (progn (line:skipbl stream) (and (not (char= #\> (peek-char nil stream  nil nil)))
                                                          (not (char= #\/ (peek-char nil stream  nil nil))))) do
                   (ecase (intern (line:next-token stream) :keyword)
                     (:|as| (read-Eq stream)
                          (setf (Array-as Array-obj) (line:read-string stream)))))
             (cond ((char= #\> (read-char stream))   ; discard '>' or '/'
                    (setf (Array-points Array-obj) (read-geometry-points stream))
                    ;; check etag of Array
                    (line:skipbl stream)
                    (assert (string= "</Array>" (line:read-line stream))))
                   (t (assert (char= #\> (read-char stream))))) ; discard '>'
             Array-obj))
          ((string= "<mxRectangle" token)
           (let ((mxRectangle-obj (make-mxRectangle)))
             (loop while (progn (line:skipbl stream) (not (char= #\/ (peek-char nil stream  nil nil)))) do
                   (ecase (intern (line:next-token stream) :keyword)
                     (:|x| (read-Eq stream)
                         (setf (mxRectangle-x mxRectangle-obj) (line:read-string stream)))
                     (:|y| (read-Eq stream)
                         (setf (mxRectangle-y mxRectangle-obj) (line:read-string stream)))
                     (:|width| (read-Eq stream)
                             (setf (mxRectangle-width mxRectangle-obj) (line:read-string stream)))
                     (:|height| (read-Eq stream)
                              (setf (mxRectangle-height mxRectangle-obj) (line:read-string stream)))
                     (:|as| (read-Eq stream)
                          (setf (mxRectangle-as mxRectangle-obj) (line:read-string stream)))))
             (read-char stream)   ; discard '/'
             (assert (char= #\> (read-char stream)))
             mxRectangle-obj))
          (t (file-position stream pos)
             nil))))

(defun read-geometry-points (stream)
  (loop for point = (read-mxPoint stream)
      while point
      collect point))

(defun read-mxGeometry (stream)
  (line:skipbl stream)
  (let ((pos (file-position stream)))
    (cond ((string= "<mxGeometry" (line:next-token stream))
           (let ((mxGeometry-obj (make-mxGeometry)))
             (loop while (progn (line:skipbl stream) (and (not (char= #\> (peek-char nil stream  nil nil)))
                                                          (not (char= #\/ (peek-char nil stream  nil nil))))) 
                 do
                   (ecase (intern (line:next-token stream) :keyword)
                     (:|x| (read-Eq stream)
                         (setf (mxGeometry-x mxGeometry-obj) (line:read-string stream)))
                     (:|y| (read-Eq stream)
                         (setf (mxGeometry-y mxGeometry-obj) (line:read-string stream)))
                     (:|width| (read-Eq stream)
                             (setf (mxGeometry-width mxGeometry-obj) (line:read-string stream)))
                     (:|height| (read-Eq stream)
                              (setf (mxGeometry-height mxGeometry-obj) (line:read-string stream)))
                     (:|relative| (read-Eq stream)
                                (setf (mxGeometry-relative mxGeometry-obj) (line:read-string stream)))
                     (:|as| (read-Eq stream)
                          (setf (mxGeometry-as mxGeometry-obj) (line:read-string stream)))))
             (let ((ch (read-char stream nil nil)))
               (cond ((char= ch #\>)
                      (setf (mxGeometry-points mxGeometry-obj)
                        (read-geometry-points stream))
                      ;; check etag of mxGeometry
                      (line:skipbl stream)
                      (assert (string= "</mxGeometry>" (line:read-line stream)))
                      mxGeometry-obj)
                     ((char= ch #\/)
                      (assert (char= #\> (read-char stream nil nil)))
                      mxGeometry-obj)))))
          (t (file-position stream pos)))))

(defun read-mxCell (stream)
  (assert (string= "<mxCell" (line:next-token stream)))
  (let ((mxCell-obj (make-mxCell)))
    (loop while (progn (line:skipbl stream) (and (not (char= #\> (peek-char nil stream  nil nil)))
                                                 (not (char= #\/ (peek-char nil stream  nil nil)))))
        do
          (ecase (intern (line:next-token stream) :keyword)
            (:|id| (read-Eq stream)
                 (setf (mxCell-id mxCell-obj) (line:read-string stream)))
            (:|value| (read-Eq stream)
                    (setf (mxCell-value mxCell-obj) (line:read-string stream)))
            (:|style| (read-Eq stream)
                    (setf (mxCell-style mxCell-obj) (line:read-string stream)))
            (:|align| (read-Eq stream)
                    (setf (mxCell-align mxCell-obj) (line:read-string stream)))
            (:|verticalAlign| (read-Eq stream)
                            (setf (mxCell-verticalAlign mxCell-obj) (line:read-string stream)))
            (:|resizable| (read-Eq stream)
                        (setf (mxCell-resizable mxCell-obj) (line:read-string stream)))
            (:|points| (read-Eq stream)
                     (setf (mxCell-points mxCell-obj) (line:read-string stream)))
            (:|edge| (read-Eq stream)
                   (setf (mxCell-edge mxCell-obj) (line:read-string stream)))
            (:|parent| (read-Eq stream)
                     (setf (mxCell-parent mxCell-obj) (line:read-string stream)))
            (:|connectable| (read-Eq stream)
                         (setf (mxCell-connectable mxCell-obj) (line:read-string stream)))
            (:|vertex| (read-Eq stream)
                     (setf (mxCell-vertex mxCell-obj) (line:read-string stream)))
            (:|source| (read-Eq stream)
                     (setf (mxCell-source mxCell-obj) (line:read-string stream)))
            (:|target| (read-Eq stream)
                     (setf (mxCell-target mxCell-obj) (line:read-string stream)))))
    (let ((ch (read-char stream  nil nil)))
      (cond ((char= ch #\/ )
             (assert (char= #\> (read-char stream  nil nil)))
             mxCell-obj)
            ((char= #\> ch)
             (line:skipbl stream)
             ;; read one geometry
             (setf (mxCell-geometry mxCell-obj)
               (read-mxGeometry stream))
             ;; check etag of mxCell
             (line:skipbl stream)
             (assert (string= "</mxCell>" (line:read-line stream)))
             mxCell-obj)
            ((error "Cant happen"))))))
#|
:cd
:cd ../../allegro-projects/drawio/
(in-package :dio)
(line:with-open-file (stream "PD3例題標準プロセスモデル_プラントエンジニア_説明用.xml" :external-format :utf-8)
  (read-xml-prolog stream)
  (read-mxfile stream)
  )
(line:with-open-file (stream "FD3-goto-test2.drawio" :external-format :utf-8)
  (read-xml-prolog stream)
  (read-mxfile stream)
  )
(line:with-open-file (stream "test10.drawio" :external-format :utf-8)
  (read-xml-prolog stream)
  (read-mxfile stream)
  )
(line:with-open-file (stream "x-rayCT2.drawio" :external-format :utf-8)
  (read-xml-prolog stream)
  (read-mxfile stream)
  )
|#
(defun read-root (stream)
  (assert (string= "<root>" (progn (line:skipbl stream)(line:read-line stream))))
  (let (pos)
    (loop for line = (progn
                       (line:skipbl stream)
                       (setq pos (file-position stream))
                       (line:read-line stream))
        until (string= line "</root>")
        do (file-position stream pos)
        collect (read-mxCell stream))))

(defun read-mxGraphModel (stream)
  (line:skipbl stream)
  (let ((pos (file-position stream))
        (line (line:next-token stream)))
    (cond ((string= "<mxGraphModel" line)
           (let ((mxGraphModel-obj (make-mxGraphModel)))
             (loop while (progn (line:skipbl stream) (not (char= #\> (peek-char nil stream  nil nil)))) do
                   (ecase (intern (line:next-token stream) :keyword)
                     (:|dx| (read-Eq stream)
                          (setf (mxGraphModel-dx mxGraphModel-obj) (line:read-string stream)))
                     (:|dy| (read-Eq stream)
                          (setf (mxGraphModel-dy mxGraphModel-obj) (line:read-string stream)))
                     (:|grid| (read-Eq stream)
                            (setf (mxGraphModel-grid mxGraphModel-obj) (line:read-string stream)))
                     (:|gridSize| (read-Eq stream)
                                (setf (mxGraphModel-gridSize mxGraphModel-obj) (line:read-string stream)))
                     (:|guides| (read-Eq stream)
                              (setf (mxGraphModel-guides mxGraphModel-obj) (line:read-string stream)))
                     (:|tooltips| (read-Eq stream)
                                (setf (mxGraphModel-tooltips mxGraphModel-obj) (line:read-string stream)))
                     (:|connect| (read-Eq stream)
                               (setf (mxGraphModel-connect mxGraphModel-obj) (line:read-string stream)))
                     (:|arrows| (read-Eq stream)
                              (setf (mxGraphModel-arrows mxGraphModel-obj) (line:read-string stream)))
                     (:|fold| (read-Eq stream)
                            (setf (mxGraphModel-fold mxGraphModel-obj) (line:read-string stream)))
                     (:|page| (read-Eq stream)
                            (setf (mxGraphModel-page mxGraphModel-obj) (line:read-string stream)))
                     (:|pageScale| (read-Eq stream)
                                 (setf (mxGraphModel-pageScale mxGraphModel-obj) (line:read-string stream)))
                     (:|pageWidth| (read-Eq stream)
                                 (setf (mxGraphModel-pageWidth mxGraphModel-obj) (line:read-string stream)))
                     (:|pageHeight| (read-Eq stream)
                                  (setf (mxGraphModel-pageHeight mxGraphModel-obj) (line:read-string stream)))
                     (:|math| (read-Eq stream)
                            (setf (mxGraphModel-math mxGraphModel-obj) (line:read-string stream)))
                     (:|shadow| (read-Eq stream)
                              (setf (mxGraphModel-shadow mxGraphModel-obj) (line:read-string stream)))))
             (read-char stream)   ; discard '>'
             (setf (mxGraphModel-root mxGraphModel-obj) (read-root stream))
             mxGraphModel-obj))
          (t (with-input-from-string (s line)
               (let ((deflate (xmlreader:read-upto-delimiter-string "</" s)))
                 (print deflate)))
             (file-position stream pos)
             )
          )))

(defun read-diagram (stream)
  (line:skipbl stream)
  (let ((pos (file-position stream)))
    (cond ((string= "<diagram" (line:next-token stream))
           (let ((diagram-obj (make-diagram)))
             (loop while (progn (line:skipbl stream) (not (char= #\> (peek-char nil stream  nil nil)))) do
                   (ecase (intern (line:next-token stream) :keyword)
                     (:|id| (read-Eq stream)
                          (setf (diagram-id diagram-obj) (line:read-string stream)))
                     (:|name| (read-Eq stream)
                            (setf (diagram-name diagram-obj) (line:read-string stream)))))
             (read-char stream)   ; discard '>'
             (setf (diagram-mxGraphModel diagram-obj) (read-mxGraphModel stream))
             diagram-obj))
          (t (file-position stream pos)))))

(defun read-mxfile (stream)
  (line:skipbl stream)
  (let ((pos (file-position stream)))
    (cond ((string= "<mxfile" (line:next-token stream))
           (let ((mxfile-obj (make-mxfile)))
             (loop while (progn (line:skipbl stream) (not (char= #\> (peek-char nil stream  nil nil)))) do
                   (ecase (intern (line:next-token stream) :keyword)
                     (:|host| (read-Eq stream)
                            (setf (mxfile-host mxfile-obj) (line:read-string stream)))
                     (:|modified| (read-Eq stream)
                                (setf (mxfile-modified mxfile-obj) (line:read-string stream)))
                     (:|agent| (read-Eq stream)
                             (setf (mxfile-agent mxfile-obj) (line:read-string stream)))
                     (:|etag| (read-Eq stream)
                            (setf (mxfile-etag mxfile-obj) (line:read-string stream)))
                     (:|compressed| (read-Eq stream)
                                  (setf (mxfile-compressed mxfile-obj) (line:read-string stream)))
                     (:|version| (read-Eq stream)
                               (setf (mxfile-version mxfile-obj) (line:read-string stream)))
                     (:|type| (read-Eq stream)
                            (setf (mxfile-type mxfile-obj) (line:read-string stream)))))
             (read-char stream)   ; discard '>'
             (setf (mxfile-diagram mxfile-obj) (read-diagram stream))
             mxfile-obj))
          (t (file-position stream pos)))))
#|
:cd
:cd ./common-lisp/drawio/
(in-package :dio)
(line:with-open-file (stream "PD3例題標準プロセスモデル_プラントエンジニア_説明用.xml" :external-format :utf-8)
  (read-xml-prolog stream)
  (read-mxfile stream)
  )
(line:with-open-file (stream "FD3-goto-test2.drawio" :external-format :utf-8)
  (read-xml-prolog stream)
  (read-mxfile stream)
  )
(line:with-open-file (stream "test10.drawio" :external-format :utf-8)
  (read-xml-prolog stream)
  (read-mxfile stream)
  )
(line:with-open-file (stream "x-rayCT2.drawio" :external-format :utf-8)
  (read-xml-prolog stream)
  (read-mxfile stream)
  )
|#
(defun split-string (string &key (item #\space) (test #'char=))
  ;; Splits the string into substrings at spaces.
  (let ((len (length string))
	(index 0) result)
    (dotimes (i len
		(progn (unless (= index len)
			 (push (subseq string index) result))
		       (reverse result)))
      (when (funcall test (char string i) item)
	(unless (= index i);; two spaces in a row
	  (push (subseq string index i) result))
        (setf index (1+ i))))))
(defun starts-with (object sequence &key (test #'eql) (key #'identity))
  "Returns true if SEQUENCE is a sequence whose first element is EQL to OBJECT.
Returns NIL if the SEQUENCE is not a sequence or is an empty sequence."
  (funcall test
           (funcall key
                    (typecase sequence
                      (cons (car sequence))
                      (sequence
                       (if (plusp (length sequence))
                           (elt sequence 0)
                         (return-from starts-with nil)))
                      (t (return-from starts-with nil))))
           object))
(defun starts-with-subseq (prefix sequence &rest args &key (return-suffix nil) &allow-other-keys)
  "Test whether the first elements of SEQUENCE are the same (as per TEST) as the elements of PREFIX.
If RETURN-SUFFIX is T the functions returns, as a second value, a
displaced array pointing to the sequence after PREFIX."
;;;  (remove-from-plistf args :return-suffix)
  (let ((sequence-length (length sequence))
        (prefix-length (length prefix)))
    (if (<= prefix-length sequence-length)
        (let ((mismatch (apply #'mismatch prefix sequence args)))
          (if mismatch
              (if (< mismatch prefix-length)
                  (values nil nil)
                  (values t (when return-suffix
                              (cl:make-array (- sequence-length mismatch)
                                          :element-type (cl:array-element-type sequence)
                                          :displaced-to sequence
                                          :displaced-index-offset prefix-length
                                          :adjustable nil))))
              (values t (when return-suffix
                          (cl:make-array 0 :element-type (cl:array-element-type sequence)
                                      :adjustable nil)))))
      (values nil nil))))

(defmacro query-path (&rest args)
  (destructuring-bind (obj &rest path) args
    (make-query obj (car path) (cdr path)))
  )

(defun make-query (qform first-path rest-path)
  (format t "~%traversing ~S for ~S ..." first-path qform)
  (cond ((null rest-path) qform)
        (t (case first-path
             ((mxfile diagram mxGraphModel mxCell)
              (make-query
               (list
                (intern (concatenate 'string (string first-path) "-" (string (car rest-path)))
                        :drawio)
                qform)
               (car rest-path)
               (cdr rest-path)))
             (root (make-query `(nth ,(car rest-path) ,qform) (cadr rest-path) (cddr rest-path)))
             (style
              (case (car rest-path)
                ((endArrow html class layer fillColor strokeColor entryX entryY entryDx entryDy
                           edgeLabel resizable html align verticalAlign)
                 `(second (split-string (find-if #'(lambda (e) (starts-with-subseq (string ',(car rest-path)) e))
                                                 (split-string ,qform :item #\;))
                                        :item #\=)))
                (otherwise qform)))
             (otherwise
              (error "illegal element ~S given." first-path))))))