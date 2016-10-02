(with-open-file (my-stream
                 "testfile.txt"
                 :direction :output
                 :if-exists :supersede)
  (princ "Hello File!" my-stream))


(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                   (concatenate 'string fname ".dot")
                   :direction :output
                   :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O" fname)))
