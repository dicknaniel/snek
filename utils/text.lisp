
(ql:quickload "split-sequence")

(defun get-words (txt)
  (loop for word in (split-sequence:split-sequence #\  txt)
        collect (list word (length word))))

