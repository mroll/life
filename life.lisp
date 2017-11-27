(defmacro cellfn (name args body)
  `(defun ,name ,args
     (let ((row (nth 0 cell))
           (col (nth 1 cell)))
       ,body)))

(cellfn up (cell)
  (list (1- row) col))

(cellfn down (cell)
  (list (1+ row) col))

(cellfn left (cell)
  (list row (1- col)))

(cellfn right (cell)
  (list row (1+ col)))

(defun up-right (cell)
  (up (right cell)))

(defun up-left (cell)
  (up (left cell)))

(defun down-right (cell)
  (down (right cell)))

(defun down-left (cell)
  (down (left cell)))

(defun neighbors (cell)
  (list (up cell)
        (down cell)
        (left cell)
        (right cell)
        (up-right cell)
        (up-left cell)
        (down-right cell)
        (down-left cell)))

(defun column (cell)
  (nth 1 cell))

(defun row (cell)
  (nth 0 cell))

(defun value-at (board cell)
  (let ((col (column cell))
        (row (row cell))
        (nrows (length board))
        (ncols (length (nth 0 board))))
    (if (or (< col 0)
            (< row 0)
            (>= row nrows)
            (>= col ncols))
        0
        (nth (column cell)
             (nth (row cell) board)))))

(defun values-at (board cells)
  (mapcar #'(lambda (cell)
              (value-at board cell))
          cells))

(defun livingp (board cell)
  (eq 1 (value-at board cell)))

(defun copy-board (board)
  (loop for row in board
     collect (loop for col in row
                  collect col)))

(defun next-board (board)
  (let ((new-board (copy-board board)))
    (loop for rowidx below (length board)
       do (loop for colidx below (length (nth rowidx board))
             do (let ((living-neighbors (remove-if #'zerop
                                                   (values-at board (neighbors
                                                                     `(,rowidx ,colidx))))))
                  (if (livingp board `(,rowidx ,colidx))
                      (if (or (< (length living-neighbors) 2)
                              (> (length living-neighbors) 3))
                          (setf (nth colidx (nth rowidx new-board)) 0)
                          (setf (nth colidx (nth rowidx new-board)) 1))
                      (if (eq 3 (length living-neighbors))
                          (setf (nth colidx (nth rowidx new-board)) 1))))))
    new-board))

(defun print-row (row)
  (format t "~{~a ~}~%" (mapcar #'(lambda (v)
                               (if (eql v 1)
                                   "*"
                                   " "))
                           row)))

(defun print-board (board)
  (loop for row in board
     do (print-row row))
  (format t "~%"))
                
(defvar *board* '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0)
                  (0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0)
                  (0 1 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                  (0 1 1 0 0 0 0 0 0 0 0 1 0 0 0 1 0 1 1 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

;; (defvar *board* '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;                   (0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0)
;;                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;                   (0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0)
;;                   (0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0)
;;                   (0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0)
;;                   (0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0)
;;                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;                   (0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0)
;;                   (0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0)
;;                   (0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0)
;;                   (0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0)
;;                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;                   (0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0)
;;                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

;; (defvar *board* '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;                   (0 0 0 0 1 1 1 1 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0)
;;                   (0 0 0 1 0 0 0 1 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0)
;;                   (0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0)
;;                   (0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0)
;;                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(defvar *last-board* nil)

(loop for i below 1000
   while (not (equal *board* *last-board*))
   do (progn
        (setf *last-board* *board*)
        (setf *board* (next-board *board*))
        (print-board *board*)
        (format t "~C[2J" #\Esc)
        (sleep 0.08)))
