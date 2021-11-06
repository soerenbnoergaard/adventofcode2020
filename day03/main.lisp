(defun get-tree-map (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
       collect line)))

(defun tree-p (cell)
  (eq cell #\#))


(defun count-trees (tree-map row-step col-step)
  (loop
     with col-count = (length (elt tree-map 0))
     for row-count from 0 below (length tree-map)
     for row = 0 then (+ row row-step)
     for col = 0 then (mod (+ col col-step) col-count)

     if (< row (length tree-map))
     sum (if (tree-p (elt (elt tree-map row) col)) 1 0)
       ))

(print (count-trees (get-tree-map "puzzle_input.txt") 1 3))

(print
 (let ((tree-map (get-tree-map "puzzle_input.txt")))
   (*
    (count-trees tree-map 1 1)
    (count-trees tree-map 1 3)
    (count-trees tree-map 1 5)
    (count-trees tree-map 1 7)
    (count-trees tree-map 2 1))))
