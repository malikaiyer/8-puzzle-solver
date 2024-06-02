(in-package :cl-user)
(defpackage :assignment-1
  (:use :cl
        :alexandria))
(in-package :assignment-1)

;; copy and paste these two defmacro forms into your project -- Just
;; below the defpackage/inpackage stuff, but before anything else
(defmacro dowhile (condition &body body)
  `(do ()
       ((not ,condition))
     ,@body))
(defmacro dountil (condition &body body)
  `(do ()
       (,condition)
     ,@body))

;given test matrix - figure 1
(defvar *puzzle-0* #2A((3 1 2)
                       (7 nil 5)
                       (4 6 8)))

;given test matrix - figure 2
(defvar *puzzle-1* #2A((7 2 4)
                       (5 nil 6)
                       (8 3 1)))

;given test matrix - figure 3
(defvar *puzzle-2* #2A((6 7 3)
                       (1 5 2)
                       (4 nil 8)))
(defvar *puzzle-3* #2A((nil 8 6)
                       (4 1 3)
                       (7 2 5)))
(defvar *puzzle-4* #2A((7 3 4)
                       (2 5 1)
                       (6 8 nil)))
(defvar *puzzle-5* #2A((1 3 8)
                       (4 7 5)
                       (6 nil 2)))
(defvar *puzzle-6* #2A((8 7 6)
                       (5 4 3)
                       (2 1 nil)))
(defvar  *puzzle-goal* #2A((nil 1 2)
                        (3 4 5)
                        (6 7 8)))



;helper function to find 0
(defun find-zero-position (board row col)
  (loop for i below row
        do (loop for j below col
                 when (= (aref board i j) 0) return (return (list i j)))))

(defun rev-helper (list reversed)
  "A helper function for reversing a list.  Returns a new list
containing the elements of LIST in reverse order, followed by the
elements in REVERSED.  (So, when REVERSED is the empty list, returns
exactly a reversed copy of LIST.)"
  (if (endp list)
      reversed
      (rev-helper (rest list)
                  (list* (first list)
                         reversed))))


;part 1: function possible-actions that takes a board as input and outputs a list of all actions
;possible on the given board
(defun possible-actions (board)
  (let((actions '())(row 0) (col 0))
    (setf row (array-dimension board  0))
    (setf col (array-dimension board 1))
    (dotimes (i row)
      (dotimes (j col)
        (when (equal (aref board i j) nil)
            ;(when (>= 0 i) (push :down actions))
            ;(when (>= 0 j) (push :right actions))
            ;(when (< i row) (push :up actions))
                                        ;(when (< j col) (push :left actions))
          (when (not (= (- i 1) -1))(push :down actions))
          (when (not (= (- j 1) -1))(push :right actions))
          (when (not (= (+ i 1) row))(push :up actions))
          (when (not (= (+ j 1) col))(push :left actions))
      )
    )
      )
   actions)
  )

;part 2: Write a function result that takes as input an action and a board and outputs the new board
;that will result after actually carrying out the input move in the input state. Be certain that you
                                        ;do not accidentally modify the input board variable
(defun result (action board)
  (let ((newBoard (alexandria:copy-array board)))
    (let ((row (array-dimension board 0)) (col (array-dimension board 1)))
      (dotimes (i row newBoard)
        (dotimes (j col)
          (when (equal (aref board i j) nil)
            (when (equal action :down)
              (setf (aref newBoard i j) (aref newBoard (1- i) j))
              (setf  (aref newBoard (1- i) j) nil)
              )
            (when (equal action :up)
              (setf (aref newBoard i j) (aref newBoard (1+ i) j))
              (setf  (aref newBoard (1+ i) j) nil)
              )
            (when (equal action :right)
              (setf (aref newBoard i j) (aref newBoard i (1- j)))
              (setf  (aref newBoard i (1- j)) nil)
              )
            (when (equal action :left)
              (setf (aref newBoard i j) (aref newBoard i (1+ j)))
              (setf  (aref newBoard i (1+ j)) nil)
              )
            )
          )
        )
      )
    )
  )


;Write a function expand that takes a board as input, and outputs a list of all states that can be
;reached in one Action from the given state
(defun expand (board)
  (let ((possibleActionsList (possible-actions board)))
    (let ((stateslist '()))
      (dolist (action possibleActionsList)
        (push (result action board) stateslist))
      (rev-helper stateslist '())))) 

;Implement an iterative deepening search which takes an initial board and a goal board and
                                        ;produces a list of actions that form an optimal path from the initial board to the goal
(defstruct node
  board
  parent
  moves
  depth
  )

(defvar visited-states '())

(defun makeNode (board parent moves depth)
  (make-node :board board :parent parent :moves moves :depth depth))

(defun is-cycle (board)
  (let ((isC nil))
    (dolist (cycle visited-states isC)
      (if (equalp cycle board)
          (return-from is-cycle t)))
    (push board visited-states)
    nil))




(defun iterative-deepening-search (goal initboard)
  (let ((result nil)(depth 0))
    (dowhile (null result)
      (setf result (depth-limited-search goal initboard depth))
      (incf depth))
    result)
  )

(defun depth-limited-search (goal initboard max-depth)
  (let ((frontier (list (makeNode initboard nil'() 0))) ;; Initialize frontier with the initial board
        (resultvar nil) (visited-states nil))
    ;(pprint frontier)
    (loop until (or (null frontier) resultvar)
          do (let ((curr-node (pop frontier))) 
               (if (equalp (node-board curr-node) goal)
                   (progn
                     (setf resultvar curr-node)
                     (return-from depth-limited-search (node-moves curr-node)))
                   (if (>= (node-depth curr-node) max-depth)
                       (setf resultvar nil)
                       (when (null (is-cycle (node-board curr-node)))
                           (dolist (action (possible-actions (node-board curr-node))) ;; Iterate through possible actions
                             (let ((child-board (result action (node-board curr-node))))
                               (push (makeNode child-board
                                               curr-node
                                               (append (node-moves curr-node) (list action)) ;; Append the action to the moves list
                                               (1+ (node-depth curr-node)))
                                     frontier))
                            ; (pprint frontier)
                             ))))))
  resultvar))


                                        ;Implement a breadth-first search which, like the iterative deepening search, takes an initial board and a goal and gives an optimal sequence of actions from the initial state to the goal.
(defun bfs (goal initboard)
  (let ((initnode (makeNode initboard nil '() 0))
        (reached (make-hash-table :test #'equal)) (resultvar nil) (frontier (queue:make-queue)))
    (setf (gethash (prin1-to-string initboard) reached) t) ;hashtable to keep track of reached boards
    (queue:push initnode frontier) ;add initial node to queue
    (loop until (queue:emptyp frontier)
      ;(pprint frontier)
          do (let ((curr-node (queue:pop frontier)))
        (dolist (action (possible-actions (node-board curr-node)))
          (let ((child-board (result action (node-board curr-node))))
            (when (equalp child-board goal)
              (setf resultvar (append (node-moves curr-node) (list action)))
              (return-from bfs resultvar)
              )
            (unless (gethash(prin1-to-string child-board) reached) ;if the board is not in reached
              (setf(gethash (prin1-to-string child-board) reached) t) ;add child-board to reached hashtable
              (queue:push (makeNode child-board
                                    curr-node
                                    (append (node-moves curr-node) (list action)) ;; Append the action to the moves list
                                    (1+ (node-depth curr-node)))
                          frontier)
              )
            )
          )
        ))
    resultvar))


(defstruct nodeastar
  board
  parent
  moves
  depth
  cost
  )


(defun makeNodeastar (board parent moves depth heuristic goal)
  ; Assume heuristic is a function that calculates h(n) for the given board.
  (let ((g-n depth) ; Assuming uniform cost, depth is equivalent to g(n)
        (h-n (funcall heuristic board goal))) ;; Calculate h(n) for the current board
    (make-nodeastar :board board :parent parent :moves moves :depth depth :cost (+ g-n h-n))))

(defun astar (goal initboard heuristic)
  (let ((initnode (makeNodeastar initboard nil '() 0 heuristic goal))
        (reached (make-hash-table :test #'equal)) (resultvar nil) (frontier (priority-queue:make-queue :key #'nodeastar-cost)))
    (setf (gethash (prin1-to-string initboard) reached) initnode) ;hashtable to keep track of reached boards
    (priority-queue:push initnode frontier) ;add initial node to queue
    (dountil (priority-queue:emptyp frontier)
      do (let ((curr-node (priority-queue:pop frontier)))
           (when (equalp (nodeastar-board curr-node) goal)
             (return-from astar (nodeastar-moves curr-node)) ;return list of moves if =puzzle-goal
             )
           (dolist (action (possible-actions (nodeastar-board curr-node)))
             (let* ((child-board (result action (nodeastar-board curr-node)))
                    (existing-node (gethash (prin1-to-string child-board) reached))
                    (new-cost (1+ (nodeastar-cost curr-node)))) ;; Assuming step cost is 1
               (when (or (null existing-node)
                         (< new-cost (nodeastar-cost existing-node)))
                 (let ((new-node (makeNodeastar child-board
                                                curr-node
                                                (append (nodeastar-moves curr-node) (list action))
                                                (1+ (nodeastar-depth curr-node))
                                                heuristic
                                                goal)))
                   (setf (gethash (prin1-to-string child-board) reached) new-node)
                   (priority-queue:push new-node frontier))))
             )
           ))
    resultvar)
  )

;manhattan dist: sum of the (orthogonal) distances of the tiles from their goal position - SKIPPING NIL VALUE
(defun manhattan-dist (board goal)
  (let ((dist 0)
        (size (array-dimension board 0))) ; Assuming square board
    (dotimes (i size)
      (dotimes (j size)
        (let ((value (aref board i j)))
          (when value
            (let ((goal-pos (find-goal-position value goal)))
              (if goal-pos
                  (let* ((goal-i (car goal-pos))
                         (goal-j (cdr goal-pos)))
                    (incf dist (+ (abs (- i goal-i))
                                  (abs (- j goal-j)))))
                  ;; Handle case when value is not found in goal array
                  (error "Value ~a not found in goal array" value)))))))
    dist))

;;helper func for manhattan dist that finds the position of targetvalue in goal board
(defun find-goal-position (targetvalue goal)
  (dotimes (i (array-dimension goal 0))
    (dotimes (j (array-dimension goal 1))
      (let ((value (aref goal i j)))  ; Fix: Access elements from the goal array
        (when (equal value targetvalue)
          (return-from find-goal-position (cons i j))))))
  nil)  ; Fix: Return nil if targetvalue is not found


;returns the number of tiles that are not in their correct position
(defun misplaced-tiles (board goal)
  (let ((dist 0) (size (array-dimension board 0)))
    (dotimes (i size)
      (dotimes (j size)
        (let ((boardvalue (aref board i j)) (goalvalue (aref goal i j)))
          (when boardvalue
            (unless (equal boardvalue goalvalue)
              (incf dist)
            ))
          )
        )
      )
    dist)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *15-puzzle-1* #2a((12 7 11 15)
                          (10 6 4 nil)
                          (1 2 9 3)
                          (13 5 8 14)))
(defvar *15-puzzle-2* #2a((12 7 11 15)
                          (9 6 4 nil)
                          (1 2 10 3)
                          (13 5 8 14)))
(defvar *15-puzzle-goal* #2a((nil 1 2 3)
                          (4 5 6 7)
                          (8 9 10 11)
                          (12 13 14 15)))



