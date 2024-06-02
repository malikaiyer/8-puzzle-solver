(in-package :cl-user)
(defpackage :priority-queue
  (:use :cl)
  (:shadow :push :pop)
  (:export :make-queue
           :emptyp
           :push
           :pop))

(in-package :priority-queue)


(defun make-queue (&key (predicate #'<) (key #'identity) (initial-size 4))
  "Returns an empty priority queue.  Items in the queue will be
ordered by PREDICATE on their keys.  KEY is a function that takes an
item and gives back the key of that item."
  (list :heap (make-array initial-size :fill-pointer 0)
        :predicate predicate
        :key key))


(defun emptyp (queue)
  (zerop (fill-pointer (getf queue :heap))))


(defun parent (index)
  (if (oddp index) (/ (1- index) 2) (1- (/ index 2))))


(defun reheap-up (queue from)
  (do ((curr from (parent curr)))
      ((or (zerop curr)
           (not (funcall (getf queue :predicate)
                         (funcall (getf queue :key)
                                  (aref (getf queue :heap) curr))
                         (funcall (getf queue :key)
                                  (aref (getf queue :heap) (parent curr))))))
       queue)
    (rotatef (aref (getf queue :heap) (parent curr))
             (aref (getf queue :heap) curr))))


(defun push (obj queue)
  (when (= (fill-pointer (getf queue :heap))
           (array-dimension (getf queue :heap) 0))
    (adjust-array (getf queue :heap)
                  (* 2 (fill-pointer (getf queue :heap)))))
  (incf (fill-pointer (getf queue :heap)))
  (setf (aref (getf queue :heap)
              (1- (fill-pointer (getf queue :heap))))
        obj)
  (reheap-up queue (1- (fill-pointer (getf queue :heap)))))


(defun pop (queue)
  (labels ((smaller-child (index)
             (let* ((left-child (1+ (* 2 index)))
                    (right-child (1+ left-child)))
               (cond ((= left-child (1- (length (getf queue :heap)))) left-child)
                     ((< right-child (length (getf queue :heap)))
                      (if (funcall (getf queue :predicate)
                                   (funcall (getf queue :key)
                                            (aref (getf queue :heap) left-child))
                                   (funcall (getf queue :key)
                                            (aref (getf queue :heap) right-child)))
                          left-child
                          right-child))))))
    (decf (fill-pointer (getf queue :heap)))
    (rotatef (aref (getf queue :heap) 0)
             (aref (getf queue :heap)
                   (fill-pointer (getf queue :heap))))
    (do* ((smaller-child (smaller-child 0) (smaller-child smaller-child))
          (curr 0 (parent (or smaller-child 0))))
         ((or (null smaller-child)
              (not (funcall (getf queue :predicate)
                            (funcall (getf queue :key)
                                     (aref (getf queue :heap) smaller-child))
                            (funcall (getf queue :key)
                                     (aref (getf queue :heap) curr)))))
          (prog1 (aref (getf queue :heap) (fill-pointer (getf queue :heap)))
            (when (= (fill-pointer (getf queue :heap))
                     (/ (array-dimension (getf queue :heap) 0) 4))
              (adjust-array (getf queue :heap)
                            (* 2 (fill-pointer (getf queue :heap)))))))
      (rotatef (aref (getf queue :heap) smaller-child)
               (aref (getf queue :heap) curr)))))
