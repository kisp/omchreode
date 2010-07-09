#+om(in-package :om)
#-om(in-package :omchreode)

(ompw:define-menu omchreode)
(ompw:in-menu omchreode)

(defun list2circlist (list)
  (rplacd (last list) list)
  list)

(defclass abstract-generator ()
  ())

(defclass flip (abstract-generator)
  ((generators :accessor generators :initarg :generators)
   (circ :accessor flip-circ)))

(defclass tab (abstract-generator)
  ((vector :accessor tab-vector :initarg :tab-vector)))

(defclass tab-norm (tab) nil)

(defclass chreode (abstract-generator)
  ((nb-period :accessor chreode-nb-period :initarg :nb-period)
   (incr/decr :accessor chreode-incr/decr :initarg :incr/decr)
   (up/down :accessor chreode-up/down :initarg :up/down)
   (offset :accessor chreode-offset :initarg :offset)
   (deviation :accessor chreode-deviation :initarg :deviation)))

(ompw:define-box get-value (generator x)
  (declare (ignore x))
  generator)

(defmethod get-value ((generator abstract-generator) x)
  (declare (ignore x))
  (error "abstract-generator get-value should not be called"))

(defmethod get-value ((generator function) x) (funcall generator x))

(defmethod get-value ((generator flip) x)
  (get-value (pop (flip-circ generator)) x))

(defmethod get-value ((generator tab) (x integer))
  (aref (tab-vector generator) x))

(defmethod get-value ((generator tab) (x number))
  (get-value generator (round x)))

(defmethod get-value ((generator tab-norm) x)
  (let ((int-index
	 (min (floor (* x (length (tab-vector generator))))
	      (1- (length (tab-vector generator))))))
    (aref (tab-vector generator) int-index)))

(defmethod get-value ((generator chreode) x)
  (let ((nx
	 (if (eql (chreode-incr/decr generator) :decr) (- 1.0 x) x)))
    (assert (<= 0.0 x 1.0))
    (+ (get-value (chreode-offset generator) x)
       (* (get-value (chreode-deviation generator) x)
	  (case (chreode-up/down generator)
	    (:down (xcosx nx (chreode-nb-period generator)))
	    (:up (-xcosx nx (chreode-nb-period generator)))
	    (otherwise
	     (error "Got ~s, was expecting one of :DOWN, :UP."
		    (chreode-up/down generator))))))))

(ompw:define-box get-values (generator (x (0 0.5 1)))
  :non-generic t
  (mapcar #'(lambda (x) (get-value generator x)) x))

(ompw:define-box make-flip (&rest generators)
  :non-generic t
  (reset-flip (make-instance 'flip :generators generators)))

(defun reset-flip (flip)
  (setf (flip-circ flip) (list2circlist (copy-list (generators flip))))
  flip)

(ompw:define-box make-tab (values)
  :non-generic t
  (make-instance 'tab :tab-vector (coerce values 'vector)))

(ompw:define-box make-tab-norm (values)
  :non-generic t
  (make-instance 'tab-norm :tab-vector (coerce values 'vector)))


(ompw:define-box make-chreode ((nb-period 4) (incr/decr :decr) (up/down :down)
			       (offset 0) (deviation 1))
  :non-generic t
  :menu (incr/decr (:decr "decr") (:incr "incr"))
  :menu (up/down (:down "down") (:up "up"))
  (assert (member incr/decr '(:incr :decr)))
  (assert (member up/down '(:up :down)))
  (make-instance 'chreode :nb-period nb-period :incr/decr incr/decr :up/down
		 up/down :offset offset :deviation deviation))


(defun xcosx (x y) (* x (cos (* x y (* 2 pi)))))

(defun -xcosx (x y) (- (* x (cos (* x y (* 2 pi))))))

(defun cosx (x y) (cos (* x y (* 2 pi))))

(defun -cosx (x y) (- (cos (* x y (* 2 pi)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass dur-sampler nil
    ((generator :accessor generator :initarg :generator)
     (duration :accessor duration :initarg :duration)
     (null-dur :accessor null-dur :initarg :null-dur))))

(ompw:define-box make-dur-sampler (generator duration &optional null-dur)
  :non-generic t
  (make-instance 'dur-sampler :generator generator :duration duration :null-dur
		 null-dur))

(defun dur-sampler-next-dur (dur-sampler time)
  (let ((dur (get-value (generator dur-sampler) time)))
    (if (= 0.0 dur) (null-dur dur-sampler) dur)))

(ompw:define-box get-durs (dur-sampler)
  :non-generic t
  :outputs 3
  (loop with time = 0 while (< time (duration dur-sampler)) for norm-offset =
       (/ time (duration dur-sampler)) for dur =
       (dur-sampler-next-dur dur-sampler norm-offset) collect dur into durs
     collect time into offsets collect norm-offset into norm-offsets do
       (incf time dur) finally (return (values durs offsets norm-offsets))))

(defun mat-trans (matrix)
  (assert (apply #'= (mapcar #'length matrix)) nil
	  "this should not happen. Please report this to Kilian Sprotte")
  (when matrix (apply #'mapcar #'list matrix)))

(ompw:define-box save-text-simple (path &rest params)
  :non-generic t
  (unless path (setq path (ompw:choose-new-file-dialog)))
  (with-open-file (out path :direction :output :if-exists :supersede)
    (loop for event in (mat-trans params) do
	 (format out "~{~A~^ ~}~%" event))))

(ompw:define-box save-text-coll (path &rest params)
  :non-generic t
  (unless path (setq path (ompw:choose-new-file-dialog)))
  (with-open-file (out path :direction :output :if-exists :supersede)
    (loop for event in (mat-trans params) for i upfrom 1 do
	 (format out "~A, ~{~A~^ ~};~%" i event))))

(ompw:install-menu omchreode)
