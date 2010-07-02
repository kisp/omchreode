(in-package :om)

(defun list2circlist (list)
  (rplacd (last list) list)
  list)

(defclass abstract-generator ()
  ())

(defmethod! get-value ((generator abstract-generator) x)
  (declare (ignore x))
  (error "abstract-generator get-value should not be called"))

(defmethod get-value ((generator t) x)
  (declare (ignore x))
  generator)

(defmethod get-value ((generator function) x)
  (funcall generator x))

(defmethod! get-values (generator (x list))
  (mapcar #'(lambda (x) (get-value generator x)) x))

(defclass flip (abstract-generator)
  ((generators :accessor generators :initarg :generators)
   (circ :accessor flip-circ)))

(defmethod! make-flip (&rest generators)
  (reset-flip (make-instance 'flip :generators generators)))

(defun reset-flip (flip)
  (setf (flip-circ flip) (list2circlist (copy-list (generators flip))))
  flip)

(defmethod get-value ((generator flip) x)
  (get-value (pop (flip-circ generator)) x))

(defclass tab (abstract-generator)
  ((vector :accessor tab-vector :initarg :tab-vector)))

(defmethod! make-tab (values)
  (make-instance 'tab :tab-vector (coerce values 'vector)))

(defmethod get-value ((generator tab) (x integer))
  (aref (tab-vector generator) x))

(defmethod get-value ((generator tab) (x number))
  (get-value generator (round x)))

(defclass tab-norm (tab)
  ())

(defmethod! make-tab-norm (values)
  (make-instance 'tab-norm :tab-vector (coerce values 'vector)))

(defmethod get-value ((generator tab-norm) x)
  (let ((int-index (min
		    (floor (* x (length (tab-vector generator))))
		    (1- (length (tab-vector generator))))))
    (aref (tab-vector generator) int-index)))

(defclass chreode (abstract-generator)
  ((nb-period :accessor chreode-nb-period :initarg :nb-period)
   (incr/decr :accessor chreode-incr/decr :initarg :incr/decr)
   (up/down :accessor chreode-up/down :initarg :up/down)
   (offset :accessor chreode-offset :initarg :offset)
   (deviation :accessor chreode-deviation :initarg :deviation)))

(defmethod! make-chreode (nb-period incr/decr up/down offset deviation)
  :initvals (list 4 :decr :down 0 1)
  :menuins '((1 ( ("decr" :decr) ("incr" :incr)))
	     (2 ( ("down" :down) ("up" :up) )))
  (assert (member incr/decr '(:incr :decr)))
  (assert (member up/down '(:up :down)))
  (make-instance 'chreode :nb-period nb-period
		 :incr/decr incr/decr
		 :up/down up/down
		 :offset offset
		 :deviation deviation))

(defun  xcosx (x y)
  (* x (cos (* x y (* 2 pi)))))

(defun -xcosx (x y)
  (- (* x (cos (* x y (* 2 pi))))))

(defun  cosx (x y)
  (cos (* x y (* 2 pi))))

(defun -cosx (x y)
  (- (cos (* x y (* 2 pi)))))

(defmethod get-value ((generator chreode) x)
  (let ((nx (if (eql (chreode-incr/decr generator) :decr)
		(- 1.0 x)
		x)))
    (assert (<= 0.0 x 1.0))
    (+ (get-value (chreode-offset generator) x)
       (* (get-value (chreode-deviation generator) x)
	  (case (chreode-up/down generator)
	    (:down (xcosx nx (chreode-nb-period generator)))
	    (:up (-xcosx nx (chreode-nb-period generator))))))))

(defclass dur-sampler ()
  ((generator :accessor generator :initarg :generator)
   (duration :accessor duration :initarg :duration)
   (null-dur :accessor null-dur :initarg :null-dur)))

(defmethod! make-dur-sampler (generator duration &optional (null-dur 0.1))
  (make-instance 'dur-sampler :generator generator
		 :duration duration
		 :null-dur null-dur))

(defun dur-sampler-next-dur (dur-sampler time)
  (let ((dur (get-value (generator dur-sampler) time)))
    (if (= 0.0 dur)
	(null-dur dur-sampler)
	dur)))

(defmethod! get-durs (dur-sampler)
  :numouts 3
  (loop
     with time = 0
     while (< time (duration dur-sampler))
     for norm-offset = (/ time (duration dur-sampler))
     for dur = (dur-sampler-next-dur dur-sampler norm-offset)
     collect dur into durs
     collect time into offsets
     collect norm-offset into norm-offsets
     do (incf time dur)
     finally (return (values durs offsets norm-offsets))))

(defun choose-new-text-file-dialog ()
  (capi:prompt-for-file ""
			:operation :save
			:filters '("Text Documents" "*.txt;*.text"
				   "All Files" "*.*")))


(defmethod! save-text-simple (path &rest params)
  (unless path (setq path (choose-new-text-file-dialog)))
  (with-open-file (out path :direction :output :if-exists :supersede)
    (loop for event in (mat-trans params)
       do (format out "~{~A~^ ~}~%" event))))

(defmethod! save-text-coll (path &rest params)
  (unless path (setq path (choose-new-text-file-dialog)))
  (with-open-file (out path :direction :output :if-exists :supersede)
    (loop for event in (mat-trans params)
       for i upfrom 1
       do (format out "~A, ~{~A~^ ~};~%" i event))))
