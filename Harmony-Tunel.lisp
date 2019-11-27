(defun get-env-time (in-env)
  (let ((env in-env)
	(times '())
	time
	)
    (setq times (loop for env-point in env 
		   collect (nth 1 env-point)))
    (setq time (loop for t-point in times sum t-point))
		    
    )) 
;; (print (get-env-time env))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun insert-by-idx (idx lst new-value)
  (let ((retval nil))
    (loop for i from 0 to (- (length lst) 1) do
      (when (= i idx)
        (push new-value retval))
      (push (nth i lst) retval))
    (when (>= idx (length lst))
      (push new-value retval))
    (nreverse retval)))
;; (setq a '(a b c d))
;; (setq a (insert-by-idx 2 a 'CH))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remove-by-idx (n list)
  (loop for elt in list
        for i from 1
     unless (= i n) collect elt))
;; (setq a '(a b c d))
;; (print (remove-by-idx 2 a))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun trans-MIDI (a b)
  (let* ((chord a)
	 (trns-dist b)
	 (transp))
    (setq transp (loop :for n :in chord :do
		    (setq transp (+ n trns-dist))
		    :collect transp))
    transp)) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun chord-place-in-measure (a b)
  (let* ((chord a)
	 (map-with b)
	 (len)
	 (dist '()))   
    (setq len (length chord))
    (loop :for i :from 0 :to (- len 1) :do
       (setq dist (econs dist (position i map-with))))
    dist))	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ts-from-chord-length (a b)
  (let* ((base b)
	 (chord a)
	 (number-of-units)
	 (ts '()))
    (setq number-of-units (length chord))
    (setq ts (push base ts))
    (setq ts (push number-of-units ts))
    ts))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun 2-note (input)
  (let* ((chord input))
    (setq chord (loop :for note :in chord
		   :collect (midi-to-note note)))
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun chord-in-measures-seq (a b)
  (let* ((chord (2-note (trans-MIDI a 72)))
	 (order b)
	 (bar '())
	 (dist '())
	 (measures)
	 (time-sig))
    (setq time-sig (ts-from-chord-length chord 16))
    (setq dist (chord-place-in-measure chord order))
    (loop :repeat (length chord) :for n :from 0 :do
       (setq bar '())
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       (loop :for i :from 0 :to (- (length chord) 1) :do
	  (if (= (nth n dist) i)
	      (progn
		(format t "~%PLACE IN : ~a CYCLE : ~a" (nth n dist) i)
		(setq bar (push (make-event chord 16) bar)))
	      (setq bar (push (make-event nil 16 :is-rest t) bar)))
	      )
       (setq bar (nreverse bar))
       (setq bar (make-rthm-seq-bar (push time-sig bar)))
       
       (setq measures (push bar measures))
       (print bar)   
       )
       (setq measures (nreverse measures))
       ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun all-intervals (icv)
  ;; this function "decompresses" the ICV into IV listing
  ;; all available intervals
  (let* ((in-icv icv)
	 (all-intervals '())
	 interval)
  (loop for pos from 0 to (- (length in-icv) 1) do ;; count from 0 to length
       ;;(format t "~%INTERVAL : ~a amount : ~a" (+ pos 1) (nth pos icv))
       (setq interval (+ pos 1))
       (loop repeat (nth pos in-icv) do
	    (push interval all-intervals)))
  (nreverse all-intervals)))    
;; (print (interval-presence icv))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to what point? in how many seconds--LIKE PD
(defun limit-env (in-env in-rthm-unit) ;; creates
  (let* ((env in-env)
	 (rthm-unit in-rthm-unit)
	 (position 0)
	 (result-env '())
	  base-rthm env-for-interp point prev-point)
    (loop for env-point in env do
	 (setq base-rthm (/ rthm-unit 2))
       ;;(print base-rthm)
       ;;(print (nth 1 env-point))
	 (when (= (nth 1 env-point) 0)
	   ;;(format t "~%~a : * FIRST * make-event ~a"
	   ;;(nth 1 env-point)(nth 0 env-point))
	   (push (nth 0 env-point) result-env)
	   (setq prev-point (nth 0 env-point))
	   )
	 (when (> (nth 1 env-point) 0)
	   (setq env-for-interp '())
	   ;; we create the envelope for INTERPOLATE with
	   ;; this format "(0 previous-point steps current-point)"
	   (setq env-for-interp (push (nth 0 env-point) env-for-interp)
		 ;; amount of beats -multiply by "BASE rthm"
		 env-for-interp (push (- (* (nth 1 env-point) base-rthm) 1) env-for-interp)
		 env-for-interp (push prev-point env-for-interp)
		 env-for-interp (push '0 env-for-interp))

	   ;; necesito el total de beats tambien para mandar al plot
	   (loop for n from 0 to (- (* (nth 1 env-point) base-rthm) 1) do
		(setq position (+ 1 position))
		(setq point (interpolate n env-for-interp))
		;(format t "~%  ~3d  ~3d" position (round point))
		(push (round point) result-env)
		)
	 
	   (setq prev-point (nth 0 env-point))    
	   (setq env-for-interp '())
	   (setq prev-point (nth 0 env-point))
	   ))
    result-env)
  )

;;(env2gnuplot env "/home/jrsv/Dropbox/Pons-Percussion/bottom-env.txt")
;;(env2gnuplot env "/Volumes/DOCS/odrive/WORK-Big/Pons-Percussion/bottom-env.txt")

;;; Now that we have the two limits, top and bottom,
;;; we start the "sequencing" by starting the count
;;; with the ICV, if the position is equal to the limit,
;;; then direction must change.

(defparameter env-top '((80 0)(80 3)(60 4)(40 14)(50 17)(50 20))) ;; 58 total
(defparameter env-bot '((40 0) (55 5) (33 2) (10 5) (10 20) (30 26))) ;; 58 total

;; (setq limits-top (limit-env env-top 32))
;; (setq limits-bot (limit-env env-bot 32))

(let* ((icv '(2 1 2 2 2 1))
       (limits-top '())
       (limits-bot '())
       (interval-v '())
       (events '())
       (music '())
       (bar '())
       (bars '())
       (t-sign '(32))
       initial-dir initial-pit curr-pitch note-dur direction
       top-limit bot-limit beat tunel b-len)
  
  ;;(print (setq interval-v (all-intervals icv)))
  (setq note-dur 32)
  (setq initial-pit 49)
  (setq initial-dir -1) ;;; or 1
  (setq limits-top (limit-env env-top note-dur))
  (setq limits-bot (limit-env env-bot note-dur))
  (setq direction initial-dir)
  (setq curr-pitch initial-pit)

  (setq beat 0)
  (push initial-pit music)

  ;;(format t "~%Beat ~d Steps ~d Pitch ~d Direction ~d" 0 0 initial-pit initial-dir)
  
  (loop repeat 90 do
       (loop for steps in interval-v do
	    (setq top-limit (nth beat limits-top)
		  bot-limit (nth beat limits-bot))
       (format t "~% TOP-LIMIT : ~d ----- BOT-LIMIT : ~d" top-limit bot-limit)
	    (if (> steps 1)
		(progn 
		  (loop repeat steps do
		       (cond ((and (>= curr-pitch top-limit) (= direction 1))
			      (setq direction -1))
			     ((and (<= curr-pitch bot-limit) (= direction -1))
			      (setq direction 1)))	    
		       (setq curr-pitch (+ curr-pitch direction)))
		  (setq beat (+ beat 1))
		  (push curr-pitch music))
		(progn
		  (setq curr-pitch (+ curr-pitch direction))
		  (setq beat (+ beat 1))
		  (push curr-pitch tunel)))
	     (format t "~%Beat ~d Steps ~d Pitch ~d Direction ~d Top-L ~d Bot-L ~d"
	     	    beat steps curr-pitch direction top-limit bot-limit)
	    )
       )

  (setq b-len (length tunel))
  (format t "~%Tunel ~d" (setq tunel (nreverse tunel)))
  
  ;; (setq tunel (2-note tunel))
  
  ;; (loop for note in tunel do
  ;;      (push (make-event (make-pitch note) 32) events))
  ;; (setq events (nreverse events))

  ;; (setq t-sign (push b-len t-sign))
  ;; (print t-sign)
  ;; (setq bar (make-rthm-seq-bar (push t-sign events)))
  ;; (setq bars (push bar bars))
  ;; (setq music (bars-to-sc bars
  ;; 			  :sc-name 'music
  ;;   			  :instrument 'piano))

  ;; (setf (title music) "Harmony Tunel")
  ;; (lp-display music :base-path "/home/jrsv/Dropbox/Pons-Percussion/scores/HT")
 
  )
