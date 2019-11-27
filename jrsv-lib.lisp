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
(defun get-env-time (in-env)
  (let ((env in-env)
	(times '())
	time
	)
    (setq times (loop for env-point in env 
		   collect (nth 1 env-point)))
    (setq time (loop for t-point in times sum t-point))
		    
    ))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remove-by-idx (idx list)
  (loop for elt in list
        for i from 0
     unless (= i idx) collect elt))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun trans-MIDI (in-chord in-order)
  (let* ((chord in-chord)
	 (trns-dist in-order)
	 (transp))
    (setq transp (loop :for n :in chord :do
		    (setq transp (+ n trns-dist))
		    :collect transp))
    transp)) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun chord-place-in-measure (in-chord in-order)
  (let* ((chord in-chord)
	 (map-with in-order)
	 (len)
	 (dist '()))   
    (setq len (length chord))
    (loop :for i :from 0 :to (- len 1) :do
       (setq dist (econs dist (position i map-with))))
    dist))	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ts-from-chord-length (amount-of-beats rthm-fig)
  (let* ((base rthm-fig)
	 (chord amount-of-beats)
	 (number-of-units)
	 (ts '()))
    (setq number-of-units (length chord))
    (setq ts (push base ts))
    (setq ts (push number-of-units ts))
    ts))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun 2-note (in-chord)
  (let* ((chord (list in-chord)))
    (setq chord (flatten chord))
    (if (and (listp chord) (= (length chord) 1))
	(progn ;;(print "here")
	       (setq chord (midi-to-note (first chord)))
	       )
	(progn ;;(print "there")
	       (setq chord (loop :for note :in chord
			      :collect (midi-to-note note)))
	       ))
	
    ))
;;(print (2-note '(48)))
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
(defun events-2-midi (in-events)
  (let*((events in-events)
	(pitches '()))
    (setq pitches (loop for e in events
		     collect (note-to-midi (get-pitch-symbol e)))
	  )
    pitches)) 
;; (defun events-2-midi (in-events)
;;   (let*((events in-events)
;; 	(pitches '())
;; 	e)
;;
;;     (if (listp events)
;; 	(progn ;; (print "is a list")
;; 	  (if (= (length events) 1)
;; 		 (setq pitches (note-to-midi (get-pitch-symbol (flatten e))))
;; 		 (setq pitches (loop for e in events
;; 				  collect (note-to-midi (get-pitch-symbol e)))
;; 		       pitches (nreverse pitches)))
;; 	      )
;; 	(progn ;; (print "not a list")
;; 	       (setq pitches (get-pitch-symbol events))
;;
;; 	       )
;; 	)
;;     pitches))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pack-in-measures (in-list-of-notes in-measure-length)
  (let* ((notes in-list-of-notes)
	 (measure-len in-measure-length)
	 (measure '())
	 (music '())
	 grouping group grp)
	 
    (setq grouping (split-groups (length notes) measure-len))
    (setq grp 0)
    (loop for note in notes
       for place from 1 to (length notes) do
	 (push note measure)
	 (setq group (nth grp grouping))
	 ;;(format t "~% ~a ~a" group note)
	 (cond ((and (= (length measure) group) (= (length measure) measure-len))
		(setq measure (nreverse measure))
		(push measure music)
		(setq measure '())
		(setq grp (1+ grp)) 
		)
	       ((and (= (length measure) group) (not (equal (length measure) measure-len)))
		(loop repeat (- measure-len (length measure)) do
				(push 'r measure)
				)
		(setq measure (nreverse measure))
		(push measure music)
		(setq measure '())
		(setq grp (1+ grp))
		)
	       )
	      )
    (setq music (nreverse music))
    music))
;;(print (pack-in-measures '(a a a a a b b b b b c c c c c d d d d d e e e) 5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten-chord-of-1 (in-chord)
  (let ((chord in-chord)
	)
    (if (< (length chord) 2)
	(setq chord (flatten chord))
	)
    chord))
;;(print (flatten-chord-of-1 '((3)))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun save-2-file (a b)
  (let ((write a)
	(file b)
	(result '()))

    ;;(print write)
    (loop for note in write for beat from 0 do
	 ;;(format t "~%~a ~a" beat note)
	 (push beat result)
	 (push note result)
	 )
    (setq result (nreverse result))
    (env2gnuplot result b)	 
    (set-sc-config 'default-dir *base-path*)

    ;; (with-open-file (stream file
    ;; 			    :direction :output
    ;; 			    :if-does-not-exist :create
    ;; 			    :if-exists :supersede)
    
    ;;   (format stream write))
    (format t "~%********* File.Saved *********")
    ))
;;(save-2-file (format nil "~a" '(1 2 3 4)) *top-env-file*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;
;; to what point? in how many seconds--LIKE PD
;; I use SC "env" function and later env2gnuplot
;;
(defun limit-env (in-env in-rthm-unit) ;; creates
  (let* ((env in-env)
	 (rthm-unit in-rthm-unit)
	 (position 0)
	 (result-env)
	  base-rthm env-for-interp point prev-point)
    (loop for env-point in env do
	 (setq base-rthm (/ rthm-unit 8))
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

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NEED TO SAVE THIS FOR PLOT
		
		;(format t "~%  ~3d  ~3d" position (round point))

		(push (round point) result-env)
		)
	 
	   (setq prev-point (nth 0 env-point))    
	   (setq env-for-interp '())
	   (setq prev-point (nth 0 env-point))
	   ))
    result-env)
  )
;; (env2gnuplot env "/home/jrsv/Dropbox/Pons-Percussion/bottom-env.txt")
;; (env2gnuplot env "/Volumes/DOCS/odrive/WORK-Big/Pons-Percussion/bottom-env.txt")
;; (env2gnuplot env "/Users/admin/odrive/WORK-Big/Pons-Percussion/scores/HT/bottom-env.txt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-sym-chord (input)  
  (let* ((chord input)
	 (base-note)
	 (new-note)
	 (new-chord)
	 (comp-chord)
	 (intervals))
    (setq intervals (loop :for i :from 1 :to (- (length chord) 1) :do
		       (setq base-note (first chord)) 
		       :collect (setq intervals (- (nth i chord) base-note))))
    (setq comp-chord (loop :for int :in intervals :do
			(setq base-note (first chord))
			(setq new-note (- base-note int))
			:collect new-note))
    (setq comp-chord (sort comp-chord '<))
    (setq new-chord (flatten (push comp-chord chord)))
    new-chord
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun chord-or-list (in-list)
  (let* ((p-list in-list)
	 f-list result-out)
    (setq f-list (flatten p-list))
    ;;(print (length f-list))
    ;;(print (length p-list))
    (if (not (= (length f-list) (length p-list)))
	(progn ;; (print "LIST")
	       (setq result-out 1)
	       )
	(progn ;; (print "ONE CHORD")
	       (setq result-out 0)
	))
    result-out
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun random-list (in-amount)
  (let ((amount in-amount)
	list'()
	)
    (setq list (loop for i from 0 to (- amount 1)
		  collect i))
    (setq list (nshuffle list))
    ))
;;(random-list 5)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;
;;
;; Chord to Measure or C2M is a collection of functions that produce 
;; musical material according to shuffling rules that determin the structure
;; pitch / rhythm / form 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;
;;
;; C2M :places the chord in the ([beat index] value) of measure number ([be-
;; at value] value) of the sequence
;;___________________________________________________________________________
(defun C2M-chordPlace-in-Measures-Seq (in-chord in-order in-beat-dur) ;; ****** R
  (let* ((chord (trans-MIDI in-chord 0))
	 (order in-order);; order which indicates the beat I insert the chord
	 ;;(chord (2-note (trans-MIDI in-chord 72)))
	 (bar '())
	 (dist '())
	 music
	 (beat-dur in-beat-dur)
	 (measures)
	 (time-sig))
    (setq time-sig (ts-from-chord-length chord in-beat-dur))
    (print (setq dist (chord-place-in-measure chord order)))
    (loop :repeat (length chord) :for n :from 0 :do
       (setq bar '())
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (loop :for i :from 0 :to (- (length chord) 1) :do
	  (if (= (nth n dist) i)
	      (progn ;;(print "here")
		     (format t "~%PLACE IN : ~a CYCLE : ~a" (nth n dist) i)
		;;;;; here we process what we want;;;;;;;;;;;;;;;;;;;;;;;;;;
		     (setq bar (push chord bar))
		)
	      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	      (progn ;;(print "___HERE___")
		     (setq bar (push 'R bar))
		     )
	      ))

       (setq bar (nreverse bar))
       (push time-sig bar)
       (setq measures (push bar measures))
       ;;(print bar)
       )
    (setq measures (nreverse measures))
    )
  )
;;
;;(print (C2M-chordPlace-in-Measures-Seq '(60 62 69 76 81) '(4 2 0 3 1) 16))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;
;;
;; C2M :Places the chord in the desired place but ommits the pitch correspon-
;; ding to that cycle.
;;___________________________________________________________________________
(defun C2M-1note-out (in-chord in-order in-beat-dur) ;; ********************** R
  (let* ((chord (trans-MIDI in-chord 0))
	 (order in-order)
	 (bar '())
	 (dist '())
	 (measures)
	 (time-sig)
	 music
	 (beat-dur in-beat-dur)
	 note-out-chord)
    (setq time-sig (ts-from-chord-length chord beat-dur))
    (setq dist (chord-place-in-measure chord order))
    (loop :repeat (length chord) :for n :from 0 :do
       (setq bar '())
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       (loop :for i :from 0 :to (- (length chord) 1) :do
	  ;;(format t "~%PLACE IN : ~a CYCLE : ~a" (nth n dist) n)
	  (if (= (nth n dist) i)
	      (progn
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;; here we process what we want we build the chord as we need ;;
		;; and make the event at the bottom ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		(setq note-out-chord (remove-by-idx n chord))
		(setq bar (push note-out-chord bar))
		)
	      (progn ;;;;;; IF NOT A PLACE OF PROCESS, INSERT SILENCE;;;;;;;;;;
		;;(print ":REST-----------------------------REST" )
		(setq bar (push 'R bar)))
	  ))
       (setq bar (nreverse bar))
       (setq bar (push time-sig bar))
       (setq measures (push bar measures))  
       )
    (print (nreverse measures))
    )
  )
;;
;;(C2M-1note-out '(60 62 69 76 81) '(4 2 0 3 1) 16)
;;(C2M-render (C2M-1note-out '(60 62 69 76 81) '(4 2 0 3 1) 16) 16 "test" *base-path*)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C2M Shrinks the chord by (-1) pitch in each cycle
;;___________________________________________________________________________
(defun C2M-shrink-chord (in-chord in-order in-beat-dur) ;; ******************* R
  (let* ((chord (trans-MIDI in-chord 0))
	 (order in-order)
	 (bar '())
	 (dist '())
	 (measures '())
	 (time-sig)
	 len music
	 (beat-dur in-beat-dur)
	 (new-chord)
	 (deleted-note))
    (setq len (- (length chord) 1))
    (setq time-sig (ts-from-chord-length chord beat-dur))
    (print (setq dist (chord-place-in-measure chord order)))
    (setq new-chord chord)
    (loop :repeat (length chord) :for n :from 0 :do ;; bar number
       (setq bar '())
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
       (loop :for i :from 0 :to (- (length chord) 1) :do
	  ;;(format t "~%PLACE IN : ~a CYCLE : ~a" (nth n dist) n)
	  (if (= (nth n dist) i)
	      (progn
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;; here we process what we want we build the chord as we need ;;
		;; and make the event at the bottom ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		(setq bar (push new-chord bar))
		)
	      (progn ;;;;;; IF NOT A PLACE OF PROCESS, INSERT SILENCE;;;;;;;;;;
		;;(print ":REST-----------------------------REST" )
		(setq bar (push 'R bar))
		))
	  )
       (setq deleted-note (nth n chord))
       (setq new-chord (remove deleted-note new-chord))
       (setq bar (nreverse bar))
       (setq bar (push time-sig bar))
       (push bar measures)
       )
  (setq measures (nreverse measures))
  (print measures)
  ))
;;
;;(C2M-shrink-chord '(60 62 69 76 81) '(4 2 0 3 1) 16)
;;(C2M-render (C2M-shrink-chord '(60 62 69 76 81) '(4 2 0 3 1) 16) 16 "test" *base-path*)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C2M will place the chord in the determined place but will leave behind 1
;;___________________________________________________________________________
(defun C2M-take1-then-pass-the-chord (in-chord in-order in-beat-dur) ;; ****** R
  (let* (;;(chord (2-note (trans-MIDI a 72)))
	 (chord in-chord)
	 (order in-order)
	 (bar '())
	 (dist '())
	 (measures '())
	 (bars '())
	 music
	 (beat-dur in-beat-dur)
	 (time-sig)
	 (len)
	 (rest-of-chord)
	 (place-to-edit)
	 (cur-measure)
	 (prev-measure)
	 beat-to-rmv bar-control)
    (setq len (- (length chord) 1))
    (setq time-sig (ts-from-chord-length chord beat-dur))
    (setq dist (chord-place-in-measure chord order))
    (setq rest-of-chord chord)
    (setq prev-measure (loop :for rest :from 0 :to len
			  :collect 'r))  
    (loop :for m-number :from 0 :to len :do
       (setq place-to-edit (nth m-number dist))
       (setq cur-measure prev-measure)
       (loop :for beat :from 0 :to len :do
	  (when (= beat place-to-edit)
	    (setq beat-to-rmv (nth m-number chord)) ;beat to extract from rest-of-chord
	    (setf (nth place-to-edit cur-measure) rest-of-chord);; insert chord in "place-to-edit"
	    (setq bar (copy-list cur-measure))
	    (setf (nth beat cur-measure) beat-to-rmv) 
	    (setq rest-of-chord (remove beat-to-rmv rest-of-chord))	    
	    )
	  )
       (push time-sig bar)
       (print bar)
       (push bar measures)
       )
    (setq measures (nreverse measures))
    (print measures)
    ))
;;
;;(C2M-take1-then-pass-the-chord '(60 62 69 76 81) '(0 1 2 3 4) 16)
;;(C2M-render (C2M-take1-then-pass-the-chord '(60 62 69 76 81) '(4 2 0 3 1) 16) 16 "test" *base-path*)
;;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C2M chord remains fixed in position but deals a note in each cycle
;;___________________________________________________________________________
(defun C2M-deal-from-chord (in-chord in-order in-beat-dur)
  (let* (;;(chord (2-note (trans-MIDI a 72)))
	 (chord in-chord)
	 (order in-order)
	 (bar '())
	 (dist '())
	 (measures '())
	 (bars '())
	 (time-sig)
	 len
	 music
	 (beat-dur in-beat-dur)
	 (rest-of-chord)
	 (place-to-edit)
	 (cur-measure)
	 (prev-measure)
	 (measure '())
	 note-to-rmv bar-control)
    (setq len (- (length chord) 1))
    (setq time-sig (ts-from-chord-length chord beat-dur))
    (print (setq dist (chord-place-in-measure chord order)))
    (setq rest-of-chord chord)

    ;;create the base measure, full of rests
    (setq prev-measure (loop :for rest :from 0 :to len
			  :collect 'R))
    
    (loop :for m-number :from 0 :to len :do
       (setq place-to-edit (nth m-number dist))    
       (setq cur-measure prev-measure)
       ;; inserts complete chord in beat of first itiration.
       (if (= m-number 0)
	   (progn ;;(format t "~%MEASURE Number ~a" m-number)
		  (setf (nth (nth 0 dist) cur-measure) chord)
		  ;;(format t "~%--- ~a ---" cur-measure)
		  ;;(setq measures (push cur-measure measures))
		  )
	   (progn ;;(format t "~%MEASURE Number ~a" m-number)
		  (setq note-to-rmv (nth 1 rest-of-chord))
		  (setq rest-of-chord (remove note-to-rmv rest-of-chord))
		  (setf (nth (nth 0 dist) cur-measure) rest-of-chord)
		  (setf (nth (nth m-number dist) cur-measure) note-to-rmv)
		  )
	   )
       (setq measure (copy-list cur-measure))
       (push time-sig measure)
       (setq measures (push measure measures))
       ;;(format t "~% ****** ~a ******" measures)
       ;;(setq prev-measure cur-measure)
       )
    (setq measures (nreverse measures))
    (format t "~% ~%******~%~a ~%" measures)
    measures
    ))
;;
;;(C2M-deal-from-chord '(60 62 69 76 81) '(4 2 0 3 1) 16)
;;(C2M-render (C2M-deal-from-chord '(60 62 69 76 81) '(4 2 0 3 1) 16) 16 "test" *base-path*)
;;
;;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C2M REQUANTIZE
;;___________________________________________________________________________
(defun C2M-requantize (in-C2M-measures in-number-of-units)
  (let ((measures in-C2M-measures)
	(number-of-units in-number-of-units)
	(out-measures '())
	(bars '())
	time-sig bar measure-noTS bt)
    
    (loop for measure in measures do
	 (setq measure-noTS (rest measure))
	 (setq bar '())
	 (loop for beat in measure-noTS do
	      (cond ((not(equal beat 'R))
		     ;; we get rid of "chords of 1 note"
		     (if (and (listp beat) (= (length beat) 1))
			 (setq bt (first beat))
			 (setq bt beat)
			 )
		     ;;(print bt)
		     (loop repeat number-of-units do
			  (setq bar (push bt bar)))
		     ;;(format t "~%****** ~a ******" beat)
		     )
		    ((equal beat 'R)
		     ;;(print "******** SILENCE")
		     (loop repeat number-of-units do
			  (setq bar (push 'R bar)))
		     ))
	      )    	 
	 (setq bar (nreverse bar))
	 (push bar bars))
    ;;(format t "~%*** *** *** *** ***~%~a ~%" (setq bars (nreverse bars)))
    (loop for bar in bars do
	 (loop for beat in bar do
	      (push beat out-measures)
	      )
	 )
    (format t "~% ~%******~%~a ~%" out-measures)
    out-measures
  ))
;;
;; (print (C2M-insert-TS (pack-in-measures 
;; 		       (C2M-requantize (C2M-deal-from-chord '(60 62 69 76 81) '(4 2 0 3 1) 16) 4)
;; 		       8
;; 		       )
;; 		      '(4 4)
;; 		      )
       
;;  )
;;
;;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C2M insert time-signature after a requantization "pack-to-measures"
;;___________________________________________________________________________
(defun C2M-insert-TS (in-measures-noTS in-new-time-sig)
  (let ((measures in-measures-noTS)
	;;(beat-dur in-beat-dur)
	(bars '())
	(new-time-sig in-new-time-sig)
	bar)   
    (loop for measure in measures do
	 (setq bar measure)
	 (push new-time-sig bar)
	 (push bar bars))
    (setq bars (nreverse bars))
  ))
;;
;;
;;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C2M bar format -> Slippery Chicken -> Lilypond. It provides a sc
;; object full of events and will be named as it is in. Renders the
;; music to a local
;; file.
;;______________________________________________________________________
(defun C2M-render-local (in-C2M-measures in-beat-dur in-tempo in-render-name in-base-path)
  (let* ((measures in-C2M-measures)
	(beat-dur in-beat-dur)
	(render-name in-render-name)
	(out-measures '())
	(tempo in-tempo)
	(ties '())
	(prev-tie 0)
	(next-tie 0)
	(prev-note-event 0)
	time-sig bar measure-noTS
	output-score midifile-name
	event-number base-path
	note-event tie)    
    (setq base-path (format base-path "~a/score/" in-base-path))
    (setq midifile-name (format nil "~a/midi/~a.mid" base-path render-name))
    ;; (loop for voice in measures
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq ties '())
    (loop for measure in measures
       for m-number from 0 do
	 (setq time-sig (first measure))
	 (setq beat-dur (* (/ (- (length measure) 1) (nth 0 time-sig))
			   (nth 1 time-sig)))
	 (setq measure-noTS (rest measure))
	 (setq bar '())
	 (loop for beat in measure-noTS
	    for b-number from 0 do
	      (setq tie '())
	      (setq tie (push b-number tie))
	      (cond ((not (equal beat 'R))		    
		     (setq bar (push (make-event (2-note beat) beat-dur)
				     bar))
		     (setq tie (push 1 tie)
			   tie (push (2-note beat) tie))
		     
		     )
		    ((equal beat 'R)
		     (setq bar (push (make-event nil beat-dur
						 :is-rest t)
				     bar))
		     (setq tie (push 0 tie)
			   tie (push beat tie))
		     )
		    
		    )
	      (setq tie (nreverse tie))
	      (push tie ties)
	      )
	 (setq bar (nreverse bar)
	       bar (make-rthm-seq-bar (push time-sig bar)))
	 
	 (push bar out-measures)
	 )
    (setq out-measures (nreverse out-measures))

    (setq output-score (bars-to-sc out-measures
				   :sc-name  (make-symbol render-name)
				   ;;:player render-name
	 			   :instrument 'piano
					;:tempo tempo
				   ))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TIE NOTES POST-GEN
    (setq ties (nreverse ties))
    (next-event output-score 'player-one nil t)
    (setq event-number 0)
    ;;This loop turns on the corresponding Ties
    (loop for next-e = (next-event output-score 'player-one)
       while next-e for tie in ties do
    	 (if (slot-value next-e 'is-rest)
    	     (progn (format nil "============= SILENCE")
    		    (format nil "~%Event: ~a Rest: ~a"
    			    (incf event-number)
			    (slot-value next-e 'is-rest))
    		    )
    	     (if (get-pitch-symbol next-e)
    		 (progn (format nil "*******************************NOTE")
    			(setq next-tie (nth (+ event-number 1) ties))
    			(format nil "~%E#: ~a | Prev Tie: ~a | Tie: ~a | Next Tie: ~a |"
    				event-number (nth 1 prev-tie) (nth 1 tie)(nth 1 next-tie))
    			(cond ((and (not (equal (nth 2 prev-tie) (nth 2 tie)))
    				   (equal (nth 2 tie) (nth 2 next-tie)))
    			       (format nil " ---- FIRST of Repeted Group ----")
    			       (format nil "~%Previous: ~a~%Current:  ~a~%Next:     ~a~%"
    				       (nth 2 prev-tie)
    				       (nth 2 tie)
    				       (nth 2 next-tie)
    				       )
    			       (setf (is-tied-from next-e) t)
    			       (setf (is-tied-to next-e) t)
    			       )
    			      )   
    			(cond ((and (equal (nth 2 prev-tie) (nth 2 tie))
    				    (equal (nth 2 tie) (nth 2 next-tie)))
    			      (format nil " ------- MIDDLE Group -------")
    			      (format nil "~%Previous: ~a~%Current:  ~a~%Next:     ~a~%"
    				      (nth 2 prev-tie)
    				      (nth 2 tie)
    				      (nth 2 next-tie)
    				      )
    			      (setf (is-tied-from next-e) t)
    			      (setf (is-tied-to next-e) t)
    			      )
    			    )
    			(cond ((and (equal (nth 2 prev-tie) (nth 2 tie))
    				    (not (equal (nth 2 tie) (nth 2 next-tie))))
    			       (format nil " ------- LAST of Repeted Group -------")
    			       (format nil "~%Previous: ~a~%Current:  ~a~%Next:     ~a~%"
    				       (nth 2 prev-tie)
    				       (nth 2 tie)
    				       (nth 2 next-tie)
    				       )
    			       (setf (is-tied-from next-e) nil)
    			       (setf (is-tied-to next-e) t)
    			       )
    			      )
    			(setq prev-note-event note-event)
    			(incf event-number)
    		 	)
    		 )
    	     )
    	 (setq prev-tie tie)
    	 )    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SCORE GENERATION
    (update-slots output-score)
    ;;(consolidate-all-rests output-score 1 nil 'player-one)
    ;;(consolidate-all-notes output-score 1 nil 'player-one)
    (handle-ties output-score)
    (rebar output-score)
    (setf (title output-score) (format nil "~a" render-name))
    ;;(midi-play output-score :midi-file midifile-name)
    (write-lp-data-for-all output-score)
    (lp-display output-score :base-path base-path)
    output-score
    ))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (C2M-render-local (C2M-insert-TS (pack-in-measures (C2M-requantize
;; 						    (C2M-deal-from-chord '(60 62 69 76 82)
;; 								   '(4 2 0 3 1)
;; 								   8)
;; 						    5)
;; 						   28)
;; 				 '(7 8)
;; 				 )
;; 		  16 70 "TEST" *base-path*)


(defun C2M-render-midi (in-C2M-measures in-beat-dur in-tempo in-render-name in-base-path)
  (let* ((measures in-C2M-measures)
	(beat-dur in-beat-dur)
	(render-name in-render-name)
	(out-measures '())
	(tempo in-tempo)
	(ties '())
	(prev-tie 0)
	(next-tie 0)
	(prev-note-event 0)
	time-sig bar measure-noTS
	output-score midifile-name
	event-number base-path
	note-event tie)    
    (setq base-path (format base-path "~a/score/" in-base-path))
    (setq midifile-name (format nil "~a/midi/measure~a.mid" base-path render-name))
    ;; (loop for voice in measures
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq ties '())
    (loop for measure in measures
       for m-number from 0 do
	 (setq time-sig (first measure))
	 (setq beat-dur (* (/ (- (length measure) 1) (nth 0 time-sig))
			   (nth 1 time-sig)))
	 (setq measure-noTS (rest measure))
	 (setq bar '())
	 (loop for beat in measure-noTS
	    for b-number from 0 do
	      (setq tie '())
	      (setq tie (push b-number tie))
	      (cond ((not (equal beat 'R))		    
		     (setq bar (push (make-event (2-note beat) beat-dur)
				     bar))
		     (setq tie (push 1 tie)
			   tie (push (2-note beat) tie)))
		    ((equal beat 'R)
		     (setq bar (push (make-event nil beat-dur
						 :is-rest t)
				     bar))
		     (setq tie (push 0 tie)
			   tie (push beat tie))))
	      (setq tie (nreverse tie))
	      (push tie ties))
	 (setq bar (nreverse bar)
	       bar (make-rthm-seq-bar (push time-sig bar)))
	 (push bar out-measures))
    (setq out-measures (nreverse out-measures))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TIE NOTES POST-GEN
    (setq ties (nreverse ties))
    (next-event output-score 'player-one nil t)
    (setq event-number 0)
    ;;This loop turns on the corresponding Ties 
    (loop for next-e = (next-event output-score 'player-one)
       while next-e for tie in ties do
    	 (if (not (slot-value next-e 'is-rest))
    	     (if (get-pitch-symbol next-e)
    		 (progn (cond ((and (not (equal (nth 2 prev-tie) (nth 2 tie)))
    				   (equal (nth 2 tie) (nth 2 next-tie))))
    			       (setf (is-tied-from next-e) t)
    			       (setf (is-tied-to next-e) t)))   
    			(cond ((and (equal (nth 2 prev-tie) (nth 2 tie))
    				    (equal (nth 2 tie) (nth 2 next-tie))))
    			      (setf (is-tied-from next-e) t)
    			      (setf (is-tied-to next-e) t)))
    			(cond ((and (equal (nth 2 prev-tie) (nth 2 tie))
    				    (not (equal (nth 2 tie) (nth 2 next-tie))))
    			       (setf (is-tied-from next-e) nil)
    			       (setf (is-tied-to next-e) t)))
    			(setq prev-note-event note-event)
    			(incf event-number))
	 (setq prev-tie tie)
    	 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MIDI OUT
    (setq ties (nreverse ties))
    (handle-ties output-score)
    (midi-play output-score :midi-file midifile-name)
    ))
    
;;
;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C2M bar format -> Slippery Chicken -> Lilypond. It provides a sc object
;; full of events and will be named as it is in. Adds C2M to an exterior SC
;; object
;;___________________________________________________________________________
(defun C2M-render (in-C2M-measures in-beat-dur in-render-name
		   in-base-path in-SC-obj in-player)
  (let ((measures in-C2M-measures)
	(beat-dur in-beat-dur)
	(render-name in-render-name)
	(out-measures '())
	(base-path in-base-path)

	(output-score in-SC-obj)
	(player in-player)
	;;(inst-Pallette in-inst-Pallette)
	time-sig bar measure-noTS  midifile-name)
    
    (setq midifile-name (format nil "~a/~a.mid" base-path render-name))
    (loop for measure in measures do
	 (setq time-sig (first measure))
	 (setq measure-noTS (rest measure))
	 (setq bar '())
	 (format t "~% ~a----~a----" time-sig measure-noTS)
	 (loop for beat in measure-noTS do
	      (cond ((not(equal beat 'R))		    
		     (setq bar (push (make-event (2-note beat) beat-dur) bar))
		     )
		    ((equal beat 'R)
		     (print "******** SILENCE")
		     (setq bar (push (make-event nil beat-dur :is-rest t) bar)))
		    )
	      
	      )
	 (setq bar (nreverse bar)
	       bar (make-rthm-seq-bar (push time-sig bar)))
	       ;;bar (auto-beam bar)
	 (push bar out-measures) 
       )
    (setq out-measures (nreverse out-measures)) 
    (bars-to-sc out-measures
		;;:sc in-SC-obj
		:sc-name 
		;;:player player
		;;:instrument player
		)
    
    ;;(midi-play output-score :midi-file midifile-name)
    ;;(write-lp-data-for-all output-score)
    ;;(lp-display output-score :base-path base-path)
    output-score
    ))
;;
;; (C2M-render (C2M-chordPlace-in-Measures-Seq '(60 62 69 76 81) '(4 2 0 3 1) 16)
;; 	    16 "test" "/tmp/" '+Percussion+ 'piano ' )
;;
;;;;;;;;;;;;;;;::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun C2M-preview (in-C2M-measures)
;;   (let((measures in-C2M-measures)
       
;;        )
;;     (cmn-display +primary-disposition+ :file "/tmp/primary-disposition.eps")
;;     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;
;;
;; Chord to Durations or C2D is a collection of functions that generate
;; structured durations and other material based on C2M
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;
;;
;; C2D-gen-I generates the durations of every element
;;___________________________________________________________________________
(defun C2D-gen-I (in-chord in-order in-beat-dur) ;; ********************* R
  (let* ((chord in-chord)
	 (order in-order);; order which indicates the beat I insert the chord
	 (bar '())
	 (dist '())
	 music total-beats
	 (deat-dur in-beat-dur)
	 (measures)
	 (time-sig))
    (setq time-sig (ts-from-chord-length chord in-beat-dur))
    (setq dist (chord-place-in-measure chord order))
    (print time-sig)
    (setq total-beats (* (first time-sig) 2))    
    
    )
  )
;;
;;(C2D-gen-I '(60 62 69 76 81) '(4 2 0 3 1) 16)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;
