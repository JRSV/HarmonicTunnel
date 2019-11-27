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
(defun pack-in-measures (in-list-of-notes in-measure-length in-note-dur)
  (let* ((notes in-list-of-notes)
	 (measure-len in-measure-length)
	 (measure '())
	 (music '())
	 (note-dur in-note-dur)
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
				(push (make-event nil note-dur :is-rest t) measure)
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
;; (print (pack-in-measures '(a a a a a b b b b b c c c c c d d d d d e e e) 5 32))
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
;;(env2gnuplot env "/home/jrsv/Dropbox/Pons-Percussion/bottom-env.txt")
;;(env2gnuplot env "/Volumes/DOCS/odrive/WORK-Big/Pons-Percussion/bottom-env.txt")
;;(env2gnuplot env "/Users/admin/odrive/WORK-Big/Pons-Percussion/scores/HT/bottom-env.txt")

;;; Now that we have the two limits, top and bottom, we start the "sequencing"
;;; by starting the count with the ICV, if the position is equal to the limit, then
;;; direction must change.

(defparameter *env-top* '((120 0)(110 3)(101 4)(100 14)(103 17)(98 20))) ;; 58 total
(defparameter *env-bot* '((110 0) (90 5) (95 2) (95 5) (97 20) (90 26))) ;; 58 total

;; (defparameter env-top '((120 0)(110 3)(100 4)(110 14)(90 17)(100 20))) ;; 58 total
;; (defparameter env-bot '((90 0) (90 5) (95 2) (90 5) (80 20) (90 26))) ;; 58 total

;; (setq limits-top (limit-env env-top 32))
;; (setq limits-bot (limit-env env-bot 32))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SET THE PLATFORM IN THE *platform* argument and points to correct
;; paths depending on the PC using and create the text files for GNU plot

(defvar *platform* 'imac) ;;; change to linux, macbook, imac

(defvar *base-path*  
  (if (equal *platform* 'linux)
      (setf base-path "/home/jrsv/Dropbox/Pons-Percussion/LISP/HT");;linux
      (if (equal *platform* 'macbook) 
	  (setf base-path "");;macbook
	  (setf base-path "/Users/admin/odrive/WORK-Big/Pons-Percussion/LISP/HT");;IMAC
	  )))

;; (defvar *top-env-file*
;;   (let (env)
;;     (setq env (format env "~a/01.top-env-file.txt" *base-path*))
;;     env))

;; (defvar *bot-env-file*
;;   (let (env)
;;     (setq env (format env "~a/02.bot-env-file.txt" *base-path*))
;;     env))

;; (defvar *tunel-env-file*
;;   (let (env)
;;     (setq env (format env "~a/03.tunel-env-file.txt" *base-path*))
;;     env))

(defvar *midi-file*
  (let (file)
    (setq file (format file "~a/HarmonicTunel.mid" *base-path*))
    file))

(set-sc-config 'default-dir *base-path*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* (
       ;;(icv '(6 5 4 3 2 1)) ;; 7-1
       ;;(icv '(6 6 8 6 6 4)) ;; 9-10
       ;;(icv '(2 5 4 3 6 1)) ;; 7-35
       (icv '(3 1 3 4 3 1)) ;; 6-Z19
       (limits-top '())
       (limits-bot '())
       (interval-v '())
       (measures '())
       (music '())
       (bar '())
       (bars '())
       (t-sign '(32))
       initial-dir initial-pit curr-pitch note-dur direction
       top-limit bot-limit beat notes-list steps)

  (setq interval-v (all-intervals icv))
  (setq interval-v (multi-shuffle-with-perms interval-v 7))
  (setq note-dur 32)
  (setq initial-pit 100)
  (setq initial-dir 1) ;;; or 1
  

  ;;;; save to file ;;;;
  ;;(setq limits-top (limit-env *env-top* note-dur));; top limits
  ;;(setq limits-bot (limit-env *env-bot* note-dur));; bot limits
  
  ;;(save-2-file limits-top *top-env-file*) 
  ;;(save-2-file limits-bot *bot-env-file*)

  (setq direction initial-dir)  
  (setq curr-pitch initial-pit)
  (setq beat 0)
  (setq steps 0)
  (push initial-pit notes-list)
 
  ;;(format t "~%Beat ~d Steps ~d Pitch ~d Direction ~d" 0 0 initial-pit initial-dir)

  (loop while (<= beat (- (length limits-top) 1))
     for steps from 0 do ;
       ;;(print steps)
       (setq interval (nth (mod steps (length interval-v)) interval-v))
	  
       (setq top-limit (nth beat limits-top)
	     bot-limit (nth beat limits-bot))
     ;;(format t "~% TOP-LIMIT : ~d ----- BOT-LIMIT : ~d" top-limit bot-limit)
       (if (> interval 1)
	   (progn ;(print "here")
	     (loop repeat interval do
		  (cond ((and (>= curr-pitch top-limit) (= direction 1))
			 (setq direction -1)
			 ;;(print interval)
			 )
			((and (<= curr-pitch bot-limit) (= direction -1))
			 (setq direction 1)
			 ;;(print interval)
			 ))	    
		  (setq curr-pitch (+ curr-pitch direction)))
	     ;;(print "here")
	     (setq beat (+ beat 1))
	     (push curr-pitch notes-list)
	     )
	   (progn
	     (setq curr-pitch (+ curr-pitch direction))
	     (setq beat (+ beat 1))
	     (push curr-pitch notes-list)
	     )
	   )
     ;; (format t "~%Beat ~d Steps ~d Pitch ~d Direction ~d Top-L ~d Bot-L ~d"
     ;; 	    beat steps curr-pitch direction top-limit bot-limit)

       
       )
  (print (length notes-list))
  
  (setq notes-list (nreverse notes-list))
  (setq notes-list (trans-MIDI notes-list -30))
  (save-2-file notes-list *tunel-env-file*)
  (format t "~%Note-List - MIDI : ~d" notes-list)
  (setq notes-list (2-note notes-list))
  )
  (loop for note in notes-list do
       (push (make-event (make-pitch note) note-dur) measures))
  
  (setq measures (nreverse measures))
  (setq measures (pack-in-measures measures 8 note-dur))
  (setq t-sign (push 8 t-sign))
 
  (loop for measure in measures do
       (setq bar measure)
       (push t-sign bar)
       (setq bar (make-rthm-seq-bar bar))
       (push bar bars))

  (setq bars (nreverse bars))       
  (setq music (bars-to-sc bars
  			  :sc-name 'music
   			  :instrument 'piano))
  
  (setf (title music) "Harmony Tunel")

  (midi-play music :midi-file *midi-file*)

  (lp-display music :base-path *base-path*
  	       :paper 'a4
  	      ;;:left-margin 0
  	      :auto-clefs NIL
  	      ;;:staff-size 7
  	      :respell-notes T
  	      ;;:indent NIL
  	      ;;:landscape T
  	      ;;:line-width 21
  	      ;;:fixed-width 1/64
  	      :auto-clefs NIL)

  ;; (write-xml music)  
 
  )




