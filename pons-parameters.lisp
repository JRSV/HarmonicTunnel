
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; MASTER CHORDS
;;____________________________________________________________________________
(defparameter *master-chords* '())

(push (make-sym-chord '(60 61 70 81 83)) *master-chords*)
(push (make-sym-chord (trans-MIDI '(60 64 77 85 87) 3)) *master-chords*)
(push (make-sym-chord '(60 65 74 75 79)) *master-chords*)
(push (make-sym-chord (trans-MIDI '(60 67 68 84) 5)) *master-chords*)
(setq *master-chords* (nreverse *master-chords*))
;;______________________________________________________chords for SC/lilypond

(let ((bars '())
      (t-sign '(4 4))
      (m-chords '())
      (bar '())
      (all-events '())
      master-chords-lp
      )
 
  (setq all-events (loop :for chord :in *master-chords* :do
		      (print chord)
		      (setq chord
			    (make-event (2-note chord) (make-rhythm 'w)))
		      :collect chord))

  (setq bars '())

  (loop :for chord :in all-events :do
     (setq bar-to-fill (make-rthm-seq-bar '((4 4))) ; make an empty bar
	   ;; fill the bar with the events we made. This method will stop once
	   ;; the bar is full and will return the number of rhythms/events it
	   ;; 'ate'.    
	   ate (fill-with-rhythms bar-to-fill all-events)
	   all-events (when ate (nthcdr ate all-events)))
     (setq bar-to-fill (auto-beam bar-to-fill))
     (setq bars (nreverse (push bar-to-fill bars))))

  (setq master-chords-lp (bars-to-sc bars
				     :sc-name 'master-chords-lp
				     :instrument 'piano))
  (setf (title master-chords-lp) "Master Chords")
  (midi-play master-chords-lp :midi-file "Master-Chords.mid")
  ;;(lp-display master-chords-lp :base-path *base-path*

)  
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *top-env-file*
  (let (env)
    (setq env (format env "~a/01.top-env-file.txt" *base-path*))
    env))

(defvar *bot-env-file*
  (let (env)
    (setq env (format env "~a/02.bot-env-file.txt" *base-path*))
    env))

(defvar *tunel-env-file*
  (let (env)
    (setq env (format env "~a/03.tunel-env-file.txt" *base-path*))
    env))

(defvar *HT_midi-file*
  (let (file)
    (setq file (format file "~a/HarmonicTunel.mid" *base-path*))
    file))

(defvar *CHORDS_midi-file*
  (let (file)
    (setq file (format file "~a/master_chords.mid" *base-path*))
    file))

(set-sc-config 'default-dir *base-path*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter Ach '((65 74 75 79)(60 70 81 83)))
(defparameter Bch '((80 88 90)(65 72 73 89)))
(defparameter Cch '((37 39 50 70)(41 65 72 73)))
(defparameter Dch '((36 38 46 59)(37 39 50)))
(defparameter Ech '((41 45 46 55)(37 39 50)))
(defparameter Fch '((36 38 46 59)(41 57 58 65)))
(defparameter Gch '((50 59 60 61 70)(59 63 67)))
(defparameter Hch '((60 61 70 81 83)(65 74 75)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Identified the 5 note subset present in each SubSymChord, 
(defparameter A5-note-sub
  (let* ((chords '(((5-2) ( 72 65 73 41 70 ))((5-2) ( 72 65 73 41 39 ))
		   ((5-3) ( 65 73 41 39 50 ))((5-3) ( 65 73 41 39 50 ))
		   ((5-4) ( 72 65 73 41 50 ))((5-5) ( 72 65 73 41 37 ))
		   ((5-6) ( 72 65 73 39 50 ))((5-9) ( 72 65 73 37 70 ))
		   ((5-10) ( 73 41 39 50 37 ))((5-10) ( 73 41 39 50 37 ))
		   ((5-10) ( 72 73 41 39 50 ))((5-11) ( 65 73 41 39 70 ))
		   ((5-11) ( 72 65 73 39 70 ))((5-12) ( 72 65 41 39 50 ))
		   ((5-13) ( 72 65 73 50 70 ))((5-13) ( 72 65 73 50 70 ))
		   ((5-14) ( 65 73 41 37 70 ))((5-14) ( 65 73 41 50 37 ))
		   ((5-15) ( 72 65 73 50 37 ))((5-17) ( 73 41 39 50 70 ))
		   ((5-18) ( 65 73 39 50 37 ))((5-18) ( 65 73 39 50 37 ))
		   ((5-20) ( 72 65 39 50 70 ))((5-20) ( 72 65 39 50 37 ))
		   ((5-21) ( 65 73 39 50 70 ))((5-23) ( 41 39 50 37 70 ))
		   ((5-23) ( 65 41 39 50 37 ))((5-23) ( 65 41 39 50 37 ))
		   ((5-23) ( 72 73 41 39 70 ))((5-23) ( 72 65 41 37 70 ))
		   ((5-23) ( 72 65 41 39 70 ))((5-24) ( 72 39 50 37 70 ))
		   ((5-24) ( 72 73 41 37 70 ))((5-24) ( 72 73 41 37 70 ))
		   ((5-24) ( 72 65 50 37 70 ))((5-25) ( 72 41 39 50 37 ))
		   ((5-25) ( 72 73 41 39 37 ))((5-25) ( 72 65 41 50 70 ))
		   ((5-26) ( 73 39 50 37 70 ))((5-26) ( 73 39 50 37 70 ))
		   ((5-26) ( 72 73 41 50 70 ))((5-27) ( 65 39 50 37 70 ))
		   ((5-27) ( 65 41 39 50 70 ))((5-27) ( 72 65 39 37 70 ))
		   ((5-27) ( 72 65 41 39 37 ))((5-27) ( 72 65 41 39 37 ))
		   ((5-28) ( 72 73 39 50 37 ))((5-28) ( 72 73 41 50 37 ))
		   ((5-29) ( 73 41 39 37 70 ))((5-29) ( 72 41 39 50 70 ))
		   ((5-29) ( 72 65 41 50 37 ))((5-29) ( 72 65 41 50 37 ))
		   ((5-30) ( 73 41 50 37 70 ))((5-30) ( 65 73 50 37 70 ))
		   ((5-30) ( 72 73 39 50 70 ))((5-32) ( 65 73 39 37 70 ))
		   ((5-33) ( 72 73 50 37 70 ))((5-33) ( 72 73 50 37 70 ))
		   ((5-34) ( 72 41 50 37 70 ))((5-34) ( 72 73 39 37 70 ))
		   ((5-35) ( 65 41 50 37 70 ))((5-35) ( 65 41 39 37 70 ))
		   ((5-35) ( 72 41 39 37 70 ))((5-35) ( 72 41 39 37 70 ))
		   ((5-36) ( 65 73 41 39 37 ))((5-37) ( 65 73 41 50 70 ))
		   ((5-38) ( 72 65 73 39 37 ))))
	(prev-pcs 0)
	(final-chords '()))
    (loop for chord in chords do
	 ;;(format t "~%Previous PCS ~a Incoming Chord ~a" prev-pcs chord)
	 (if (not (equal (nth 0 chord) prev-pcs))
	     (progn ;; (print "here")
		  (push chord final-chords)
		  (setq prev-pcs (nth 0 chord))
		  )
	     ;;(print "reject")
	     ))
    (setq final-chords (nreverse final-chords))
    (print final-chords)
    (print (length final-chords))
    (format t "~%Amount of 5 note PCS in A SubChord ~a" (length chords))
    ;;final-chords
    )
  )

(defparameter B5-note-sub
  (let  ((chords '(((5-6) ( 41 65 39 70 50 ))
		   ((5-6) ( 41 65 39 70 50 ))
		   ((5-11) ( 65 39 70 50 37 ))
		   ((5-13) ( 41 39 70 50 37 ))
		   ((5-20) ( 41 65 70 50 37 ))
		   ((5-21) ( 41 65 39 70 37 ))
		   ((5-30) ( 41 65 39 50 37 ))
		   ((5-30) ( 41 65 39 50 37 ))))
	 (prev-pcs 0)
	 (final-chords '()))
    (loop for chord in chords do
	 ;;(format t "~%Previous PCS ~a Incoming Chord ~a" prev-pcs chord)
	 (if (not (equal (nth 0 chord) prev-pcs))
	     (progn ;; (print "here")
	       (push chord final-chords)
	       (setq prev-pcs (nth 0 chord))
	       )
	     ;;(print "reject")
	     ))
    (setq final-chords (nreverse final-chords))
    (print final-chords)
    (print (length final-chords))
    (format t "~%Amount of 5 note PCS in B SubChord ~a" (length chords))
    final-chords
    )
  )

(defparameter C5-note-sub
  (let ((chords '(((5-2) ( 65 37 50 39 41 ))
		  ((5-2) ( 70 65 37 50 39 ))
		  ((5-11) ( 70 37 50 39 41 ))
		  ((5-11) ( 70 65 37 50 41 ))
		  ((5-23) ( 70 65 50 39 41 ))
		  ((5-23) ( 70 65 37 39 41 )))))

    (print chords)
    chords
    )
  )

(defparameter D5-note-sub
  (let ((chords '(((5-1) ( 70 37 41 39 65 ))
		  ((5-1) ( 50 70 37 41 39 ))
		  ((5-2) ( 50 37 41 39 65 ))
		  ((5-2) ( 50 70 37 41 65 ))
		  ((5-3) ( 50 70 41 39 65 ))
		  ((5-3) ( 50 70 37 39 65 ))))
	)
    (print chords)
    chords
    )
  )

(defparameter E5-note-sub
  (let ((chords '(((5-6) ( 41 72 65 39 50 ))
		  ((5-9) ( 41 72 65 37 70 ))
		  ((5-11) ( 41 72 65 37 50 ))
		  ((5-13) ( 41 72 65 37 39 ))
		  ((5-15) ( 41 72 65 70 39 ))
		  ((5-15) ( 41 72 65 70 39 ))
		  ((5-18) ( 41 72 70 39 50 ))
		  ((5-20) ( 72 65 70 39 50 ))
		  ((5-20) ( 72 65 37 39 50 ))
		  ((5-21) ( 41 72 37 39 50 ))
		  ((5-24) ( 65 37 70 39 50 ))
		  ((5-24) ( 65 37 70 39 50 ))
		  ((5-24) ( 72 65 37 70 39 ))
		  ((5-26) ( 41 37 70 39 50 ))
		  ((5-27) ( 72 37 70 39 50 ))
		  ((5-27) ( 72 65 37 70 50 ))
		  ((5-28) ( 41 65 70 39 50 ))
		  ((5-28) ( 41 65 70 39 50 ))
		  ((5-30) ( 41 65 37 39 50 ))
		  ((5-30) ( 41 72 37 70 39 ))
		  ((5-32) ( 41 72 37 70 50 ))
		  ((5-33) ( 41 65 37 70 39 ))
		  ((5-34) ( 41 65 37 70 50 ))
		  ((5-34) ( 41 65 37 70 50 ))
		  ((5-38) ( 41 72 65 70 50 ))))
	(prev-pcs 0)
	(final-chords '()))
    (loop for chord in chords do
	 ;;(format t "~%Previous PCS ~a Incoming Chord ~a" prev-pcs chord)
	 (if (not (equal (nth 0 chord) prev-pcs))
	     (progn ;; (print "here")
	       (push chord final-chords)
	       (setq prev-pcs (nth 0 chord))
	       )
	     ;;(print "reject")
	     ))
    (setq final-chords (nreverse final-chords))
    (print final-chords)
    (print (length final-chords))
    (format t "~%Amount of 5 note PCS in E SubChord ~a" (length chords))
    final-chords
    )
  )

(defparameter F5-note-sub
  (let ((chords '(((5-2) ( 65 50 70 37 39 ))
		  ((5-5) ( 65 50 70 37 41 ))
		  ((5-25) ( 65 70 37 39 41 ))
		  ((5-25) ( 65 70 37 39 41 ))
		  ((5-27) ( 65 50 37 39 41 ))
		  ((5-36) ( 50 70 37 39 41 ))
		  ((5-38) ( 65 50 70 39 41 ))))
	(prev-pcs 0)
	(final-chords '()))
    (loop for chord in chords do
	 ;;(format t "~%Previous PCS ~a Incoming Chord ~a" prev-pcs chord)
	 (if (not (equal (nth 0 chord) prev-pcs))
	     (progn ;; (print "here")
	       (push chord final-chords)
	       (setq prev-pcs (nth 0 chord))
	       )
	     ;;(print "reject")
	     ))
    (setq final-chords (nreverse final-chords))
    (print final-chords)
    (print (length final-chords))
    (format t "~%Amount of 5 note PCS in F SubChord ~a" (length chords))
    final-chords
    )
  )


(defparameter *5N-Subsets-BCDF*
  (let ((chords '()))
    (push F5-note-sub chords)
    (push D5-note-sub chords)
    (push C5-note-sub chords)
    (push B5-note-sub chords)
    ))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter permutations-3
  (let ((perm (permutations 3))
	)
    perm
    ))
(defparameter permutations-4 '((0 2 1 3)(1 3 2 0)(2 0 3 1)(3 1 0 2)))
(defparameter permutations-5
  (let ((order '())
	;;(chord'(65 74 75 79 83))
	(idx-2-sort 0) ;;when beat hits x beat-number 
	(idx 2) ;; in measure x
	order-sort
      )
  (setq order (permutations 5))
  (setq order (loop for seq in order do
		   (if (= (nth idx seq) idx-2-sort)
		       (push seq order-sort)
		       )
		   ))
       
  ;;(print (length order-sort))
  order-sort
  ))
(defparameter perm-5B
  (let ((order '())
	(idx-2-sort 1) ;;when beat hits x beat-number 
	(idx 0) ;; in measure x
	order-sort
      )
  (setq order permutations-5)
  (setq order (loop for seq in order do
		   (if (= (nth idx seq) idx-2-sort)
		       (push seq order-sort)
		       )
		   ))
       
  ;;(print (length order-sort))
  order-sort
  ))
(defparameter perm-5C
  (let ((order '())
	(idx-2-sort 2) ;;when beat hits x beat-number 
	(idx 0) ;; in measure x
	order-sort
      )
  (setq order permutations-5)
  (setq order (loop for seq in order do
		   (if (= (nth idx seq) idx-2-sort)
		       (push seq order-sort)
		       )
		   ))
       
  ;;(print (length order-sort))
  order-sort
  ))
(defparameter perm-5D
  (let ((order '())
	(idx-2-sort 3) ;;when beat hits x beat-number 
	(idx 0) ;; in measure x
	order-sort
      )
  (setq order permutations-5)
  (setq order (loop for seq in order do
		   (if (= (nth idx seq) idx-2-sort)
		       (push seq order-sort)
		       )
		   ))
       
  ;;(print (length order-sort))
  order-sort
  ))
(defparameter perm-5F
  (let ((order '())
	(idx-2-sort 4) ;;when beat hits x beat-number 
	(idx 0) ;; in measure x
	order-sort
      )
  (setq order permutations-5)
  (setq order (loop for seq in order do
		   (if (= (nth idx seq) idx-2-sort)
		       (push seq order-sort)
		       )
		   ))
  order-sort
  ))

(defparameter All-5N-perm
  (let ((perms '())
	(grp-6perms)
	(all-groups))
    (push perm-5F perms)
    (push perm-5D perms)
    (push perm-5C perms)
    (push perm-5B perms)
    (loop for x from 0 to 5 do
	 (loop for perm in perms do
	      (push (nth x perm) grp-6perms)
	      )
	 (setq grp-6perms (nreverse grp-6perms))
	 (push grp-6perms all-groups)
	 (setq grp-6perms '())	 
	 )
    (nreverse all-groups)
    )
  )

(defparameter *5N-perms-BCDF*
  (let ((5N-pool-BCDF '())
	)
    (push (nth 1 All-5N-perm) 5N-pool-BCDF)
    (push (nth 2 All-5N-perm) 5N-pool-BCDF)
    (push (nth 3 All-5N-perm) 5N-pool-BCDF)
    (push (nth 5 All-5N-perm) 5N-pool-BCDF)
    (setq 5N-pool-BCDF (nreverse 5N-pool-BCDF))
    5N-pool-BCDF
    ))
 
	
