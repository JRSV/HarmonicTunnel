;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SET THE PLATFORM IN THE *platform* argument and points to correct
;; paths depending on the PC using and create the text files for GNU plot
;;___________________________________________________________________________
(in-package SC)
(defparameter *platform* 'linux) ;;; change to linux, macbook, imac
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *base-path*  
  (if (equal *platform* 'linux)
      (progn
	     (setq *base-path* "/home/jrsv/Dropbox/Pons-Percussion/LISP/HT/")
	     )
      (if (equal *platform* 'macbook) 
	  (setq *base-path* "/home/jrsv/Dropbox/Pons-Percussion/LISP/HT/")
	  (setq *base-path* "/Users/admin/odrive/WORK-Big/Pons-Percussion/LISP/HT/")
	  )))
(load (format nil "~a/jrsv-lib.lisp" *base-path*))
(load (format nil "~a/pons-parameters.lisp" *base-path*))
;;(load (format nil "~a/C2M-class.lisp" *base-path*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; '((
;; 4-1
;;(defparameter *env-top* '((70 0)(89 8)(60 8)(58 4)(54 8) (54 4) (56 4) (61 8) (68 4) (89 4) (85 12) (60 8))) 
;;(defparameter *env-bot* '((53 0)(89 8)(60 8)(55 4)(53 8) (53 4) (53 4) (55 8) (60 4) (60 4) (79 12) (55 8))) 
;; 4-2
;;(defparameter *env-top* '((65 0)(89 8)(58 8)(77 4)(65 8) (89 4) (60 4) (68 8) (57 4) (70 4) (60 12) (89 8))) 
;;(defparameter *env-bot* '((60 0)(63 8)(56 8)(56 4)(60 8) (54 4) (54 4) (60 8) (53 4) (53 4) (55 12) (70 8))) 
;; 4-4
;;(defparameter *env-top* '((63 0)(89 8)(71 8)(89 4)(69 8) (89 4) (75 4) (70 8) (68 4) (60 4) (55 12) (54 8))) 
;;(defparameter *env-bot* '((53 0)(53 8)(60 8)(65 4)(53 8) (53 4) (59 4) (59 8) (56 4) (53 4) (53 12) (53 8))) 
;; 4-11
;;(defparameter *env-top* '((89 0)(89 12)(89 4)(82 12)(71 4)(63 12))) ;;
;;(defparameter *env-bot* '((89 0) (77 6) (88 6) (88 4) (60 7) (55 14) (66 10))) ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4-9 (1)
;;(defparameter *env-top* '((65 0)(63 8)(60 8)(60 4)(58 8) (72 4) (53 4) (55 8) (50 4) (50 4) (53 12) (55 8))) 
;;(defparameter *env-bot* '((60 0)(60 8)(45 8)(53 4)(55 8) (45 4) (50 4) (49 8) (50 4) (45 4) (48 12) (50 8)))
;;4-28 (2)
;;(defparameter *env-top* '((65 0)(55 8)(71 8)(55 4)(69 8) (89 4) (75 4) (70 8) (68 4) (60 4) (55 12) (54 8))) 
;;(defparameter *env-bot* '((53 0)(53 8)(60 8)(53 4)(53 8) (53 4) (45 4) (59 8) (56 4) (45 4) (45 12) (45 8)))
;; 4-9 (3)
;;(defparameter *env-top* '((63 0)(63 8)(60 8)(60 4)(58 8) (72 4) (53 4) (55 8) (50 4) (50 4) (53 12) (55 8))) 
;;(defparameter *env-bot* '((45 0)(60 8)(45 8)(53 4)(55 8) (45 4) (50 4) (49 8) (50 4) (45 4) (48 12) (50 8)))
;;4-28 (4)
(defparameter *env-top* '((65 0)(55 8)(71 8)(55 4)(50 8) (89 4) (60 4) (60 8) (68 4) (60 4) (55 12) (54 8))) 
(defparameter *env-bot* '((45 0)(48 8)(48 8)(52 4)(45 8) (53 4) (45 4) (59 8) (56 4) (45 4) (45 12) (45 8)))
;; Gets the total amount of rhythm units in the envelope
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
(let* (;;(icv '(3 2 1 0 0 0)) ;;4-1 second
       ;;(icv '(2 2 1 1 0 0)) ;;4-2 third
       ;;(icv '(2 1 1 1 1 0)) ;;4-4 first
       ;;(icv '(2 1 1 1 1 0)) ;;4-11 fourth
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; for Vertigo
       ;;
       ;;(icv '(2 0 0 0 2 2)) ;; 4-9
       (icv '(0 0 4 0 0 2)) ;; 4-28
       (limits-top '())
       (limits-bot '())
       (interval-v '())
       (measures '())
       (music '())
       (bar '())
       (bars '())
       (t-sign '(64))
       initial-dir initial-pit curr-pitch note-dur direction
       top-limit bot-limit beat notes-list tmp-measure)
  (setq interval-v (all-intervals icv))
  (setq note-dur 64)
  (setq initial-pit 59)
  (setq initial-dir -1) ;;; or 1
  ;;;; save to file ;;;;
  (setq limits-top (limit-env *env-top* note-dur));; top limits
  (setq limits-top (nreverse limits-top))  
  (setq limits-bot (limit-env *env-bot* note-dur));; bot limits
  (setq limits-bot (nreverse limits-bot))  
  (setq direction initial-dir)  
  (setq curr-pitch initial-pit)
  (setq beat 0)
  (push initial-pit notes-list)
  ;;(format t "~%Beat ~d Steps ~d Pitch ~d Direction ~d" 0 0 initial-pit initial-dir)
  (loop repeat 8 do
       (loop for steps in interval-v do
	    (setq top-limit (nth beat limits-top)
		  bot-limit (nth beat limits-bot))
       ;;(format t "~% TOP-LIMIT : ~d ----- BOT-LIMIT : ~d" top-limit bot-limit)
	    (if (> steps 1)
		(progn 
		  (loop repeat steps do
		       (cond ((and (>= curr-pitch top-limit) (= direction 1))
			      (setq direction -1))
			     ((and (<= curr-pitch bot-limit) (= direction -1))
			      (setq direction 1)))	    
		       (setq curr-pitch (+ curr-pitch direction)))
		  (setq beat (+ beat 1))
		  (push curr-pitch notes-list))
		(progn
		  (setq curr-pitch (+ curr-pitch direction))
		  (setq beat (+ beat 1))
		  (push curr-pitch notes-list)))
	  ;; (format t "~%Beat ~d Steps ~d Pitch ~d Direction ~d Top-L ~d Bot-L ~d"
	  ;; 	    beat steps curr-pitch direction top-limit bot-limit)
	    
	    )
       )
  (setq notes-list (nreverse notes-list))
  (setq notes-list (trans-MIDI notes-list 0))
  (setq notes-list (2-note notes-list))
  (setq notes-list (pack-in-measures notes-list 8))
  ;;(format t "~%Note-List - MIDI : ~d" notes-list)
  (loop for measure in notes-list do
       (setq tmp-measure '())
       (loop for note in measure do
	    (if (not (equal note 'r))
		(progn ;;(print "NOTE")
		  (setq tmp-measure
			(push (make-event (make-pitch note) note-dur) tmp-measure))
		       )
		(progn ;;(print "REST")
		  (setq tmp-measure
			(push (make-event nil note-dur :is-rest t) tmp-measure))
		       )
		))
       (setq tmp-measure (nreverse tmp-measure))
       (push tmp-measure measures)
       )
  (setq measures (nreverse measures)) 
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
  (write-xml music)  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



