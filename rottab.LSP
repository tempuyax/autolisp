(vl-load-com)
(defun c:rottab	(/ es ed esl edl New old)
  (if (setq es (car (entsel "Select Source from hwin : ")))
    (if	(setq ed (car (entsel "\nSelect Distination to table : ")))
      (progn
	(setq
	  esl (entget es)
	  edl (entget ed)
	) ;_ end of setq
	(if
	  (and
	    (eq (cdr (assoc 0 esl)) "INSERT")
	    ;(or
	      (eq (cdr (assoc 2 esl)) "hwin")
	      ;(eq (cdr (assoc 2 edl)) "fctable")
	    ;) ;_ end of or
	  ) ;_ end of and
	   (progn
	     (setq New (cdr (assoc 50 esl)))
	     (setq Old (cdr (assoc 50 edl)))
	     (setq bpt (cdr (assoc 10 edl))) ; base point of block 
	     (LM:RotateByMatrix		; Lee mac function
	       (vlax-ename->vla-object ed) ; nama entitiy
	       bpt			;koordinat tidak perlu di translate
	       (- New Old)		; sudut relatif menjadi statif
	     ) ;_ end of LM:RotateByMatrix
	     (prompt "\nDONE")
	   ) ;_ end of progn
	   (prompt "\n*** Selection is not specification ***")
	) ;_ end of if
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of if
  (princ)
) ;_ end of defun

(defun c:rottxt	(/ ed edl New old)
  (if (setq ed (car (entsel "Select of text : ")))
    ;;(if	(setq bpt (getpoint "\nBase Point : "))
      (progn
	(setq
	  edl (entget ed)
	) ;_ end of setq
	(if
	  (and
	    (eq (cdr (assoc 0 edl)) "TEXT")
	    (eq (cdr (assoc 8 edl)) "STA")
	  ) ;_ end of and
	   (progn
	     (setq Old (cdr (assoc 50 edl)))
	     (setq bpt (cdr (assoc 11 edl)))
	     (setq New (* 1 pi)); 180d
	     (LM:RotateByMatrix		; Lee mac function
	       (vlax-ename->vla-object ed) ; nama entitiy
	       bpt			;koordinat tidak perlu di translate
	       New			; sudut tujuan
	     ) ;_ end of LM:RotateByMatrix
	     (prompt "\nDONE")
	   ) ;_ end of progn
	   (prompt "\n*** Selection is not specification ***")
	) ;_ end of if
      ) ;_ end of progn
    ;;) ;_ end of if
  ) ;_ end of if
  (princ)
) ;_ end of defun
