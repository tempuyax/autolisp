(defun c:ModTxtSTA
	      (/ Txtlayer Star-AND End-AND Catl)

  (defun DelTxtPCPT (s / ls)
    (setq ls (list "PC" "PT" "PRC" "=" ""))
    (defun DelStr (ls s)
      (if (null ls)
	s
	(progn
	  (setq s (vl-string-subst "" (car ls) s))
	  (DelStr (cdr ls) s)
	) ;_ end of progn
      ) ;_ end of if
    ) ;_ end of defun
    (vl-string-trim " " (DelStr ls s))
  ) ;_ end of defun
  
  (defun StaTxtMod (e)
    (entmod
      (subst (cons 1 (DelTxtPCPT (cdr (assoc 1 (entget e)))))
	     (assoc 1 (entget e))
	     (entget e)
      ) ;_ end of subst
    ) ;_ end of entmod
    (entupd e)
  ) ;_ end of defun

  (defun enttxt	(elst)
    (if	(null elst)			; via satu data jika dua data pakai (cdr elst)
      t
      (progn
	(StaTxtMod (car elst))
	(enttxt (cdr elst))
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of defun

  (defun SelCatl (Catl / ss res)
    (defun GetEntName (ss ct)
      (if (>= ct (sslength ss))
	(list)
	(cons
	  (ssname ss ct)
	  (GetEntName ss (1+ ct))
	) ;_ end of cons
      ) ;_ end of if
    ) ;_ end of defun
    (GetEntName (ssget Catl) 0)
  ) ;_ end of defun

  (setq
    Txtlayer "TXTPTS"
    Star-AND (cons -4 "<AND")
    End-AND  (cons -4 "AND>")
    Catl
	     (list
	       Star-AND
	       (cons 0 "TEXT")
	       (cons 8 Txtlayer)
	       End-AND
	     ) ;_ end of list
  ) ;_ end of setq
  (enttxt (SelCatl Catl))
  (princ)
) ;_ end of defun


