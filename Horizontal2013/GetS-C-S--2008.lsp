(defun GET:GetS-C-S (TriPointInputLst	 RcInputLst	     
		     SF	       /	 Delta	   Rc	     Ls	       Qs
		     Qc	       p	 k	   Xs	     Ys	       Ts
		     Es	       Lc	 L	   SpiralPtLst
		    )
  (setq	Delta	    (cdr (assoc "DeltaAng" TriPointInputLst))
	Rc	    (cdr (assoc "Rc" RcInputLst))
	Ls	    (cdr (assoc "LsMin" (SUPERELEV:Auto-Vr-For-LsMin RcInputLst)));;(cdr (assoc "Ls" LsInputLst))
	SpiralPtLst (GET:SpiralPt '(0. 0. 0.) Rc Ls)

	Qs	    (/ Ls (* 2. Rc))
	Qc	    (- Delta (* 2.0 Qs))

	Xs	    (car (last SpiralPtLst))
	Ys	    (cadr (last SpiralPtLst))

	p	    (- Ys (* Rc (- 1.0 (cos Qs)))) ;Y axis
	k	    (- Xs (* Rc (sin Qs))) ;X axis

	Ts	    (+ (* (+ Rc p) (TAN (* Delta 0.5))) k)
	Es	    (- (* (+ Rc p) (SEC (* Delta 0.5))) Rc)
	Lc	    (* Qc Rc)
	L	    (+ Lc (* 2.0 Ls))
  ) ;End of setq
  (list
    (cons "Rc" Rc)
    (cons "Ls" Ls)
    (cons "Delta" Delta)
    (cons "Qs" Qs)
    (cons "Qc" Qc)
    (cons "Lc" Lc)
    (cons "L" L)
    (cons "p" p)
    (cons "k" k)
    (cons "Es" Es)
    (cons "Ts" Ts)
    (cons "Xs" Xs)
    (cons "Ys" Ys)
  ) ;_ end of list
) ;_ end of defun
  ;========================================================

