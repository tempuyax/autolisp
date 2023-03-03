(defun GET:GetS-SLst (TriPointInputLst	  RcInputLst	      /		Delta
		      Rc	Ls	  Qs	    p	      k		Ts
		      Es	L	  SpiralPtLst
		     )
  (setq	Delta	    (cdr (assoc "DeltaAng" TriPointInputLst))
	Rc	    (cdr (assoc "Rc" RcInputLst))

	Qs	    (/ Delta 2.)
	Ls	    (* 2. Qs Rc) ;in Radians
	L	    (* 2. Ls)

	SpiralPtLst (GET:SpiralPt '(0. 0. 0.) Rc Ls)

	Xs	    (car (last SpiralPtLst))
	Ys	    (cadr (last SpiralPtLst))

	p	    (- Ys (* Rc (- 1.0 (cos Qs)))) ;Y axis
	k	    (- Xs (* Rc (sin Qs))) ;X axis

	Ts	    (+ (* (+ Rc p) (TAN (* Delta 0.5))) k)
	Es	    (- (* (+ Rc p) (SEC (* Delta 0.5))) Rc)
  ) ; end of setq
  (list
    (cons "Rc" Rc)
    (cons "Ls" Ls)
    (cons "Delta" Delta)
    (cons "Qs" Qs)
    (cons "L" L)
    (cons "p" p)
    (cons "k" k)
    (cons "Es" Es)
    (cons "Ts" Ts)
  ) ;_ end of list
) ;end of defun