  ;========================================================
(defun GET:GetCoordFC (TriPointInputLst	       GetFCLst	  SF /	       BasePt
		       Ang1Rad	   Ang2Rad     CptAng	   Tc	       Ec
		       EC_Coord	   TC_Coord    CT_Coord
		      )
  (setq
    BasePt   (cdr (assoc "BasePt" TriPointInputLst))
    Ang1Rad  (cdr (assoc "Ang1Rad" TriPointInputLst))
    Ang2Rad  (cdr (assoc "Ang2Rad" TriPointInputLst))
    CptAng   (cdr (assoc "CptAng" TriPointInputLst))

    Tc	     (* (cdr (assoc "Tc" GetFCLst)) SF)
    Ec	     (* (cdr (assoc "Ec" GetFCLst)) SF)

    EC_Coord (polar BasePt CptAng Ec)
    TC_Coord (polar BasePt Ang1Rad Tc)
    CT_Coord (polar BasePt Ang2Rad Tc)

  ) ;_ end of setq
  (list
    (cons "TC-Coord" TC_Coord)
    (cons "CT-Coord" CT_Coord)
    (cons "EC-Coord" EC_Coord)
  ) ;_ end of list
) ;_ end of defun
  ;========================================================
(defun GET:GetCoordS-C-S (TriPtLst   S-C-SDataLst	   SF	      /
			  Ang1Rad    Ang2Rad	Pt2	   Ts	      Es
			  Xs	     Ys		k	   p	      AngESPtRad
			  Angyp1Pt   Angyp2Pt	TS-coord   x1-coord   k1-coord
			  ST-coord   x2-coord	k2-coord   y1-coord   p1-coord
			  y2-coord   p2-coord	Es-coord
			 )

  (setq	Ang1Rad	   (cdr (assoc "Ang1Rad" TriPtLst))
	Ang2Rad	   (cdr (assoc "Ang2Rad" TriPtLst))
	Pt2	   (cdr (assoc "Pt2" TriPtLst)) ;Same as BasePt

	Ts	   (* (cdr (assoc "Ts" S-C-SDataLst)) SF)
	Es	   (* (cdr (assoc "Es" S-C-SDataLst)) SF)
	Xs	   (* (cdr (assoc "Xs" S-C-SDataLst)) SF)
	Ys	   (* (cdr (assoc "Ys" S-C-SDataLst)) SF)
	k	   (* (cdr (assoc "k" S-C-SDataLst)) SF)
	p	   (* (cdr (assoc "p" S-C-SDataLst)) SF)

	AngESPtRad (cdr (assoc "CptAng" TriPtLst))
	Angyp1Pt   (cdr (assoc "AngKP1Rad" TriPtLst))
	Angyp2Pt   (cdr (assoc "AngKP2Rad" TriPtLst))

	TS-coord   (polar Pt2 Ang1Rad Ts)
	x1-coord   (polar Pt2 Ang1Rad (abs (- Xs Ts)))
	k1-coord   (polar Pt2 Ang1Rad (abs (- k Ts)))

	ST-coord   (polar Pt2 Ang2Rad Ts)
	x2-coord   (polar Pt2 Ang2Rad (abs (- Xs Ts)))
	k2-coord   (polar Pt2 Ang2Rad (abs (- k Ts)))

	y1-coord   (polar x1-coord Angyp1Pt Ys)
	p1-coord   (polar k1-coord Angyp1Pt p)

	y2-coord   (polar x2-coord Angyp2Pt Ys)
	p2-coord   (polar k2-coord Angyp2Pt p)

	Es-coord   (polar Pt2 AngESPtRad Es)
  ) ;_ end of setq

  (list
    (cons "TS-coord" TS-coord)
    (cons "x1-coord" x1-coord)
    (cons "k1-coord" k1-coord)

    (cons "ST-coord" ST-coord)
    (cons "x2-coord" x2-coord)
    (cons "k2-coord" k2-coord)

    (cons "y1-coord" y1-coord)
    (cons "p1-coord" p1-coord)

    (cons "y2-coord" y2-coord)
    (cons "p2-coord" p2-coord)

    (cons "Es-Coord" Es-coord)
  ) ;_ end of list
) ;_ end of defun
  ;========================================================
(defun GET:GetCoordS-S (TriPtLst   S-SDataLst SF	 /	    Ang1Rad
			Ang2Rad	   Pt2	      Ts	 Es	    k
			p	   AngESPtRad Angyp1Pt	 Angyp2Pt   TS-coord
			k1-coord   ST-coord   k2-coord	 p1-coord   p2-coord
			Es-coord
		       )

  (setq	Ang1Rad	   (cdr (assoc "Ang1Rad" TriPtLst))
	Ang2Rad	   (cdr (assoc "Ang2Rad" TriPtLst))
	Pt2	   (cdr (assoc "Pt2" TriPtLst))

	Ts	   (* (cdr (assoc "Ts" S-SDataLst)) SF)
	Es	   (* (cdr (assoc "Es" S-SDataLst)) SF)
	k	   (* (cdr (assoc "k" S-SDataLst)) SF)
	p	   (* (cdr (assoc "p" S-SDataLst)) SF)

	AngESPtRad (cdr (assoc "CptAng" TriPtLst))
	Angyp1Pt   (cdr (assoc "AngKP1Rad" TriPtLst))
	Angyp2Pt   (cdr (assoc "AngKP2Rad" TriPtLst))

	TS-coord   (polar Pt2 Ang1Rad Ts)
	k1-coord   (polar Pt2 Ang1Rad (abs (- k Ts)))

	ST-coord   (polar Pt2 Ang2Rad Ts)
	k2-coord   (polar Pt2 Ang2Rad (abs (- k Ts)))

	p1-coord   (polar k1-coord Angyp1Pt p)
	p2-coord   (polar k2-coord Angyp2Pt p)

	Es-coord   (polar Pt2 AngESPtRad Es)
  ) ;_ end of setq

  (list
    (cons "TS-coord" TS-coord)
    (cons "k1-coord" k1-coord)

    (cons "ST-coord" ST-coord)
    (cons "k2-coord" k2-coord)

    (cons "p1-coord" p1-coord)
    (cons "p2-coord" p2-coord)

    (cons "Es-Coord" Es-coord)
  ) ;_ end of list
) ;_ end of defun
  ;========================================================
(defun GP:CreatePoint (PtLst)
  (mapcar
    '(lambda (Pt) ;define lambda expression
       (GP:ENTMAKE-POINT Pt) ;place number at point pt
     ) ;_ end of lambda
    PtLst ;list supplied to lambda
  ) ;_ end of mapcar
) ;_ end of defun
  ;========================================================
(defun CDRfuction (AssocData) (list (cdr AssocData)))
  ;========================================================
(defun SHOW:DrawingPoint (ListData)
  (GP:CreatePoint
    (apply 'append (mapcar 'CDRfuction ListData))
  ) ;_ end of GP:CreatePoint
) ;_ end of defun
;=============================================================
