(defun GET:GetBulge (Cr Rc Ls / 1/2Alfa BulgeFactor)
  (setq
    1/2Alfa	(/ Ls (* Cr 200. Rc))
    BulgeFactor	(/ (- 1 (cos 1/2Alfa)) (sin 1/2Alfa))
  ) ;_ end of setq
  (list BulgeFactor)
) ;_ end of defun
  ;======================================================
(defun GET:DeltaXY (LastPt Cro Tn Rc Ls / Ro <b dx dy DeltaPt)
  (setq
    Ro	    (* Cro 10. Rc (sin (/ Ls (* Cro 100. Rc))))
    <b	    (/ (* Tn Ls) (* 1200. Rc))
    dx	    (+ (car LastPt) (* Ro (cos <b)))
    dy	    (+ (cadr LastPt) (* Ro (sin <b)))
    DeltaPt (list dx dy 0.0)
  ) ;_ end of setq
  DeltaPt
) ;_ end of defun
  ;====================================
(defun GET:BulgeLst (Rc Ls / B1 B2 B3 B4 B5 B6 B7 B8 B9 B10 B11 BulgeRslt)
  (setq
    B1	      (GET:GetBulge 3. Rc Ls)
    B2	      (GET:GetBulge (/ 2. 3.) Rc Ls)
    B3	      (GET:GetBulge (/ 2. 5.) Rc Ls)
    B4	      (GET:GetBulge (/ 2. 7.) Rc Ls)
    B5	      (GET:GetBulge (/ 2. 9.) Rc Ls)
    B6	      (GET:GetBulge (/ 2. 11.) Rc Ls)
    B7	      (GET:GetBulge (/ 2. 13.) Rc Ls)
    B8	      (GET:GetBulge (/ 2. 15.) Rc Ls)
    B9	      (GET:GetBulge (/ 2. 17.) Rc Ls)
    B10	      (GET:GetBulge (/ 3. 29.) Rc Ls)
    B11	      (GET:GetBulge (/ 1. 10.) Rc Ls)
    BulgeRslt (list B1 B2 B3 B4 B5 B6 B7 B8 B9 B10 B11)
  ) ;_ end of setq
  BulgeRslt
) ;_ end of defun
  ;==============================================================
(defun GET:SpiralPt (BasePt Rc Ls / SpiralPtLst	Pt1 Pt2	Pt3 Pt4	Pt5 Pt6	Pt7 Pt8	Pt9
		     Pt10)
  (setq
    Pt1		(GET:DeltaXY BasePt 6. 2. Rc Ls)
    Pt2		(GET:DeltaXY Pt1 (/ 4. 3.) 14. Rc Ls)
    Pt3		(GET:DeltaXY Pt2 (/ 4. 5.) 38. Rc Ls)
    Pt4		(GET:DeltaXY Pt3 (/ 4. 7.) 74. Rc Ls)
    Pt5		(GET:DeltaXY Pt4 (/ 4. 9.) 122. Rc Ls)
    Pt6		(GET:DeltaXY Pt5 (/ 4. 11.) 182. Rc Ls)
    Pt7		(GET:DeltaXY Pt6 (/ 4. 13.) 254. Rc Ls)
    Pt8		(GET:DeltaXY Pt7 (/ 4. 15.) 338. Rc Ls)
    Pt9		(GET:DeltaXY Pt8 (/ 4. 17.) 434. Rc Ls)
    Pt10	(GET:DeltaXY Pt9 (/ 6. 29.) 542. Rc Ls)
    SpiralPtLst	(list BasePt Pt1 Pt2 Pt3 Pt4 Pt5 Pt6 Pt7 Pt8 Pt9 Pt10)
  ) ;_ end of setq
  SpiralPtLst
) ;_ end of defun
  ;=======================================================
(defun GET:Rotate-Pt&Bulge (TriPointInputLst	    ClothoidLst	BulgeLst
			    flangLine	/	    AngPt	m
			    Ang		Dist	    SepiralPt	SepiralPtLst
			    BasePt	ct	    DataLst	Bulge
			    nB
			   )
  (if (eq flangLine "L1")
    (progn
      (setq
	AngPt (cdr (assoc "Ang12" TriPointInputLst))
      ) ;_ end of setq
    ) ;_ end of progn
    (progn
      (setq
	AngPt (cdr (assoc "Ang32" TriPointInputLst))
      ) ;_ end of setq
    ) ;_ end of progn
  ) ;_ end of if
  (setq
    BasePt (car ClothoidLst)
    ct	   0
  ) ;_ end of setq
  (repeat (length ClothoidLst)
    (setq
  ;**************************Rotation-Coord*******************
      m		   (angle
		     BasePt
		     (nth ct ClothoidLst)
		   ) ;_ end of angle
  ;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      Ang
		   (if (eq flangLine "L1")
		     (LOGIC:GetLogicValue
		       TriPointInputLst
		       (- AngPt m)
		       (+ AngPt m)
		     ) ;_ end of LOGIC:GetLogicValue
		     (LOGIC:GetLogicValue
		       TriPointInputLst
		       (+ AngPt m)
		       (- AngPt m)
		     ) ;_ end of LOGIC:GetLogicValue
		   ) ;_ end of if
      Dist	   (distance
		     BasePt
		     (nth ct ClothoidLst)
		   ) ;_ end of distance
      SepiralPt	   (polar BasePt Ang Dist)
  ;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      nB	   (if (eq flangLine "L1")
		     (LOGIC:GetLogicValue
		       TriPointInputLst
		       -1
		       +1
		     ) ;_ end of LOGIC:GetLogicValue
		     (LOGIC:GetLogicValue
		       TriPointInputLst
		       +1
		       -1
		     ) ;_ end of LOGIC:GetLogicValue
		   ) ;_ end of if
      Bulge	   (* nB (car (nth ct BulgeLst)))
  ;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      SepiralPtLst (append SepiralPtLst
			   (list
			     (list
			       (cons "Pt" SepiralPt)
			       (cons "Bulge" Bulge)
			     ) ;_ end of list
			   ) ;_ end of list
		   ) ;_ end of append
      ct	   (1+ ct)
    ) ;_ end of setq
  ) ;_ end of repeat
  SepiralPtLst
) ;_ end of defun
  ;**********************************************************



