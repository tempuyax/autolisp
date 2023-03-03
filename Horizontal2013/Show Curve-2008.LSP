(defun FC:GetCircle (TriPointInputLst	       GetFCLst	    /
		     DeltaAng	  BasePt       CptAng	    Rc
		     Ec		  DistCpt      CptArcPt	    AngCenterPtRad
		     StartAng	  EndAng
		    )
  (setq
    DeltaAng	   (cdr (assoc "DeltaAng" TriPointInputLst))
    BasePt	   (cdr (assoc "BasePt" TriPointInputLst))
    CptAng	   (cdr (assoc "CptAng" TriPointInputLst))

    Rc		   (cdr (assoc "Rc" GetFCLst))
    Ec		   (cdr (assoc "Ec" GetFCLst))

    DistCpt	   (+ Ec Rc)

    CptArcPt	   (polar BasePt CptAng DistCpt)
    AngCenterPtRad (angle CptArcPt BasePt)

    StartAng	   (- AngCenterPtRad (* 0.5 DeltaAng))
    EndAng	   (+ AngCenterPtRad (* 0.5 DeltaAng))
  ) ;_ end of setq
  (list
    (cons "Cen-Pt" CptArcPt)
    (cons "Rc" Rc)
    (cons "StartAng" StartAng)
    (cons "EndAng" EndAng)
  ) ;_ end of list
) ;_ end of defun
  ;=====================================for all Arc scs & FC Type========
(defun SHOW:ShowCircle (GetCircleLst / CenterPt Radius StartAng EndAng Color)
  (setq
    CenterPt (cdr (assoc "Cen-Pt" GetCircleLst))
    Radius   (cdr (assoc "Rc" GetCircleLst))
    StartAng (cdr (assoc "StartAng" GetCircleLst))
    EndAng   (cdr (assoc "EndAng" GetCircleLst))
    Color    3
  ) ;_ end of setq
  ;(print "Show SCS Circle :")
  ;(foreach n CircleLst (print n))
  (ENTMAKE:ARC CenterPt Radius StartAng EndAng Color)
) ;_ end of defun
  ;==============================================
(defun SHOW:TwoSpiral (TriPointInputLst		 GetS-C-SLst  /
		       Rc	    Ls		 Ts	      Ang1Rad
		       Ang2Rad	    BasePt	 BasePtL1     BasePtL2
		       SpiralPtLstL1		 SpiralPtLstL2
		       BulgeLst	    Rot1	 Rot2
		      )
  (setq
    Rc		  (cdr (assoc "Rc" GetS-C-SLst))
    Ls		  (cdr (assoc "Ls" GetS-C-SLst))
    Ts		  (cdr (assoc "Ts" GetS-C-SLst))
    Ang1Rad	  (cdr (assoc "Ang1Rad" TriPointInputLst))
    Ang2Rad	  (cdr (assoc "Ang2Rad" TriPointInputLst))
    BasePt	  (cdr (assoc "BasePt" TriPointInputLst))

    BasePtL1	  (polar BasePt Ang1Rad Ts)
    BasePtL2	  (polar BasePt Ang2Rad Ts)

    SpiralPtLstL1 (GET:SpiralPt BasePtL1 Rc Ls)
    SpiralPtLstL2 (GET:SpiralPt BasePtL2 Rc Ls)
    BulgeLst	  (GET:BulgeLst Rc Ls)

    Rot1	  (GET:Rotate-Pt&Bulge
		    TriPointInputLst
		    SpiralPtLstL1
		    BulgeLst
		    "L1"
		  ) ;_ end of GET:Rotate-Pt&Bulge
    Rot2	  (GET:Rotate-Pt&Bulge
		    TriPointInputLst
		    SpiralPtLstL2
		    BulgeLst
		    "L2"
		  ) ;_ end of GET:Rotate-Pt&Bulge
  ) ;_ end of setq
  (GP:ENTMAKE-POLYLINEARC Rot1)
  (GP:ENTMAKE-POLYLINEARC Rot2)
) ;_ end of defun
  ;=======================================
(defun SCS:GetCircle (TriPointInputLst		GetS-C-SLst  /
		      BasePt	   CptAng	Rc	     Es
		      Qc	   DistCpt	CptArcPt     EsPt
		      AngCenterPtRad		StartAng     EndAng
		     )
  (setq
    BasePt	   (cdr (assoc "BasePt" TriPointInputLst))
    CptAng	   (cdr (assoc "CptAng" TriPointInputLst))

    Rc		   (cdr (assoc "Rc" GetS-C-SLst))
    Es		   (cdr (assoc "Es" GetS-C-SLst))
    Qc		   (cdr (assoc "Qc" GetS-C-SLst))

    DistCpt	   (+ Es Rc)

    CptArcPt	   (polar BasePt CptAng DistCpt)
    EsPt	   (polar BasePt CptAng Es)

    AngCenterPtRad (angle CptArcPt BasePt)

    StartAng	   (- AngCenterPtRad (* 0.5 Qc))
    EndAng	   (+ AngCenterPtRad (* 0.5 Qc))
  ) ;_ end of setq
  (list
    (cons "Cen-Pt" CptArcPt)
    (cons "Rc" Rc)
    (cons "StartAng" StartAng)
    (cons "EndAng" EndAng)
  ) ;_ end of list
) ;_ end of defun
  ;==============================================


