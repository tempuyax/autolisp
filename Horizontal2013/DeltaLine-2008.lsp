(defun GET:CreateTanLinePtLst (TriPointInputLst		     TanLine1Pt
			       TanLine2Pt     /		     Pt2
			       Ang1Rad	      Ang2Rad	     Dist1
			       Dist2	      DistTanLine1   DistTanLine2
			       TanLinePt1     TanLinePt2     PolyLineTanLine
			      )
  (setq	Pt2		(cdr (assoc "Pt2" TriPointInputLst))

	Ang1Rad		(cdr (assoc "Ang1Rad" TriPointInputLst))
	Ang2Rad		(cdr (assoc "Ang2Rad" TriPointInputLst))

	Dist1		(distance Pt2 TanLine1Pt)
	Dist2		(distance Pt2 TanLine2Pt)

	DistTanLine1	(* Dist1 0.8)
	DistTanLine2	(* Dist2 0.8)

	TanLinePt1	(polar Pt2 Ang1Rad DistTanLine1)
	TanLinePt2	(polar Pt2 Ang2Rad DistTanLine2)

	PolyLineTanLine	(list TanLinePt1 Pt2 TanLinePt2)
  ) ;_ end of setq
  PolyLineTanLine
) ;_ end of defun
  ;=========================================================================================
(defun SHOW:DrawingTanLine (TriPointInputLst		  TanLine1Pt
			    TanLine2Pt	   Color	  /
			    TanLinePtLst   BasePt	  BlockName
			   )
  (setq	TanLinePtLst (GET:CreateTanLinePtLst
		       TriPointInputLst
		       TanLine1Pt
		       TanLine2Pt
		     ) ;_ end of GET:CreateTanLinePtLst
	BasePt	     (cdr (assoc "Pt2" TriPointInputLst))
	BlockName    "deltah"
  ) ;_ end of setq
  (GP:ENTMAKE-POLYLINE TanLinePtLst Color)
  (GP:ENTMAKE-INSERT BasePt BlockName)
) ;_ end of defun
  ;=========================================================================================
(defun SHOW:DrawingTanLineFC (TriPointInputLst		      GetCoordFCLst
			      Color	      /		      TanLine1Pt
			      TanLine2Pt
			     )
  (setq
    TanLine1Pt (cdr (assoc "TC-Coord" GetCoordFCLst))
    TanLine2Pt (cdr (assoc "CT-Coord" GetCoordFCLst))
  ) ;_ end of setq
  (SHOW:DrawingTanLine
    TriPointInputLst
    TanLine1Pt
    TanLine2Pt
    Color
  ) ;_ end of SHOW:DrawingTanLine
) ;_ end of defun
  ;=========================================================================================
(defun SHOW:DrawingTanLineSCS (TriPointInputLst		       GetCoordSCSLst
			       Color	       /	       TanLine1Pt
			       TanLine2Pt
			      )
  (setq
    TanLine1Pt (cdr (assoc "TS-coord" GetCoordSCSLst))
    TanLine2Pt (cdr (assoc "ST-coord" GetCoordSCSLst))
  ) ;_ end of setq
  (SHOW:DrawingTanLine
    TriPointInputLst
    TanLine1Pt
    TanLine2Pt
    Color
  ) ;_ end of SHOW:DrawingTanLine
) ;_ end of defun
  ;=========================================================================================
(defun SHOW:DrawingTanLineSS (TriPointInputLst		      GetCoordSSLst
			      Color	      /		      TanLine1Pt
			      TanLine2Pt
			     )
  (setq
    TanLine1Pt (cdr (assoc "TS-coord" GetCoordSSLst))
    TanLine2Pt (cdr (assoc "ST-coord" GetCoordSSLst))
  ) ;_ end of setq
  (SHOW:DrawingTanLine
    TriPointInputLst
    TanLine1Pt
    TanLine2Pt
    Color
  ) ;_ end of SHOW:DrawingTanLine
) ;_ end of defun
