(defun SHOW:LineSTA (TriPointInputLst	GetObjectLst StringTC      StringCT
		     TanLine1Pt	   TanLine2Pt	 / Rc Rc%      DistLineIn
		     TxtDist	   TxtHeight	 DistLineOut   LColor
		     TColor	   AngLine1	 AngLine2      LineIn1Pt
		     LineOut1Pt	   LineIn2Pt	 LineOut2Pt    TxtPosTCPt
		     TxtPosCTPt 
		    )
  (Setq Rc          (cdr (assoc "Rc" GetObjectLst))
	Rc%         Rc
	DistLineOut 8.0
        DistLineIn  (min (- Rc% (/ Rc 3))  (* DistLineOut 3))    
	LColor	    5

	TxtDist	    10.5
	TxtHeight   2.5
	TColor	    71

	AngLine1    (cdr (assoc "AngKP1Rad" TriPointInputLst))
	AngLine2    (cdr (assoc "AngKP2Rad" TriPointInputLst))

	LineIn1Pt   (polar TanLine1Pt AngLine1 DistLineIn)
	LineOut1Pt  (polar TanLine1Pt (+ AngLine1 PI) DistLineOut)

	LineIn2Pt   (polar TanLine2Pt AngLine2 DistLineIn)
	LineOut2Pt  (polar TanLine2Pt (+ AngLine2 PI) DistLineOut)

	TxtPosTCPt  (polar TanLine1Pt (+ AngLine1 PI) TxtDist)
	TxtPosCTPt  (polar TanLine2Pt (+ AngLine2 PI) TxtDist)

  ) ;_ end of Setq
  (GP:ENTMAKE-LINE LineIn1Pt LineOut1Pt LColor)
  (GP:ENTMAKE-LINE LineIn2Pt LineOut2Pt LColor)
  (GP:ENTMAKE-TEXT
    TxtPosTCPt
    TxtHeight
    (+ AngLine1 (* PI 0.5))
    TColor
    (LOGIC:GetLogicValue TriPointInputLst StringTC StringCT)
  ) ;_ end of GP:ENTMAKE-TEXT
  (GP:ENTMAKE-TEXT
    TxtPosCTPt
    TxtHeight
    (+ AngLine2 (* PI 0.5))
    TColor
    (LOGIC:GetLogicValue TriPointInputLst StringCT StringTC)
  ) ;_ end of GP:ENTMAKE-TEXT
) ;_ end of defun
  ;=======================================================================
(defun SHOW:LineSTAArcSCS (TriPointInputLst	       GetS-C-SLst   GetCoordSCSLst
			   StringSC	 StringCS      /	     DistLineIn
			   DistLineOut	 LColor	       TColor	     Radius
			   Pt		 y1Pt	       y2Pt	     RadESPt
			   AngLine1	 AngLine2      LineIn1Pt     LineOut1Pt
			   LineIn2Pt	 LineOut2Pt    TxtDist	     TxtHeight
			  )
  (Setq	DistLineIn  38.0
	DistLineOut 8.0
	LColor	    5

	TxtDist	    10.5
	TxtHeight   2.5
	TColor	    71

	RadESPt	    (cdr (assoc "CptAng" TriPointInputLst))
	Radius	    (cdr (assoc "Rc" GetS-C-SLst))
	Pt	    (cdr (assoc "Es-Coord" GetCoordSCSLst))
	y1Pt	    (cdr (assoc "y1-coord" GetCoordSCSLst))
	y2Pt	    (cdr (assoc "y2-coord" GetCoordSCSLst))

	CenterPt    (polar Pt RadESPt Radius)

	AngLine1    (angle y1Pt CenterPt)
	AngLine2    (angle y2Pt CenterPt)

	LineIn1Pt   (polar y1Pt AngLine1 DistLineIn)
	LineOut1Pt  (polar y1Pt (+ AngLine1 PI) DistLineOut)

	LineIn2Pt   (polar y2Pt AngLine2 DistLineIn)
	LineOut2Pt  (polar y2Pt (+ AngLine2 PI) DistLineOut)

	TxtPosTCPt  (polar y1Pt (+ AngLine1 PI) TxtDist)
	TxtPosCTPt  (polar y2Pt (+ AngLine2 PI) TxtDist)
  ) ;_ end of Setq
  (GP:ENTMAKE-LINE LineIn1Pt LineOut1Pt LColor)
  (GP:ENTMAKE-LINE LineIn2Pt LineOut2Pt LColor)
  (GP:ENTMAKE-TEXT
    TxtPosTCPt
    TxtHeight
    (+ AngLine1 (* PI 0.5))
    TColor
    (LOGIC:GetLogicValue TriPointInputLst StringSC StringCS)
  ) ;_ end of GP:ENTMAKE-TEXT
  (GP:ENTMAKE-TEXT
    TxtPosCTPt
    TxtHeight
    (+ AngLine2 (* PI 0.5))
    TColor
    (LOGIC:GetLogicValue TriPointInputLst StringCS StringSC)
  ) ;_ end of GP:ENTMAKE-TEXT
) ;_ end of defun
  ;=======================================================================
(defun SHOW:LineSTA-FC (TriPointInputLst GetFCLst	GetCoordFCLst
			/		TanLine1Pt	TanLine2Pt
			StringTC	StringCT
		       )
  (setq
    TanLine1Pt
     (cdr (assoc "TC-Coord" GetCoordFCLst))
    TanLine2Pt
     (cdr (assoc "CT-Coord" GetCoordFCLst))
    StringTC "TC"
    StringCT "CT"
  ) ;_ end of setq
  (SHOW:LineSTA
    TriPointInputLst GetFCLst StringTC StringCT TanLine1Pt TanLine2Pt) ;_ end of SHOW:LineSTA
 ;_ end of SHOW:LineSTA
 ;_ end of SHOW:LineSTA
) ;_ end of defun
  ;=======================================================================
(defun SHOW:LineSTA-SCS	(TriPointInputLst		 GetS-C-SLst
			 GetCoordSCSLst	 /		 TanLine1Pt
			 TanLine2Pt	 StringTC	 StringCT
			 StringSC	 StringCS
			)
  (setq
    TanLine1Pt
     (cdr (assoc "TS-coord" GetCoordSCSLst))
    TanLine2Pt
     (cdr (assoc "ST-coord" GetCoordSCSLst))
    StringTC "TS"
    StringCT "ST"
    StringSC "SC"
    StringCS "CS"
  ) ;_ end of setq
  (SHOW:LineSTA
    TriPointInputLst GetS-C-SLst StringTC StringCT TanLine1Pt TanLine2Pt) ;_ end of SHOW:LineSTA
  (SHOW:LineSTAArcSCS
    TriPointInputLst GetS-C-SLst GetCoordSCSLst	StringSC StringCS) ;_ end of SHOW:LineSTAArcSCS
) ;_ end of defun
  ;=======================================================================
(defun SHOW:LineSTA-SS (TriPointInputLst GetS-SLst GetCoordSSLst / TanLine1Pt TanLine2Pt)
  (setq
    TanLine1Pt
     (cdr (assoc "TS-coord" GetCoordSSLst))
    TanLine2Pt
     (cdr (assoc "ST-coord" GetCoordSSLst))
    StringTC "TS"
    StringCT "ST"
  ) ;_ end of setq
  (SHOW:LineSTA
    TriPointInputLst GetS-SLst StringTC StringCT TanLine1Pt TanLine2Pt) ;_ end of SHOW:LineSTA
 ;_ end of SHOW:LineSTA
 ;_ end of SHOW:LineSTA
) ;_ end of defun
  ;=================Tangen Text==========================================
