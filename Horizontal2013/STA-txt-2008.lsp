(defun SET:GetElemen (Ass Lst)
  (cdr (assoc Ass Lst))
) ;_ end of defun
  ;==========================================================
(defun INIT:GetPropSTATextLst (/ TxtOffs TxtDist TxtHeight Color Layer)
  (setq
    TxtOffs   2.5
    TxtDist   24.0
    TxtHeight 2.5
    Color     8
    Layer     "HORIZONTAL-TANSTALBL"
  ) ;_ end of setq
  (list
    (cons 'TXTOFFS TxtOffs)
    (cons 'TXTDIST TxtDist)
    (cons 'TXTHEIGHT TxtHeight)
    (cons 'COLOR Color)
    (cons 'LAYER Layer)
  ) ;_ end of list
) ;_ end of defun
  ;==========================================================
(defun STA:CreateSTATextFC (TriPointInputLst	    FCDataLst
			    CoordFCLst	    /		    TxtDist
			    TxtHeight	    StringStart-STA StringEnd-STA
			    STA-TCPt	    STA-CTPt	    AngLine1
			    AngLine2	    AngTxtLine1	    AngTxtLine2
			    TxtPosSTA-TCPt  TxtPosSTA-CTPt  Color
			    STALst	    Start-STA	    End-STA
			    TxtOffs	    PropSTATextLst
			   )


  (if (setq STALst (STA-FC:GetValueSTALst TriPointInputLst FCDataLst))
    (progn
      (setq
	PropSTATextLst	(INIT:GetPropSTATextLst)

	TxtOffs		(SET:GetElemen 'TXTOFFS PropSTATextLst)
	TxtDist		(SET:GetElemen 'TXTDIST PropSTATextLst)
	TxtHeight	(SET:GetElemen 'TXTHEIGHT PropSTATextLst)
	Color		(SET:GetElemen 'COLOR PropSTATextLst)

	Start-STA	(SET:GetElemen 'START-STA STALst)
	End-STA		(SET:GetElemen 'END-STA STALst)

	StringStart-STA	(strcat	"STA. "
				(STA:RealToAlfaNumSTA Start-STA 2)
			) ;_ end of strcat
	StringEnd-STA	(strcat "STA. " (STA:RealToAlfaNumSTA End-STA 2))

	STA-TCPt	(polar
			  (SET:GetElemen "TC-Coord" CoordFCLst)
			  (SET:GetElemen "Ang1Rad" TriPointInputLst)
			  TxtOffs
			) ;_ end of polar

	STA-CTPt	(polar
			  (SET:GetElemen "CT-Coord" CoordFCLst)
			  (SET:GetElemen "Ang2Rad" TriPointInputLst)
			  TxtOffs
			) ;_ end of polar

	AngLine1       (SET:GetElemen "AngKP1Rad" TriPointInputLst)
	AngLine2       (SET:GetElemen "AngKP2Rad" TriPointInputLst)

	TxtPosSTA-TCPt	(polar STA-TCPt AngLine1 TxtDist)
	TxtPosSTA-CTPt	(polar STA-CTPt AngLine2 TxtDist)
      ) ;_ end of setq
      (GP:ENTMAKE-TEXT
	TxtPosSTA-TCPt TxtHeight AngLine1 Color	StringStart-STA) ;_ end of GP:ENTMAKE-TEXT
      (GP:ENTMAKE-TEXT
	TxtPosSTA-CTPt TxtHeight AngLine2 Color	StringEnd-STA) ;_ end of GP:ENTMAKE-TEXT
    ) ;_ end of progn
    (prompt "\nNothing Creation text Station Label of FC")
  ) ;_ end of if
) ;_ end of defun
  ;==========================================================
(defun SCS:GetAngleLineSC&CSLst	(TriPointInputLst SCSDataLst CoordSCSLst
				 /	     Radius	 Pt	     SCPt
				 CSPt	     RadESPt	 CenterPt    AngLine1
				 AngLine2
				)
  (setq
    Radius   (SET:GetElemen "Rc" SCSDataLst)
    Pt	     (SET:GetElemen "Es-Coord" CoordSCSLst)
    SCPt     (SET:GetElemen "y1-coord" CoordSCSLst)
    CSPt     (SET:GetElemen "y2-coord" CoordSCSLst)
    RadESPt  (SET:GetElemen "CptAng" TriPointInputLst)

    CenterPt (polar Pt RadESPt Radius)

    AngLine1 (angle SCPt CenterPt)
    AngLine2 (angle CSPt CenterPt)
  ) ;_ end of setq
  (list
    (cons 'ANGLINE-SC AngLine1)
    (cons 'ANGLINE-CS AngLine2)
  ) ;_ end of list
) ;_ end of defun
  ;==========================================================
(defun STA:CreateSTATextSCS-Circle (TriPointInputLst STALst	    PropSTATextLst  SCSDataLst
				    CoordSCSLst	    /
				    TxtDist	    TxtHeight	    StringStart-STA
				    StringEnd-STA   STA-SCPt	    STA-CSPt
				    AngLine1	    AngLine2	    AngTxtLine1
				    AngTxtLine2	    TxtPosSTA-SCPt  TxtPosSTA-STPt
				    Color	    Start-STA	    End-STA
				    TxtOffs	    AngleLineSC&CSLst
				   )
  (setq
    TxtOffs	      (SET:GetElemen 'TXTOFFS PropSTATextLst)
    TxtDist	      (SET:GetElemen 'TXTDIST PropSTATextLst)
    TxtHeight	      (SET:GetElemen 'TXTHEIGHT PropSTATextLst)
    Color	      (SET:GetElemen 'COLOR PropSTATextLst)

    Start-STA	      (SET:GetElemen 'START-CIRCLE-STA STALst)
    End-STA	      (SET:GetElemen 'END-CIRCLE-STA STALst)

    StringStart-STA   (strcat "STA. " (STA:RealToAlfaNumSTA Start-STA 2))
    StringEnd-STA     (strcat "STA. " (STA:RealToAlfaNumSTA End-STA 2))

    AngleLineSC&CSLst
		      (SCS:GetAngleLineSC&CSLst
			TriPointInputLst
			SCSDataLst
			CoordSCSLst
		      ) ;_ end of SCS:GetAngleLineSC&CSLst

    STA-SCPt	      (polar
			(SET:GetElemen "y1-coord" CoordSCSLst)
			(- (SET:GetElemen 'ANGLINE-SC AngleLineSC&CSLst)
			   (* 0.5 pi)
			) ;_ end of -
			TxtOffs
		      ) ;_ end of polar

    STA-CSPt	      (polar
			(SET:GetElemen "y2-coord" CoordSCSLst)
			(+ (SET:GetElemen 'ANGLINE-CS AngleLineSC&CSLst)
			   (* 0.5 pi)
			) ;_ end of +
			TxtOffs
		      ) ;_ end of polar

    AngLine1	      (SET:GetElemen 'ANGLINE-SC AngleLineSC&CSLst)
    AngLine2	      (SET:GetElemen 'ANGLINE-CS AngleLineSC&CSLst)

    TxtPosSTA-SCPt    (polar STA-SCPt AngLine1 TxtDist)
    TxtPosSTA-STPt    (polar STA-CSPt AngLine2 TxtDist)

  ) ;_ end of setq
  (GP:ENTMAKE-TEXT
    TxtPosSTA-SCPt TxtHeight AngLine1 Color StringStart-STA) ;_ end of GP:ENTMAKE-TEXT
  (GP:ENTMAKE-TEXT
    TxtPosSTA-STPt TxtHeight AngLine2 Color StringEnd-STA) ;_ end of GP:ENTMAKE-TEXT
) ;_ end of defun
  ;==========================================================
(defun STA:CreateSTATextSCS (TriPointInputLst SCSDataLst
			     CoordSCSLst     /		     TxtDist
			     TxtHeight	     StringStart-STA StringEnd-STA
			     STA-TSPt	     STA-STPt	     AngLine1
			     AngLine2	     AngTxtLine1     AngTxtLine2
			     TxtPosSTA-TSPt  TxtPosSTA-STPt  Color
			     STALst	     Start-STA	     End-STA
			     TxtOffs	     PropSTATextLst
			    )


  (if (setq STALst (STA-SCS:GetValueSTALst TriPointInputLst SCSDataLst))
    (progn
      (setq
	PropSTATextLst	(INIT:GetPropSTATextLst)

	TxtOffs		(SET:GetElemen 'TXTOFFS PropSTATextLst)
	TxtDist		(SET:GetElemen 'TXTDIST PropSTATextLst)
	TxtHeight	(SET:GetElemen 'TXTHEIGHT PropSTATextLst)
	Color		(SET:GetElemen 'COLOR PropSTATextLst)

	Start-STA	(SET:GetElemen 'START-SPIRAL-STA STALst)
	End-STA		(SET:GetElemen 'END-SPIRAL-STA STALst)

	StringStart-STA	(strcat	"STA. "
				(STA:RealToAlfaNumSTA Start-STA 2)
			) ;_ end of strcat
	StringEnd-STA	(strcat "STA. " (STA:RealToAlfaNumSTA End-STA 2))

	STA-TSPt	(polar
			  (SET:GetElemen "TS-coord" CoordSCSLst)
			  (SET:GetElemen "Ang1Rad" TriPointInputLst)
			  TxtOffs
			) ;_ end of polar

	STA-STPt	(polar
			  (SET:GetElemen "ST-coord" CoordSCSLst)
			  (SET:GetElemen "Ang2Rad" TriPointInputLst)
			  TxtOffs
			) ;_ end of polar

	AngLine1       (SET:GetElemen "AngKP1Rad" TriPointInputLst)
	AngLine2       (SET:GetElemen "AngKP2Rad" TriPointInputLst)

	TxtPosSTA-TSPt	(polar STA-TSPt AngLine1 TxtDist)
	TxtPosSTA-STPt	(polar STA-STPt AngLine2 TxtDist)

      ) ;_ end of setq
      (GP:ENTMAKE-TEXT
	TxtPosSTA-TSPt TxtHeight AngLine1 Color	StringStart-STA) ;_ end of GP:ENTMAKE-TEXT
      (STA:CreateSTATextSCS-Circle
	TriPointInputLst STALst PropSTATextLst SCSDataLst CoordSCSLst) ;_ end of STA:CreateSTATextSCS-Circle
      (GP:ENTMAKE-TEXT
	TxtPosSTA-STPt TxtHeight AngLine2 Color	StringEnd-STA) ;_ end of GP:ENTMAKE-TEXT
    ) ;_ end of progn
    (prompt "\nNothing Creation text for Station Label of SCS")
  ) ;_ end of if
) ;_ end of defun
  ;==========================================================
(defun STA:CreateSTATextSS (TriPointInputLst   SSDataLst
			    CoordSSLst	    /		    TxtDist
			    TxtHeight	    StringStart-STA StringEnd-STA
			    STA-TSPt	    STA-STPt	    AngLine1
			    AngLine2	    AngTxtLine1	    AngTxtLine2
			    TxtPosSTA-TSPt  TxtPosSTA-STPt  Color
			    STALst	    Start-STA	    End-STA
			    TxtOffs	    PropSTATextLst
			   )


  (if (setq STALst (STA-SS:GetValueSTALst TriPointInputLst SSDataLst))
    (progn
      (setq
	PropSTATextLst	(INIT:GetPropSTATextLst)

	TxtOffs		(SET:GetElemen 'TXTOFFS PropSTATextLst)
	TxtDist		(SET:GetElemen 'TXTDIST PropSTATextLst)
	TxtHeight	(SET:GetElemen 'TXTHEIGHT PropSTATextLst)
	Color		(SET:GetElemen 'COLOR PropSTATextLst)

	Start-STA	(SET:GetElemen 'START-STA STALst)
	End-STA		(SET:GetElemen 'END-STA STALst)

	StringStart-STA	(strcat	"STA. "
				(STA:RealToAlfaNumSTA Start-STA 2)
			) ;_ end of strcat
	StringEnd-STA	(strcat "STA. " (STA:RealToAlfaNumSTA End-STA 2))

	STA-TSPt	(polar
			  (SET:GetElemen "TS-coord" CoordSSLst)
			  (SET:GetElemen "Ang1Rad" TriPointInputLst)
			  TxtOffs
			) ;_ end of polar

	STA-STPt	(polar
			  (SET:GetElemen "ST-coord" CoordSSLst)
			  (SET:GetElemen "Ang2Rad" TriPointInputLst)
			  TxtOffs
			) ;_ end of polar

	AngLine1       (SET:GetElemen "AngKP1Rad" TriPointInputLst)
	AngLine2       (SET:GetElemen "AngKP2Rad" TriPointInputLst)

	TxtPosSTA-TSPt	(polar STA-TSPt AngLine1 TxtDist)
	TxtPosSTA-STPt	(polar STA-STPt AngLine2 TxtDist)
      ) ;_ end of setq
      (GP:ENTMAKE-TEXT
	TxtPosSTA-TSPt TxtHeight AngLine1 Color	StringStart-STA) ;_ end of GP:ENTMAKE-TEXT
      (GP:ENTMAKE-TEXT
	TxtPosSTA-STPt TxtHeight AngLine2 Color	StringEnd-STA) ;_ end of GP:ENTMAKE-TEXT
    ) ;_ end of progn
    (prompt "\nNothing Creation text Station Label of SS")
  ) ;_ end of if
) ;_ end of defun
  ;==========================================================
(defun SELECT:GetSigleObj (/ SsSet)
  (if (setq SsSet (ssget ":S" (list (cons 0 "TEXT") (cons 62 8))))
    (ssname SsSet 0)
  ) ;_ end of if
) ;_ end of defun
  ;==========================================================
(defun STA:GetValTxtSta	(/ EntObj EntLst TxtVal)
  (prompt "Select Object Text of Station (STA) <0>")
  (if (setq EntObj (SELECT:GetSigleObj))
    (progn
      (setq EntLst (entget EntObj))
      (setq TxtVal (SET:GetElemen 1 EntLst))
      (if (< (strlen TxtVal) 16)
	(atof
	  (vl-string-trim
	    " "
	    (vl-string-subst
	      ""
	      " "
	      (vl-string-subst
		""
		"  "
		(vl-string-subst
		  ""
		  "+"
		  (vl-string-left-trim "STA." TxtVal)
		) ;_ end of vl-string-subst
	      ) ;_ end of vl-string-subst
	    ) ;_ end of vl-string-subst
	  ) ;_ end of vl-string-trim
	) ;_ end of atof
	0.0
      ) ;_ end of if
    ) ;_ end of progn
    0.0
  ) ;_ end of if
) ;_ end of defun
  ;==========================================================
(defun STA:InitDatumSTA	(/ DatumSTA)
  (setq
    DatumSTA (getreal "\nEnter Datum STA <From Entities> : ")
  ) ;_ end of setq
  (if (not DatumSTA)
    (STA:GetValTxtSta)
    DatumSTA
  ) ;_ end of if
) ;_ end of defun
  ;==========================================================
(defun STA-FC:GetValueSTALst (TriPointInputLst	 FCDataLst  /	       Pt1	  Pt2
			      Dist	 Tc	    Lc	       DatumSTA	  NumStartSTA
			      NumEndSTA
			     )
  (setq
    Pt1	 (SET:GetElemen "Pt1" TriPointInputLst)
    Pt2	 (SET:GetElemen "Pt2" TriPointInputLst)
    Dist (distance Pt1 Pt2)
    Tc	 (SET:GetElemen "Tc" FCDataLst)
    Lc	 (SET:GetElemen "Lc" FCDataLst)
  ) ;_ end of setq
  (if (>= Dist Tc)
    (progn
      (setq
	DatumSTA    (STA:InitDatumSTA)
	NumStartSTA (+ DatumSTA (- Dist Tc))
	NumEndSTA   (+ NumStartSTA Lc)
      ) ;_ end of setq
      (list
	(cons 'START-STA NumStartSTA)
	(cons 'END-STA NumEndSTA)
      ) ;_ end of list
    ) ;_ end of progn
  ) ;_ end of if
) ;_ end of defun
  ;==========================================================
(defun STA-SCS:GetValueSTALst (TriPointInputLst	      SCSDataLst     /
			       Pt1	      Pt2	     Dist
			       Ts	      Ls	     Lc
			       DatumSTA	      NumStartSpiralSTA
			       NumStartCircleSTA	     NumEndCircleSTA
			       NumEndSpiralSTA
			      )
  (setq
    Pt1	 (SET:GetElemen "Pt1" TriPointInputLst)
    Pt2	 (SET:GetElemen "Pt2" TriPointInputLst)
    Dist (distance Pt1 Pt2)
    Ts	 (SET:GetElemen "Ts" SCSDataLst)
    Ls	 (SET:GetElemen "Ls" SCSDataLst)
    Lc	 (SET:GetElemen "Lc" SCSDataLst)
  ) ;_ end of setq
  (if (>= Dist Ts)
    (progn
      (setq
	DatumSTA	  (STA:InitDatumSTA)
	NumStartSpiralSTA (+ DatumSTA (- Dist Ts))
	NumStartCircleSTA (+ NumStartSpiralSTA Ls)
	NumEndCircleSTA	  (+ NumStartCircleSTA Lc)
	NumEndSpiralSTA	  (+ NumEndCircleSTA Ls)
      ) ;_ end of setq
      (list
	(cons 'START-SPIRAL-STA NumStartSpiralSTA)
	(cons 'START-CIRCLE-STA NumStartCircleSTA)
	(cons 'END-CIRCLE-STA NumEndCircleSTA)
	(cons 'END-SPIRAL-STA NumEndSpiralSTA)
      ) ;_ end of list
    ) ;_ end of progn
  ) ;_ end of if
) ;_ end of defun
  ;==========================================================
(defun STA-SS:GetValueSTALst (TriPointInputLst	 SSDataLst  /	       Pt1	  Pt2
			      Dist	 Ts	    Ls	       DatumSTA	  NumStartSTA
			      NumEndSTA
			     )
  (setq
    Pt1	 (SET:GetElemen "Pt1" TriPointInputLst)
    Pt2	 (SET:GetElemen "Pt2" TriPointInputLst)
    Dist (distance Pt1 Pt2)
    Ts	 (SET:GetElemen "Ts" SSDataLst)
    Ls	 (SET:GetElemen "Ls" SSDataLst)
  ) ;_ end of setq
  (if (>= Dist Ts)
    (progn
      (setq
	DatumSTA    (STA:InitDatumSTA)
	NumStartSTA (+ DatumSTA (- Dist Ts))
	NumEndSTA   (+ NumStartSTA (* Ls 2.0))
      ) ;_ end of setq
      (list
	(cons 'START-STA NumStartSTA)
	(cons 'END-STA NumEndSTA)
      ) ;_ end of list
    ) ;_ end of progn
  ) ;_ end of if
) ;_ end of defun
  ;==========================================================
(defun STA:RealToAlfaNumSTA (RealNumber		 Prec	   /	     Dkm
			     Hkm       Lkm3	 Lkm2	   Lkm1	     StringReal
			     StringDec
			    )
  (setq Dkm (itoa (fix RealNumber)))
  (if
    (<= (- (strlen Dkm) 3) 0)
     (setq Hkm "0")
     (setq Hkm (substr Dkm 1 (- (strlen Dkm) 3)))
  ) ;_ end of if
  (if
    (<= (- (strlen Dkm) 2) 0)
     (setq Lkm3 "0")
     (setq Lkm3 (substr Dkm (- (strlen Dkm) 2) 1))
  ) ;_ end of if
  (if
    (<= (- (strlen Dkm) 1) 0)
     (setq Lkm2 "0")
     (setq Lkm2 (substr Dkm (- (strlen Dkm) 1) 1))
  ) ;_ end of if
  (if
    (<= (- (strlen Dkm) 0) 0)
     (setq Lkm1 "0")
     (setq Lkm1 (substr Dkm (- (strlen Dkm) 0) 1))
  ) ;_ end of if
  (if (eq Prec 0)
    (strcat Hkm "+" Lkm3 Lkm2 Lkm1)
    (progn
      (setq StringReal (rtos RealNumber 2 Prec)
	    StringDec  (substr
			 StringReal
			 (+ (vl-string-position (ascii ".") StringReal) 1)
			 (strlen StringReal)
		       ) ;_ end of substr
      ) ;_ end of setq
      (strcat (strcat Hkm "+" Lkm3 Lkm2 Lkm1) StringDec)
    ) ;_ end of progn
  ) ;_ end of if
) ;_ end of defun
  ;==========================================================

