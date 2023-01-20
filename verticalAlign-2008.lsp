(defun VERTICAL:getTriPointInput (/ Pt1	Pt2 Pt3	Ang1Rad	Ang2Rad	Ang1Deg	Ang2Deg	Dist1 Dist2 Dist3 s Q BetaDeg DeltaDeg AbsElev1	AbsDist1 Grade1% AbsElev2
				  AbsDist2 Grade2%
				 )

 (if (setq Pt1 (getpoint "\nGet First point <Start Point>: "))
  (if (setq Pt2 (getpoint "\nGet Second point <Center Point>: "))
   (if (setq Pt3 (getpoint "\nGet Therth point <End Point> "))
    (progn
     (setq Ang1Rad  (angle Pt2 Pt1)
	   Ang2Rad  (angle Pt2 Pt3)

	   Ang1Deg  (Radian->Degrees Ang1Rad)
	   Ang2Deg  (Radian->Degrees Ang2Rad)

	   Dist1    (distance Pt1 Pt2)
	   Dist2    (distance Pt2 Pt3)
	   Dist3    (distance Pt1 Pt3)

	   AbsElev1 (- (cadr pt2) (cadr pt1))
	   AbsDist1 (- (car pt2) (car pt1))
	   Grade1%  (* (/ AbsElev1 AbsDist1) 100)

	   AbsElev2 (- (cadr pt3) (cadr pt2))
	   AbsDist2 (- (car pt3) (car pt2))
	   Grade2%  (* (/ AbsElev2 AbsDist2) 100)

	   s	    (* 0.5 (+ Dist1 Dist2 Dist3))
	   Q	    (sqrt (/ (* (- s Dist1) (- s Dist2) (- s Dist3)) s))
	   BetaDeg  (Radian->Degrees (* 2 (atan (/ Q (- s Dist3)))))
	   DeltaDeg (- 180 BetaDeg)

     ) ;_ end of setq

     (list
      (cons "Pt1" Pt1)
      (cons "Pt2" Pt2)
      (cons "Pt3" Pt3)

      (cons "Ang1Rad" Ang1Rad)
      (cons "Ang2Rad" Ang2Rad)

      (cons "Ang1Deg" Ang1Deg)
      (cons "Ang2Deg" Ang2Deg)

      (cons "Dist1" Dist1)
      (cons "Dist2" Dist2)
      (cons "Dist3" Dist3)

      (cons "BetaDeg" BetaDeg)
      (cons "DeltaDeg" DeltaDeg)

      (cons "AbsElev1" AbsElev1)
      (cons "AbsDist1" AbsDist1)
      (cons "Grade1%" Grade1%)

      (cons "AbsElev2" AbsElev2)
      (cons "AbsDist2" AbsDist2)
      (cons "Grade2%" Grade2%)
     ) ;end of list
    ) ;_ end of progn
   ) ;end of if
  ) ;end of if
 ) ;end of if
) ;end of defun
 ;------------------------------------------------------------------------------------
(defun VERTICAL:ENTMAKE-DIMENSION-Vertical (PosPt PosTxt StartLeft EndRight Layer Color)
 (entmake
  (list
   (cons 0 "DIMENSION")
   (cons 8 Layer)
   (cons 62 Color)
   (cons 10 (trans PosPt 1 (getvar 'WORLDUCS))) ;Basepoint(Right)
   (cons 11 (trans PosTxt 1 (getvar 'WORLDUCS))) ;CenterTxt
   (cons 70 32) ;TYpe Horizontal
   (cons 13 (trans StartLeft 1 (getvar 'WORLDUCS))) ;StarLPt
   (cons 14 (trans EndRight 1 (getvar 'WORLDUCS))) ;EndRPt
  ) ;_ end of list
 ) ;_ end of entmake
) ;_ end of defun
 ;------------------------------------------------------------------------------------
(defun Radian->Degrees (nbrOfRadians)
 (* 180.0 (/ nbrOfRadians pi))
) ;_ end of defun
 ; Convert value in degrees to radians
(defun Degrees->Radian (nbrOfDegrees)
 (* pi (/ nbrOfDegrees 180.0))
) ;_ end of defun
 ;------------------------------------------------------------------------------------
(defun VERTICAL:PrintLine (DataList StringMss)
 (princ StringMss)
 (foreach n DataList (print n))
) ;_ end of defun
 ;------------------------------------------------------------------------------------
(defun VERTICAL:GetLvDataInput (TriPointLst / Grade1% Grade2% A Ev Lv)

 (setq Grade1% (cdr (assoc "Grade1%" TriPointLst))
       Grade2% (cdr (assoc "Grade2%" TriPointLst))
       A       (- Grade1% Grade2%)
 ) ;_ end of setq

 (if (setq
      Lv (* (getreal
	     (strcat "\nEnter Value of Length (Lv) < See Table A="
		     (rtos (* A *GlobScaleHrzt) 2 2)
		     " > : "
	     ) ;_ end of strcat
	    ) ;_ end of getreal
	    *GlobScaleHrzt
	 ) ;_ end of *
     ) ;_ end of setq
  (progn
   (setq Ev (/ (* A Lv) 800.0))
   (list
    (cons "A" A)
    (cons "Ev" Ev)
    (cons "Lv" Lv)
   ) ;_ end of list
  ) ;_ end of progn
 ) ;_ end of if
) ;_ end of defun
 ;------------------------------------------------------------------------------------
(defun VERTICAL:GetTriPtVertical (triPointLst LvDataLst	/ Pt2 Grade1% Grade2% HalpLv Ev	XPT2 YPT2 ZPT2 XPLVPt XS XPTVPt	YPLVPt YS YPTVPt PLVPt SPt PTVPt
				 )
 (setq Pt2     (cdr (assoc "Pt2" TriPointLst))
       Grade1% (cdr (assoc "Grade1%" TriPointLst))
       Grade2% (cdr (assoc "Grade2%" TriPointLst))
       HalpLv  (* 0.5 (cdr (assoc "Lv" LvDataLst)))
       Ev      (cdr (assoc "Ev" LvDataLst))

       XPT2    (car Pt2) ;x
       YPT2    (cadr Pt2) ;Y
       ZPT2    (caddr Pt2) ;Z

       XPLVPt  (- XPT2 HalpLv)
       XS      XPT2
       XPTVPt  (+ XPT2 HalpLv)

       YPLVPt  (- YPT2 (* (/ Grade1% 100.0) HalpLv))
       YS      (- YPT2 Ev)
       YPTVPt  (+ YPT2 (* (/ Grade2% 100.0) HalpLv))

       PLVPt   (list XPLVPt YPLVPt ZPT2)
       SPt     (list XS YS ZPT2)
       PTVPt   (list XPTVPt YPTVPt ZPT2)
 ) ;_ end of setq
 (list
  (cons "PLVPt" PLVPt)
  (cons "SPt" SPt)
  (cons "PTVPt" PTVPt)
 ) ;_ end of list
) ;_ end of defun
 ;------------------------------------------------------------------------------------
(defun VERTICAL:ENTMAKE-TEXT-Vertical
				      (FstAlgPt TxtHeight Rotation Layer Color TheString)
 ;if ml y10=y11-h+(h/2) 
 (entmake
  (list
   (cons 0 "TEXT")
   (cons 8 Layer)
   (cons 62 Color)
   (cons 10 (trans FstAlgPt 1 (getvar 'WORLDUCS)))
   (cons 40 TxtHeight)
   (cons 1 TheString)
   (cons 50 Rotation)
   (cons 72 1)
   (cons 11 (trans FstAlgPt 1 (getvar 'WORLDUCS)))
   (cons 73 2)
  ) ;_ end of list
 ) ;_ end of entmake
) ;_ end of defun
 ;------------------------------------------------------------------------------------
(defun VERTICAL:CreateParPt (triPointLst LvDataLst TriPtVerticalLst / Lv NumPt PLVPt Xp	Yp A ParPt m XAcad YAcad Xresult Yresult
			    )
 (setq Lv    (cdr (assoc "Lv" LvDataLst))
       A     (cdr (assoc "A" LvDataLst))
       Pt1   (cdr (assoc "Pt1" triPointLst))
       Pt2   (cdr (assoc "Pt2" triPointLst))
       m     (/
	      (- (cadr Pt2) (cadr Pt1))
	      (- (car Pt2) (car Pt1))
	     ) ;_ end of /

       NumPt 20
       PLVPt (cdr (assoc "PLVPt" TriPtVerticalLst))
       ct    0
 ) ;_ end of setq

 (repeat (+ NumPt 1)
  (setq Xp (* (/ Lv (float NumPt)) ct))
  (setq Yp (/ (* A (expt Xp 2)) (* 200.0 Lv)))

  (setq XAcad (+ (- (car PLVPt) (car Pt2)) Xp))
  (setq YAcad (- (* m XAcad) Yp))

  (setq Xresult (+ XAcad (car Pt2)))
  (setq Yresult (+ YAcad (cadr Pt2)))

  (setq ParPt (append ParPt (list (list Xresult Yresult 0.0))))

  (setq ct (1+ ct))
 ) ;_ end of repeat
 ParPt
) ;_ end of defun
 ;------------------------------------------------------------------------------------
(defun VERTICAL:ENTMAKE-POLYLINE-Vertical (ParPtLst Layer Color)
 (entmake
  (List
   (cons 0 "POLYLINE") ; Object type
   (cons 8 Layer)
   (cons 62 Color) ; Color
   (cons 66 1) ; Vertices follow
  ) ;_ end of List
 ) ;_ end of entmake

 (mapcar '(lambda (Pt)
	   (entmake
	    (list
	     (cons 0 "VERTEX") ; Object type
	     (cons 10 (trans Pt 1 (getvar 'WORLDUCS)))
	    ) ;_end of list
	   ) ;_end of entmake
	  ) ;_end of lambda
	 ParPtLst
 ) ;_ ; end of mapcar

 (entmake (list (cons 0 "SEQEND")))
) ;_ end of defun
 ;------------------------------------------------------------------------------------
(defun VERTICAL:ENTMAKE-INSERT-Vertical	(BasePt BlockName)
 (entmake
  (list
   (cons 0 "INSERT")
   (cons 66 0)
   (cons 2 BlockName)
   (cons 10 (trans BasePt 1 (getvar 'WORLDUCS)))
  ) ;_ end of list
 ) ;_ end of entmake

 ;  (entmake
 ;    (list
 ;      (cons 0 "ATTRIB")
 ;      (cons 10 BasePt)
 ;      (cons 1 "33")
 ;      (cons 2 "PINUM")
 ;    )
 ;  )

 (entmake (list (cons 0 "SEQEND"))) ; Sequence end 
) ;_ end of defun
 ;------------------------------------------------------------------------------------
(defun VERTICAL:SetTable&LabelPt (TriPtVerticalLst / PLVPt SPt PTVPt Grade1%TxtPt NumPPVTxtPt Grade2%TxtPt TablePt)
 (setq PLVPt	    (cdr (Assoc "PLVPt" TriPtVerticalLst))
       SPt	    (cdr (Assoc "SPt" TriPtVerticalLst))
       PTVPt	    (cdr (Assoc "PTVPt" TriPtVerticalLst))
       Grade1%TxtPt (list (car PLVPt) (+ (cadr PLVPt) 1) (caddr PLVPt))
       NumPPVTxtPt  (list (car SPt) (- (cadr SPt) 3) (caddr SPt))
       TablePt	    (list (car SPt) (+ (cadr SPt) 5) (caddr SPt))
       Grade2%TxtPt (list (car PTVPt) (+ (cadr PTVPt) 1) (caddr PTVPt))
 ) ;_ end of setq

 (list
  (cons "Grade1%TxtPt" Grade1%TxtPt)
  (cons "NumPPVTxtPt" NumPPVTxtPt)
  (cons "TablePt" TablePt)
  (cons "Grade2%TxtPt" Grade2%TxtPt)
 ) ;_ end of list
) ;_ end of defun
 ;------------------------------------------------------------------------------------
(defun Sign (Number)
 (if (minusp Number)
  -1 ;yes
  1 ;no
 ) ;_ end of if
) ;_ end of defun
 ;------------------------------------------------------------------------------------
(defun VERTICAL:CreateBlockDelta (TriPointLst / BasePt)
 (setq BasePt (cdr (assoc "Pt2" TriPointLst)))
 (VERTICAL:ENTMAKE-INSERT-Vertical BasePt "deltav")
) ;_ end of defun
 ;------------------------------------------------------------------------------------
(defun NumPPVDeflt (str1 def)
 (strcat str1 "Last PPV Number is <" (rtos def 2 0) ">: ")
) ;_ end of defun
 ;------------------------------------------------------------------------------------
(defun VERTICAL:DrawGradeTxt (TriPointLst SetTable&LabelPtLst / Grade1%Txt Grade2%Txt Grade1%TxtPt Grade2%TxtPt GradeTxtHeight GradeTxtColor Rotation)
  (setq
        Grade1%Txt     (strcat
			 (rtos
			   (* (cdr (assoc "Grade1%" TriPointLst))
			      *GlobScaleHrzt
			   ) ;_ end of *
			   2
			   2
			 ) ;_ end of rtos
			 " %"
		       ) ;_ end of strcat
	Grade2%Txt     (strcat
			 (rtos
			   (* (cdr (assoc "Grade2%" TriPointLst))
			      *GlobScaleHrzt
			   ) ;_ end of *
			   2
			   2
			 ) ;_ end of rtos
			 " %"
		       ) ;_ end of strcat

	Grade1%TxtPt   (cdr (assoc "Grade1%TxtPt" SetTable&LabelPtLst))
	Grade2%TxtPt   (cdr (assoc "Grade2%TxtPt" SetTable&LabelPtLst))

	GradeTxtHeight 0.1750
	GradeTxtColor  4 ; cyan
	Rotation       0
  ) ;_ end of setq

  (VERTICAL:ENTMAKE-TEXT-Vertical
    Grade1%TxtPt GradeTxtHeight	Rotation "VERTICAL-GLBL" GradeTxtColor Grade1%Txt) ;_ end of VERTICAL:ENTMAKE-TEXT-Vertical
 ;_ end of VERTICAL:ENTMAKE-TEXT-Vertical

  (VERTICAL:ENTMAKE-TEXT-Vertical
    Grade2%TxtPt GradeTxtHeight	Rotation "VERTICAL-GLBL" GradeTxtColor Grade2%Txt) ;_ end of VERTICAL:ENTMAKE-TEXT-Vertical
 ;_ end of VERTICAL:ENTMAKE-TEXT-Vertical
) ;_ end of defun
 ;------------------------------------------------------------------------------------
(defun VERTICAL:DrawTable&Label	(SetTable&LabelPtLst LvDataLst TriPointLst / Grade1%Txt	Grade2%Txt NumPPVTxt Grade1%TxtPt NumPPVTxtPt Grade2%TxtPt
				 GradeTxtHeight	NumPPVHeight Rotation TablePt ATxt EvTxt LvTXT NumPPVTableTxt SingTxt
				)
 (if (not *GlobNumPPV)
  (setq *GlobNumPPV 1)
 ) ;_ end of if
 (setq NumPPV (getint (NumPPVDeflt "\nEnter PPV Number->" *GlobNumPPV)))
 (if (not NumPPV)
  (setq NumPPV *GlobNumPPV)
  (setq *GlobNumPPV NumPPV)
 ) ;_ end of if

 (setq
;;;      Grade1%Txt     (strcat
;;;		       (rtos
;;;			(* (cdr (assoc "Grade1%" TriPointLst))
;;;			   *GlobScaleHrzt
;;;			) ;_ end of *
;;;			2
;;;			2
;;;		       ) ;_ end of rtos
;;;		       " %"
;;;		      ) ;_ end of strcat
;;;       Grade2%Txt     (strcat
;;;		       (rtos
;;;			(* (cdr (assoc "Grade2%" TriPointLst))
;;;			   *GlobScaleHrzt
;;;			) ;_ end of *
;;;			2
;;;			2
;;;		       ) ;_ end of rtos
;;;		       " %"
;;;		      ) ;_ end of strcat
       NumPPVTxt      (strcat "PPV. "
			      (if (eq (strlen (itoa NumPPV)) 1)
			       (strcat "0" (itoa NumPPV))
			       (itoa NumPPV)
			      ) ;_ end of if
		      ) ;_ end of strcat

       NumPPVTableTxt (if (eq (strlen (itoa NumPPV)) 1)
		       (strcat "0" (itoa NumPPV))
		       (itoa NumPPV)
		      ) ;_ end of if

       ;Grade1%TxtPt   (cdr (assoc "Grade1%TxtPt" SetTable&LabelPtLst))
       NumPPVTxtPt    (cdr (assoc "NumPPVTxtPt" SetTable&LabelPtLst))
       TablePt	      (cdr (assoc "TablePt" SetTable&LabelPtLst))
       ;Grade2%TxtPt   (cdr (assoc "Grade2%TxtPt" SetTable&LabelPtLst))

       ;GradeTxtHeight 0.1750
       ;GradeTxtColor  4 ; cyan
       
       Rotation	      0
       NumPPVHeight   0.240
       NumPPVColor    2 ; Yellow


       SingTxt	      (if (minusp (cdr (assoc "Ev" LvDataLst)))
		       "+"
		       "-"
		      ) ;_ end of if
       ATxt	      (strcat
		       (rtos (* (cdr (assoc "A" LvDataLst)) *GlobScaleHrzt) 2 2)
		       " %"
		      ) ;_ end of strcat
       EvTxt	      (strcat SingTxt
			      (rtos (abs (cdr (assoc "Ev" LvDataLst))) 2 2)
			      " m"
		      ) ;_ end of strcat
       LvTXT	      (strcat
		       (rtos (/ (cdr (assoc "Lv" LvDataLst)) *GlobScaleHrzt) 2 2)
		       " m"
		      ) ;_ end of strcat
 ) ;_ end of setq

; (VERTICAL:ENTMAKE-TEXT-Vertical
;  Grade1%TxtPt GradeTxtHeight Rotation "VERTICAL-GLBL" GradeTxtColor Grade1%Txt
; ) ;_ end of VERTICAL:ENTMAKE-TEXT-Vertical

  (VERTICAL:ENTMAKE-TEXT-Vertical
  NumPPVTxtPt NumPPVHeight Rotation "VERTICAL-PPVLBL" NumPPVColor NumPPVTxt
 ) ;_ end of VERTICAL:ENTMAKE-TEXT-Vertical

;  (VERTICAL:ENTMAKE-TEXT-Vertical
;  Grade2%TxtPt GradeTxtHeight Rotation "VERTICAL-GLBL" GradeTxtColor Grade2%Txt
; ) ;_ end of VERTICAL:ENTMAKE-TEXT-Vertical

 (setvar "OSMODE" 20517) ;off
 (command "-insert" "vtable" TablePt 1 1 0 NumPPVTableTxt ATxt LvTXT EvTxt) ;_ end of command
 (setvar "OSMODE" 4133) ;on
  
) ;_ end of defun
 ;------------------------------------------------------------------------------------
(defun VERTICAL:MakePoint-Vertical (Pt)
 (entmake
  (list
   (cons 0 "POINT")
   (cons 10 (trans Pt 1 (getvar 'WORLDUCS)))
  ) ;_ end of list
 ) ;_ end of entmake
) ;_ end of defun
 ;------------------------------------------------------------------------------------
(defun CDRfuction-Vertical (Data)
 (list (cdr Data))
) ;_ end of defun
 ;-------------------------------------------------------------------------------------
(defun VERTICAL:DrawingPoint (ListData / ListDt)
 (setq ListDt (apply 'append (mapcar 'CDRfuction-Vertical ListData))) ;_ end of setq
 (mapcar '(lambda (Pt) (VERTICAL:MakePoint-Vertical Pt))
	 ListDt
 ) ;_ end of mapcar
) ;_ end of defun
 ;------------------------------------------------------------------------------------
(defun Bulge&RadiusData	(3PtLst / Pt1 Pt2 Pt3 Dist1 Dist2 Dist3 s Q Beta Delta Radius LengthArc BulgeFactor)
 (setq
  Pt1	      (nth 0 3PtLst)
  Pt2	      (nth 1 3PtLst)
  Pt3	      (nth 2 3PtLst)

  Dist1	      (distance Pt1 Pt2)
  Dist2	      (distance Pt2 Pt3)
  Dist3	      (distance Pt1 Pt3)

  s	      (* 0.5 (+ Dist1 Dist2 Dist3))
  Q	      (sqrt (/ (* (- s Dist1) (- s Dist2) (- s Dist3)) s))

  Beta	      (* 2.0 (atan (/ Q (- s Dist3))))
  Delta	      (- pi Beta)

  Radius      (/ (* 0.5 Dist3) (sin Beta)) ;jari2 luar
  LengthArc   (* 2. Delta Radius)
  BulgeFactor (/ (- 1 (cos Delta)) (sin Delta))
 ) ;_ end of setq
 (list
  (cons "Pt1" Pt1)
  (cons "Pt2" Pt2)
  (cons "Pt3" Pt3)
  (cons "Bulge-ARC" BulgeFactor)
 ) ;_ end of list
) ;_ end of defun
 ;==============================
(defun Pt:PointTransition (Pt)
 (trans Pt 1 (getvar 'WORLDUCS))
) ;_ end of defun
 ;============================================
(defun VER:ENTMAKE-POLYLINE-Vertical (CenPtCtrl ParPtLst Layer Color)
 (entmake
  (List
   (cons 0 "POLYLINE") ; Object type
   (cons 8 Layer)
   (cons 62 Color) ; Color
   (cons 66 1) ; Vertices follow
   (cons 70 4) ; 4 = Spline-fit vertices have been added
   (cons 75 5) ; 5 = Quadratic B-spline surface
  ) ;_ end of List
 ) ;_ end of entmake

 ;start point==================
 (entmake
  (list
   (cons 0 "VERTEX") ; Object type
   (cons 10 (trans (car ParPtLst) 1 (getvar 'WORLDUCS)))
   (cons 70 16) ; 16 = Spline frame control point
  ) ;_ end of list
 ) ;_ end of entmake

 (mapcar '(lambda (Pt)
	   (entmake
	    (list
	     (cons 0 "VERTEX") ; Object type
	     (cons 10 (trans Pt 1 (getvar 'WORLDUCS)))
	     (cons 70 8) ; 8 = Spline vertex created by spline-fitting
	    ) ;_end of list
	   ) ;_end of entmake
	  ) ;_end of lambda
	 ParPtLst
 ) ;_ ; end of mapcar

 ;ceter point==================
 (entmake
  (list
   (cons 0 "VERTEX") ; Object type
   (cons 10 (trans CenPtCtrl 1 (getvar 'WORLDUCS)))
   (cons 70 16) ; 16 = Spline frame control point
  ) ;_ end of list
 ) ;_ end of entmake

 ;end point==================
 (entmake
  (list
   (cons 0 "VERTEX") ; Object type
   (cons 10 (trans (last ParPtLst) 1 (getvar 'WORLDUCS)))
   (cons 70 16) ; 16 = Spline frame control point
  ) ;_ end of list
 ) ;_ end of entmake

 (entmake (list (cons 0 "SEQEND")))
) ;_ end of defun
 ;------------------------------------------------------------------------------------
;;;(defun GP:ENTMAKE-POLYLINEARC1 (LvDataLst VertexPt&BulgeLst Color / SingBulge)
;;; (setq
;;;  SingBulge (if	(minusp (cdr (assoc "Ev" LvDataLst)))
;;;	     +1
;;;	     -1
;;;	    ) ;_ end of if
;;; ) ;_ end of setq
;;;
;;; (entmake
;;;  (List
;;;   (cons 0 "POLYLINE") ; Object type
;;;   (cons 62 Color) ; Color
;;;   (cons 66 1) ; Vertices follow
;;;  ) ;_ end of List
;;; ) ;_ end of entmake
;;;
;;; (mapcar '(lambda (Pt)
;;;	   (entmake
;;;	    (list
;;;	     (cons 0 "VERTEX") ; Object type
;;;	     (cons 10 (PT:PointTransition (cdr (assoc "Pt1" Pt))))
;;;	     (cons 42 (* (cdr (assoc "Bulge-ARC" Pt)) SingBulge))
;;;	    ) ;_ end of list
;;;	   ) ;_ end of entmake
;;;	  ) ;_ end of lambda
;;;	 VertexPt&BulgeLst
;;; ) ;_ end of mapcar
;;;
;;; (entmake
;;;  (list
;;;   (cons 0 "VERTEX") ; Object type
;;;   (cons 10
;;;	 (PT:PointTransition
;;;	  (cdr (assoc "Pt3" (last VertexPt&BulgeLst)))
;;;	 ) ;_ end of PT:PointTransition
;;;   ) ;_ end of cons
;;;   (cons 42 0.)
;;;  ) ;_ end of list
;;; ) ;_ end of entmake
;;; (entmake (list (cons 0 "SEQEND")))
;;;) ;_ end of defun
 ;===============================Convert3PtLst====================
(defun GP:LoadVertex3PtLst (VertexPtLst / ct Pt0 Pt1 Pt2 3PtLst All3PtLst)
 (setq ct 0)
 (repeat (fix (/ (length VertexPtLst) 2))
  (setq
   Pt0	     (nth (+ (* ct 2) 0) VertexPtLst)
   Pt1	     (nth (+ (* ct 2) 1) VertexPtLst)
   Pt2	     (nth (+ (* ct 2) 2) VertexPtLst)
   3PtLst    (list Pt0 Pt1 Pt2)
   All3PtLst (append All3PtLst (list 3PtLst))
   ct	     (1+ ct)
  ) ;_ end of setq
 ) ;_ end of repeat
 All3PtLst
) ;_ end of defun
 ;------------------------------------------------------------------------------------
(defun VERTICAL:DrawingPolyline	(TriPointLst ParPtLst Color / CenPtCtrl)
 (setq CenPtCtrl (cdr (assoc "Pt2" TriPointLst)))
 (VER:ENTMAKE-POLYLINE-Vertical CenPtCtrl ParPtLst "0" Color) ;VER:ENTMAKE-POLYLINE-Vertical (CenPtCtrl ParPtLst Layer Color)
) ;_ end of defun
 ;====================================================================================
 ;(defun VERTICAL:DrawingPolyline (LvDataLst ParPtLst Color / All3PtLst PolyPointsArcLst)
 ;        (setq All3PtLst	 (GP:LoadVertex3PtLst ParPtLst)
 ;	      PolyPointsArcLst (apply 'append (list (mapcar 'Bulge&RadiusData All3PtLst))
 ;	)
 ;  ) ;_ end of setq
 ; (GP:ENTMAKE-POLYLINEARC1 LvDataLst PolyPointsArcLst Color)
 ;(VERTICAL:ENTMAKE-POLYLINE-Vertical ParPtLst "0" Color)
 ;) ;_ end of defun
 ;------------------------------------------------------------------------------------
(defun VERTICAL:DrawingDeltaLine (DeltaLinePtLst Color)
 (VERTICAL:ENTMAKE-POLYLINE-Vertical
  DeltaLinePtLst
  "VERTICAL-TANLINE"
  Color
 ) ;_ end of VERTICAL:ENTMAKE-POLYLINE-Vertical
) ;_ end of defun
 ;------------------------------------------------------------------------------------
(defun VERTICAL:DrawingAll (TriPtVerticalLst SetTable&LabelPtLst LvDataLst ParPtLst TriPointLst DeltaLinePtLst)
 (VERTICAL:DrawTable&Label SetTable&LabelPtLst LvDataLst TriPointLst ) ;_ end of VERTICAL:DrawTable&Label
 (VERTICAL:DrawingPolyline TriPointLst ParPtLst 3) ; Color Green
 (VERTICAL:DrawingDeltaLine DeltaLinePtLst 1) ;Color Red
 (VERTICAL:DrawingDimension TriPtVerticalLst)
 (VERTICAL:CreateBlockDelta TriPointLst)
 (VERTICAL:DrawGradeTxt TriPointLst SetTable&LabelPtLst)
) ;_ end of defun
 ;------------------------------------------------------------------------------------
(defun VERTICAL:SelDrawingVertical (TriPtVerticalLst SetTable&LabelPtLst LvDataLst ParPtLst TriPointLst DeltaLinePtLst / num)

 (initget 1 "P C L D")
 (setq num (strcase
	    (getstring
	     "\Create Selected (<P>oint/<C>urve/<L>ine PPV./<D>Dimensi/<Enter For All>) : "
	    ) ;_ end of getstring
	   ) ;_ end of strcase
 ) ;_ end of setq

 (cond
  ((eq num "P") (VERTICAL:DrawingPoint TriPtVerticalLst))
  ((eq num "C") (VERTICAL:DrawingPolyline LvDataLst ParPtLst 3))
  ((eq num "L") (VERTICAL:DrawingDeltaLine DeltaLinePtLst 1))
  ((eq num "D") (VERTICAL:DrawingDimension TriPtVerticalLst))
  ((VERTICAL:DrawingAll TriPtVerticalLst SetTable&LabelPtLst LvDataLst ParPtLst TriPointLst DeltaLinePtLst))
 ) ;_ end of cond
) ;_ end of defun
 ;====================================================================
(defun VERTICAL:ScaleDeflt (str1 def)
 (strcat str1 "<" (rtos def 2 2) "> :")
) ;_ end of defun
 ;====================================================================
(defun VERTICAL:ScaleInput (/ ScaleVert ScaleHrzt)
 (if (not *GlobScaleVert)
  (progn
   (setq *GlobScaleVert 1.0)
   (setq ScaleVert (getreal (VERTICAL:ScaleDeflt
			     "\nEnter Vertical Scale Faktor "
			     *GlobScaleVert
			    ) ;_ end of VERTICAL:ScaleDeflt
		   ) ;_ end of getreal
   ) ;_ end of setq
   (if (not ScaleVert)
    (setq ScaleVert *GlobScaleVert)
    (setq *GlobScaleVert ScaleVert)
   ) ;_ end of if
  ) ;_ end of progn
 ) ;_ end of if

 (if (not *GlobScaleHrzt)
  (progn
   (setq *GlobScaleHrzt 0.1)
   (setq ScaleHrzt (getreal (VERTICAL:ScaleDeflt
			     "\nEnter Horizontal Scale Faktor "
			     *GlobScaleHrzt
			    ) ;_ end of VERTICAL:ScaleDeflt
		   ) ;_ end of getreal
   ) ;_ end of setq
   (if (not ScaleHrzt)
    (setq ScaleHrzt *GlobScaleHrzt)
    (setq *GlobScaleHrzt ScaleHrzt)
   ) ;_ end of if
  ) ;_ end of progn
 ) ;_ end of if
) ;_ end of defun
 ;====================================================================
(defun VERTICAL:CreateDeltaLine	(TriPointLst TriPtVerticalLst /	Pt2 Ang1Rad Ang2Rad Dist1 Dist2	DistDeltaLine1 DistDeltaLine2 DeltaLinePt1 DeltaLinePt2
				 PolyLineDeltaLine
				)
 (setq Pt2		 (cdr (assoc "Pt2" TriPointLst))

       Ang1Rad		 (cdr (assoc "Ang1Rad" TriPointLst))
       Ang2Rad		 (cdr (assoc "Ang2Rad" TriPointLst))

       Dist1		 (distance Pt2 (cdr (assoc "PLVPt" TriPtVerticalLst)))
       Dist2		 (distance Pt2 (cdr (assoc "PTVPt" TriPtVerticalLst)))

       DistDeltaLine1	 (* Dist1 0.6)
       DistDeltaLine2	 (* Dist2 0.6)

       DeltaLinePt1	 (polar Pt2 Ang1Rad DistDeltaLine1)
       DeltaLinePt2	 (polar Pt2 Ang2Rad DistDeltaLine2)

       PolyLineDeltaLine (list DeltaLinePt1 Pt2 DeltaLinePt2)
 ) ;_ end of setq
 PolyLineDeltaLine
) ;_ end of defun
 ;==========================================
(defun VERTICAL:DrawingDimension
				 (TriPtVerticalLst / PLVPt SPt PTVPt OffsetDim PosPt PosTxt PLVPt PTVPt Layer Color Y-Dim)
 (setq PLVPt	 (cdr (assoc "PLVPt" TriPtVerticalLst))
       SPt	 (cdr (assoc "SPt" TriPtVerticalLst))
       PTVPt	 (cdr (assoc "PTVPt" TriPtVerticalLst))

       OffsetDim -1.0
       Color	 9
       Layer	 "VERTICAL-DIM"

       Y-Dim	 (+ (min (cadr PLVPt) (cadr PTVPt)) OffsetDim)

       PosPt	 (list (car PTVPt) Y-Dim 0.0)
       PosTxt	 (list (car SPt) Y-Dim 0.0)
 ) ;_ end of setq

 (VERTICAL:ENTMAKE-DIMENSION-Vertical
  PosPt	PosTxt PLVPt PTVPt Layer Color
 ) ;_ end of VERTICAL:ENTMAKE-DIMENSION-Vertical
 
) ;_ end of defun
 ;==========================================				

 ;==========================================				
 ;==========================================				
 ;==========================================				
 ;==========================================				
 ;==========================================				
 ;==========================================				
(defun C:vertical (/ triPointLst LvDataLst TriPtVerticalLst ParPtLst SetTable&LabelPtLst DeltaLinePtLst)
 ;======Initializ==============================================================
 ;(VERTICAL:ScaleInput) matikan saja
 (setq *GlobScaleVert 1.0)
 (setq *GlobScaleHrzt 0.1)
 ;======Main Procces==============================================================
 (if (setq triPointLst (VERTICAL:getTriPointInput))
  (if (setq LvDataLst (VERTICAL:GetLvDataInput TriPointLst))
   (progn
    (setq TriPtVerticalLst
			      (VERTICAL:GetTriPtVertical triPointLst LvDataLst)
	  ParPtLst
			      (VERTICAL:CreateParPt triPointLst LvDataLst TriPtVerticalLst) 
	  SetTable&LabelPtLst
			      (VERTICAL:SetTable&LabelPt TriPtVerticalLst)
	  DeltaLinePtLst
			      (VERTICAL:CreateDeltaLine TriPointLst TriPtVerticalLst)
    ) ;_ end of setq

 ;======Control List==============================================================
    ;;(VERTICAL:PrintLine triPointLst "\nData of triPointLst") ; data ok!
    ;;(VERTICAL:PrintLine LvDataLst "\nData of LvDataLst") ; data ok!
    ;;(VERTICAL:PrintLine	TriPtVerticalLst "\nData of TriPtVerticalLst") ; data ok!
    ;;(VERTICAL:PrintLine ParPtLst "\nData of ParPtLst") ; data ok!
    ;;(VERTICAL:PrintLine	SetTable&LabelPtLst "\nData of SetTable&LabelPtLst") ; data ok!
 ;======Control Rutin==============================================================
    ;(VERTICAL:DrawTable&Label SetTable&LabelPtLst LvDataLst TriPointLst ) ;_ end of VERTICAL:DrawTable&Label
    ;(VERTICAL:DrawingPolyline TriPointLst ParPtLst 3) ; Color Green ==> Fuction OK!
    ;(VERTICAL:DrawingDeltaLine DeltaLinePtLst 1) ;Color Red
 ;======Control End Of==============================================================

 ;======Menu Acces==============================================================
    (VERTICAL:SelDrawingVertical TriPtVerticalLst SetTable&LabelPtLst LvDataLst ParPtLst TriPointLst DeltaLinePtLst) 
   ) ;_ end of progn
   (princ "\nPlease Enter  Value of Length (Lv) ")
  ) ;_ end of if
 ) ;_ end of if
 (princ)
) ;_ end of defun


