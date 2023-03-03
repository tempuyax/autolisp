(defun SUPERELEV:GetCalc-Rmin-LsMin
       (Vr eMax en Rc / fMax Rmin LsMin1 LsMin2 LsMin3 LsMin)

  (setq	fMax (if (< 80.0 Vr)
	       (+ (* -0.00065 Vr) 0.102)
	       (+ (* -0.00125 Vr) 0.24)
	     )
  )
;;======================================================================
  (if (/= Rc -1)
    (setq Rmin Rc)
    (setq Rmin (/ (expt Vr 2.0) (* 127.0 (+ eMax fMax))))
    ;;(Vr^2/(127*(Emax+fMax))
  )
;;======================================================================
  (setq
    LsMin1 (/ (* Vr 3.0) 3.6)
    LsMin2 (-
	     (/ (* 0.022 (expt Vr 3.0)) (* Rmin 0.4))
	     (/ (* 2.727 Vr eMax) 0.4)
	   )
    LsMin3 (/ (* (- eMax en) Vr) (* 3.6 0.03))


    LsMin  (max LsMin1 LsMin2 LsMin3)

  ) ;_ end of setq


  (list
    (cons "Vr" Vr)
    (cons "eMax" eMax)
    (cons "Rmin" Rmin)

    (cons "LsMin1" LsMin1)
    (cons "LsMin2" LsMin2)
    (cons "LsMin3" LsMin3)

    (cons "LsMin" LsMin)
  ) ;_ end of list
) ;_ end of defun
;;================================================================================
;Menghitung Vr Untuk mencari Nilai LsMin
(defun SUPERELEV:Auto-Vr-For-LsMin (GetRcLst   /	  Rc
				    Vr	       eMax	  en
				    Rmin       LsMin
				    Tabel-Rmin-LsMinLst	  Vr-Temp
				    R-Off      R-Out	  LsMin1
				    LsMin2     LsMin3
				   )

  (setq
    Rc	    (cdr (assoc "Rc" GetRcLst))
    eMax    0.04
    en	    0.02
    Vr-Temp 20.0
    Rmin    (cdr (assoc "Rmin" (SUPERELEV:GetCalc-Rmin-LsMin
	                         Vr-Temp
	                         eMax
	                         en
	                         -1
	                        )
            ))
  ) ;_ end of setq

  ;;Membandingkan Nilai Rc dgn Rmin Untuk Optimasi Nilai Vr
  (while (> Rc Rmin)
    (progn
      (setq
	Tabel-Rmin-LsMinLst
	 (SUPERELEV:GetCalc-Rmin-LsMin
	   Vr-Temp
	   eMax
	   en
	   -1
	 )
	Rmin (cdr (assoc "Rmin" Tabel-Rmin-LsMinLst))
	Vr (cdr (assoc "Vr" Tabel-Rmin-LsMinLst))
	Vr-Temp	(+ Vr-Temp 10.0)
      )
    )
  )
;;==============================
  (if (>= Vr 20.0)
    (setq Vr (- Vr 10.0))
  )
;;==============================
  ;;Jika Vr sudah didapat. Menghitung Kembali Tabel Untuk Menyesuaikan Nilai Rc.
  ;;Dengan tujuan pendapatkan Nilai LsMin. 
  (setq
    Tabel-Rmin-LsMinLst
     (SUPERELEV:GetCalc-Rmin-LsMin Vr eMax en Rc)
    LsMin1 (cdr (assoc "LsMin1" Tabel-Rmin-LsMinLst))
    LsMin2 (cdr (assoc "LsMin2" Tabel-Rmin-LsMinLst))
    LsMin3 (cdr (assoc "LsMin3" Tabel-Rmin-LsMinLst))
    LsMin (cdr (assoc "LsMin" Tabel-Rmin-LsMinLst))
    R-Off (/ (* LsMin 2.0) 3.0)
    R-Out (- LsMin R-Off)
  )

  (list
    (cons "Rmin" Rmin)
    (cons "Vr" Vr)
    (cons "eMax" eMax)
    (cons "en" en)

    (cons "LsMin1" LsMin1)
    (cons "LsMin2" LsMin2)
    (cons "LsMin3" LsMin3)

    (cons "LsMin" LsMin)
    (cons "R-Out" R-Out)
    (cons "R-Off" R-Off)

  ) ;_ end of list
) ;_ end of defun
;;================================================================================
(defun SUPERELEV:CoordinatDiagramFC (GetFCLst	InitTextCoordLst
				     /		BasePt	   Tabel-LsMin
				     Lc		eMax	   en
				     R-Off	R-Out	   h-eMax
				     h-en	L-Aspal	   B
				     VectorPt	DiagramPts L-Ext
				     Lline-AS	Lline-En   TC-Pt1
				     TC-Pt2	Lline-TC   SC-Pt1
				     SC-Pt2	Lline-SC   TS-Pt1
				     TS-Pt2	Lline-TS   Pt1
				     Pt2	Pt3	   Pt4
				     XScale	YScale
				    )
  (setq
    L-Aspal	8.0
    B		(* L-Aspal 0.5)
    L-Ext	5.0

    Tabel-LsMin	(SUPERELEV:Auto-Vr-For-LsMin GetFCLst)
    BasePt	(cdr (assoc "DiagramPt" InitTextCoordLst))
    eMax	(cdr (assoc "eMax" Tabel-LsMin))
    en		(cdr (assoc "en" Tabel-LsMin))

    XScale	10.0
    ;; Fit /
    Lc		(/ (cdr (assoc "Lc" GetFCLst)) XScale)
    LsMin	(/ (cdr (assoc "LsMin" Tabel-LsMin)) XScale)
    R-Off	(/ (cdr (assoc "R-Off" Tabel-LsMin)) XScale)
    R-Out	(/ (cdr (assoc "R-Out" Tabel-LsMin)) XScale)

    YScale	10.0
    ;; Big *
    h-eMax	(* (* eMax B) YScale)
    h-en	(* (* en B) YScale)

    VectorPt	(list
		  (car BasePt)
		  (+ (cadr BasePt) h-eMax)
		  0.0
		)
    NodPt	(list
		  (- (car BasePt) (* Lc 0.5))
		  (cadr BasePt)
		  0.0
		)
    ;;====================================== for Diagram
    Pt1		(list
		  (+ (car NodPt) R-Out)
		  (- (cadr NodPt) h-eMax)
		  0.0
		)
    Pt2		(list
		  (car NodPt)
		  (- (cadr NodPt) h-en)
		  0.0
		)
    Pt3		(list
		  (- (car NodPt) R-Off)
		  (- (cadr NodPt) h-en)
		  0.0
		)
    Pt4		(list
		  (+ (car NodPt) R-Out)
		  (+ (cadr NodPt) h-eMax)
		  0.0
		)

    ;;====================================== for ExtLine
    EnPt	(list
		  (- (car NodPt) R-Off L-Ext)
		  (- (cadr NodPt) h-en)
		  0.0
		)
    AsPt	(list
		  (- (car NodPt) R-Off L-Ext)
		  (cadr NodPt)
		  0.0
		)
    ;;====================================== IntSecPt Ok for As InterSection
    IntSecPt	(list
		  (- (car NodPt) R-Out)
		  (cadr NodPt)
		  0.0
		)

    ;;=======================================
    TC-Pt1	(list
		  (car NodPt)
		  (+ (cadr NodPt) h-eMax L-Ext)
		  0.0
		)

    TC-Pt2	(list
		  (car NodPt)
		  (- (cadr NodPt) h-eMax L-Ext)
		  0.0
		)
    ;;=======================================X SC-Pt1 = X Pt4 Nilai X same as
    SC-Pt1	(list
		  (+ (car NodPt) R-Out)
		  (+ (cadr NodPt) h-eMax L-Ext)
		  0.0
		)
    SC-Pt2	(list
		  (+ (car NodPt) R-Out)
		  (- (cadr NodPt) h-eMax L-Ext)
		  0.0
		)
    ;;=======================================
    TS-Pt1	(list
		  (- (car NodPt) R-Off)
		  (+ (cadr NodPt) h-eMax L-Ext)
		  0.0
		)
    TS-Pt2	(list
		  (- (car NodPt) R-Off)
		  (- (cadr NodPt) h-eMax L-Ext)
		  0.0
		)
    ;;Init point Diagram
    DiagramPts	(list Pt1 Pt2 Pt3 Pt4)
    ;;Init Line As
    Lline-AS	(list IntSecPt AsPt)
    ;;Init Line En
    Lline-En	(list Pt3 EnPt)
    ;;Init Line TC
    Lline-TC	(list TC-Pt1 TC-Pt2)
    ;;Init Line SC
    Lline-SC	(list SC-Pt1 SC-Pt2)
    ;;Init Line TS
    Lline-TS	(list TS-Pt1 TS-Pt2)
  )

  (SHOW:DiagramPolyLine DiagramPts BasePt VectorPt 3)
  (SHOW:DiagramLine Lline-AS BasePt VectorPt 4 nil nil)
  (SHOW:DiagramLine Lline-En BasePt VectorPt 4 nil nil)
  (SHOW:DiagramLine Lline-TC BasePt VectorPt 4 "TC" "CT")
  (SHOW:DiagramLine Lline-SC BasePt VectorPt 9 nil nil)
  (SHOW:DiagramLine Lline-TS BasePt VectorPt 9 nil nil)

  (SHOW:DrawingDiagramTblFC1
    (list (car BasePt) (+ (cadr BasePt) h-eMax L-Ext) 0.0)
    InitTextCoordLst
    Tabel-LsMin
    1
  )
  (SHOW:DrawingDiagramTblFC2
    (list (car BasePt) (- (cadr BasePt) h-eMax L-Ext) 0.0)
    InitTextCoordLst
    Tabel-LsMin
    1
  )

)
;;=========================================================================
(defun SHOW:DiagramPolyLine (Targetpts	 BasePt	     VectorPt
			     Color	 /	     Reflectpts
			     Mergepts
			    )
  (setq
    Reflectpts (reverse (LM:ReflectByMatrix Targetpts BasePt VectorPt))
    Mergepts   (append Targetpts Reflectpts)
  )
  (GP:ENTMAKE-LWPOLYLINE Mergepts Color 1)
)
;;=========================================================================
(defun SHOW:DiagramLine	(Targetpts   BasePt	 VectorPt
			 Color	     TextOut1	 TextOut2
			 /	     Reflectpts
			)
  (setq
    Reflectpts
     (reverse (LM:ReflectByMatrix Targetpts BasePt VectorPt))
  )
  (GP:ENTMAKE-LINE (nth 0 Targetpts) (nth 1 Targetpts) Color)
  (GP:ENTMAKE-LINE
    (nth 0 Reflectpts)
    (nth 1 Reflectpts)
    Color
  )
  (if (and (/= TextOut1 nil) (/= TextOut2 nil))
    (progn
      (GP:ENTMAKE-TEXT (car Targetpts) 1.5 0.0 Color TextOut1)
      (GP:ENTMAKE-TEXT (cadr Reflectpts) 1.5 0.0 Color TextOut2) 
    )
  )
)
;;=========================================================================
(defun GP:NumDiagTxtTabel (NumPI)
  (strcat "D. "
	  (if (eq (strlen (itoa NumPI)) 1)
	    (strcat "0" (itoa NumPI)) ;true
	    (itoa NumPI) ;false
	  ) ;_ end of if
  ) ;_ end of strcat
) ;_ end of defun
;;==========================================================================
;;;vRunOut <0.00>:
;;;vRunOff <0.00>:
;;;vnumDiag <00>:
(defun SHOW:DrawingDiagramTblFC1 (TablePt InitTextCoordLst Tabel-LsMin SF /
     	                        TableName  TablePt ScaleX ScaleY Rotation
		                RunOut  RunOff  numDiag
			        )
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 20517)
  (setq	TableName "diagramtabel"
	ScaleX	  (* 1 SF)
	ScaleY	  (* 1 SF)
	Rotation  0.0

	numDiag	  (GP:NumDiagTxtTabel (cdr (assoc "NumPI" InitTextCoordLst)))
	RunOut	  (strcat (rtos (cdr (assoc "R-Out" Tabel-LsMin)) 2 2) " m")
	RunOff	  (strcat (rtos (cdr (assoc "R-Off" Tabel-LsMin)) 2 2) " m")
  ) ;_ end of setq
  (command "-insert" TableName TablePt ScaleX ScaleY Rotation RunOut RunOff numDiag) ;_ end of command
  (setvar "CMDECHO" 1)
  (setvar "OSMODE" 4133)
) ;_ end of defun
;;==========================================================================
;;;Enter attribute values
;;;vVr <00>:
;;;VeN <00>:
;;;VEMAX <00>:
(defun SHOW:DrawingDiagramTblFC2 (TablePt InitTextCoordLst Tabel-LsMin SF /
     	                        TableName  TablePt ScaleX ScaleY Rotation
		                RunOut  RunOff  numDiag Vr eMax en
			        )
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 20517)
  (setq	TableName "diagramtabel2"
	ScaleX	  (* 1 SF)
	ScaleY	  (* 1 SF)
	Rotation  0.0

	Vr	  (strcat (rtos (cdr (assoc "Vr" Tabel-LsMin)) 2 0) " Km/jam")
	eMax	  (strcat (rtos (* 100 (cdr (assoc "eMax" Tabel-LsMin))) 2 0) " %") 
	en	  (strcat (rtos (* 100 (cdr (assoc "en"   Tabel-LsMin))) 2 0) " %") 
  ) ;_ end of setq
  (command "-insert" TableName TablePt ScaleX ScaleY Rotation Vr eN eMax) ;_ end of command
  (setvar "CMDECHO" 1)
  (setvar "OSMODE" 4133)
) ;_ end of defun
;;==========================================================================
;;;(defun Get-Vr (Rc eMax eN LAspal)
;;;  (setq	Vr-Temp	20.0
;;;	B	(* LAspal 0.5)
;;;	
;;;	fMax	(if (< 80.0 Vr-Temp)
;;;		  (+ (* -0.00065 Vr-Temp) 0.102)
;;;		  (+ (* -0.00125 Vr-Temp) 0.24)
;;;		)
;;;	Rmin	(/ (expt Vr-Temp 2)
;;;		   (* 127.0 (eMax fMax))
;;;
;;;		)
;;;  )
;;;  ;;===================================
;;;  (while (> Rc Rmin)
;;;    (setq
;;;      fMax (if (< 80.0 Vr-Temp)
;;;	     (+ (* -0.00065 Vr-Temp) 0.102)
;;;	     (+ (* -0.00125 Vr-Temp) 0.24)
;;;	   )
;;;      Rmin (/ (expt Vr-Temp 2)
;;;	      (* 127.0 (eMax fMax))
;;;
;;;	   )
;;;      Vr-Temp (+ Vr-Temp 10.0)
;;;    )
;;;  )
;;;  ;;===================================
;;;)
