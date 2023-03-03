(defun GP:NumPITxtTabel	(NumPI)
  (strcat
    (if	(eq (strlen (itoa NumPI)) 1)
      (strcat "0" (itoa NumPI)) ;true
      (itoa NumPI) ;false
    ) ;_ end of if
  ) ;_ end of strcat
) ;_ end of defun
  ;==========================================================
(defun GP:NumPITxt (NumPI)
  (strcat "PI. "
	  (if (eq (strlen (itoa NumPI)) 1)
	    (strcat "0" (itoa NumPI)) ;true
	    (itoa NumPI) ;false
	  ) ;_ end of if
  ) ;_ end of strcat
) ;_ end of defun
  ;------------------------------------------------------------------------------------
(defun INIT:InitTextCoord (TriPointInputLst	       SF	     /
			   NumPI	 CptAng	       BasePt	     SetCptAng
			   DistPIText	 DistTableText PITextPt	     TableTextPt
			   DistDiagram   DiagramPt
			  )

  (setq
    NumPI	  (cdr (assoc "NumPI" TriPointInputLst))
    CptAng	  (cdr (assoc "CptAng" TriPointInputLst))
    BasePt	  (cdr (assoc "BasePt" TriPointInputLst))

    SetCptAng	  (+ CptAng pi)

    DistPIText	  (* 25 SF)
    DistTableText (* (+ 50 DistPIText) SF)
    DistDiagram   (* (+ 50 DistTableText) SF)

    PITextPt	  (polar BasePt SetCptAng DistPIText)
    TableTextPt	  (polar BasePt SetCptAng DistTableText)
    DiagramPt     (polar BasePt SetCptAng DistDiagram)
  ) ;_ end of setq
  (list
    (cons "PITextPt" PITextPt)
    (cons "TableTextPt" TableTextPt)
    (cons "DiagramPt" DiagramPt)
    (cons "CptAng" CptAng)
    (cons "NumPI" NumPI)
  ) ;_ end of list
) ;_ end of defun
  ;================================================================
(defun SHOW:DrawingTextPI (InitTextCoordLst / PITextPt CptAng NumPI TextOut)

  (setq	PITextPt (cdr (assoc "PITextPt" InitTextCoordLst))
	CptAng	 (cdr (assoc "CptAng" InitTextCoordLst))
	NumPI	 (cdr (assoc "NumPI" InitTextCoordLst))
	TextOut	 (GP:NumPITxt NumPI)
	Color	 2
  ) ;_ end of setq
  (GP:ENTMAKE-TEXT PITextPt 3 CptAng Color TextOut)
) ;_ end of defun
  ;================================================================
(defun SHOW:DrawingTableFC (InitTextCoordLst	  FCDataLst  SF		/
			    TableName  TablePt	  ScaleX     ScaleY	Rotation
			    PINUM      Rc	  Delta	     Tc		Ec
			    Lc
			   )
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 20517)
  (setq	TableName "fctable"
	TablePt	  (cdr (assoc "TableTextPt" InitTextCoordLst))
	ScaleX	  (* 1 SF)
	ScaleY	  (* 1 SF)
	Rotation  0.0

	PINUM	  (GP:NumPITxtTabel (cdr (assoc "NumPI" InitTextCoordLst)))
	Rc	  (strcat (rtos (cdr (assoc "Rc" FCDataLst)) 2 2) " m")
	Delta	  (vl-string-subst
		    (chr 176)
		    "d"
		    (angtos (cdr (assoc "Delta" FCDataLst)) 1)
		  ) ;_ end of vl-string-subst
	Tc	  (strcat (rtos (cdr (assoc "Tc" FCDataLst)) 2 2) " m")
	Ec	  (strcat (rtos (cdr (assoc "Ec" FCDataLst)) 2 2) " m")
	Lc	  (strcat (rtos (cdr (assoc "Lc" FCDataLst)) 2 2) " m")
  ) ;_ end of setq
  (command "-insert" TableName TablePt ScaleX ScaleY Rotation PINUM Rc Delta Tc	Ec Lc) ;_ end of command
  (setvar "CMDECHO" 1)
  (setvar "OSMODE" 4133)
  (SHOW:DrawingTextPI InitTextCoordLst)
) ;_ end of defun
  ;================================================================
  ;****************For SCS***********************
(defun SHOW:DrawingTableSCS (InitTextCoordLst  SCSDataLst	 SF	  /
			     TableName	       TablePt	ScaleX	 ScaleY	  Rotation
			     PINUM    Rc       Ls	Delta	 Qs	  Qc
			     Lc	      L	       p	k	 Es	  Ts
			     Ys	      Xs
			    )

  (setvar "OSMODE" 20517)
  (setvar "CMDECHO" 0)
  (setq	TableName "scstable"
	TablePt	  (cdr (assoc "TableTextPt" InitTextCoordLst))
	ScaleX	  (* 1 SF)
	ScaleY	  (* 1 SF)
	Rotation  0.0

	PINUM	  (GP:NumPITxtTabel (cdr (assoc "NumPI" InitTextCoordLst)))
	Rc	  (strcat (rtos (cdr (assoc "Rc" SCSDataLst)) 2 2) " m")
	Ls	  (strcat (rtos (cdr (assoc "Ls" SCSDataLst)) 2 2) " m")
	Delta	  (vl-string-subst
		    (chr 176)
		    "d"
		    (angtos (cdr (assoc "Delta" SCSDataLst))
			    1
		    ) ;_ end of angtos
		  ) ;_ end of vl-string-subst
	Qs	  (vl-string-subst
		    (chr 176)
		    "d"
		    (angtos (cdr (assoc "Qs" SCSDataLst)) 1)
		  ) ;_ end of vl-string-subst
	Qc	  (vl-string-subst
		    (chr 176)
		    "d"
		    (angtos (cdr (assoc "Qc" SCSDataLst)) 1)
		  ) ;_ end of vl-string-subst
	Lc	  (strcat (rtos (cdr (assoc "Lc" SCSDataLst)) 2 2) " m")
	L	  (strcat (rtos (cdr (assoc "L" SCSDataLst)) 2 2) " m")
	p	  (strcat (rtos (cdr (assoc "p" SCSDataLst)) 2 2) " m")
	k	  (strcat (rtos (cdr (assoc "k" SCSDataLst)) 2 2) " m")
	Es	  (strcat (rtos (cdr (assoc "Es" SCSDataLst)) 2 2) " m")
	Ts	  (strcat (rtos (cdr (assoc "Ts" SCSDataLst)) 2 2) " m")
	Xs	  (strcat (rtos (cdr (assoc "Xs" SCSDataLst)) 2 2) " m")
	Ys	  (strcat (rtos (cdr (assoc "Ys" SCSDataLst)) 2 2) " m")
  ) ;_ end of setq
  (command "-insert"	   TableName	   TablePt ScaleX  ScaleY  Rotation
	   Ys	   Xs	   Ts	   Es	   k	   p	   L	   Lc	   Qc
	   Qs	   Delta   Ls	   Rc	   PINUM
	  ) ;_ end of command
 ;_ end of command
 ;_ end of command
 ;_ end of command
  (setvar "OSMODE" 4133)
  (setvar "CMDECHO" 1)
  (SHOW:DrawingTextPI InitTextCoordLst)
) ;_ end of defun
  ;================================================================
(defun SHOW:DrawingTableSS (InitTextCoordLst	SSDataLst SF	    /
			    TableName TablePt	ScaleX	  ScaleY    Rotation
			    PINUM     Rc	Ls	  Delta	    Qs
			    L	      p		k	  Es	    Ts
			   )
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 20517)
  (setq	TableName "sstable"
	TablePt	  (cdr (assoc "TableTextPt" InitTextCoordLst))
	ScaleX	  (* 1 SF)
	ScaleY	  (* 1 SF)
	Rotation  0.0

	PINUM	  (GP:NumPITxtTabel (cdr (assoc "NumPI" InitTextCoordLst)))
	Rc	  (strcat (rtos (cdr (assoc "Rc" SSDataLst)) 2 2) " m")
	Ls	  (strcat (rtos (cdr (assoc "Ls" SSDataLst)) 2 2) " m")
	Delta	  (vl-string-subst
		    (chr 176)
		    "d"
		    (angtos (cdr (assoc "Delta" SSDataLst)) 1)
		  ) ;_ end of vl-string-subst
	Qs	  (vl-string-subst
		    (chr 176)
		    "d"
		    (angtos (cdr (assoc "Qs" SSDataLst)) 1)
		  ) ;_ end of vl-string-subst
	L	  (strcat (rtos (cdr (assoc "L" SSDataLst)) 2 2) " m")
	p	  (strcat (rtos (cdr (assoc "p" SSDataLst)) 2 2) " m")
	k	  (strcat (rtos (cdr (assoc "k" SSDataLst)) 2 2) " m")
	Es	  (strcat (rtos (cdr (assoc "Es" SSDataLst)) 2 2) " m")
	Ts	  (strcat (rtos (cdr (assoc "Ts" SSDataLst)) 2 2) " m")
  ) ;_ end of setq
  (command "-insert"	     TableName	       TablePt	ScaleX	 ScaleY	  Rotation
	   Ts	    Es	     k	      p	       L	Qs	 Delta	  Ls
	   Rc	    PINUM
	  ) ;_ end of command
 ;_ end of command
 ;_ end of command
 ;_ end of command
  (setvar "CMDECHO" 1)
  (setvar "OSMODE" 4133)
  (SHOW:DrawingTextPI InitTextCoordLst)
) ;_ end of defun
