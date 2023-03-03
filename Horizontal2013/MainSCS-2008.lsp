(defun MAIN:MainSCS (SF	/ TriPointInputLst RcInputLst LsInputLst GetS-C-SLst GetCoordS-C-SLst)
  (if (setq TriPointInputLst (GET:getTriPointInput))
    (if (setq RcInputLst (GET:GetRcInput TriPointInputLst))
      ;;(if (setq LsInputLst (GET:GetLsInput))
	(if (setq GetS-C-SLst (GET:GetS-C-S TriPointInputLst RcInputLst SF))
	  (if (setq GetCoordS-C-SLst
		     (GET:GetCoordS-C-S TriPointInputLst GetS-C-SLst SF)
	      ) ;_ end of setq
	    (progn
	      (MENU:DrawingSCS
		TriPointInputLst
		GetS-C-SLst
		GetCoordS-C-SLst
		SF
	      ) ;_ end of MENU:DrawingSCS
	    ) ;_ end of progn
	  ) ;_ end of if
	) ;_ end of if
  ;;    ) ;_ end of if
    ) ;_ end of if
  ) ;_ end of if
) ;_ end of defun
  ;=========================================================
(defun MENU:DrawingSCS (TriPointInputLst GetS-C-SLst GetCoordS-C-SLst SF / num)
  (if (> (float (cdr (assoc "Ls" GetS-C-SLst))) 0.0)
    (if	(> (cdr (assoc "Lc" GetS-C-SLst)) 20.0)
      (progn
	(initget 1 "P N B L A S T")
	(setq num
	       (strcase
		 (getstring
		   "\nDrawing Spiral-Circle-Spiral Selected (Point/sta-liNe/sta-laBel/tan-Line/Arc/Spiral/Table) <Enter Drawing All>: "
		 ) ;_ end of getstring
	       ) ;_ end of strcase
	) ;_ end of setq
	(cond
	  ((eq num "P") (SEL:P GetCoordS-C-SLst))
	  ((eq num "N")
	   (SEL:N TriPointInputLst GetS-C-SLst GetCoordS-C-SLst)
	  )
	  ((eq num "B")
	   (SEL:B TriPointInputLst GetS-C-SLst GetCoordS-C-SLst)
	  )
	  ((eq num "L") (SEL:L TriPointInputLst GetCoordS-C-SLst))
	  ((eq num "A") (SEL:A TriPointInputLst GetS-C-SLst))
	  ((eq num "S") (SEL:S TriPointInputLst GetS-C-SLst))
	  ((eq num "T") (SEL:T TriPointInputLst GetS-C-SLst SF))
	  ((SEL:AllSCS
	     TriPointInputLst
	     GetS-C-SLst
	     GetCoordS-C-SLst
	     SF
	   ) ;_ end of SEL:AllSCS
	  )
	) ;_ end of cond
      ) ;end of Progn
      (progn
	(textpage)
	(prompt "\nWARNING!: Don't use SCS Type with Lc <= 20 m")
      ) ;_ end of progn
    ) ;end If
    (progn
      (textpage)
      (prompt "\nWARNING!: Don't use SCS Type with Ls <= 0.0 m")
    ) ;_ end of progn
  ) ;end If	
) ;_ end of defun
  ;==============================================
(defun SEL:P (GetCoordS-C-SLst)
  (SHOW:DrawingPoint
    GetCoordS-C-SLst
  ) ;_ end of SHOW:DrawingPoint
) ;_ end of defun
  ;==============================================
(defun SEL:N (TriPointInputLst GetS-C-SLst GetCoordS-C-SLst)
  (SHOW:LineSTA-SCS
    TriPointInputLst
    GetS-C-SLst
    GetCoordS-C-SLst
  ) ;_ end of SHOW:LineSTA-SCS
) ;_ end of defun
  ;==============================================
(defun SEL:B (TriPointInputLst GetS-C-SLst GetCoordS-C-SLst)
  (STA:CreateSTATextSCS
    TriPointInputLst
    GetS-C-SLst
    GetCoordS-C-SLst
  ) ;_ end of STA:CreateSTATextSCS
) ;_ end of defun
  ;==============================================
(defun SEL:L (TriPointInputLst GetCoordS-C-SLst)
  (SHOW:DrawingTanLineSCS
    TriPointInputLst
    GetCoordS-C-SLst
    1
  ) ;_ end of SHOW:DrawingTanLineSCS
) ;_ end of defun
  ;==============================================
(defun SEL:A (TriPointInputLst GetS-C-SLst)
  (SHOW:ShowCircle
    (SCS:GetCircle TriPointInputLst GetS-C-SLst)
  ) ;_ end of SHOW:ShowCircle
) ;_ end of defun
  ;==============================================
(defun SEL:S (TriPointInputLst GetS-C-SLst)
  (SHOW:TwoSpiral TriPointInputLst GetS-C-SLst)
) ;_ end of defun
  ;==============================================
(defun SEL:T (TriPointInputLst GetS-C-SLst SF)
  (SHOW:DrawingTableSCS
    (INIT:InitTextCoord TriPointInputLst SF)
    GetS-C-SLst
    SF
  ) ;_ end of SHOW:DrawingTableSCS
) ;_ end of defun
  ;==============================================
(defun SEL:AllSCS (TriPointInputLst GetS-C-SLst GetCoordS-C-SLst SF)
  (SHOW:TwoSpiral TriPointInputLst GetS-C-SLst)
  (SHOW:ShowCircle
    (SCS:GetCircle TriPointInputLst GetS-C-SLst)
  ) ;_ end of SHOW:ShowCircle
  (SHOW:DrawingTableSCS
    (INIT:InitTextCoord TriPointInputLst SF)
    GetS-C-SLst
    SF
  ) ;_ end of SHOW:DrawingTableSCS
  (SHOW:DrawingTanLineSCS
    TriPointInputLst
    GetCoordS-C-SLst
    1
  ) ;_ end of SHOW:DrawingTanLineSCS
  (SHOW:LineSTA-SCS
    TriPointInputLst
    GetS-C-SLst
    GetCoordS-C-SLst
  ) ;_ end of SHOW:LineSTA-SCS
  ;(STA:CreateSTATextSCS
  ;  TriPointInputLst
  ;  GetS-C-SLst
  ;  GetCoordS-C-SLst
  ;) ;_ end of STA:CreateSTATextSCS
) ;_ end of defun
