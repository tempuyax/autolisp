(defun MAIN:MainSS (SF / TriPointInputLst RcInputLst LsInputLst	GetS-SLst GetCoordS-SLst)
  (if (setq TriPointInputLst (GET:getTriPointInput))
    (if (setq RcInputLst (GET:GetRcInput TriPointInputLst))
      (if (setq GetS-SLst (GET:GetS-SLst TriPointInputLst RcInputLst))
	(if (setq GetCoordS-SLst (GET:GetCoordS-S TriPointInputLst GetS-SLst SF))
	  (progn
	    (MENU:DrawingSS
	      TriPointInputLst
	      GetS-SLst
	      GetCoordS-SLst
	      SF
	    ) ;_ end of MENU:DrawingSS
	  ) ;_ end of progn
	) ;_ end of if
      ) ;_ end of if
    ) ;_ end of if
  ) ;_ end of if
) ;_ end of defun
  ;=========================================================
(defun MENU:DrawingSS (TriPointInputLst GetS-SLst GetCoordS-SLst SF / num)
  (initget 1 "P N B L S T")
  (setq	num
	 (strcase
	   (getstring
	     "\nDrawing Spiral-Spiral Selected (Point/sta-liNe/sta-laBel/tan-Line/Spiral/Table) <Enter Drawing All>: "
	   ) ;_ end of getstring
	 ) ;_ end of strcase
  ) ;_ end of setq
  (cond
    ((eq num "P") (SEL:P GetCoordS-SLst))
    ((eq num "N") (SEL:N TriPointInputLst GetCoordS-SLst))
    ((eq num "B")
     (SEL:B TriPointInputLst GetS-SLst GetCoordS-SLst)
    )
    ((eq num "L") (SEL:L TriPointInputLst GetCoordS-SLst))
    ((eq num "S") (SEL:S TriPointInputLst GetS-SLst))
    ((eq num "T") (SEL:T TriPointInputLst GetS-SLst SF))
    ((SEL:AllSS TriPointInputLst GetS-SLst GetCoordS-SLst SF))
  ) ;_ end of cond
) ;_ end of defun
  ;=========================================================
(defun SEL:S (TriPointInputLst GetS-SLst)
  (SHOW:TwoSpiral TriPointInputLst GetS-SLst)
) ;_ end of defun
  ;=========================================================
(defun SEL:P (GetCoordS-SLst)
  (SHOW:DrawingPoint
    GetCoordS-SLst
  ) ;_ end of SHOW:DrawingPoint
) ;_ end of defun
  ;=========================================================
(defun SEL:T (TriPointInputLst GetS-SLst SF)
  (SHOW:DrawingTableSS
    (INIT:InitTextCoord TriPointInputLst SF)
    GetS-SLst
    SF
  ) ;_ end of SHOW:DrawingTableSS
) ;_ end of defun
  ;=========================================================
(defun SEL:L (TriPointInputLst GetCoordS-SLst)
  (SHOW:DrawingTanLineSS
    TriPointInputLst
    GetCoordS-SLst
    1
  ) ;_ end of SHOW:DrawingTanLineSS
) ;_ end of defun
  ;=========================================================
(defun SEL:N (TriPointInputLst GetCoordS-SLst)
  (SHOW:LineSTA-SS
    TriPointInputLst
    GetS-SLst
    GetCoordS-SLst
  ) ;_ end of SHOW:LineSTA-SS
) ;_ end of defun
  ;=========================================================
(defun SEL:B (TriPointInputLst GetS-SLst GetCoordS-SLst)
  (STA:CreateSTATextSS
    TriPointInputLst
    GetS-SLst
    GetCoordS-SLst
  ) ;_ end of STA:CreateSTATextSS
) ;_ end of defun
  ;=========================================================
(defun SEL:AllSS (TriPointInputLst GetS-SLst GetCoordS-SLst SF)
  (SHOW:TwoSpiral TriPointInputLst GetS-SLst)
  (SHOW:DrawingTableSS
    (INIT:InitTextCoord TriPointInputLst SF)
    GetS-SLst
    SF
  ) ;_ end of SHOW:DrawingTableSS
  (SHOW:DrawingTanLineSS
    TriPointInputLst
    GetCoordS-SLst
    1
  ) ;_ end of SHOW:DrawingTanLineSS
  (SHOW:LineSTA-SS
    TriPointInputLst
    GetS-SLst
    GetCoordS-SLst
  ) ;_ end of SHOW:LineSTA-SS
) ;_ end of SEL:AllSS
