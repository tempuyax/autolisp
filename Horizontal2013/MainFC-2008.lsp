(defun MAIN:MainFC (SF / TriPointInputLst RcInputLst LsInputLst	GetFCLst
		    GetCoordFCLst)
  (if (setq TriPointInputLst (GET:getTriPointInput))
    (if (setq RcInputLst (GET:GetRcInput TriPointInputLst))
      (if (setq GetFCLst (GET:GetFCLst TriPointInputLst RcInputLst))
	(if (setq GetCoordFCLst (GET:GetCoordFC TriPointInputLst GetFCLst SF))
	  (progn
	    (MENU:DrawingFC TriPointInputLst GetFCLst GetCoordFCLst SF)
	  ) ;_ end of progn
	) ;_ end of if
      ) ;_ end of if
    ) ;_ end of if
  ) ;_ end of if
) ;_ end of defun
  ;=========================================================
(defun MENU:DrawingFC (TriPointInputLst GetFCLst GetCoordFCLst SF / num)
  (initget 1 "P N B L A T D")
  (setq	num
	 (strcase
	   (getstring
	     "\nDrawing by Full Circle (<P>oint/Sta-li<N>e/sta-la<B>el/tan-<L>ine/<A>rc/<T>able/<D>iagram)<Enter for All>: "
	   ) ;_ end of getstring
	 ) ;_ end of strcase
  ) ;_ end of setq
  (cond
    ((eq num "P") (SEL:P GetCoordFCLst))
    ((eq num "N") (SEL:N TriPointInputLst GetFCLst GetCoordFCLst))
    ((eq num "B") (SEL:B TriPointInputLst GetFCLst GetCoordFCLst))
    ((eq num "L") (SEL:L TriPointInputLst GetCoordFCLst))
    ((eq num "A") (SEL:A TriPointInputLst GetFCLst))
    ((eq num "T") (SEL:T TriPointInputLst GetFCLst SF))
    ((eq num "D") (SEL:D GetFCLst TriPointInputLst SF))
    ((SEL:AllFC TriPointInputLst GetFCLst GetCoordFCLst SF))
  ) ;_ end of cond
) ;_ end of defun
  ;=========================================================
(defun SEL:A (TriPointInputLst GetFCLst)
  (SHOW:ShowCircle (FC:GetCircle TriPointInputLst GetFCLst))
) ;_ end of defun
  ;=========================================================
(defun SEL:P (GetCoordFCLst)
  (SHOW:DrawingPoint GetCoordFCLst)
) ;_ end of defun
  ;=========================================================
(defun SEL:T (TriPointInputLst GetFCLst SF)
  (SHOW:DrawingTableFC
    (INIT:InitTextCoord TriPointInputLst SF)
    GetFCLst
    SF
  ) ;_ end of SHOW:DrawingTableFC
) ;_ end of defun
  ;=========================================================
(defun SEL:L (TriPointInputLst GetCoordFCLst)
  (SHOW:DrawingTanLineFC
    TriPointInputLst
    GetCoordFCLst
    1
  ) ;_ end of SHOW:DrawingTanLineFC
) ;_ end of defun
  ;=========================================================
(defun SEL:N (TriPointInputLst GetFCLst GetCoordFCLst)
  (SHOW:LineSTA-FC
    TriPointInputLst
    GetFCLst
    GetCoordFCLst
  ) ;_ end of SHOW:LineSTA-FC
) ;_ end of defun
  ;=========================================================
(defun SEL:B (TriPointInputLst
	      GetFCLst
	      GetCoordFCLst
	     )
  (STA:CreateSTATextFC
    TriPointInputLst
    GetFCLst
    GetCoordFCLst
  ) ;_ end of STA:CreateSTATextFC
) ;_ end of defun
  ;=========================================================
(defun PRINT:PrintLine (EntLst StrMess)
  (prompt (strcat "\n" StrMess))
  (foreach n EntLst (print n))
) ;_ end of defun
 ;=======================================================
(defun SEL:D (GetFCLst TriPointInputLst SF)
  ;;(PRINT:PrintLine (SUPERELEV:Auto-Vr-For-LsMin GetFCLst) "Ini data LsMin :")
  ;(SHOW:DrawingDiagramFC
   ; (INIT:InitTextCoord TriPointInputLst SF)
  ;)  
  (SUPERELEV:CoordinatDiagramFC GetFCLst (INIT:InitTextCoord TriPointInputLst SF)) 
) ;_ end of defun
  ;=========================================================
(defun SEL:AllFC (TriPointInputLst GetFCLst GetCoordFCLst SF)
  (SHOW:ShowCircle (FC:GetCircle TriPointInputLst GetFCLst))
  (SHOW:DrawingTableFC
    (INIT:InitTextCoord TriPointInputLst SF)
    GetFCLst
    SF
  ) ;_ end of SHOW:DrawingTableFC
  (SHOW:DrawingTanLineFC
    TriPointInputLst
    GetCoordFCLst
    1
  ) ;_ end of SHOW:DrawingTanLineFC
  (SHOW:LineSTA-FC
    TriPointInputLst
    GetFCLst
    GetCoordFCLst
  ) ;_ end of SHOW:LineSTA-FC
;;;  (STA:CreateSTATextFC
;;;    TriPointInputLst
;;;    GetFCLst
;;;    GetCoordFCLst
;;;  ) ;_ end of STA:CreateSTATextFC

    (SUPERELEV:CoordinatDiagramFC GetFCLst (INIT:InitTextCoord TriPointInputLst SF)) 

) ;_ end of defun
