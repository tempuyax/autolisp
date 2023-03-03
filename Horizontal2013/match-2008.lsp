(defun SEC (val)
  (/ 1.0 (COS val))
) ;_ end of defun
  ;==============================================
(defun TAN (val)
  (/ (SIN val) (COS val))
) ;_ end of defun
  ;==============================================
(defun LOGIC:GetLogicValue (TriPtLst ValA ValB / Ang1 Ang2)
  (setq	Ang1 (cdr (assoc "Ang1Rad" TriPtLst))
	Ang2 (cdr (assoc "Ang2Rad" TriPtLst))
  ) ;_ end of setq
  (IF (< (ABS (- Ang1 Ang2)) pi)
    (IF	(= (MAX Ang1 Ang2) Ang1)
      ValB
      ValA
    ) ;_ end of IF
    (IF	(= (MIN Ang1 Ang2) Ang1)
      ValB
      ValA
    ) ;_ end of IF
  ) ;_ end of IF
) ;_ end of defun
  ;==========================================================
(defun LOGIC:GetAngleKPPt (Ang1Rad Ang2Rad Ang-x)
  (IF (< (ABS (- Ang1Rad Ang2Rad)) pi)
    (IF	(= (MAX Ang1Rad Ang2Rad) Ang-x)
      (- Ang-x (* 0.5 pi))
      (+ Ang-x (* 0.5 pi))
    ) ;_ end of IF
    (IF	(= (MIN Ang1Rad Ang2Rad) Ang-x)
      (- Ang-x (* 0.5 pi))
      (+ Ang-x (* 0.5 pi))
    ) ;_ end of IF
  ) ;_ end of IF
) ;_ end of defun
