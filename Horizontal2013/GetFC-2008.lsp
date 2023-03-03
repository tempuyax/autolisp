(defun GET:GetFCLst (TriPointInputLst RcInputLst / Delta Rc Tc Ec Lc)
  (setq
    Delta (cdr (assoc "DeltaAng" TriPointInputLst))
    Rc	  (cdr (assoc "Rc" RcInputLst))

    Tc	  (* Rc (TAN (* 0.5 Delta)))
    Ec	  (* Tc (TAN (* 0.25 Delta)))
    Lc	  (* Rc Delta)
  ) ;end of setq
  (list
    (cons "Rc" Rc)
    (cons "Delta" Delta)
    (cons "Tc" Tc)
    (cons "Ec" Ec)
    (cons "Lc" Lc)
  ) ;_ end of list
) ;end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
