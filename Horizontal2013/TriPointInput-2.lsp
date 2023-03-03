  ;==========================================================
(defun NumPIDeflt (str1 def)
  (strcat str1 "Last PI Number is <" (itoa def) ">: ")
) ;_ end of defun
  ;================================================================
(defun GET:getTriPointInput (/	      Pt1      Pt2	Pt3	 Ang1Rad  Ang2Rad
			     MaxAng   MinAng   Dist1	Dist2	 Dist3	  BetaAng
			     DeltaAng s	       Q	AngKP1Rad	  AngKP2Rad
			    )
  (if (not *GlobNumPI)
    (setq *GlobNumPI 1)
  ) ;_ end of if
  (setq NumPI (getint (NumPIDeflt "\nEnter PI Number->" *GlobNumPI)))
  (if (not NumPI)
    (setq NumPI *GlobNumPI)
    (setq *GlobNumPI NumPI)
  ) ;_ end of if

  (prompt "\rPick 3 Magic Points............: ")
  (initget 128)
  (if (setq Pt1 (getpoint "\nGet First point <Start Point>: "))
    (if	(setq Pt2 (getpoint "\nGet Second point <Center Point>: "))
      (if (setq Pt3 (getpoint "\nGet Third point <End Point> "))
	(progn
	  (setq	Ang1Rad	  (angle Pt2 Pt1)
		Ang2Rad	  (angle Pt2 Pt3)

		Ang12	  (angle Pt1 Pt2)
		Ang32	  (angle Pt3 Pt2)

		MaxAng	  (max Ang1Rad Ang2Rad)
		MinAng	  (min Ang1Rad Ang2Rad)

		Dist1	  (distance Pt1 Pt2)
		Dist2	  (distance Pt2 Pt3)
		Dist3	  (distance Pt1 Pt3)

		s	  (* 0.5 (+ Dist1 Dist2 Dist3))
		Q	  (sqrt (/ (* (- s Dist1) (- s Dist2) (- s Dist3)) s))
		BetaAng	  (* 2.0 (atan (/ Q (- s Dist3))))
		DeltaAng  (- pi BetaAng)

		CptAng
			  (IF (< (ABS (- Ang2Rad Ang1Rad)) pi)
			    (- MaxAng (* BetaAng 0.5))
			    (- MinAng (* BetaAng 0.5))
			  ) ;_ end of IF

		AngKP1Rad (LOGIC:GetAngleKPPt Ang1Rad Ang2Rad Ang1Rad)
		AngKP2Rad (LOGIC:GetAngleKPPt Ang1Rad Ang2Rad Ang2Rad)

	  ) ;_ end of setq

	  (list
            (cons "NumPI" NumPI)
	    (cons "Pt1" Pt1)
	    (cons "Pt2" Pt2)
	    (cons "Pt3" Pt3)

	    (cons "BasePt" Pt2)

	    (cons "Ang1Rad" Ang1Rad)
	    (cons "Ang2Rad" Ang2Rad)

	    (cons "Ang12" Ang12)
	    (cons "Ang32" Ang32)

	    (cons "Dist1" Dist1)
	    (cons "Dist2" Dist2)
	    (cons "Dist3" Dist3)

	    (cons "MaxAng" MaxAng)
	    (cons "MinAng" MinAng)

	    (cons "BetaAng" BetaAng)
	    (cons "DeltaAng" DeltaAng)

	    (cons "CptAng" CptAng)
	    (cons "AngKP1Rad" AngKP1Rad)
	    (cons "AngKP2Rad" AngKP2Rad)

	  ) ;_ end of list
	) ;_ end of progn
      ) ;_ end of if
    ) ;_ end of if
  ) ;_ end of if
) ;_ end of defun
  ;==============================================Fungsi Ls Input Tdk Digunakan
(defun GET:getLsInput (/ Ls)
  (initget 128)
  (if (setq Ls (getreal "\nEnter Length Spiral (Ls): "))
    (list
      (cons "Ls" Ls)
    ) ;_ end of list
  ) ;_ end of if
) ;_ end of defun
  ;===================================================
(defun GET:getVrInput (/ Vr)
  (initget 128)
  (if (setq Vr (getreal "\nEnter Speed Plant (Vr): "))
    (list
      (cons "Vr" Vr)
    ) ;_ end of list
  ) ;_ end of if
) ;_ end of defun
  ;===================================================
(defun GET:get-eMax-Input (/ eMax)
  (initget 128)
  (if (setq eMax (getreal "\nEnter e Maximum (eMax): "))
    (list
      (cons "eMax" eMax)
    ) ;_ end of list
  ) ;_ end of if
) ;_ end of defun
  ;===================================================
(defun GET:get-en-Input (/ en)
  (initget 128)
  (if (setq en (getreal "\nEnter e Normal (en): "))
    (list
      (cons "en" en)
    ) ;_ end of list
  ) ;_ end of if
) ;_ end of defun
  ;===================================================
(defun GET:getDistanceInput (/ TC PT1 PT2 Dist)
  (prompt "\nGet Distace from two points: ")
  (initget 128)
  (if (setq Pt1 (getpoint "\nGet First point <Start Point>: "))
    (if	(setq Pt2 (getpoint "\nGet Second point <End Point> : "))
      (progn
	(setq Dist (distance Pt1 Pt2))
	(list (cons "Dist" Dist))
      )
    )
  )
)
  ;==============================================
(defun GET:getRealRcInput (/ Rc)
  (initget 128)
  (if (setq Rc (getreal "\nEnter Radius (Rc): "))
    (list
      (cons "Rc" Rc)
    ) ;_ end of list
  ) ;_ end of if
) ;_ end of defun
  ;===================================================
(defun GET:GetECtoRC (TriPointInputLst / DistanceInputLst Delta Rc)
  (setq
    DistanceInputLst (GET:getDistanceInput)
    Delta (cdr (assoc "DeltaAng" TriPointInputLst))
    Dist  (cdr (assoc "Dist" DistanceInputLst))
    ;Dist/((TAN(RADIANS(Delta)/2))*(TAN(RADIANS(Delta)/4)))
    Rc	  (fix (/ Dist
	         (*
	           (TAN (* 0.5 Delta))
	           (TAN (* 0.25 Delta))
	         )
	       )
	   )    
  )
  (list
    (cons "Rc" Rc)
  )
)
  ;===================================================
(defun GET:GetTCtoRC (TriPointInputLst / DistanceInputLst Delta Rc)
  (setq
    DistanceInputLst (GET:getDistanceInput)
    Delta (cdr (assoc "DeltaAng" TriPointInputLst))
    Dist  (cdr (assoc "Dist" DistanceInputLst))
    ;Dist/TAN(RADIANS(Delta)*0.5)
    Rc	  (/ Dist
            (TAN (* 0.5 Delta))
	  )
  )
  (list
    (cons "Rc" Rc)
  )
)
  ;===================================================
(defun GET:getRcInput (TriPointInputLst / TC PT1 PT2)
  (prompt "\nGet Rc............: ")
  (initget 1 "R T E")
  (setq	num
	 (strcase
	   (getstring
	     "\nInput Selected of circle Elemen (R)adius Rc/(T)angen Tc/(E)centricity Ec : "
	   ) ;_ end of getstring
	 ) ;_ end of strcase
  ) ;_ end of setq
  (cond
    ((eq num "R") (GET:getRealRcInput))
    ((eq num "T") (GET:GetTCtoRC TriPointInputLst))
    ((eq num "E") (GET:GetECtoRC TriPointInputLst))
    ((GET:getRealRcInput))
  ) ;_ end of cond
) ;_ end of defun
