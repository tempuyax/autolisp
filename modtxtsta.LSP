;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; c:ModTxtSTA                                                     ;
;;;           Date          ; Nunukan, April 2019                                             ;
;;;           Project       ; -                                                               ;
;;;	      File          ; ModTxtSTA.LSP                                                   ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; Modufikasi Text STA Tangen hasil dari LDT                       ;
;;;                                                                                           ;
;;;           NOTES : Baru                                                                    ;
;;;********************************************************************************************
(defun c:ModTxtSTA (/ Txtlayer Star-AND End-AND Catl el)

  (defun DelTxtPCPT (s / ls)
    (defun DelStr (ls s)
      (if (null ls)
	s
	(progn
	  (setq s (vl-string-subst "" (car ls) s))
	  (DelStr (cdr ls) s)
	) ;_ end of progn
      ) ;_ end of if
    ) ;_ end of defun
    (setq ls (list "PC" "PT" "PRC" "=" ""))
    (vl-string-trim " " (DelStr ls s))
  ) ;_ end of defun

  (defun TxtMod (e)
    (entmod
      (subst (cons 1 (DelTxtPCPT (cdr (assoc 1 (entget e)))))
	     (assoc 1 (entget e))
	     (entget e)
      ) ;_ end of subst
    ) ;_ end of entmod
    (entupd e)
  ) ;_ end of defun

  (defun ModTxtBySel (elst)
    (if	(null elst)			; periksa satu satu
      t
      (progn
	(TxtMod (car elst))
	(ModTxtBySel (cdr elst))
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of defun

  (defun SelCatl (Catl / ss res)
    (defun GetEntName (ss ct)
      (if (>= ct (sslength ss))
	(list)
	(cons
	  (ssname ss ct)
	  (GetEntName ss (1+ ct))
	) ;_ end of cons
      ) ;_ end of if
    ) ;_ end of defun
    (if (setq ss (ssget Catl))
      (GetEntName ss 0)
    ) ;_ end of if
  ) ;_ end of defun

  (setq
    Txtlayer "TXTPTS"
    Star-AND (cons -4 "<AND")
    End-AND  (cons -4 "AND>")
    Catl
	     (list
	       Star-AND
	       (cons 0 "TEXT")
	       (cons 8 Txtlayer)
	       End-AND
	     ) ;_ end of list
  ) ;_ end of setq
  (if (setq el (SelCatl Catl))
          (ModTxtBySel el)
          (prompt "\n**Not Object**")
  ) ;_ end of if
  (princ)
) ;_ end of defun

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; c:GetElevEG                                                     ;
;;;           Date          ; Nunukan, April 2019                                             ;
;;;           Project       ; -                                                               ;
;;;	      File          ; GetElevEG.LSP                                                   ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; Membuat text Elevasi Existing Ground (EG)                       ;
;;;                                                                                           ;
;;;           NOTES : Baru                                                                    ;
;;;********************************************************************************************
(defun c:GetElevEG (/ ls XYDELst ObjLayEG Star-AND End-AND Catl ss)
  (defun GetPts	(ls / tem res)
    (mapcar
      '(lambda (as)
	 (if (setq tem (assoc 10 (list as)))
	   (setq res (append res (list (cdr tem))))
	 ) ;_ end of if
       ) ;_ end of lambda
      ls
    ) ;_ end of mapcar
    res
  ) ;_ end of defun

  (defun ToXYDE	(Pts Bpt Bev / Res)
    (mapcar '(lambda (Pt)
	       (setq Res
		      (append
			Res
			(list
			  (apply
			    '(lambda (x y)
			       (list
				 (list x y)
				 (list (- x (car Bpt)) (+ (- y (cadr Bpt)) Bev))
			       ) ;_ end of list
			     ) ;_ end of lambda
			    Pt
			  ) ;_ end of apply
			) ;_ end of list
		      ) ;_ end of append
	       ) ;_ end of setq
	     ) ;_ end of lambda
	    Pts
    ) ;_ end of mapcar
    Res
  ) ;_ end of defun

  (defun PrintElevTXT (XYDELst)
    (defun CrtTxt (pt txt lay)
      (Create-Text
	(list Text:
	      txt
	      layer:
	      lay
	      Height:
	      0.1750
	      centre:
	      (PutPtTrans pt)
	      TxtPoint:
	      (PutPtTrans pt)
	      Rotation:
	      0
	      AlgnHor:
	      1
	      AlgnVer:
	      2
	) ;_ end of list
      ) ;_ end of Create-Text
    ) ;_ end of defun
    (mapcar
      '(lambda (XYDE)
	 (CrtTxt
	   (car XYDE)
	   (rtos (cadr (cadr XYDE)) 2 3)
	   "TxtElevEG"
	 ) ;_ end of CrtTxt
       ) ;_ end of lambda
      XYDELst
    ) ;_ end of mapcar
  ) ;_ end of defun

  (setq
    ObjLayEG  "as-Existing"
    Star-AND (cons -4 "<AND")
    End-AND  (cons -4 "AND>")
    Catl
	     (list
	       Star-AND
	       (cons 0 "LWPOLYLINE")
	       (cons 8 ObjLayEG)
	       End-AND
	     ) ;_ end of list
  ) ;_ end of setq
  (prompt "\nGet object EG : ")
  (if (setq ss (ssget ":S" Catl))
    (progn
      (setq ls (entget (ssname ss 0)))
      (if (setq Bev (getreal "\nEnter Base Elevation : "))
	(if (setq Bpt (GetPtTrans (getpoint "\nEnter Base Point : ")))
	  (progn
	    (setq XYDELst (ToXYDE (GetPts ls) Bpt Bev))
	    (PrintElevTXT XYDELst)
	    ;;(foreach n XYDELst (print n))
	  ) ;_ end of progn
	) ;_ end of if
      ) ;_ end of if
    ) ;_ end of progn
    (prompt "\n**Not Object**")
  ) ;_ end of if
  (princ)
) ;_ end of defun


;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; c:GetStaEG                                                      ;
;;;           Date          ; Nunukan, April 2019                                             ;
;;;           Project       ; -                                                               ;
;;;	      File          ; ModTxtSTA.LSP                                                   ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; Membuat text Elevasi Finishing Ground (FG)                      ;
;;;                                                                                           ;
;;;           NOTES : Baru                                                                    ;
;;;********************************************************************************************
(defun c:GetStaEG (/ XYDLst ObjLayEG Star-AND End-AND Catl ss)

  (defun GetPts	(ls / tem res)
    (mapcar
      '(lambda (as)
	 (if (setq tem (assoc 10 (list as)))
	   (setq res (append res (list (cdr tem))))
	 ) ;_ end of if
       ) ;_ end of lambda
      ls
    ) ;_ end of mapcar
    res
  ) ;_ end of defun

  (defun ToXYD (Pts Bpt / Res)
    (mapcar '(lambda (Pt)
	       (setq Res
		      (append
			Res
			(list
			  (apply
			    '(lambda (x y)
			       (list
				 (list x y)
				 (list (- x (car Bpt)))
			       ) ;_ end of list
			     ) ;_ end of lambda
			    Pt
			  ) ;_ end of apply
			) ;_ end of list
		      ) ;_ end of append
	       ) ;_ end of setq
	     ) ;_ end of lambda
	    Pts
    ) ;_ end of mapcar
    Res
  ) ;_ end of defun

  (defun PrintStaTxt (XYDLst / s ntrim STAtxt)
    (defun CrtTxt (pt txt lay)
      (Create-Text
	(list Text:
	      txt
	      layer:
	      lay
	      Height:
	      0.1750
	      centre:
	      (PutPtTrans pt)
	      TxtPoint:
	      (PutPtTrans pt)
	      Rotation:
	      0
	      AlgnHor:
	      1
	      AlgnVer:
	      2
	) ;_ end of list
      ) ;_ end of Create-Text
    ) ;_ end of defun
    (mapcar
      '(lambda (XYD)
	 (setq
	   s	  (rtos (car (cadr XYD)) 2 2)
	   ntrim  (substr s (+ (vl-string-position (ascii ".") s) 1))
					;nilai desimal
	   STAtxt (vl-string-subst
		    "+"
		    "."
		    (rtos (/ (atof (vl-string-subst "" ntrim s)) 100)
			  2
			  3
		    ) ;_ end of rtos
		  ) ;_ end of vl-string-subst
	 ) ;_ end of setq
	 (CrtTxt (car XYD) STAtxt "TxtStaEG")
       ) ;_ end of lambda
      XYDLst
    ) ;_ end of mapcar
  ) ;_ end of defun

  (setq
    ObjLayEG "as-Existing"
    Star-AND (cons -4 "<AND")
    End-AND  (cons -4 "AND>")
    Catl
	     (list
	       Star-AND
	       (cons 0 "LWPOLYLINE")
	       (cons 8 ObjLayEG)
	       End-AND
	     ) ;_ end of list
  ) ;_ end of setq
  (prompt "\nGet object EG : ")
  (if (setq ss (ssget ":S" Catl))
    (progn
      (setq ls (entget (ssname ss 0)))
      (if (setq Bpt (GetPtTrans (getpoint "\nEnter Base Point : ")))
	(progn
	  (setq XYDLst (ToXYD (GetPts ls) Bpt))
	  (PrintStaTxt XYDLst)
	  ;;(foreach n XYDLst (print n))
	) ;_ end of progn
      ) ;_ end of if
    ) ;_ end of progn
    (prompt "\n**Not Object**")
  ) ;_ end of if
  (princ)
) ;_ end of defun

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; c:MoveTxtLine                                                   ;
;;;           Date          ; Nunukan, April 2019                                             ;
;;;           Project       ; -                                                               ;
;;;	      File          ; ModTxtSTA.LSP                                                   ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; Memindah text ke posisi tertentu secara horizontal linier       ;
;;;                                                                                           ;
;;;           NOTES : Baru                                                                    ;
;;;********************************************************************************************
(defun c:MoveTxtLine (/ Txtlayer Star-AND End-AND Catl Bpt txt num el)

  (defun MoveTxt (Pt Bpt)
    (list (car Pt) (cadr Bpt))
  ) ;_ end of defun

  (defun TxtMod	(e Bpt)
    (entmod
      (subst (cons 11 (MoveTxt (cdr (assoc 11 (entget e))) Bpt))
	     (assoc 11 (entget e))
	     (entget e)
      ) ;_ end of subst
    ) ;_ end of entmod
    (entupd e)
  ) ;_ end of defun

  (defun MoveTxtBySel (elst Bpt)
    (if	(null elst)			; periksa satu satu
      t
      (progn
	(TxtMod (car elst) Bpt)
	(MoveTxtBySel (cdr elst) Bpt)
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of defun

  (defun SelCatl (Catl / ss res)
    (defun GetEntName (ss ct)
      (if (>= ct (sslength ss))
	(list)
	(cons
	  (ssname ss ct)
	  (GetEntName ss (1+ ct))
	) ;_ end of cons
      ) ;_ end of if
    ) ;_ end of defun
    (if (setq ss (ssget Catl))
      (GetEntName ss 0)
    ) ;_ end of if
  ) ;_ end of defun

  (defun main ()
    (setq
      Star-AND (cons -4 "<AND")
      End-AND  (cons -4 "AND>")
      Catl
	       (list
		 Star-AND
		 (cons 0 "TEXT")
		 (cons 8 Txtlayer)
		 End-AND
	       ) ;_ end of list
    ) ;_ end of setq
    (if	(setq Bpt
	       (GetPtTrans
		 (getpoint (strcat "\nGet Point LINIER Text " txt " Pos : "))
	       ) ;_ end of PtTrans
	) ;_ end of setq
        (if (setq el (SelCatl Catl))
            (MoveTxtBySel el Bpt)
            (prompt "\n**Not Object**")
        ); if	
    ) ;_ end of if
  ) ;_ end of defun

  (initget 1 "E S F")
  (setq	num
	 (strcase
	   (getstring
	     "\nMove Linier Text Selected elevation-(E)G/(S)tation/elevation-(F)G :"
	   ) ;_ end of getstring
	 ) ;_ end of strcase
  ) ;_ end of setq
  (cond
    ((eq num "E")
     (setq Txtlayer "TxtElevEG")
     (setq txt "Elevation EG")
     (main)
    )
    ((eq num "S")
     (setq Txtlayer "TxtStaEG")
     (setq txt "Station")
     (main)
    )
    ((eq num "F")
     (setq Txtlayer "TxtElevFG")
     (setq txt "Elevation FG")
     (main)
    )
    (t num)
  ) ;_ end of cond
  (princ)
) ;_ end of defun

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; c:GetElevFG                                                     ;
;;;           Date          ; Nunukan, April 2019                                             ;
;;;           Project       ; -                                                               ;
;;;	      File          ; ModTxtSTA.LSP                                                   ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; Mengambil & menampilkan text elevasi rencana                    ;
;;;                                                                                           ;
;;;           NOTES : Baru                                                                    ;
;;;********************************************************************************************

(defun c:GetElevFG (/ Star-OR End-OR Star-AND FGCatl VGlayer PGlayer VGCat Bev Bpt FGe VGes InterPts XYELst)

(defun InterAnyByAny (elst1 elst2 mss / res tem)
  (prompt mss)
  (defun RemovSome (ls)
    (if	(null ls)			; periksa satu satu
      (list)
      (cons
	(car ls)
	(RemovSome (vl-remove (car ls) ls))
      ) ;_ end of cons
    ) ;_ end of if
  ) ;_ end of defun
  (mapcar '(lambda (e1)
	     (mapcar '(lambda (e2)
			(if (setq tem (vlax-invoke
					(vlax-ename->vla-object e1)
					'intersectwith
					(vlax-ename->vla-object e2)
					2
				      ) ;_ end of vlax-invoke
			    ) ;_ end of setq
			  (setq res (append res (list tem)))
			) ;_ end of if
		      ) ;_ end of lambda
		     elst2
	     ) ;_ end of mapcar
	   ) ;_ end of lambda
	  elst1
  ) ;_ end of mapcar
  (setq res (RemovSome res))
) ;_ end of defun  

(defun SelCatl (Catl mss / ss res)
  (defun GetEntName (ss ct)
    (if	(>= ct (sslength ss))
      (list)
      (cons
	(ssname ss ct)
	(GetEntName ss (1+ ct))
      ) ;_ end of cons
    ) ;_ end of if
  ) ;_ end of defuna
  (prompt mss)
  (if (setq ss (ssget Catl))
    (GetEntName ss 0)
  ) ;_ end of if
) ;_ end of defun

  (defun OneSelCatl (Catl mss / ss)
  (prompt mss)
    (if	(setq ss (ssget ":S" Catl))
      (ssname ss 0)
    ) ;_ end of if
  ) ;_ end of defun

  (defun ToXYE (Pts Bpt Bev / Res)
    (mapcar '(lambda (Pt)
	       (setq Res
		      (append
			Res
			(list
			  (apply
			    '(lambda (x y z)
			       (list
				 (list x y)
				 (list (+ (- y (cadr Bpt)) Bev))
			       ) ;_ end of list
			     ) ;_ end of lambda
			    Pt
			  ) ;_ end of apply
			) ;_ end of list
		      ) ;_ end of append
	       ) ;_ end of setq
	     ) ;_ end of lambda
	    Pts
    ) ;_ end of mapcar
    Res
  ) ;_ end of defun

  (defun PrintElevTXT (XYELst)
    (defun CrtTxt (pt txt lay)
      (Create-Text
	(list Text:
	      txt
	      layer:
	      lay
	      Height:
	      0.1750
	      centre:
	      (PutPtTrans pt)
	      TxtPoint:
	      (PutPtTrans pt)
	      Rotation:
	      0
	      AlgnHor:
	      1
	      AlgnVer:
	      2
	) ;_ end of list
      ) ;_ end of Create-Text
    ) ;_ end of defun
    (mapcar
      '(lambda (XYE)
	 (CrtTxt
	   (car XYE)
	   (rtos (car (cadr XYE)) 2 3)
	   "TxtElevFG"
	 ) ;_ end of CrtTxt
       ) ;_ end of lambda
      XYELst
    ) ;_ end of mapcar
  ) ;_ end of defun

  (setq
    Star-OR  (cons -4 "<OR")
    End-OR   (cons -4 "OR>")
    Star-AND (cons -4 "<AND")
    End-AND  (cons -4 "AND>")
    InSecmss "\n**Wait In Progress......."
    FGmss    "\n!Select any Finishing Ground/Design ->"
    FGlayer  "As Renc"
    FGCatl
	     (list
	       Star-OR
	       Star-AND
	       (cons 0 "LWPOLYLINE")
	       (cons 8 FGlayer)
	       End-AND
	       Star-AND
	       (cons 0 "LINE")
	       (cons 8 FGlayer)
	       End-AND
	       Star-AND
	       (cons 0 "POLYLINE")
	       (cons 8 FGlayer)
	       End-AND
	       End-OR
	     ) ;_ end of list
    VGmss    "\n!Select any Line Vertical Grid/Station ->"
    VGlayer  "v-grid"
    PGlayer  "p-grid"
    VGCatl
	     (list
	       Star-OR
	       Star-AND
	       (cons 0 "LINE")
	       (cons 8 VGlayer)
	       End-AND
	       Star-AND
	       (cons 0 "LINE")
	       (cons 8 PGlayer)
	       End-AND
	       End-OR
	     ) ;_ end of list
  ) ;_ end of setq

  (initget 1)
  (if (setq Bev (getreal "\nEnter Base Elevation : "))
    (if	(setq Bpt (GetPtTrans (getpoint "\nEnter Base Point : ")))
      (if (setq FGes (SelCatl FGCatl FGmss))
	(if (setq VGes (SelCatl VGCatl VGmss))
	  (if (setq InterPts (InterAnyByAny FGes VGes InSecmss))
	    (progn
	      (setq XYELst (ToXYE InterPts Bpt Bev))
	      (foreach n InterPts (print n))
	      (PrintElevTXT XYELst)
	    ) ;_ end of progn
	  ) ;_ end of if
	) ;_ end of if
      ) ;_ end of if
    ) ;_ end of if
  ) ;_ end of if

  (princ)
) ;_ end of defun
;;;;;;
;;;;;


;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; c:DelTxtCrt                                                     ;
;;;           Date          ; Nunukan, Oktober 2022                                           ;
;;;           Project       ; -                                                               ;
;;;	      File          ; ModTxtSTA.LSP                                                   ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; Delete Text yang dibuat dari lisp ini                           ;
;;;                                                                                           ;
;;;           NOTES : Baru                                                                    ;
;;;********************************************************************************************
(defun c:DelTxtCrt (/ Txtlayer Star-AND End-AND Catl Bpt txt num el)

  (defun SelCatl (Catl / ss res)
    (defun GetEntName (ss ct)
      (if (>= ct (sslength ss))
	(list)
	(cons
	  (ssname ss ct)
	  (GetEntName ss (1+ ct))
	) ;_ end of cons
      ) ;_ end of if
    ) ;_ end of defun
    (if (setq ss (ssget Catl))
      (GetEntName ss 0)
    ) ;_ end of if
  ) ;_ end of defun

  (defun main ()
    (setq
      Star-AND (cons -4 "<AND")
      End-AND  (cons -4 "AND>")
      Catl
	       (list
		 Star-AND
		 (cons 0 "TEXT")
		 (cons 8 Txtlayer)
		 End-AND
	       ) ;_ end of list
    ) ;_ end of setq
    (if (setq el (SelCatl Catl))
          (foreach e el (entdel e))
          (prompt "\n**Not Object**")
    ) ;_ end of if
) ;_ end of defun

  (initget 1 "E S F")
  (setq	num
	 (strcase
	   (getstring
	     "\nDelete Text Selected <elev-(E)G/(S)tation/elev-(F)G> :"
	   ) ;_ end of getstring
	 ) ;_ end of strcase
  ) ;_ end of setq
  (cond
    ((eq num "E")
     (setq Txtlayer "TxtElevEG")
     (setq txt "Elevation EG")
     (main)
    )
    ((eq num "S")
     (setq Txtlayer "TxtStaEG")
     (setq txt "Station")
     (main)
    )
    ((eq num "F")
     (setq Txtlayer "TxtElevFG")
     (setq txt "Elevation FG")
     (main)
    )
    (t num)
  ) ;_ end of cond
  (princ)
) ;_ end of defun



;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; c:GetHV                                                         ;
;;;           Date          ; Nunukan, April 2019                                             ;
;;;           Project       ; -                                                               ;
;;;	      File          ; ModTxtSTA.LSP                                                   ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; Mengambil view & mengambil block hwin utk initial rot viewport  ;
;;;                                                                                           ;
;;;           NOTES : Baru                                                                    ;
;;;********************************************************************************************
(defun c:GetHV ()

  (defun CrtCircle (p r)
    (Create-Circle
      (list
	centre:
	(PutPtTrans p)
	radius:
	r
	color:
	-Red
      ) ;_ end of list
    ) ;_ end of Create-Circle
  ) ;_ end of defun

  (defun InitPt	(/ ss Star-AND End-AND Catl ls ang bpt pt tptls obj)
    (setq
      Star-AND (cons -4 "<AND")
      End-AND  (cons -4 "AND>")
      Catl
	       (list
		 Star-AND
		 (cons 0 "INSERT")
		 (cons 2 "hwin")
		 End-AND
	       ) ;_ end of list
    ) ;_ end of setq
    (prompt "\nGet block of view : ")
    (if	(setq ss (ssget ":S" Catl))
      (progn
	(setq ls (entget (ssname ss 0)))
	(setq ang (cdr (assoc 50 ls)))
	(setq bpt (GetPtTrans (cdr (assoc 10 ls)))); base point of block, use GetPtTrans
	(setq pt (polar bpt ang 50))
	(setq tptls (LM:ReflectByMatrix
		      (list pt)
		      bpt
		      (polar bpt 0 50)
		    ) ;_ end of LM:ReflectByMatrix
	      obj   (handent (CrtCircle (car tptls) 5))
	) ;_ end of setq
	(list
	  (cons 'obj obj)
	  (cons 'pt pt)
	  (cons 'bpt bpt)
	  (cons 'tpt (car tptls))	;one point must be car
	) ;_ end of list
      ) ;_ end of progn    
      (prompt "\n**Not Object**")
    ) ;_ end of if
  ) ;_ end of defun

  (if (eq (getvar 'TILEMODE) 1)
    (progn
      (setq *view (InitPt))
      (command "-view" "w" "tes")
      (prompt "\nGet View Ok")
    ) ;_ end of progn
    (prompt "\n**harus ke layar model**")
  ) ;_ end of if
  (princ)
) ;_ end of defun


;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; c:PutHV                                                         ;
;;;           Date          ; Nunukan, April 2019                                             ;
;;;           Modified      ; Nunukan, Oktober 2022                                           ;
;;;           Project       ; -                                                               ;
;;;	      File          ; ModTxtSTA.LSP                                                   ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; Merotasi viewport di layout longsection                         ;
;;;                                                                                           ;
;;;           NOTES : Baru                                                                    ;
;;;********************************************************************************************
(defun c:PutHV ()
  (if (eq (getvar 'TILEMODE) 0)
    (progn
      (command "-view" "r" "tes" pause)
      (prompt "\nPut View Ok")
      (c:mvsetup)
      (command "_.PSPACE")
    ) ;_ end of progn
    (prompt "\n**harus ke layar layot**")
  ) ;_ end of if
  (princ)
) ;_ end of defun

;;;Restoring Model space view.
;;;Select Viewport for view: Regenerating model.
;;;cara memutar viewport
;;;Command: mvsetup
;;;Initializing...
;;;Enter an option [Align/Create/Scale viewports/Options/Title block/Undo]: a
;;;Enter an option [Angled/Horizontal/Vertical alignment/Rotate view/Undo]: r
;;;Specify basepoint in the viewport with the view to be rotated: cen
;;;Specify angle from basepoint: cen
;;;Enter an option [Angled/Horizontal/Vertical alignment/Rotate view/Undo]:
;;;Enter an option [Align/Create/Scale viewports/Options/Title block/Undo]:




  



	     
	     
