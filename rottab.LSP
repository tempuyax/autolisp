(vl-load-com)
(defun c:rottab	(/ es ed esl edl New old)
  (if (setq es (car (entsel "Select Source from hwin : ")))
    (if	(setq ed (car (entsel "\nSelect Distination to table : ")))
      (progn
	(setq
	  esl (entget es)
	  edl (entget ed)
	) ;_ end of setq
	(if
	  (and
	    (eq (cdr (assoc 0 esl)) "INSERT")
					;(or
	    (eq (cdr (assoc 2 esl)) "hwin")
					;(eq (cdr (assoc 2 edl)) "fctable")
					;) ;_ end of or
	  ) ;_ end of and
	   (progn
	     (setq New (cdr (assoc 50 esl)))
	     (setq Old (cdr (assoc 50 edl)))
	     (setq bpt (cdr (assoc 10 edl))) ; base point of block 
	     (LM:RotateByMatrix		; Lee mac function
	       (vlax-ename->vla-object ed) ; nama entitiy
	       bpt			;koordinat tidak perlu di translate
	       (- New Old)		; sudut relatif menjadi statif
	     ) ;_ end of LM:RotateByMatrix
	     (prompt "\nDONE")
	   ) ;_ end of progn
	   (prompt "\n*** Selection is not specification ***")
	) ;_ end of if
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of if
  (princ)
) ;_ end of defun

;;;           NOTES : untuk seleksi hanya satu objek tertentu                                 ;
(defun c:rottxt	(/ ed edl New old)
  (if (setq ed (car (entsel "Select of text : ")))
    ;;(if	(setq bpt (getpoint "\nBase Point : "))
    (progn
      (setq
	edl (entget ed)
      ) ;_ end of setq
      (if
	(and
	  (eq (cdr (assoc 0 edl)) "TEXT")
	  ;;(eq (cdr (assoc 8 edl)) "STA") dimatikan
	) ;_ end of and
	 (progn
	   (setq Old (cdr (assoc 50 edl)))
	   (setq bpt (cdr (assoc 11 edl)))
	   (setq New (* 1 pi))		; 180d
	   (LM:RotateByMatrix		; Lee mac function
	     (vlax-ename->vla-object ed) ; nama entitiy
	     bpt			;koordinat tidak perlu di translate
	     New			; sudut tujuan
	   ) ;_ end of LM:RotateByMatrix
	   (prompt "\nDONE")
	 ) ;_ end of progn
	 (prompt "\n*** Selection is not specification ***")
      ) ;_ end of if
    ) ;_ end of progn
    ;;) ;_ end of if
  ) ;_ end of if
  (princ)
) ;_ end of defun

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; c:RotTxtSTA                                                     ;
;;;           Date          ; Nunukan, Mei 2023                                               ;
;;;           Project       ; -                                                               ;
;;;	      File          ; RotTxtSTA.LSP                                                   ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; Rotate Text STA hasil dari LDT                                  ;
;;;                                                                                           ;
;;;           NOTES : untuk seleksi banyak objek tertentu                                     ;
;;;********************************************************************************************
(defun c:RotTxtSTA (/ Txtlayer Begin-AND End-AND Catl el)

  (defun RotTxt	(a)
    (+ a (* 1 pi))
  ) ;_ end of defun

  (defun RotTxtMod (e)
    (entmod
      (subst (cons 50 (RotTxt (cdr (assoc 50 (entget e)))))
	     (assoc 50 (entget e))
	     (entget e)
      ) ;_ end of subst
    ) ;_ end of entmod
    (entupd e)
  ) ;_ end of defun

  (defun ModTxtBySel (elst)
    (if	(null elst)
      ;; periksa satu satu
      t
      (progn
	(RotTxtMod (car elst))
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
    (if	(setq ss (ssget Catl))
      (GetEntName ss 0)
    ) ;_ end of if
  ) ;_ end of defun

  (setq
    Txtlayer  "STALBL"
    Begin-AND (cons -4 "<AND")
    End-AND   (cons -4 "AND>")
    Catl
	      (list
		Begin-AND
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
;;;           Procedure     ; c:RotTab4View                                                   ;
;;;           Date          ; Nunukan, Mei 2023                                               ;
;;;           Project       ; -                                                               ;
;;;	      File          ; RotTab4View.LSP                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; Rotate hanya semua Tabel Alignment Horizontal                   ;
;;;                                                                                           ;
;;;           NOTES : Baru                                                                    ;
;;;********************************************************************************************
(defun c:RotTab4View (/	       dMss	sMss	 BlkName  Begin-AND
		      End-AND  Begin-OR	End-OR	 sCatl	  dCatl
		      els      eld
		     )

  (defun RotTabMod (ed es / esl edl New Old bpt)
    (setq
      esl (entget es)
      edl (entget ed)
      New (cdr (assoc 50 esl))
      Old (cdr (assoc 50 edl))
      bpt (cdr (assoc 10 edl))		; base point of block 
    ) ;_ end of setq
    (LM:RotateByMatrix			; Lee mac function
      (vlax-ename->vla-object ed)	; nama entitiy
      bpt				; koordinat tidak perlu di translate
      (- New Old)			; sudut relatif menjadi statif
    ) ;_ end of LM:RotateByMatrix
    (prompt "\nDONE")
  ) ;_ end of defun

  (defun RotTabBySel (eld els)
    (if	(null eld)			; periksa satu satu
      t
      (progn
	(RotTabMod (car eld) els)
	(RotTabBySel (cdr eld) els)
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of defun

  (defun OneSelCatl (Catl mss / ss)
    (prompt mss)
    (if	(setq ss (ssget ":S" Catl))
      (ssname ss 0)
    ) ;_ end of if
  ) ;_ end of defun

  (defun SelCatl (Catl Mss / ss res)
    (prompt mss)
    (defun GetEntBlkName (ss ct)
      (if (>= ct (sslength ss))
	(list)
	(cons
	  (ssname ss ct)
	  (GetEntBlkName ss (1+ ct))
	) ;_ end of cons
      ) ;_ end of if
    ) ;_ end of defun
    (if	(setq ss (ssget Catl))
      (GetEntBlkName ss 0)
    ) ;_ end of if
  ) ;_ end of defun

  (setq
    sBlkName  "hwin"
    sMss      "==Pick View hwin block=="
    dMss      "==Select All Table block and enter=="
    Begin-AND (cons -4 "<AND")
    End-AND   (cons -4 "AND>")
    Begin-OR  (cons -4 "<OR")
    End-OR    (cons -4 "OR>")
    sCatl
	      (list
		Begin-AND
		(cons 0 "INSERT")
		(cons 2 sBlkName)
		End-AND
	      ) ;_ end of list
    dCatl
	      (list
		Begin-AND
		(cons 0 "INSERT")
		Begin-OR
		(cons 2 "FCTABLE")
		(cons 2 "SCSTABLE")
		(cons 2 "SSTABLE")
		End-OR
		End-AND
	      ) ;_ end of list
  ) ;_ end of setq

  (if (setq eld (SelCatl dCatl dMss))
    (if	(setq els (OneSelCatl sCatl sMss))
      (RotTabBySel eld els)
      (prompt "\n**Not hwin Blok Object**")
    ) ;_ end of if
    (prompt "\n**Not Tabel Blok Object**")
  ) ;_ end of if
  (princ)
) ;_ end of defun

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; c:RotObj                                                        ;
;;;           Date          ; Nunukan, Mei 2023                                               ;
;;;           Project       ; -                                                               ;
;;;	      File          ; RotObj.LSP                                                      ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; Rotate hanya Pilihan Object yang berlayer "0"                   ;
;;;                                                                                           ;
;;;           NOTES : Baru                                                                    ;
;;;********************************************************************************************
(defun c:RotObj	(/	  dMss	   sMss	    BlkName  Begin-AND
		 End-AND  Begin-OR End-OR   sCatl    dCatl    els
		 eld	  Bpt
		)

  (defun RotObjMod (ed es Bpt / esl Ang)
    (setq
      esl (entget es)
      Ang (cdr (assoc 50 esl))
    ) ;_ end of setq
    (LM:RotateByMatrix			; Lee mac function
      (vlax-ename->vla-object ed)	; nama entitiy
      Bpt				; koordinat tidak perlu di translate
      Ang				; sudut relatif menjadi statif
    ) ;_ end of LM:RotateByMatrix
    (prompt "\nDONE")
  ) ;_ end of defun

  (defun RotObjBySel (eld els Bpt)
    (if	(null eld)			; periksa satu satu
      t
      (progn
	(RotObjMod (car eld) els Bpt)
	(RotObjBySel (cdr eld) els Bpt)
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of defun

  (defun OneSelCatl (Catl mss / ss)
    (prompt mss)
    (if	(setq ss (ssget ":S" Catl))
      (ssname ss 0)
    ) ;_ end of if
  ) ;_ end of defun

  (defun SelCatl (Catl Mss / ss res)
    (prompt mss)
    (defun GetEntBlkName (ss ct)
      (if (>= ct (sslength ss))
	(list)
	(cons
	  (ssname ss ct)
	  (GetEntBlkName ss (1+ ct))
	) ;_ end of cons
      ) ;_ end of if
    ) ;_ end of defun
    (if	(setq ss (ssget Catl))
      (GetEntBlkName ss 0)
    ) ;_ end of if
  ) ;_ end of defun

  (setq
    sBlkName  "hwin"
    sMss      "==Pick View hwin block=="
    dMss      "==Select Any Object and enter=="
    Begin-AND (cons -4 "<AND")
    End-AND   (cons -4 "AND>")
    Begin-OR  (cons -4 "<OR")
    End-OR    (cons -4 "OR>")
    sCatl
	      (list
		Begin-AND
		(cons 0 "INSERT")
		(cons 2 sBlkName)
		End-AND
	      ) ;_ end of list
    dCatl
	      (list
		Begin-AND
		(cons 8 "0")
		Begin-OR
		(cons 0 "LWPOLYLINE")
		(cons 0 "LINE")
		(cons 0 "TEXT")
		End-OR
		End-AND
	      ) ;_ end of list
  ) ;_ end of setq
  (if (setq Bpt
	     (GetPtTrans
	       (getpoint (strcat "\nGet Point BasePoint : "))
	     ) ;_ end of GetPtTrans
      ) ;_ end of setq  
    (if	(setq eld (SelCatl dCatl dMss))
      (if (setq els (OneSelCatl sCatl sMss))
	(RotObjBySel eld els Bpt)
	(prompt "\n**Noting hwin Blok Object**")
      ) ;_ end of if
      (prompt "\n**Noting Tabel Blok Object**")
    ) ;_ end of if
    (prompt "\n**Noting BasePoint**")
  ) ;_ end of if
  (princ)
) ;_ end of defun
