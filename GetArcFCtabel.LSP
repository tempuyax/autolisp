;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; c:GetArcTabel                                                   ;
;;;           Date          ; Nunukan, 7 Juni 2023                                            ;
;;;           Project       ; -                                                               ;
;;;	      File          ; GetArcTabel.LSP                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; Menampilkan Tabel & Label Tipe FC dari Object ARC yg di seleksi ;
;;;                                                                                           ;
;;;           NOTES : Spesial Hari Ultah Ananda Alzena Nabilah yg ke 17 tahun                 ;
;;;                                                                                           ;
;;;           Tetap semangat Dan Terus Berkarya, Selama Kita masih bisa...!!!                 ;
;;;                                                                                           ;
;;;                                                                                           ;
;;;********************************************************************************************
(vl-load-com)
(defun c:GetArcTabel (/ Start-AND End-AND Catl el)

  (if (/= (getvar 'ATTREQ) 1)
    (progn (setvar "ATTREQ" 1)
	   (prompt "\nThe system Attribut Request")
    ) ;_ end of progn
  ) ;_ end of if
  (if (/= (getvar 'AUPREC) 4)
    (progn (setvar "AUPREC" 4)
	   (prompt "\nThe system Angle Unit Prection 4")
    ) ;_ end of progn
  ) ;_ end of if
  (if (/= (getvar 'DIMZIN) 1)
    (progn (setvar "DIMZIN" 1)
	   (prompt "\nThe system Dimensions Zero In ")
    ) ;_ end of progn
  ) ;_ end of if
  (setq *SF 1)

  (defun DrwNumPItxt (Prefix NumPI)
    (strcat Prefix
	    (if	(eq (strlen (itoa NumPI)) 1)
	      (strcat "0" (itoa NumPI))	;true
	      (itoa NumPI)		;false
	    ) ;_ end of if
    ) ;_ end of strcat
  ) ;_ end of defun

  (defun DrawingTableFC	(AllArcLst SF /	NumPI TableName	TblBpt ScaleX ScaleY Rotation PINUM Rc
			 DeltaAng Tc Ec	Lc)
    (setvar "CMDECHO" 0)
    (setvar "OSMODE" 20517)

    (mapcar '(lambda (ArcLst)
	       (setq NumPI     (cdr (assoc 'NumPI ArcLst))
		     TableName "fctable"
		     TblBpt    (cdr (assoc 'TblBpt ArcLst))
		     ScaleX    (* 1 SF)
		     ScaleY    (* 1 SF)
		     Rotation  0.0

		     PINUM     (DrwNumPItxt "" (cdr (assoc 'NumPI ArcLst)))
		     Rc	       (strcat (rtos (cdr (assoc 'Rc ArcLst)) 2 2) " m")
		     DeltaAng  (vl-string-subst
				 (chr 176)
				 "d"
				 (angtos (cdr (assoc 'DeltaAng ArcLst)) 1)
			       ) ;_ end of vl-string-subst
		     Tc	       (strcat (rtos (cdr (assoc 'TC ArcLst)) 2 2) " m")
		     Ec	       (strcat (rtos (cdr (assoc 'EC ArcLst)) 2 2) " m")
		     Lc	       (strcat (rtos (cdr (assoc 'LC ArcLst)) 2 2) " m")
	       ) ;_ end of setq
	       (command	"-insert" TableName TblBpt ScaleX ScaleY Rotation PINUM	Rc DeltaAng Tc Ec
			Lc) ;_ end of command
	     ) ;_ end of lambda
	    AllArcLst
    ) ;_ end of mapcar

    ;;(DrwNumPItxt "PI. " ArcLst)

    (setvar "CMDECHO" 1)
    (setvar "OSMODE" 4133)
    NumPI
  ) ;_ end of defun

  ;; Angle Tangen Lisp Formula
  (defun TAN (val)
    (/ (SIN val) (COS val))
  ) ;_ end of defun

  (defun GetArcDXF (e NumPI / edl CtrPt	Rc SAng	EAng DeltaAng EC Pt1 Pt2 Pt3 TC	LC CptAng)
    (setq
      ;;Data ARC
      edl      (entget e)
      CtrPt    (cdr (assoc 10 edl))
      Rc       (cdr (assoc 40 edl))
      SAng     (cdr (assoc 50 edl))
      EAng     (cdr (assoc 51 edl))

      ;;Calc Elemen Arc
      LC       (vlax-curve-getdistatparam e (vlax-curve-getendparam e))
      DeltaAng (/ LC Rc)
      CptAng
	       (IF (< (ABS (- SAng EAng)) pi)
		 (- (max SAng EAng) (* DeltaAng 0.5))
		 (- (min SAng EAng) (* DeltaAng 0.5))
	       ) ;_ end of IF

      ;;Alignment Horizontal Variables
      TC       (* Rc (TAN (* 0.5 DeltaAng)))
      EC       (* TC (TAN (* 0.25 DeltaAng)))

;;;      EC       (* Rc (* 0.5 DeltaAng) (* 0.25 DeltaAng))

      Pt1      (polar CtrPt SAng Rc)
      Pt2      (polar CtrPt CptAng (+ Rc EC))
      Pt3      (polar CtrPt EAng Rc)

      ;;Setup Tabel BasePoint
      NumPIBpt (polar Pt2 CptAng 25.0)
      TblBpt   (polar Pt2 CptAng 50.0)

    ) ;_ end of setq

    ;;DRAWING EXCHANGE FORMAT
    (list
      (cons 'NumPI NumPI)
      (cons 'NumPIBpt NumPIBpt)
      (cons 'CtrPt CtrPt)
      (cons 'Rc Rc)
      (cons 'SAng SAng)
      (cons 'EAng EAng)
      (cons 'DeltaAng DeltaAng)
      (cons 'TC TC)
      (cons 'EC EC)
      (cons 'LC LC)
      (cons 'TanTriPoints
	    (list
	      (cons 'Pt1 Pt1)
	      (cons 'Pt2 Pt2)
	      (cons 'Pt3 Pt3)
	    ) ;_ end of list
      ) ;_ end of cons
      (cons 'TblBpt TblBpt)
    ) ;_ end of list
  ) ;_ end of defun

  (defun GetArcBySel (elst NumPI)
    (if	(null elst)			; periksa satu satu
      (list)
      (cons
	(GetArcDXF (car elst) NumPI)
	(GetArcBySel (cdr elst) (1+ NumPI))
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
    Start-AND (cons -4 "<AND")
    End-AND   (cons -4 "AND>")
    Catl
	      (list
		Start-AND
		(cons 0 "ARC")
		(cons 8 "0")
		End-AND
	      ) ;_ end of list
  ) ;_ end of setq
  (if (setq el (SelCatl Catl))
    (progn
      ;; Membuat, menyimpan & menampilkan Global Variabel utk Start Number
      (setq *NumPI (cond ((getint (strcat "\nthe Begining by Number: <"
					  (itoa	(setq *NumPI (cond (*NumPI)
								   (1)
							     ) ;_ end of cond
						) ;_ end of setq
					  ) ;_ end of itoa
					  ">: "
				  ) ;_ end of strcat
			  ) ;_ end of getint
			 )
			 (*NumPI)
		   ) ;_ end of cond
      ) ;_ end of setq
      (setq *NumPI (DrawingTableFC (GetArcBySel el *NumPI) *SF))
    ) ;_ end of progn
    (prompt "\n**Not Object**")
  ) ;_ end of if
  (princ)
) ;_ end of defun

