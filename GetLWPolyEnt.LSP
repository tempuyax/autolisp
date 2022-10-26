;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@gmail.com                             ;
;;;           Procedure     ; c:GetLWPolyEnt                                                  ;
;;;           Date          ; Nunukan, Maret 2021                                             ;
;;;           Project       ; Get data of entites Tools                                       ;
;;;	      File          ; GetLWPolyLineEnt.LSP                                            ;
;;;           Version       ; Betatester 27.03.2021                                           ;
;;;	      Description   ; Mengambil data vertex dari entity type LWPOLY sekaligus         ;
;;;                           mengeset layer secara recrusive. pilihan hasil Data bisa        ;
;;;                           disimpan ke file                                                ;
;;;                                                                                           ;
;;;           NOTES : Copas dari c:ElemenByEnt                                                ;
;;;********************************************************************************************
(defun c:GetLWPolyEnt (/	  Elmlayer   ElmColor	Setlayer
		       Star-AND	  End-AND    SelCatl	ModCatl
		       enl
		      )

  (defun GetPtsLWPoly (e el / Vertex Bulge AssLst SortAssLst Pts)
    (setq
      Vertex 10
      Bulge 42
    )
    (mapcar
      '(lambda (Ass)
	 (setq AssLst (list Ass))
	 (if (setq SortAssLst (assoc Vertex AssLst))
	   (setq Pts (append Pts (list (cdr SortAssLst))))
	 ) ;_ end of if
       ) ;_ end of lambda
      el
    ) ;_ end of mapcar
    (list
      (cons 'EntName e)
      (cons 'Pts Pts)
    ) ;_ end of list
  ) ;_ end of defun

  (defun ObjMod	(e ModCatl / el)
    (setq el (entget e))
    (mapcar
      '(lambda (as)
	 (setq el
		(subst as
		       (assoc (car as) el)
		       el
		) ;_ end of subst
	 ) ;_ end of setq
       ) ;_ end of lambda
      ModCatl
    ) ;_ end of mapcar
    (entmod el)
    (entupd e)
    (GetPtsLWPoly e el)			;Output Langsung Membuat List
  ) ;_ end of defun

  (defun ModElmBySel (elst ModCatl)
    (if	(null elst)			; periksa satu satu
      (list)
      (cons
	(ObjMod (car elst) ModCatl)
	(ModElmBySel (cdr elst) ModCatl)
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of defun

  (defun fSelCatl (Catl / ss)
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

  (defun fSaveFile (/ opt file mdir dir fn)
    (initget "Y N  _Yes No")
    (setq
      opt  (getkword "\nDo you want to Save File ? (Yes/No): ")
      mdir "D:/DATA-pahor/Lisp Excel"
      dir  "Struktur"
      fn   "TES1.txt"
    )
    (if	(= opt "Yes")
      (progn
	(setq file (open (strcat mdir "/" dir "/" fn) "w"))
	(foreach n *ElemenVektor (print n file))
					;	(princ *ElemenVektor file)
	(close file)
      )
      (foreach n *ElemenVektor (print (car n)))
					;      (princ *ElemenVektor)
    )
  )

  (setq
    Elmlayer "Elemen"
    ElmColor 3				;green
    SelLayer "0"
    Star-AND (cons -4 "<AND")
    End-AND  (cons -4 "AND>")
    SelCatl
	     (list
	       Star-AND
	       (cons 0 "LWPOLYLINE")
	       (cons 8 SelLayer)
	       End-AND
	     ) ;_ end of list
    ModCatl
	     (list
	       (cons 62 ElmColor)
	       (cons 8 ElmLayer)
	     ) ;_ end of list

  ) ;_ end of setq
  (if (setq enl (reverse (fSelCatl SelCatl)))
    (progn
      (setq *ElemenVektor (ModElmBySel enl ModCatl))
      (fSaveFile)
    ) ;_ end of progn
    (prompt "Tidak ada input")
  ) ;_ end of if
  (princ)
) ;_ end of defun
