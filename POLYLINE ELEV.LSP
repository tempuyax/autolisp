;;;Editor     : Pahor. M
;;;Tanggal    : nunukan, 07 Februari 2010
;;;Koreksi    : 04 April 2019
;;;DIskripsi  : Membaca data bulge & titik tipe POLYLINE
;;;=======================================================================================
(defun C:PolyElev (/		    CatagorySelectionLst-01
		   NilaiElev	    BasePt	     ObjName
		   polypointsLst    OutList	     Elev-Dist-List
		   Elev-List	    Dist-List        HorDistLst val-z
		  )
;;;=======================================================================================
  (defun POLYLINE:ReadBulgeData	(SelectionNameObjLst
				 /		    ct
				 NextSelectionNameObjLst
				 ObjLst		    PointData
				 BulgeData	    polypointsLst
				)
    (setq ct 0)
    (repeat
      (length
	SelectionNameObjLst
      ) ;_ end of length
       (setq
	 NextSelectionNameObjLst
	  (entnext
	    (nth
	      ct
	      SelectionNameObjLst
	    ) ;_ end of nth
	  ) ;_ end of entnext
       ) ;_ end of setq
       (while
	 (/=
	   (cdr
	     (assoc
	       0
	       (entget
		 NextSelectionNameObjLst
	       ) ;_ end of entget
	     ) ;_ end of assoc
	   ) ;_ end of cdr
	   "SEQEND"
	 ) ;_ end of /=
	  (setq
;;;========================================
;;;++++++++++++++++++++++++++++++++++++++++
	    ObjLst
	     (entget
	       NextSelectionNameObjLst
	     ) ;_ end of entget
;;;========================================
;;;++++++++++++++++++++++++++++++++++++++++
	    PointData
	     (cdr (assoc
		    10
		    ObjLst
		  ) ;_ end of assoc
	     ) ;_ end of cdr
;;;========================================
;;;++++++++++++++++++++++++++++++++++++++++
	    BulgeData
	     (cdr (assoc
		    42
		    ObjLst
		  ) ;_ end of assoc
	     ) ;_ end of cdr
;;;========================================
;;;++++++++++++++++++++++++++++++++++++++++
	    NextSelectionNameObjLst
	     (entnext
	       NextSelectionNameObjLst
	     ) ;_ end of entnext
;;;========================================
;;;++++++++++++++++++++++++++++++++++++++++
	    polypointsLst
	     (append polypointsLst
		     (list (list BulgeData PointData))
;;;========================================
;;;++++++++++++++++++++++++++++++++++++++++
	     ) ;_ end of append
	  ) ;_ end of setq
       ) ;_ end of while
       (setq ct (1+ ct))
    ) ;_ end of repeat
    polypointsLst
  ) ;_ end of defun
;;;=======================================================================================
  (defun POLYLINE:GetPointFromLst (polypointsLst / OutList)
    (mapcar '(lambda (Pt)
	       (setq
		 OutList
		  (append OutList (cdr pt))
;;;hilangkan bulge hanya ambil point
	       ) ;_ end of setq
	     ) ;_ end of lambda
	    polypointsLst
    ) ;_ end of mapcar
    OutList
  ) ;_ end of defun
;;;=======================================================================================
(defun POLYLINE:Z (OutList)
  (if (null OutList)
    (list)
    (cons
      (caddr (car OutList))
      (POLYLINE:Z (cdr OutList))
    ) ;_ end of cons
  ) ;_ end of if
) ;_ end of defun
;;;=======================================================================================
(defun POLYLINE:HorDist (OutList)
  (if (null (cadr OutList))
    (list)
    (cons
      (dist (car OutList)(cadr OutList))
      (POLYLINE:HorDist (cdr OutList))
    ) ;_ end of cons
  ) ;_ end of if
) ;_ end of defun
;;;=======================================================================================
(defun dist (p0 p1)
  (sqrt
        (+ (expt (- (cx p1) (cx p0)) 2)
	   (expt (- (cy p1) (cy p0)) 2)
	)
  )
)
;;;=======================================================================================
  (defun ELEVASI:Set-Elev-Dist (BasePt Pt NilaiElev / XBasePt YBasePt)
    (setq
      XBasePt (car BasePt)
      YBasePt (cadr BasePt)
    ) ;_ end of setq
    (apply
      '(lambda (x y z)			; isi data pt berupa x, y dan z
	 (list (- x XBasePt) (+ (- y YBasePt) NilaiElev) z)
       ) ;_ end of lambda
      Pt
    ) ;_ end of apply
  ) ;_ end of defun
;;;=======================================================================================
  (defun PRINT:PrintLine (EntLst StrMess)
    (prompt (strcat "\n" StrMess))
    (foreach n EntLst (print n))
  ) ;_ end of defun
;;;=======================================================================================
  (defun SELECT:GetSigleObj (CatagorySelectionLst / SsSet)
    (if	(setq SsSet (ssget ":S" CatagorySelectionLst))
      (list (ssname SsSet 0))		;hanya menambahkan perintah list
					; if object is polyline, add command "list", please!
    ) ;_ end of if
  ) ;_ end of defun
;;;=======================================================================================
  (defun POLYLINE:GetElev-Dist-List
	 (polypointsLst NilaiElev BasePt / OutList)
    (mapcar '(lambda (Pt)
	       (setq OutList
		      (append
			OutList
			(list (ELEVASI:Set-Elev-Dist BasePt Pt NilaiElev))
		      )
	       ) ;_ end of setq
	     ) ;_ end of lambda
	    polypointsLst
    ) ;_ end of mapcar
    OutList
  ) ;_ end of defun
;;;=======================================================================================
  (defun POLYLINE:GetDist-List (polypointsLst / OutList)
    (mapcar '(lambda (Pt)
	       (setq OutList (append OutList (list (car pt)))) ;_ end of setq
	     ) ;_ end of lambda
	    polypointsLst
    ) ;_ end of mapcar
    OutList
  ) ;_ end of defun
;;;=======================================================================================
  (defun POLYLINE:GetElev-List (polypointsLst / OutList)
    (mapcar '(lambda (Pt)
	       (setq OutList (append OutList (list (cadr pt)))) ;_ end of setq
	     ) ;_ end of lambda
	    polypointsLst
    ) ;_ end of mapcar
    OutList
  ) ;_ end of defun
;;;=======================================================================================
  (setq
    CatagorySelectionLst-01
     (list (cons 0 "POLYLINE"))

  ) ;_ end of setq
  (if (setq ObjName (SELECT:GetSigleObj CatagorySelectionLst-01))
    (if	(setq NilaiElev (getreal "\nReferensi elevasi : "))
      (if (setq
	    BasePt (trans (getpoint "\rRefensi titik elevasi : ") 1 0)
	  )
	(progn
	  (setq
	    polypointsLst  (POLYLINE:ReadBulgeData ObjName)
	    OutList	   (POLYLINE:GetPointFromLst polypointsLst)
	    HorDistLst     (POLYLINE:HorDist OutList)
	    val-Z          (POLYLINE:Z OutList)
	    Elev-Dist-List (POLYLINE:GetElev-Dist-List
			     OutList
			     NilaiElev
			     BasePt
			   )
	    Elev-List	   (POLYLINE:GetElev-List Elev-Dist-List)
	    Dist-List	   (POLYLINE:GetDist-List Elev-Dist-List)
	  ) ;_ end of setq
	  (PRINT:PrintLine ObjName "Object is ..... :")
;;;	  (PRINT:PrintLine polypointsLst "polypointsLst is ..... :")
;;;	  (PRINT:PrintLine
;;;	    OutList
;;;	    "<Hanya ambil titik> OutList is ..... :"
;;;	  )
	  (PRINT:PrintLine
	    HorDistLst
	    "<Horizontal Distance is ..... :"
	  )
	  (PRINT:PrintLine
	    val-z
	    "<Z Value is ..... :"
	  )
;;;	  (PRINT:PrintLine
;;;	    Elev-Dist-List
;;;	    "<Elevasi & Distance> Elev-Dist-List is ..... :"
;;;	  )
	  (PRINT:PrintLine Dist-List "<Distance> :")
	  (PRINT:PrintLine Elev-List "<Elevation> :")
	) ;_ end of progn
      ) ;_ end of if
      (progn
	(prompt "\nPlease Enter Referensi elevasi ")
      ) ;_ end of progn
    )
    (progn
      (prompt "\nNothing Selection, POLYLINE Object only")
    ) ;_ end of progn
  )
  (princ)
)
;;;=======================================================================================
;;;(defun ELEVASI:SetDistance (BasePt Pt / XBasePt YBasePt)
;;;  (setq
;;;    XBasePt (car BasePt)
;;;    YBasePt (cadr BasePt)
;;;  ) ;_ end of setq
;;;  (apply
;;;    '(lambda (x y z) ; isi data pt berupa x, y dan z
;;; ;       (list (- X XBasePt) (+ (- y YBasePt) NilaiElev) z)
;;;       (list
;;;	 (- X XBasePt) ;konpersi ke elevasi
;;;	 (list x y z);titik text elevasi
;;;       ) ;_ end of list
;;;     ) ;_ end of lambda
;;;    Pt
;;;  ) ;_ end of apply
;;;) ;_ end of defun
;;; ;================================================
;;;(defun SHOW:TxtElevasi (OutList / NilaiElev BasePt ResOutList ProcesDataLst)
;;;  (if (setq NilaiElev (getreal "\nReferensi elevasi : "))
;;;    (if (setq BasePt (getpoint "\rRefensi titik elevasi : "))
;;;      (progn
;;;	(prompt (strcat "\nX= " (rtos (car BasePt) 2 3)   " Y= " (rtos (cadr BasePt) 2 3) ))
;;;        (prompt "\nElevasi :")
;;;        (mapcar '(lambda (pt)
;;;                   (setq ProcesDataLst (ELEVASI:SetElevasi BasePt Pt NilaiElev))
;;;                   (prompt (strcat "\n" (rtos (car ProcesDataLst) 2 3)))
;;;		   
;;;;;;                   (GP:ENTMAKE-TEXT
;;;;;;                     ;(car (cdr ProcesDataLst))
;;;;;;                     (cadr ProcesDataLst)
;;;;;;                     0.18
;;;;;;                     0.0
;;;;;;                     4
;;;;;;                     (rtos (car ProcesDataLst) 2 3)
;;;;;;                   ) ;_ end of GP:ENTMAKE-TEXT
;;;		   
;;;;;;                   (setq ProcesDataLst (list (ELEVASI:SetElevasi BasePt Pt NilaiElev)))
;;;;;;                   (setq ResOutList (append ResOutList ProcesDataLst))
;;;                 ) ;_ end of lambda
;;;                OutList
;;;        ) ;_ end of mapcar
;;;;;;        ResOutList
;;;      ) ;_ end of progn
;;;    ) ;_ end of if
;;;  ) ;_ end of if
;;;) ;_ end of defun
;;; ;================================================
;;;;GP:ENTMAKE-TEXT (Pt TxtHeight Rotation Color TheString)


