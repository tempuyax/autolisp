;;;*******************************************************************;;;
;;;Kota Nunukan                                                       ;;;
;;;Programmer : Pahor M                                               ;;;
;;;                                                                   ;;;
;;;Dibuat     : 12 September 2006                                     ;;;
;;;Revisi     : 26 September 2012                                     ;;;
;;;Revisi     : 21 September 2014                                     ;;;
;;;Koreksi    : 04 April 2019                                         ;;;
;;;Deskripsi  : Menampilkan Elevasi dan Jarak di Object LWPOLYLINE    ;;;
;;;Fungsi     : Dipakai Sebagai tools cross section                   ;;;
;;;*******************************************************************;;;
(defun C:LWPolyElev
(/
  CatagorySelectionLst		ObjName	       NilaiElev
  BasePt	 EntLst		Vertex	       Bulge
  BulgeLst	 VertexLst	Elev-Dist-List Elev-List
  Dist-List
 )
;;;=======================================================================================
(defun SELECT:GetSigleObj (CatagorySelectionLst / SsSet)
  (if (setq SsSet (ssget ":S" CatagorySelectionLst))
    (ssname SsSet 0)
  ) ;_ end of if
) ;_ end of defun
;;;=======================================================================================
(defun LWPOLYLINE:GetElemenLst
       (EntLst AssCode / AddLst SortAssLst ResAssLst)
  (mapcar
    '(lambda (Ass)
       (setq AssLst (list Ass))
       (if (setq SortAssLst (assoc AssCode AssLst))
	 (setq ResAssLst (append ResAssLst (list (cdr SortAssLst))))
       ) ;_ end of if
     ) ;_ end of lambda
    EntLst
  ) ;_ end of mapcar
  ResAssLst
) ;_ end of defun
;;;=======================================================================================
(defun Lw-ELEVASI:Set-Elev-Dist	(BasePt Pt NilaiElev / XBasePt YBasePt)
  (setq
    XBasePt (car BasePt)
    YBasePt (cadr BasePt)
  ) ;_ end of setq
  (apply
    '(lambda (x y)	;; isi data pt berupa x, y saja
       (list (- x XBasePt) (+ (- y YBasePt) NilaiElev))
     ) ;_ end of lambda
    Pt
  ) ;_ end of apply
) ;_ end of defun
;;;=======================================================================================
(defun LWPOLYLINE:GetElev-Dist-List
       (polypointsLst NilaiElev BasePt / OutList)
  (mapcar '(lambda (Pt)
	     (setq OutList
		    (append
		      OutList
		      (list (Lw-ELEVASI:Set-Elev-Dist BasePt Pt NilaiElev))
		    )
	     ) ;_ end of setq
	   ) ;_ end of lambda
	  polypointsLst
  ) ;_ end of mapcar
  OutList
) ;_ end of defun
;;;=======================================================================================
(defun LWPOLYLINE:GetDist-List (polypointsLst / OutList)
  (mapcar '(lambda (Pt)
	     (setq OutList (append OutList (list (car pt)))) ;_ end of setq
	   ) ;_ end of lambda
	  polypointsLst
  ) ;_ end of mapcar
  OutList
) ;_ end of defun
;;;=======================================================================================
(defun LWPOLYLINE:GetElev-List (polypointsLst / OutList)
  (mapcar '(lambda (Pt)
	     (setq OutList (append OutList (list (cadr pt)))) ;_ end of setq
	   ) ;_ end of lambda
	  polypointsLst
  ) ;_ end of mapcar
  OutList
) ;_ end of defun
;;;=======================================================================================
(defun PRINT:PrintLine (EntLst StrMess)
  (prompt (strcat "\n" StrMess))
  (foreach n EntLst (print n))
) ;_ end of defun
;;;=======================================================================================
  (if (/= (getvar 'AUPREC) 4)
    (setvar "AUPREC" 4)
  )

  (if (/= (getvar 'DIMZIN) 1)
    (setvar "DIMZIN" 1)
  ) ;_ end of if

  (setq CatagorySelectionLst (list (cons 0 "LWPOLYLINE"))) ;_ end of setq

  (if (setq ObjName (SELECT:GetSigleObj CatagorySelectionLst)) ;_ end of setq
    (if	(setq NilaiElev (getreal "\nReferensi elevasi : "))
      (if (setq
	    BasePt (trans (getpoint "\rRefensi titik elevasi : ") 1 0)
	  )
	(progn
	  (setq
	    EntLst
			   (entget ObjName)
	    Vertex
			   10
	    Bulge
			   42
	    BulgeLst
			   (LWPOLYLINE:GetElemenLst EntLst Bulge)
	    VertexLst
			   (LWPOLYLINE:GetElemenLst EntLst Vertex)

	    Elev-Dist-List (LWPOLYLINE:GetElev-Dist-List
			     VertexLst
			     NilaiElev
			     BasePt
			   )
	    Elev-List	   (LWPOLYLINE:GetElev-List Elev-Dist-List)
	    Dist-List	   (LWPOLYLINE:GetDist-List Elev-Dist-List)

	  ) ;_ end of setq
	  (PRINT:PrintLine (list ObjName) "ObjName is ..... :")
	  (PRINT:PrintLine (list NilaiElev) "NilaiElev is ..... :")
	  (PRINT:PrintLine BulgeLst "BulgeLst is ..... :")
	  (PRINT:PrintLine VertexLst "VertexLst is ..... :")
	  (PRINT:PrintLine (list BasePt) "BasePt is ..... :")
	  (PRINT:PrintLine
	    Elev-Dist-List
	    "<Elevasi & Distance> Elev-Dist-List is ..... :"
	  )
	  (PRINT:PrintLine Dist-List "<Distance> :")
	  (PRINT:PrintLine Elev-List "<Elevation> :")
	) ;_ end of progn
      )
    )
    (progn
      (prompt "\nNothing Selection by LWPOLYLINE Object")
    ) ;_ end of progn
  ) ;_ end of if
  (princ)
) ;_ end of defun


;=======================================================
;=======================================================
;=======================================================
;=======================================================
;=======================================================
;;;(defun GROUP:GroupLst (ListA ListB / ct ValA ValB ResGroupLst)
;;;  (setq
;;;    ct
;;;     0
;;;  ) ;_ end of setq
;;;  (repeat (length ListA)
;;;    (setq
;;;      ValA
;;;       (nth ct ListA)
;;;      ValB
;;;       (nth ct ListB)
;;;      ResGroupLst
;;;       (append ResGroupLst (list (list ValA ValB)))
;;;      ct (1+ ct)
;;;    ) ;_ end of setq
;;;  ) ;_ end of repeat
;;;  ResGroupLst
;;;) ;_ end of defun
;;;;;;=======================================================================================
;;;(defun LWPOLYLINE:GetPointFromLst (polypointsLst / OutList)
;;;  (mapcar '(lambda (Pt)
;;;	     (setq
;;;	       OutList
;;;		(append OutList (cdr pt))
;;;					;hilangkan bulge hanya ambil point
;;;	     ) ;_ end of setq
;;;	   ) ;_ end of lambda
;;;	  polypointsLst
;;;  ) ;_ end of mapcar
;;;  OutList
;;;) ;_ end of defun
;;;=======================================================================================
;;;(defun DISTANCE:SetDistance (BasePt Pt NilaiDist / XBasePt YBasePt)
;;;  (setq
;;;    XBasePt (car BasePt)
;;;    YBasePt (cadr BasePt)
;;;  ) ;_ end of setq
;;;  (apply
;;;    '(lambda (x y)			; isi data pt berupa x, y dan z / di rubah setelah di revisi
;;;					;       (list (- X XBasePt) (+ (- y YBasePt) NilaiElev) z)
;;;       (list
;;;	 (+ (- x XBasePt) NilaiDist)	;konpersi ke elevasi
;;;	 (list x (- y 0.20))		;titik text elevasi
;;;       ) ;_ end of list
;;;     ) ;_ end of lambda
;;;    Pt
;;;  ) ;_ end of apply
;;;) ;_ end of defun
;;;=======================================================================================
;;;(defun ELEVASI:SetElevasi (BasePt Pt NilaiElev Unit / XBasePt YBasePt)
;;;  (setq
;;;    XBasePt (car BasePt)
;;;    YBasePt (cadr BasePt)
;;;  ) ;_ end of setq
;;;  (apply
;;;    '(lambda (x y)			; isi data pt berupa x, y dan z
;;;					;       (list (- X XBasePt) (+ (- y YBasePt) NilaiElev) z)
;;;       (list
;;;	 (+ (/ (- y YBasePt) Unit) NilaiElev) ;konpersi ke elevasi
;;;	 (list x y)			;titik text elevasi
;;;       ) ;_ end of list
;;;     ) ;_ end of lambda
;;;    Pt
;;;  ) ;_ end of apply
;;;) ;_ end of defun
;;;=======================================================================================

