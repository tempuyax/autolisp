;;;Programmer : Pahor M
;;;
;;;Tanggal    : 12 September 2006
;;;Revisi     : 26 September 2012
;;;Deskripsi  : Menampilkan Elevasi dan Jarak di Object LWPOLYLINE 
;;;Fungsi     : Dipakai Sebagai tools cross section
(load "D:\DATA-pahor\Lisp Excel\Lisp-2013\entmake-2008.lsp" "File not Found")
 ;=======================================================
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
 ;=======================================================
(defun PRINT:PrintLine (EntLst StrMess)
  (prompt (strcat "\n" StrMess))
  (foreach n EntLst (print n))
) ;_ end of defun
 ;=======================================================
(defun SELECT:GetSigleObj (CatagorySelectionLst / SsSet)
  (if (setq SsSet (ssget ":S" CatagorySelectionLst))
    (ssname SsSet 0)
  ) ;_ end of if
) ;_ end of defun
 ;=======================================================
(defun GROUP:GroupLst (ListA ListB / ct ValA ValB ResGroupLst)
  (setq
    ct
     0
  ) ;_ end of setq
  (repeat (length ListA)
    (setq
      ValA
       (nth ct ListA)
      ValB
       (nth ct ListB)
      ResGroupLst
       (append ResGroupLst (list (list ValA ValB)))
      ct (1+ ct)
    ) ;_ end of setq
  ) ;_ end of repeat
  ResGroupLst
) ;_ end of defun
 ;=======================================================
(defun DISTANCE:SetDistance (BasePt Pt NilaiDist / XBasePt YBasePt)
  (setq
    XBasePt (car BasePt)
    YBasePt (cadr BasePt)
  ) ;_ end of setq
  (apply
    '(lambda (x y ) ; isi data pt berupa x, y dan z / di rubah setelah di revisi
 ;       (list (- X XBasePt) (+ (- y YBasePt) NilaiElev) z)
       (list
	 (+ (- x XBasePt) NilaiDist);konpersi ke elevasi
	 (list x  (- y 0.20) );titik text elevasi
       ) ;_ end of list
     ) ;_ end of lambda
    Pt
  ) ;_ end of apply
) ;_ end of defun
 ;=======================================================
(defun ELEVASI:SetElevasi (BasePt Pt NilaiElev / XBasePt YBasePt)
  (setq
    XBasePt (car BasePt)
    YBasePt (cadr BasePt)
  ) ;_ end of setq
  (apply
    '(lambda (x y ) ; isi data pt berupa x, y dan z
 ;       (list (- X XBasePt) (+ (- y YBasePt) NilaiElev) z)
       (list
	 (+ (- y YBasePt) NilaiElev);konpersi ke elevasi
	 (list x y );titik text elevasi
       ) ;_ end of list
     ) ;_ end of lambda
    Pt
  ) ;_ end of apply
) ;_ end of defun
 ;================================================
(defun SHOW:TxtElevasi (OutList / NilaiElev BasePt ResOutList Proces-Y-Lst Proces-X-Lst DataXYlst)
  (setq
        BasePt (getpoint "\rBase Point <0.0 0.0 0.0>: ")
        NilaiElev (getreal "\nBase elevastion <0.0>: ")
        NilaiDist (getreal "\nBase Distance <0.0>: ")
        NilaiElevDef 0.0
        NilaiDistDef 0.0
        BasePtDef '(0.0 0.0 0.0)
        ;Proces-Y-Lst 0.0
        ;Proces-X-Lst 0.0
  )
    
  (if (not NilaiElev) (setq NilaiElev 0.000))
  (if (not NilaiDist) (setq NilaiDist 0.000))
  (if (not BasePt) (setq BasePt '(0.0 0.0 0.0)))

    (mapcar '(lambda (pt)
                   (setq Proces-Y-Lst (ELEVASI:SetElevasi BasePt Pt NilaiElev))
                   (GP:ENTMAKE-TEXT
                     (cadr Proces-Y-Lst)
                     0.18
                     0.0
                     4
                     (rtos (car Proces-Y-Lst) 2 3)
                   ) ;_ end of GP:ENTMAKE-TEXT

	           (setq Proces-X-Lst (DISTANCE:SetDistance BasePt Pt NilaiDist))
                   (GP:ENTMAKE-TEXT
                     (cadr Proces-X-Lst)
                     0.18
                     0.0
                     3
                     (rtos (car Proces-X-Lst) 2 2)
                   ) ;_ end of GP:ENTMAKE-TEXT

	           (setq DataXYlst (append DataXYlst (list (list (car Proces-X-Lst) (car Proces-Y-Lst)))))
	       
                 ) ;_ end of lambda
                OutList
        ) ;_ end of mapcar
        DataXYlst
) ;_ end of defun
 ;================================================GP:ENTMAKE-TEXT
;GP:ENTMAKE-TEXT (Pt TxtHeight Rotation Color TheString)
(defun C:LWPolyElev
                      (/
                       Vertex
                       Bulge
                       EntType
                       CatagorySelectionLst
                       ObjName
                       GroupLst
                      )
  (setq
    CatagorySelectionLst
     (list (cons 0 "LWPOLYLINE"))
  ) ;_ end of setq

  (if
    (setq
      ObjName
       (SELECT:GetSigleObj CatagorySelectionLst)
    ) ;_ end of setq
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
         GroupLst
                   (GROUP:GroupLst BulgeLst VertexLst)
       ) ;_ end of setq
       (princ ObjName)
       (print BulgeLst)
       (prompt
         (strcat "\nTotal BulgeLst : " (itoa (length BulgeLst)))
       ) ;_ end of prompt
       (print VertexLst)
       (prompt
         (strcat "\nTotal VertexLst : " (itoa (length VertexLst)))
       ) ;_ end of prompt
       (print GroupLst)
       (prompt
         (strcat "\nTotal GroupLst : " (itoa (length GroupLst)))
       ) ;_ end of prompt
       (foreach n (SHOW:TxtElevasi VertexLst) (print n))       
     ) ;_ end of progn
     (progn
       (princ ObjName)
       (prompt "\nNothing Selection by LWPOLYLINE Object")
     ) ;_ end of progn
  ) ;_ end of if
  (princ)
) ;_ end of defun
 ;=======================================================

