  ;==============================================
(defun POINT:PointTransition (Pt)
  (trans Pt 1 (getvar 'WORLDUCS))
) ;_ end of defun
  ;==============================================
(defun ENTMAKE:ARC (CenterPt Radius StartAng EndAng Color)
  (entmake
    (list
      (cons 0 "ARC")
      (cons 62 Color)
      (cons 10 (POINT:PointTransition CenterPt))
      (cons 40 Radius)
      (cons 50 StartAng)
      (cons 51 EndAng)
    ) ;_ end of list
  ) ;_ end of entmake
) ;_ end of defun
  ;==============================================
(defun GP:ENTMAKE-POLYLINEARC (VertexPt&BulgeLst)
  (entmake
    (List
      (cons 0 "POLYLINE") ; Object type
      (cons 62 143) ; Color
      (cons 66 1) ; Vertices follow
    ) ;_ end of List
  ) ;_ end of entmake
  (mapcar '(lambda (Pt)
	     (entmake
	       (list
		 (cons 0 "VERTEX") ; Object type
		 (cons 10 (POINT:PointTransition (cdr (assoc "Pt" Pt))))
		 (cons 42 (cdr (assoc "Bulge" Pt)))
	       ) ;_ end of list
	     ) ;_ end of entmake
	   ) ;_ end of lambda
	  VertexPt&BulgeLst
  ) ;_ end of mapcar
  (entmake (list (cons 0 "SEQEND")))
) ;_ end of defun
  ;==============================================
(defun GP:ENTMAKE-POINT	(Pt)
  (entmake
    (list
      (cons 0 "POINT")
      (cons 62 1) ; Color
      (cons 10 (POINT:PointTransition Pt))
    ) ;_ end of list
  ) ;_ end of entmake
) ;_ end of defun
  ;==============================================
(defun GP:ENTMAKE-POLYLINE (ParPtLst Color)
  (entmake
    (List
      (cons 0 "POLYLINE") ; Object type
      (cons 62 Color) ; Color
      (cons 66 1) ; Vertices follow
    ) ;_ end of List
  ) ;_ end of entmake
  (mapcar '(lambda (Pt)
	     (entmake
	       (list
		 (cons 0 "VERTEX") ; Object type
		 (cons 10 (POINT:PointTransition Pt))
	       ) ;_ end of list
	     ) ;_ end of entmake
	   ) ;_ end of lambda
	  ParPtLst
  ) ;_ end of mapcar
  (entmake (list (cons 0 "SEQEND")))
) ;_ end of defun
  ;=====================================================================
(defun GP:ENTMAKE-TEXT (Pt TxtHeight Rotation Color TheString)
  (entmake
    (list
      (cons 0 "TEXT")
      (cons 62 Color)
      (cons 10 (POINT:PointTransition Pt))
      (cons 40 TxtHeight)
      (cons 1 TheString)
      (cons 50 Rotation)
      (cons 72 1)
      (cons 11 (POINT:PointTransition Pt))
      (cons 73 2)
    ) ;_ end of list
  ) ;_ end of entmake
) ;_ end of defun
  ;============================================================== 
(defun GP:ENTMAKE-INSERT (BasePt BlockName)
  (entmake
    (list
      (cons 0 "INSERT")
  ;(cons 66 0)
      (cons 2 BlockName)
      (cons 10 (POINT:PointTransition BasePt))
    ) ;_ end of list
  ) ;_ end of entmake
) ;_ end of defun
  ;============================================================== 
(defun GP:ENTMAKE-LINE (Pt1 Pt2 Color)
  (entmake
    (list (cons 0 "LINE")
	  (cons 62 Color)
	  (cons 10 (POINT:PointTransition Pt1))
	  (cons 11 (POINT:PointTransition Pt2))
    ) ;_ end of list
  ) ;_ end of entmake
) ;_ end of defun
  ;============================================================== 
(defun GP:ENTMAKE-LWPOLYLINE (l Color c)
  (entmake
    (append
      (list
	(cons 0 "LWPOLYLINE")
	(cons 100 "AcDbEntity")
        (cons 100 "AcDbPolyline")
	(cons 62 Color)
	(cons 90 (length l))
	(cons 70 c)
      )
      (mapcar '(lambda (p) (cons 10 (POINT:PointTransition p))) l)
    )
  )
) ;_ end of defun