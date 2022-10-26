;; Company      : CV. Global Consultant       
;; Editor       : Pahor .M                    
;; Date         : 09 mei 2016                 
;; Description  : Membuat Objek-objek CAD     
;;++++++++++++++++++++++++++++++++++++++++++++

;; Cartesian Coordinat system
(defun xyz (x y z)
  (list x y z)
) ;_ end of defun

(defun xy (x y)
  (xyz x y 0)
) ;_ end of defun

(defun cx (c)
  (nth 0 c)
) ;_ end of defun
(defun cy (c)
  (nth 1 c)
) ;_ end of defun
(defun cz (c)
  (nth 2 c)
) ;_ end of defun

;; Plus Move Coordinat
(defun +xyz (p dx dy dz)
  (xyz (+ (cx p) dx)
       (+ (cy p) dy)
       (+ (cz p) dz)
  ) ;_ end of xyz
) ;_ end of defun

(defun +x (p dx)
  (+xyz p dx 0 0)
) ;_ end of defun
(defun +y (p dy)
  (+xyz p 0 dy 0)
) ;_ end of defun
(defun +z (p dz)
  (+xyz p 0 0 dz)
) ;_ end of defun

(defun +xy (p dx dy)
  (+xyz p dx dy 0)
) ;_ end of defun
(defun +xz (p dx dz)
  (+xyz p dx 0 dz)
) ;_ end of defun
(defun +yz (p dy dz)
  (+xyz p 0 dy dz)
) ;_ end of defun

;;;;; Minus Delta Coordinat
;;;(defun -xyz (p dx dy dz)
;;;  (xyz (- (cx p) dx)
;;;       (- (cy p) dy)
;;;       (- (cz p) dz)
;;;  )
;;;)
;;;
;;;(defun -x (p dx)
;;;  (-xyz p dx 0 0)
;;;)
;;;(defun -y (p dy)
;;;  (-xyz p 0 dy 0)
;;;)
;;;(defun -z (p dz)
;;;  (-xyz p 0 0 dz)
;;;)
;;;
;;;(defun -xy (p dx dy)
;;;  (-xyz p dx dy 0)
;;;)
;;;(defun -xz (p dx dz)
;;;  (-xyz p dx 0 dz)
;;;)
;;;(defun -yz (p dy dz)
;;;  (-xyz p 0 dy dz)
;;;)

;; Relatif Polar
(defun pol (L Ang)
  (xy (* L (cos Ang))
      (* L (sin Ang))
  ) ;_ end of xy
) ;_ end of defun
;; Statif Polar
(defun +pol (p L Ang)
  (+xy p
       (* L (cos Ang))
       (* L (sin Ang))
  ) ;_ end of +xy
) ;_ end of defun

;; Distance Relatif Polar 
(defun pol-L (c)
  (sqrt (+ (quadrado (cx c)) (quadrado (cy c))))
) ;_ end of defun
;; angle Relatif Polar 
(defun pol-Ang (c)
  (atan (cy c) (cx c))
) ;_ end of defun

;; FIB2 A function to return the nth term in the series
;; 1, 1, 2, 3, 5, 8, 13, 21...... (fibonacci series)
(defun fib2 (n)
  (if (or (= n 1) (= n 2))
    (setq x 1)
    (setq x (+ (fib2 (- n 1)) (fib2 (- n 2))))
  ) ;_ end of if
  ;; if
) ;_ end of defun
;; end

;; Vector Calculation
;; Pahor .M
;; Vector memiliki besar (magnitude) dan arah (direction)
;; Magnitude (Mag) = (sqrt (+ (expt dx 2) (expt dy 2)))
;; direction (Dir )= (atan dy dx) searah jarum jam (clockwise)
;;             Autocad menyatakan nilai - (minus) dalam
;;             satuan radian.
(defun ij (i j)
  (list	i
	j
  ) ;_ end of list
) ;_ end of defun

(defun Mag (ij)
  (sqrt (+ (expt (cx ij) 2) (expt (cy ij) 2)))
) ;_ end of defun

(defun Dir (ij)
  (atan (cy ij) (cx ij))
) ;_ end of defun

(defun Vec (ij)
  (list	(Mag ij)
	(Dir ij)
  ) ;_ end of list
) ;_ end of defun


;;=======================================
(defun +c (p0 p1)
  (xyz (+ (cx p0) (cx p1))
       (+ (cy p0) (cy p1))
       (+ (cz p0) (cz p1))
  ) ;_ end of xyz
) ;_ end of defun

(defun -c (p0 p1)
  (xyz (- (cx p0) (cx p1))
       (- (cy p0) (cy p1))
       (- (cz p0) (cz p1))
  ) ;_ end of xyz
) ;_ end of defun

(defun *c (p a)
  (xyz (* (cx p) a)
       (* (cy p) a)
       (* (cz p) a)
  ) ;_ end of xyz
) ;_ end of defun

(defun /c (p a)
  (*c p (/ 1.0 a))
) ;_ end of defun

;; Poligon Area
(defun area-trapesium (p0 p1)
  (* 0.5
     (- (cx p1) (cx p0))
     (+ (cy p0) (cy p1))
  ) ;_ end of *
) ;_ end of defun

(defun area-vertex (vertices)
  (if (null (cdr vertices))
    0
    (+ (area-trapesium (car vertices) (cadr vertices))
       (area-vertex (cdr vertices))
    ) ;_ end of +
  ) ;_ end of if
) ;_ end of defun


;; Proprties
;; COLOR Parameters
(setq Color: 62
      -Red 1
      -Yellow 2
      -Green 3
      -Cyan 4
      -Blue 5
      -Magenta 6
      -White 7
      -Dark 8
      -Gray 9
) ;_ end of setq

;; LAYER Parameters
(setq Layer: 8)

;; Object
;; LINE Parameters
(setq StartPt: 10
      EndPt: 11
) ;_ end of setq

;; CIRCLE Parameters
(setq centre: 10
      radius: 40
) ;_ end of setq

;; ARC Parameters
(setq StartAng:	50
      EndAng: 51
) ;_ end of setq
;;;Polyline flag (bit-coded); default is 0:
;;;1 = Closed; 128 = Plinegen 

;;;Polyline flag (bit-coded; default = 0):
;;;1 = This is a closed polyline (or a polygon mesh closed in the M direction)
;;;2 = Curve-fit vertices have been added
;;;4 = Spline-fit vertices have been added
;;;8 = This is a 3D polyline
;;;16 = This is a 3D polygon mesh
;;;32 = The polygon mesh is closed in the N direction
;;;64 = The polyline is a polyface mesh

;;;Curves and smooth surface type (optional; default = 0); integer codes, not bit-coded:
;;;0 = No smooth surface fitted
;;;5 = Quadratic B-spline surface
;;;6 = Cubic B-spline surface
;;;8 = Bezier surface 

;; PLINE Parameters
(setq
  SubClass: 100
  NumberOfVertex:
   90
  PolyLinePlag:
   70
  -Open	0
  -Close 1
  -Curve-fit
   2
  -Spline-fit
   4
  -3Dpolyline
   8
  -3DpolygonMesh
   16
  -PolygonMeshClosed
   32
  -PolyfaceMesh
   64

  CurvesSmooth:
   75
  -NoSmooth 0
  -Quadratic
   5
  -Cubic 6
  -Bezier 8
) ;_ end of setq

;; TEXT Parameters
(setq
  TxtPoint: 11
  Height: 40
  Text:	1
  Rotation: 50
  AlgnHor: 72
  AlgnVer: 73
) ;_ end of setq
;; INSERT Parameters
(setq
  BlockName:
   2
) ;_ end of setq

(defun alist<-plist (plist)
  (if (null plist)
    (list)
    (cons (cons (car plist) (cadr plist))
	  (alist<-plist (cddr plist))
    ) ;_ end of cons
  ) ;_ end of if
) ;_ end of defun

(defun plist<-alist (alist)
  (if (null alist)
    (list)
    (cons (caar alist)
	  (cons	(cdar alist)
		(plist<-alist (cdr alist))
	  ) ;_ end of cons
    ) ;_ end of cons
  ) ;_ end of if
) ;_ end of defun

(defun Create-entity (Object Params)
  (entmake (cons (cons 0 Object)
		 (alist<-plist params)
	   ) ;_ end of cons
  ) ;_ end of entmake
) ;_ end of defun

(defun GetPtTrans (p)
  (trans p (getvar 'WORLDUCS) 1)
) ;_ end of defun

(defun PutPtTrans (p)
  (trans p 1 (getvar 'WORLDUCS))
) ;_ end of defun

(defun GetHandle ()
  (cdr (assoc 5 (entget (entlast))))
) ;_ end of defun

;;===ENDOF LEVEL 0=====================================================================================

;;(0 . "ELLIPSE")
;;(10 -0.0583483 -0.150565 0.0) point
;;;(11 0.095043 0.0 0.0) panjang mayor 
;;;(40 . 0.160348)  ratio
;;(41 . 0.0) harus nol
;;(42 . 6.28319)) 2pi
;; 10 Center point (in WCS)
;; 11 Endpoint of major axis, relative to the center (in WCS)
;; 40 Ratio of minor axis to major axis 
;; 41 Start parameter (this value is 0.0 for a full ellipse) 
;; 42 End parameter (this value is 2pi for a full ellipse) 
(setq
  EllipseStart:	41
  ;;0
  EllipseEnd:
		42
		;;2pi
) ;_ end of setq

(defun Create-Ellipse (Params)
  (Create-entity
    "ELLIPSE"
    (append
      (list SubClass:
	    "AcDbEntity"
	    SubClass:
	    "AcDbEllipse"
      ) ;_ end of list
      Params
    ) ;_ end of append
  ) ;_ end of Create-entity
  (GetHandle)				; handle object
) ;_ end of defun

(defun Create-Circle (Params)
  (Create-entity "CIRCLE" Params)
  (GetHandle)				; handle object
) ;_ end of defun

(defun Create-Arc (params)
  (Create-entity "ARC" params)
  (GetHandle)				; handle object
) ;_ end of defun

(defun Create-text (params)
  (Create-entity "TEXT" params)
  (GetHandle)				; handle object
) ;_ end of defun

(defun Create-line (params)
  (Create-entity "LINE" params)
  (GetHandle)				; handle object
) ;_ end of defun

(defun Create-Point (params)
  (Create-entity "POINT" params)
  (GetHandle)				; handle object
) ;_ end of defun

(defun Create-Insert (params)
  (Create-entity "INSERT" params)
  (GetHandle)				; handle object
) ;_ end of defun

(defun Create-2DPolyline (params vertices)
  (Create-entity
    "POLYLINE"
    params
  ) ;_ end of Create-entity
  (foreach p vertices
    (Create-entity
      "VERTEX"
      (list centre: (PutPtTrans p))
    ) ;_ end of Create-entity
  ) ;_ end of foreach
  (Create-entity "SEQEND" (list))
  (GetHandle)				; handle object
) ;_ end of defun

(defun Create-3DPolyline (params vertices)
  (Create-entity
    "POLYLINE"
    (append
      (list PolyLinePlag: -3Dpolyline)
      params
    ) ;_ end of append
  ) ;_ end of Create-entity
  (foreach p vertices
    (Create-entity
      "VERTEX"
      (list
	PolyLinePlag:
	-PolygonMeshClosed
	centre:
	(PutPtTrans p)
      ) ;_ end of list
    ) ;_ end of Create-entity
  ) ;_ end of foreach
  (Create-entity "SEQEND" (list))
  (GetHandle)				; handle object
) ;_ end of defun

(defun Create-LWPolyline (params vertices)
  (Create-entity
    "LWPOLYLINE"
    (append
      (list SubClass:
	    "AcDbEntity"
	    SubClass:
	    "AcDbPolyline"
	    NumberOfVertex:
	    (length vertices)
      ) ;_ end of list
      params
      (apply 'append
	     (mapcar '(lambda (p) (list centre: (PutPtTrans p))) vertices)
      ) ;_ end of apply
    ) ;_ end of append
  ) ;_ end of Create-entity
  (GetHandle)	; handle object
) ;_ end of defun

;;;ELLIPSE
;;;A=10;
;;;B=20;        
;;;             
;;;luas=phi*A*B;
;;;keliling=2*phi*sqrt(0.5*(A*A+B*B));
;;;isi=phi*A*B*B;

(defun cil (ro fi z)
  (xyz (* ro (cos fi))
       (* ro (sin fi))
       z
  ) ;_ end of xyz
) ;_ end of defun

(defun +cil (p ro fi z)
  (+xyz	p
	(* ro (cos fi))
	(* ro (sin fi))
	z
  ) ;_ end of +xyz
) ;_ end of defun

(defun esf (ro fi psi)
  (xyz (* ro (sin psi) (cos fi))
       (* ro (sin psi) (sin fi))
       (* ro (cos psi))
  ) ;_ end of xyz
) ;_ end of defun

(defun +esf (p ro fi psi)
  (+xyz	p
	(* ro (sin psi) (cos fi))
	(* ro (sin psi) (sin fi))
	(* ro (cos psi))
  ) ;_ end of +xyz
) ;_ end of defun
