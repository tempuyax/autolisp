;;------------------=={ Scale by Matrix }==-------------------;;
;;                                                            ;;
;;  Scales a VLA-Object or Point List using a                 ;;
;;  Transformation Matrix                                     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  target - VLA-Object or Point List to transform            ;;
;;  p1     - Base Point for Scaling Transformation            ;;
;;  scale  - Scale Factor by which to scale object            ;;
;;------------------------------------------------------------;;

(defun LM:ScaleByMatrix ( target p1 scale / m )

  (LM:ApplyMatrixTransformation target
    (setq m
      (list
        (list scale 0. 0.)
        (list 0. scale 0.)
        (list 0. 0. scale)
      )
    )
    (mapcar '- p1 (mxv m p1))
  )
)

;;----------------=={ Translate by Matrix }==-----------------;;
;;                                                            ;;
;;  Translates a VLA-Object or Point List using a             ;;
;;  Transformation Matrix                                     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  target - VLA-Object or Point List to transform            ;;
;;  p1, p2 - Points representing vector by which to translate ;;
;;------------------------------------------------------------;;

(defun LM:TranslateByMatrix ( target p1 p2 )

  (LM:ApplyMatrixTransformation target
    (list
      (list 1. 0. 0.)
      (list 0. 1. 0.)
      (list 0. 0. 1.)
    )
    (mapcar '- p2 p1)
  )
)

;;------------------=={ Rotate by Matrix }==------------------;;
;;                                                            ;;
;;  Rotates a VLA-Object or Point List using a                ;;
;;  Transformation Matrix                                     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  target - VLA-Object or Point List to transform            ;;
;;  p1     - Base Point for Rotation Transformation           ;;
;;  ang    - Angle through which to rotate object             ;;
;;------------------------------------------------------------;;

(defun LM:RotateByMatrix ( target p1 ang )
  
  (LM:ApplyMatrixTransformation target
    (setq m
      (list
        (list (cos ang) (- (sin ang)) 0.)
        (list (sin ang)    (cos ang)  0.)
        (list    0.           0.      1.)
      )
    )
    (mapcar '- p1 (mxv m p1))
  )
)

;;-----------------=={ Reflect by Matrix }==------------------;;
;;                                                            ;;
;;  Reflects a VLA-Object or Point List using a               ;;
;;  Transformation Matrix                                     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  target - VLA-Object or Point List to transform            ;;
;;  p1, p2 - Points representing vector in which to reflect   ;;
;;------------------------------------------------------------;;

(defun LM:ReflectByMatrix ( target p1 p2 )
  (
    (lambda ( a / m )
      (LM:ApplyMatrixTransformation target
        (setq m
          (list
            (list (cos a)    (sin a)  0.)
            (list (sin a) (- (cos a)) 0.)
            (list    0.         0.    1.)
          )
        )
        (mapcar '- p1 (mxv m p1))
      )
    )
    (* 2. (angle p1 p2))
  )
)

;;-----------=={ Apply Matrix Transformation }==--------------;;
;;                                                            ;;
;;  Transforms a VLA-Object or Point List using a             ;;
;;  Transformation Matrix                                     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  target - VLA-Object or Point List to Transform            ;;
;;  matrix - 3x3 Matrix by which to Transform object          ;;
;;  vector - 3D translation vector                            ;;
;;------------------------------------------------------------;;

(defun LM:ApplyMatrixTransformation ( target matrix vector ) (vl-load-com)
  (cond
    ( (eq 'VLA-OBJECT (type target))
     
      (vla-TransformBy target
        (vlax-tMatrix
          (append (mapcar '(lambda ( x v ) (append x (list v))) matrix vector)
           '((0. 0. 0. 1.))
          )
        )
      )
    )
    ( (listp target)

      (mapcar
        (function
          (lambda ( point ) (mapcar '+ (mxv matrix point) vector))
        )
        target
      )
    )        
  )
)

;; Matrix x Vector - Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
  (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)



;=======================================================================
(defun c:scaleobject ( / e p s )
  (if
    (and
      (setq e (car (entsel)))
      (setq p (getpoint "\nBase Point: "))
      (setq s (getdist  "\nScale: " p))
    )
    (LM:ScaleByMatrix (vlax-ename->vla-object e) (trans p 1 0) s)
  )
  (princ)
)

(defun c:scalelist ( / e p s )
  (if
    (and
      (setq e (car (entsel)))
      (eq "LWPOLYLINE" (cdr (assoc 0 (entget e))))
      (setq p (getpoint "\nBase Point: "))
      (setq s (getdist  "\nScale: " p))
    )
    (LWPolyline
      (LM:ScaleByMatrix (Vertices e) (trans p 1 0) s) (cdr (assoc 70 (entget e)))
    )
  )
  (princ)
)
;=======================================================================
(defun c:translateobject ( / e p q )
  (if
    (and
      (setq e (car (entsel)))
      (setq p (getpoint "\nBase Point: "))
      (setq q (getpoint "\nDisplacement: " p))
    )
    (LM:TranslateByMatrix (vlax-ename->vla-object e) (trans p 1 0) (trans q 1 0))
  )
  (princ)
)

(defun c:translatelist ( / e p q )
  (if
    (and
      (setq e (car (entsel)))
      (eq "LWPOLYLINE" (cdr (assoc 0 (entget e))))
      (setq p (getpoint "\nBase Point: "))
      (setq q (getpoint "\nDisplacement: " p))
    )
    (LWPolyline
      (LM:TranslateByMatrix (Vertices e) (trans p 1 0) (trans q 1 0)) (cdr (assoc 70 (entget e)))
    )
  )
  (princ)
)
;=======================================================================
(defun c:rotateobject ( / e p a )
  (if
    (and
      (setq e (car (entsel)))
      (setq p (getpoint "\nBase Point: "))
      (setq a (getangle "\nRotation: " p))
    )
    (LM:RotateByMatrix (vlax-ename->vla-object e) (trans p 1 0) a)
  )
  (princ)
)

(defun c:rotatelist ( / e p a )
  (if
    (and
      (setq e (car (entsel)))
      (eq "LWPOLYLINE" (cdr (assoc 0 (entget e))))
      (setq p (getpoint "\nBase Point: "))
      (setq a (getangle "\nRotation: " p))
    )
    (LWPolyline
      (LM:RotateByMatrix (Vertices e) (trans p 1 0) a) (cdr (assoc 70 (entget e)))
    )
  )
  (princ)
)
;=======================================================================
(defun c:reflectobject ( / e p q )
  (if
    (and
      (setq e (car (entsel)))
      (setq p (getpoint "\nBase Point: "))
      (setq q (getpoint "\nSecond Point of Reflection Vector: " p))
    )
    (LM:ReflectByMatrix (vlax-ename->vla-object e) (trans p 1 0) (trans q 1 0))
  )
  (princ)
)

(defun c:reflectlist ( / e p q )
  (if
    (and
      (setq e (car (entsel)))
      (eq "LWPOLYLINE" (cdr (assoc 0 (entget e))))
      (setq p (getpoint "\nBase Point: "))
      (setq q (getpoint "\nSecond Point of Reflection Vector: " p))
    )
    (LWPolyline
      (LM:ReflectByMatrix (Vertices e) (trans p 1 0) (trans q 1 0)) (cdr (assoc 70 (entget e)))
    )
  )
  (princ)
)
;=======================================================================
(defun LWPolyline ( l c )
  (entmakex
    (append
      (list
        (cons 0 "LWPOLYLINE")
        (cons 100 "AcDbEntity")
        (cons 100 "AcDbPolyline")
        (cons 90 (length l))
        (cons 70 c)
      )
      (mapcar '(lambda ( p ) (cons 10 p)) l)
    )
  )
)

(defun Vertices ( e )
  (mapcar '(lambda ( x ) (append x (list 0.0)))
    (mapcar 'cdr
      (vl-remove-if-not '(lambda ( x ) (= 10 (car x))) (entget e))
    )
  )
)

