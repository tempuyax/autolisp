;;----------------=={ 3D Rotate by Matrix }==-----------------;;
;;                                                            ;;
;;  Rotates a VLA-Object or Point List about a 3D axis using  ;;
;;  a Transformation matrix.                                  ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  target - VLA-Object or Point List to Rotate               ;;
;;  p1,p2  - Two 3D points defining the axis of rotation      ;;
;;  ang    - Rotation Angle                                   ;;
;;------------------------------------------------------------;;

(defun LM:Rotate3D ( target p1 p2 ang / ux uy uz )

  (mapcar 'set '(ux uy uz) (setq u (unit (mapcar '- p2 p1))))

  (LM:ApplyMatrixTransformation target
    (setq m
      (m+m
        (list
          (list (cos ang) 0. 0.)
          (list 0. (cos ang) 0.)
          (list 0. 0. (cos ang))
        )
        (m+m
          (mxs
            (list
              (list 0. (- uz) uy)
              (list uz 0. (- ux))
              (list (- uy) ux 0.)
            )
            (sin ang)
          )
          (mxs (mapcar '(lambda ( e ) (vxs u e)) u) (- 1. (cos ang)))
        )
      )
    )      
    (mapcar '- p1 (mxv m p1))
  )
)

;;----------------=={ 3D Reflect by Matrix }==----------------;;
;;                                                            ;;
;;  Reflects a VLA-Object or Point List in a plane using a    ;;
;;  Transformation matrix.                                    ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  target   - VLA-Object or Point List to Reflect            ;;
;;  p1,p2,p3 - Three 3D points defining the reflection plane  ;;
;;------------------------------------------------------------;;

(defun LM:Reflect3D ( target p1 p2 p3 / m u ux uy uz )

  (mapcar 'set '(ux uy uz) (setq u (unit (v^v (mapcar '- p2 p1) (mapcar '- p3 p1)))))

  (LM:ApplyMatrixTransformation target
    (setq m
      (list
        (list (- 1. (* 2. ux ux)) (* -2. uy ux) (* -2. ux uz))
        (list (* -2. ux uy) (- 1. (* 2. uy uy)) (* -2. uy uz))
        (list (* -2. ux uz) (* -2. uy uz) (- 1. (* 2. uz uz)))
      )
    )
    (mapcar '- p1 (mxv m p1))
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

;; Matrix x Scalar - Lee Mac
;; Args: m - nxn matrix, n - real scalar

(defun mxs ( m s )
  (mapcar '(lambda ( r ) (mapcar '(lambda ( n ) (* n s)) r)) m)
)

;; Matrix + Matrix - Lee Mac
;; Args: m,n - nxn matrices

(defun m+m ( m n )
  (mapcar '(lambda ( r s ) (mapcar '+ r s)) m n)
)

;; Vector Norm - Lee Mac
;; Args: v - vector in R^n

(defun norm ( v )
  (sqrt (apply '+ (mapcar '* v v)))
)

;; Vector x Scalar - Lee Mac
;; Args: v - vector in R^n, s - real scalar

(defun vxs ( v s )
  (mapcar '(lambda ( n ) (* n s)) v)
)

;; Unit Vector - Lee Mac
;; Args: v - vector in R^n

(defun unit ( v )
  ( (lambda ( n ) (if (equal 0.0 n 1e-14) nil (vxs v (/ 1.0 n)))) (norm v))
)

;; Vector Cross Product - Lee Mac
;; Args: u,v - vectors in R^3

(defun v^v ( u v )
  (list
    (- (* (cadr u) (caddr v)) (* (cadr v) (caddr u)))
    (- (* (car  v) (caddr u)) (* (car  u) (caddr v)))
    (- (* (car  u) (cadr  v)) (* (car  v) (cadr  u)))
  )
)
;=======================================================================
(defun c:rotateobject ( / e p q a )
  (if
    (and
      (setq e (car (entsel)))
      (setq p (getpoint "\nFirst Point of Rotation Axis: "))
      (setq q (getpoint p "\nSecond Point of Rotation Axis: "))
      (setq a (getangle "\nRotation: " p))
    )
    (LM:Rotate3D (vlax-ename->vla-object e) (trans p 1 0) (trans q 1 0) a)
  )
  (princ)
)

(defun c:rotatelist ( / l p q a )
  (if
    (and
      (car (setq l (list (getpoint "\nSpecify First Point: "))))
      (progn
        (while (car (setq l (cons (getpoint "\nNext Point: ") l))))
        (setq p (getpoint "\nFirst Point of Rotation Axis: "))
      )
      (setq q (getpoint p "\nSecond Point of Rotation Axis: "))
      (setq a (getangle "\nRotation: " p))
    )
    (foreach p
      (LM:Rotate3D
        (mapcar '(lambda ( x ) (trans x 1 0)) (cdr l))
        (trans p 1 0)
        (trans q 1 0)
        a
      )
      (entmakex (list (cons 0 "POINT") (cons 10 p)))
    )
  )
  (princ)
)
;=======================================================================
(defun c:reflectobject ( / e p1 p2 p3 )
  (if
    (and
      (setq e (car (entsel)))
      (setq p1 (getpoint "\nFirst Point of Reflection Plane: "))
      (setq p2 (getpoint "\nSecond Point of Reflection Plane: " p1))
      (setq p3 (getpoint "\nThird Point of Reflection Plane: "  p1))
    )
    (LM:Reflect3D (vlax-ename->vla-object e) (trans p1 1 0) (trans p2 1 0) (trans p3 1 0))
  )
  (princ)
)

(defun c:reflectlist ( / l p1 p2 p3 )
  (if
    (and
      (car (setq l (list (getpoint "\nSpecify First Point: "))))
      (progn
        (while (car (setq l (cons (getpoint "\nNext Point: ") l))))
        (setq p1 (getpoint "\nFirst Point of Reflection Plane: "))
      )
      (setq p2 (getpoint "\nSecond Point of Reflection Plane: " p1))
      (setq p3 (getpoint "\nThird Point of Reflection Plane: "  p1))
    )
    (foreach p
      (LM:Reflect3D
        (mapcar '(lambda ( x ) (trans x 1 0)) (cdr l))
        (trans p1 1 0)
        (trans p2 1 0)
        (trans p3 1 0)
      )
      (entmakex (list (cons 0 "POINT") (cons 10 p)))
    )
  )
  (princ)
)

