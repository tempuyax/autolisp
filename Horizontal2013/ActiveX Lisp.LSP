;;;Sub Example_IntersectWith()
;;;    ' This example creates a line and circle and finds the points at
;;;    ' which they intersect.
;;;    
;;;    ' Create the line
;;;    Dim lineObj As AcadLine
;;;    Dim startPt(0 To 2) As Double
;;;    Dim endPt(0 To 2) As Double
;;;    startPt(0) = 1: startPt(1) = 1: startPt(2) = 0
;;;    endPt(0) = 5: endPt(1) = 5: endPt(2) = 0
;;;    Set lineObj = ThisDrawing.ModelSpace.AddLine(startPt, endPt)
;;;        
;;;    ' Create the circle
;;;    Dim circleObj As AcadCircle
;;;    Dim centerPt(0 To 2) As Double
;;;    Dim radius As Double
;;;    centerPt(0) = 3: centerPt(1) = 3: centerPt(2) = 0
;;;    radius = 1
;;;    Set circleObj = ThisDrawing.ModelSpace.AddCircle(centerPt, radius)
;;;    ZoomAll
;;;      
;;;    ' Find the intersection points between the line and the circle
;;;    Dim intPoints As Variant
;;;    intPoints = lineObj.IntersectWith(circleObj, acExtendNone)
;;;    
;;;    ' Print all the intersection points
;;;    Dim I As Integer, j As Integer, k As Integer
;;;    Dim str As String
;;;    If VarType(intPoints) <> vbEmpty Then
;;;        For I = LBound(intPoints) To UBound(intPoints)
;;;            str = "Intersection Point[" & k & "] is: " & intPoints(j) & "," & intPoints(j + 1) & "," & intPoints(j + 2)
;;;            MsgBox str, , "IntersectWith Example"
;;;            str = ""
;;;            I = I + 2
;;;            j = j + 3
;;;            k = k + 1
;;;        Next
;;;    End If
;;;End Sub

(defun c:myarr (/ obj1 bpt n)
  ;;tes by pahor
  ;;circleObj.ArrayPolar(noOfObjects, angleToFill, basePnt)
  (if (and (setq obj1 (car (entsel "\nSelect 1 Object: ")))
	   (setq bpt (GetPtTrans (getpoint "\nBase Point: ")))
	   (setq n (getint "Jumlah: "))
      ) ;_ end of and
    (vlax-invoke
      (vlax-ename->vla-object obj1)
      'ArrayPolar
      n
      (* 2 pi)
      bpt
    ) ;_ end of vlax-invoke
  ) ;_ end of if
  (princ)
) ;_ end of defun

(setq acadObject (vlax-get-acad-object))
;;#<VLA-OBJECT IAcadApplication 00a4b2b4>
;;Get the AutoCAD ActiveDocument property: 

(setq acadDocument (vlax-get-property acadObject 'ActiveDocument))
;;#<VLA-OBJECT IAcadDocument 00302a18>
;;The function returns the current document object. 

;;Get the ModelSpace property of the ActiveDocument object: 
(setq mSpace (vlax-get-property acadDocument 'Modelspace))
;;#<VLA-OBJECT IAcadModelSpace 00c14b44>
;;The model space object of the current document is returned. 

;;Convert a drawing entity to a VLA-object: 
(setq vlaobj (vlax-ename->vla-object e))
;;#<VLA-OBJECT IAcadLWPolyline 0467114c>
;;Get the color property of the object: 
(vlax-get-property vlaobj 'Color)

;;Subsitusi Intruksi akan menjadi seperti ini
;;ThisDrawing.ModelSpace.AddCircle(centerPt, radius)
(vl-load-com)                        ; load ActiveX support
(defun c:MyCrtCircle (/ mSpace bpt r)
  ;;tes by pahor
  ;;circleObj.ArrayPolar(noOfObjects, angleToFill, basePnt)
  (if (and (setq mSpace	(vlax-get-property
			  (vlax-get-property
			    (vlax-get-acad-object)
			    'ActiveDocument
			  ) ;_ end of vlax-get-property
			  'Modelspace
			) ;_ end of vlax-get-property
	   ) ;_ end of setq
	   (setq bpt (GetPtTrans (getpoint "\nBase Point: ")))
	   (setq r 10) ;;(getint "\nRadius: "))
      ) ;_ end of and
    (progn
      (vlax-invoke			;fungsi invoke
	mSpace				;Object pointer
	'AddCircle			;Property fungsi
	bpt				;Parameter
	r				;Parameter
      ) ;_ end of vlax-invoke
      (vlax-release-object mSpace)
    ) ;_ end of progn
  ) ;_ end of if
  (princ)
) ;_ end of defun

(eval a) 

;;cara mengambil pointer amplikasi yang lain...contoh Excel App
;;(vlax-get-object "Excel.Application")

;;ActivX hanya memmiliki 3 dasar
;;1. Object    Pemanggilan Aplikasi
;;2. Property  Penggunaan arah pointer
;;4. invoke    Eksikusi Pointer Properti dengan memasukan Parameter nya
;;Semua Bahasa program memiliki format ini jika ingin menjalankan pointer
;; Fungsi :  Invoke->Object->Property(Parameters)

;;;(vl-load-com)                        ; load ActiveX support
;;;(vlax-for for-item 
;;;   (vla-get-modelspace
;;;        (vla-get-activedocument (vlax-get-acad-object))
;;;   )
;;;  (vlax-dump-object for-item)        ; list object properties
;;;)

;;;(vlax-variant-type var) 
;;;Arguments 
;;;var 
;;;A variable whose value is a variant. 
;;;Return Values 
;;;If var contains a variant, one of the following integers is returned: 
;;;0 Uninitialized (vlax-vbEmpty) 
;;;1 Contains no valid data (vlax-vbNull) 
;;;2 Integer (vlax-vbInteger) 
;;;3 Long integer (vlax-vbLong) 
;;;4 Single-precision floating-point number (vlax-vbSingle) 
;;;5 Double-precision floating-point number (vlax-vbDouble) 
;;;8 String (vlax-vbString) 
;;;9 Object (vlax-vbObject) 
;;;11 Boolean (vlax-vbBoolean) 
;;;8192 + n Safearray (vlax-vbArray) of some data type. For example, an array of doubles (vlax-vbDouble) returns 8197 (8192 + 5). 

;;;From model space, you can switch to the last active paper space layout. 
;;;To switch to the last active paper space layout 
;;;1. Set the ActiveSpace property to acPaperSpace: 
;;;ThisDrawing.ActiveSpace = acPaperSpace 
;;;2. Toggle the MSpace property to FALSE: 
;;;ThisDrawing.MSpace = FALSE
;;;acPaperSpace layout dalam keadaan mode di atas kertas
;;;acModelSpace layout dalam keadaan mode di didalam viewport

;;;Sub Example_AddPolyfaceMesh()
;;;    
;;;    Dim vertexList(0 To 17) As Double
;;;
;;;    'Data
;;;    vertexList(0) = 4: vertexList(1) = 7: vertexList(2) = 0
;;;    vertexList(3) = 5: vertexList(4) = 7: vertexList(5) = 0
;;;    vertexList(6) = 6: vertexList(7) = 7: vertexList(8) = 0
;;;    vertexList(9) = 4: vertexList(10) = 6: vertexList(11) = 0
;;;    vertexList(12) = 5: vertexList(13) = 6: vertexList(14) = 0
;;;    vertexList(15) = 6: vertexList(16) = 6: vertexList(17) = 1
;;;    
;;;
;;;    Dim FaceList(0 To 7) As Integer
;;;
;;;    FaceList(0) = 1
;;;    FaceList(1) = 2
;;;    FaceList(2) = 5
;;;    FaceList(3) = 4
;;;    FaceList(4) = 2
;;;    FaceList(5) = 3
;;;    FaceList(6) = 6
;;;    FaceList(7) = 5
;;;
;;;    Dim obj As AcadPolyfaceMesh
;;;    Set obj = ModelSpace.AddPolyfaceMesh(vertexList, FaceList)
;;;    obj.Update
;;;
;;;    ' Change the viewing direction of the viewport to
;;;    ' better see the polyface mesh
;;;    Dim NewDirection(0 To 2) As Double
;;;    NewDirection(0) = -1: NewDirection(1) = -1: NewDirection(2) = 1
;;;    ThisDrawing.ActiveViewport.direction = NewDirection
;;;    ThisDrawing.ActiveViewport = ThisDrawing.ActiveViewport
;;;    ZoomAll
;;;End Sub
;;;Errrooorr
(defun c:MyCrtPolyfaceMesh (/ mSpace vertexList FaceList)
  ;;tes by pahor
  (defun AX@MakePtsArr (pts / row n)
    (if	(not (eq (setq n (- (length pts) 1)) 0))
      (if (setq	vertexList
		 (vlax-make-safearray vlax-vbDouble (cons 0 n) (cons 0 2))
		row 0
	  ) ;_ end of setq
	(progn
	  (mapcar
	    '(lambda (pt)
	       (apply
		 '(lambda (x y z)
		    (vlax-safearray-put-element vertexList row 0 x)
		    (vlax-safearray-put-element vertexList row 1 y)
		    (vlax-safearray-put-element vertexList row 2 z)
		    (setq row (1+ row))
		  ) ;_ end of lambda
		 pt
	       ) ;_ end of mapcar
	     ) ;_ end of lambda
	    pts
	  ) ;_ end of mapcar
	  vertexList			; out array
	) ;_ end of progn
      ) ;_ end of if
    ) ;_ end of if
  ) ;_ end of defun

  (defun AX@MakeDtsArr (dts / n DataList)
    (if	(not (eq (setq n (- (length dts) 1)) 0))
      (if (setq DataList (vlax-make-safearray vlax-vbInteger (cons 0 n)))
	(vlax-safearray-fill DataList dts)
      ) ;_ end of if
    ) ;_ end of if
  ) ;_ end of defun

  (if (setq mSpace (vlax-get-property
		     (vlax-get-property
		       (vlax-get-acad-object)
		       'ActiveDocument
		     ) ;_ end of vlax-get-property
		     'Modelspace
		   ) ;_ end of vlax-get-property
      ) ;_ end of setq

    (progn
      (setq
	pts
		   (list
		     (xyz 4 7 0)
		     (xyz 5 7 0)
		     (xyz 6 7 0)
		     (xyz 4 6 0)
		     (xyz 5 6 0)
		     (xyz 6 6 1)
		   ) ;_ end of list
	vls        (list 4 7 0  5 7 0  6 7 0  4 6 0  5 6 0  6 6 1)
	dts	   (list 1 2 5 4 2 3 6 5)
	vertexList (AX@MakeDtsArr vls)
	FaceList   (AX@MakeDtsArr dts)
      ) ;_ end of setq
      ;;(vlax-invoke-method 		;fungsi invoke
	(vlax-invoke			;fungsi invoke
	  mSpace			;Object pointer
	  'AddPolyfaceMesh		;Property fungsi
	  vertexList			;Parameter
	  FaceList			;Parameter
	) ;_ end of vlax-invoke
;;	'Update
      ;;) ;_ end of vlax-invoke
    ) ;_ end of progn
  ) ;_ end of if
  (princ)
) ;_ end of defun



(vl-list* 1 2 3 '(3 4)) 


;;;Sub Example_Add3DMesh()
;;;    ' This example creates a 4 X 4 polygonmesh in model space.
;;;    Dim meshObj As AcadPolygonMesh
;;;    Dim mSize, nSize, count As Integer
;;;    Dim points(0 To 47) As Double
;;;    
;;;    ' Create the matrix of points
;;;    points(0) = 0: points(1) = 0: points(2) = 0
;;;    points(3) = 2: points(4) = 0: points(5) = 1
;;;    points(6) = 4: points(7) = 0: points(8) = 0
;;;    points(9) = 6: points(10) = 0: points(11) = 1
;;;    points(12) = 0: points(13) = 2: points(14) = 0
;;;    points(15) = 2: points(16) = 2: points(17) = 1
;;;    points(18) = 4: points(19) = 2: points(20) = 0
;;;    points(21) = 6: points(22) = 2: points(23) = 1
;;;    points(24) = 0: points(25) = 4: points(26) = 0
;;;    points(27) = 2: points(28) = 4: points(29) = 1
;;;    points(30) = 4: points(31) = 4: points(32) = 0
;;;    points(33) = 6: points(34) = 4: points(35) = 0
;;;    points(36) = 0: points(37) = 6: points(38) = 0
;;;    points(39) = 2: points(40) = 6: points(41) = 1
;;;    points(42) = 4: points(43) = 6: points(44) = 0
;;;    points(45) = 6: points(46) = 6: points(47) = 0
;;;    
;;;    mSize = 4: nSize = 4
;;;    
;;;    ' creates a 3Dmesh in model space
;;;    Set meshObj = ThisDrawing.ModelSpace.Add3DMesh(mSize, nSize, points)
;;;    
;;;    ' Change the viewing direction of the viewport to better see the polygonmesh
;;;    Dim NewDirection(0 To 2) As Double
;;;    NewDirection(0) = -1: NewDirection(1) = -1: NewDirection(2) = 1
;;;    ThisDrawing.ActiveViewport.direction = NewDirection
;;;    ThisDrawing.ActiveViewport = ThisDrawing.ActiveViewport
;;;    ZoomAll
;;;    
;;;End Sub

(defun c:MyCrt3DMesh (/ AcadObject ThisDrawing mSpace mSize nSize pts)
  ;;tes by pahor
  (defun AX@PtLstMerger	(pts / res)
    (if	(eq (length (car pts)) 3)
      (progn
	(mapcar
	  '(lambda (pt)
	     (setq Res
		    (append
		      Res
		      (apply
			'(lambda (x y z)
			   (vl-list* x y z (list))
			 ) ;_ end of lambda
			Pt
		      ) ;_ end of apply
		    ) ;_ end of append
	     ) ;_ end of setq
	   ) ;_ end of lambda
	  pts
	) ;_ end of mapcar
	res				; merger all point's
      ) ;_ end of progn
      pts				; one point
    ) ;_ end of if
  ) ;_ end of defun

  (defun AX@FillLstArr (dts As / n DataList)
    (if	(not (eq (setq n (- (length dts) 1)) 0))
      (if (setq DataList (vlax-make-safearray As (cons 0 n)))
	(vlax-safearray-fill DataList dts)
      ) ;_ end of if
    ) ;_ end of if
  ) ;_ end of defun

  (if
    (and
      (setq
	AcadObject  (vlax-get-acad-object)
	ThisDrawing (vlax-get-property
		      AcadObject
		      'ActiveDocument
		    ) ;_ end of vlax-get-property
	mSpace	    (vlax-get-property
		      ThisDrawing
		      'Modelspace
		    ) ;_ end of vlax-get-property
	pts
		    (list
		      (xyz 0 0 0)
		      (xyz 2 0 1)
		      (xyz 4 0 0)
		      (xyz 6 0 1)
		      (xyz 0 2 0)
		      (xyz 2 2 1)
		      (xyz 4 2 0)
		      (xyz 6 2 1)
		      (xyz 0 4 0)
		      (xyz 2 4 1)
		      (xyz 4 4 0)
		      (xyz 6 4 0)
		      (xyz 0 6 0)
		      (xyz 2 6 1)
		      (xyz 4 6 0)
		      (xyz 6 6 0)
		    ) ;_ end of list
	mSize	    (vlax-make-variant 4 vlax-vbInteger)
	nSize	    (vlax-make-variant 4 vlax-vbInteger)
	points	    (AX@FillLstArr (AX@PtLstMerger pts) vlax-vbDouble)
      ) ;_ end of setq
    ) ;_ end of and
     ;;ThisDrawing.ModelSpace.Add3DMesh(mSize, nSize, points)
    (vla-add3DMesh mSpace mSize nSize points)
  ) ;_ end of if
  (princ)
) ;_ end of defun

;;;Sub Example_AddText()
;;;    ' This example creates a text object in model space.
;;;
;;;    Dim textObj As AcadText
;;;    Dim textString As String
;;;    Dim insertionPoint(0 To 2) As Double
;;;    Dim height As Double
;;;    
;;;    ' Define the text object
;;;    textString = "Hello, World."
;;;    insertionPoint(0) = 2: insertionPoint(1) = 2: insertionPoint(2) = 0
;;;    height = 0.5
;;;    
;;;    ' Create the text object in model space
;;;    Set textObj = ThisDrawing.ModelSpace.AddText(textString, insertionPoint, height)
;;;    ZoomAll
;;;    
;;;End Sub
(defun c:MyCrtText (/ mSpace textString insertionPoin height pts)
  (defun LM:SafearrayVariant (datatype data)
    ;; © Lee Mac 2010
    (vlax-make-variant
      (vlax-safearray-fill
	(vlax-make-safearray
	  datatype
	  (cons 0 (1- (length data)))
	) ;_ end of vlax-make-safearray
	data
      ) ;_ end of vlax-safearray-fill
    ) ;_ end of vlax-make-variant
  ) ;_ end of defun
  ;;tes by pahor
  (defun AX@MakeDtsArr (dts As / n DataList)
    (if	(not (eq (setq n (- (length dts) 1)) 0))
      (if (setq DataList (vlax-make-safearray As (cons 0 n)))
	(vlax-safearray-fill DataList dts)
      ) ;_ end of if
    ) ;_ end of if
  ) ;_ end of defun

  (if
    (setq mSpace (vlax-get-property
		   (vlax-get-property
		     (vlax-get-acad-object)
		     'ActiveDocument
		   ) ;_ end of vlax-get-property
		   'Modelspace
		 ) ;_ end of vlax-get-property
    ) ;_ end of setq
     (progn
       (setq
	 pts (list 2 2 0) ;_ end of list
	 insertionPoin 
	  (getpoint "point: ")
	 textString
	  "PAHOR M"
	 height	10
       ) ;_ end of setq
       (vlax-invoke			;fungsi invoke
	 mSpace				;Object pointer
	 'AddText			;Property fungsi
	 textString			;Parameter
	 insertionPoin			;Parameter
	 height				;Parameter
) ;_ end of vlax-invoke
 ;_ end of vlax-invoke
       (vlax-release-object mSpace)
     ) ;_ end of progn
  ) ;_ end of if
  (princ)
) ;_ end of defun
;vlax-vbVariant
;;;(vlax-make-variant [value] [type]) 
;;;Arguments 
;;;value 
;;;The value to be assigned to the variant. If omitted, the variant is created with the vlax-vbEmpty type (uninitialized). 
;;;type 
;;;The type of variant. This can be represented by one of the following constants: 
;;;
;;;vlax-vbEmpty (0)  Uninitialized (default value) 
;;;vlax-vbNull (1)  Contains no valid data 
;;;vlax-vbInteger (2)  Integer 
;;;vlax-vbLong (3)  Long integer 
;;;vlax-vbSingle (4)  Single-precision floating-point number 
;;;vlax-vbDouble (5)  Double-precision floating-point number 
;;;vlax-vbString (8)  String 
;;;vlax-vbObject (9)  Object 
;;;vlax-vbBoolean (11)  Boolean 
;;;vlax-vbArray (8192)  Array 
(setq DataList (vlax-make-safearray As (cons 0 1)))
(vlax-safearray-fill DataList dts)

;;Create an array with two dimensions, each dimension starting at index 1
;;;(setq matrix (vlax-make-safearray vlax-vbString (cons 1 2) (cons 1 2)))
;;;(vlax-safearray-put-element matrix 1 1 "a")
;;;(vlax-safearray-put-element matrix 1 2 "b")
;;;(vlax-safearray-put-element matrix 2 1 "c")
;;;(vlax-safearray-put-element matrix 2 2 "d")
;;;(vlax-safearray->list matrix)
;;;
;;;(vlax-safearray-get-element matrix 1 2)
;;;
;;#<safearray...> 
;;"a" 
;;"b" 
;;"c" 
;;"d" 
;;(("a" "b") ("c" "d")) 
;;"b"


(setq a (vl-list* 1 2 3 (list)))
(setq a (append a (vl-list* 4 5 6 (list))))
(setq a (append a (vl-list* 7 8 9 (list))))


(defun AX@PtLstMerger (pts / res)
  (if (eq (length (car pts)) 3)
    (progn
      (mapcar
	'(lambda (pt)
	   (setq Res
		  (append
		    Res
		    (apply
		      '(lambda (x y z)
			 (vl-list* x y z (list))
		       ) ;_ end of lambda
		      Pt
		    ) ;_ end of apply
		  ) ;_ end of append
	   ) ;_ end of setq
	 ) ;_ end of lambda
	pts
      ) ;_ end of mapcar
      res
    ) ;_ end of progn
    pts ; normal
  ) ;_ end of if
) ;_ end of defun

;;(4 7 0 5 7 0 6 7 0 4 6 0 5 6 0 6 6 1)
;;;(0 0 0 2 0 1 4 0 0 6 0 1 0 2 0 2 2 1 4 2 0 6 2 1 0 4 0 2 4 1 4 4 0 6 4 0 0 6 0 
;;;2 6 1 4 6 0 6 6 0)

(defun c:tes ()
  (setq
    pts0 (list (xyz 0 1 1))
    pts1
	 (list
	   (xyz 0 0 0)
	   (xyz 2 0 1)
	   (xyz 4 0 0)
	   (xyz 6 0 1)
	   (xyz 0 2 0)
	   (xyz 2 2 1)
	   (xyz 4 2 0)
	   (xyz 6 2 1)
	   (xyz 0 4 0)
	   (xyz 2 4 1)
	   (xyz 4 4 0)
	   (xyz 6 4 0)
	   (xyz 0 6 0)
	   (xyz 2 6 1)
	   (xyz 4 6 0)
	   (xyz 6 6 0)
	 ) ;_ end of list
 ;_ end of list
    pts2
	 (list
	   (xyz 4 7 0)
	   (xyz 5 7 0)
	   (xyz 6 7 0)
	   (xyz 4 6 0)
	   (xyz 5 6 0)
	   (xyz 6 6 1)
	 ) ;_ end of list
  ) ;_ end of setq
  (setq dts (list 1 2 3 4 5 6 7 9))
  (print (AX@PtLstMerger pts0))
  ;;(vlax-safearray->list (AX@MakePtsArr pts))
  ;;(vlax-safearray->list (AX@MakeDtsArr dts))
  (princ)
) ;_ end of defun
(
 (4 7 0 5 7 0 6 7 0 4 6 0 5 6 0 6 6 1 4 7 0)
 (4 7 0 5 7 0 6 7 0 4 6 0 5 6 0 6 6 1 4 7 0 5 7 0)
 (4 7 0 5 7 0 6 7 0 4 6 0 5 6 0 6 6 1 4 7 0 5 7 0 6 7 0)
 (4 7 0 5 7 0 6 7 0 4 6 0 5 6 0 6 6 1 4 7 0 5 7 0 6 7 0 4 6 0)
 (4 7 0 5 7 0 6 7 0 4 6 0 5 6 0 6 6 1 4 7 0 5 7 0 6 7 0 4 6 0 5 6 0)
 (4 7 0 5 7 0 6 7 0 4 6 0 5 6 0 6 6 1 4 7 0 5 7 0 6 7 0 4 6 0 5 6 0 6 6 1)
)

(defun LM:DXF->Variants ( lst *typ *val)
  ;; © Lee Mac 2010
  (set *typ (LM:SafearrayVariant vlax-vbInteger (mapcar 'car lst))) 

  (set *val
    (LM:SafearrayVariant vlax-vbVariant
      (mapcar
       '(lambda ( data )
          (if (listp (setq data (cdr data)))
            (vlax-3D-point data)
            (vlax-make-variant data)
          )
        )
       lst       
      )
    )
  )
)
(vlax-variant-value (LM:DXF->Variants (list (cons 10 '(1 1 1))) 'typ 'val))



(defun c:TransformBy ()			; / lineObj startPt endPt matList transMat) 

  (vl-load-com)				; Load ActiveX support
  (setq acadObject (vlax-get-acad-object))
  (setq acadDocument (vla-get-ActiveDocument acadObject))
  (setq mSpace (vla-get-ModelSpace acadDocument))
  ;; Create a line
  (setq startPt (getpoint "Pick the start point"))
  (setq endPt (vlax-3d-point (getpoint startPt "Pick the end point")))
  (setq lineObj (vla-addline mSpace (vlax-3d-point startPt) endPt))
;;; Initialize the transMat variable with a transformation matrix
;;; that will rotate an object by 90 degrees about the point(0,0,0).
;;; Begin by Creating a list of four lists, each containing four
;;; numbers, representing transformation matrix elements.
  (setq matList (list '(0 -1 0 0) '(1 0 0 0) '(0 0 1 0) '(0 0 0 1)))
;;; Use vlax-tmatrix to convert the list to a variant.
  (setq transmat (vlax-tmatrix matlist))
;;;  Transform the line using the defined transformation matrix
  (vla-transformby lineObj transMat)
  (vla-zoomall acadObject)
  (princ "The line is transformed ")
  (princ)
) ;_ end of defun


 
;;;OpenAsTextStream Method 
;;;
;;;Description
;;;
;;;Opens a specified file and returns a TextStream object that can be used to read from, write to, or append to the file.
;;;
;;;Syntax
;;;
;;;object.OpenAsTextStream([iomode, [format]])
;;;
;;;The OpenAsTextStream method syntax has these parts:
;;;
;;;Part Description 
;;;object Required. Always the name of a File object. 
;;;iomode Optional. Indicates input/output mode. Can be one of three constants: ForReading, ForWriting, or ForAppending. 
;;;format Optional. One of three Tristate values used to indicate the format of the opened file. If omitted, the file is opened as ASCII. 
;;;
;;;Settings
;;;
;;;The iomode argument can have any of the following settings:
;;;
;;;Constant Value Description 
;;;ForReading 1 Open a file for reading only. You can't write to this file. 
;;;ForWriting 2 Open a file for writing. If a file with the same name exists, its previous contents are overwritten. 
;;;ForAppending 8 Open a file and write to the end of the file. 
;;;
;;;The format argument can have any of the following settings:
;;;
;;;Constant Value Description 
;;;TristateUseDefault –2 Opens the file using the system default. 
;;;TristateTrue –1 Opens the file as Unicode. 
;;;TristateFalse   0 Opens the file as ASCII. 
;;;
;;;Remarks
;;;
;;;The OpenAsTextStream method provides the same functionality as the OpenTextFile method of the FileSystemObject. In addition, the OpenAsTextStream method can be used to write to a file.
;;;
;;;The following code illustrates the use of the OpenAsTextStream method:
;;;
;;;Sub TextStreamTest
;;;    Const ForReading = 1, ForWriting = 2, ForAppending = 3
;;;    Const TristateUseDefault = -2, TristateTrue = -1, TristateFalse = 0
;;;    Dim fs, f, ts, s
;;;    Set fs = CreateObject("Scripting.FileSystemObject")
;;;    fs.CreateTextFile "test1.txt"            'Create a file
;;;    Set f = fs.GetFile("test1.txt")
;;;    Set ts = f.OpenAsTextStream(ForWriting, TristateUseDefault)
;;;    ts.Write "Hello World"
;;;    ts.Close
;;;    Set ts = f.OpenAsTextStream(ForReading, TristateUseDefault)
;;;    s = ts.ReadLine
;;;    MsgBox s
;;;    ts.Close
;;;End Sub
;;; 
;;;
;;;
;;;==============================================================
;;;
;;;
;;;Dim fs, a, retstring
;;;Set fs = CreateObject("Scripting.FileSystemObject")
;;;Set a = fs.OpenTextFile("c:\testfile.txt", ForReading, False)
;;;Do While a.AtEndOfStream <> True
;;;    retstring = a.ReadLine
;;;    ...
;;;Loop
;;;a.Close
;;;
