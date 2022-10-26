
;;;********************************************************************************************
;;; Main       ; C:PolyGpsElev                                                                #
;;; Editor     : Pahor. M                                                                     #
;;; Tanggal    : nunukan, 07 Februari 2010                                                    #
;;; Koreksi    : 04 April 2019                                                                #
;;; DIskripsi  : Membaca data bulge & titik tipe POLYLINE                                     #
;;;********************************************************************************************
(defun C:PolyGpsElev (/		       CatagorySelectionLst-01
		      NilaiElev	       BasePt
		      ObjName	       polypointsLst
		      OutList	       Elev-Dist-List
		      Elev-List	       Dist-List
		      TxtHeight	       AsTxtlayer
		      AsPolylayer
		     )
;;(load "//selection")
;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; POLYLINE:ReadBulgeData                                          ;
;;;           Date          ; Nunukan, Februari 2010                                          ;
;;;           Project       ; -                                                               ;
;;;	      File          ; polygpselev.lsp                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; mengambil nilai '(bulge (x y z)) dari polyline yg di pilih      ;
;;;                                                                                           ;
;;;                                                                                           ;
;;;           NOTES : File di COPAS dari polyelev.lsp                                         ;
;;;********************************************************************************************
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
;;;============================================================================================
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	    ObjLst
	     (entget
	       NextSelectionNameObjLst
	     ) ;_ end of entget
;;;============================================================================================
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	    PointData
	     (cdr (assoc
		    10
		    ObjLst
		  ) ;_ end of assoc
	     ) ;_ end of cdr
;;;============================================================================================
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	    BulgeData
	     (cdr (assoc
		    42
		    ObjLst
		  ) ;_ end of assoc
	     ) ;_ end of cdr
;;;============================================================================================
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	    NextSelectionNameObjLst
	     (entnext
	       NextSelectionNameObjLst
	     ) ;_ end of entnext
;;;============================================================================================
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	    polypointsLst
	     (append polypointsLst
		     (list (list BulgeData PointData))
	     ) ;_ end of append
;;;============================================================================================
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	  ) ;_ end of setq
       ) ;_ end of while
       (setq ct (1+ ct))
    ) ;_ end of repeat
    polypointsLst
  ) ;_ end of defun

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; POLYLINE:GetPointFromLst                                        ;
;;;           Date          ; Nunukan, Februari 2010                                          ;
;;;           Project       ; -                                                               ;
;;;	      File          ; polygpselev.lsp                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; mengambil koordinat '(x y z) dari list "polypointsLst"          ;
;;;                                                                                           ;
;;;                                                                                           ;
;;;           NOTES : File di COPAS dari polyelev.lsp                                         ;
;;;********************************************************************************************
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

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; ELEVASI:Set-Elev-Dist                                           ;
;;;           Date          ; Nunukan, Februari 2010                                          ;
;;;           Project       ; -                                                               ;
;;;	      File          ; polygpselev.lsp                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; kalkulasi x y yg bernilai relatif menjadi nilai statif          ;
;;;                           {x = x - xBasePt}                                               ;
;;;                           {y = y - yBasePt + NilaiElev}                                   ;
;;;                                                                                           ;
;;;           NOTES : File di COPAS dari polyelev.lsp                                         ;
;;;********************************************************************************************
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

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; PRINT:PrintLine                                                 ;
;;;           Date          ; Nunukan, Februari 2010                                          ;
;;;           Project       ; -                                                               ;
;;;	      File          ; polygpselev.lsp                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; print ke layar, isi semua list secara berbaris ke bawah         ;
;;;                                                                                           ;
;;;                                                                                           ;
;;;           NOTES : File di COPAS dari polyelev.lsp                                         ;
;;;********************************************************************************************
  (defun PRINT:PrintLine (EntLst StrMess)
    (prompt (strcat "\n" StrMess))
    (foreach n EntLst (print n))
  ) ;_ end of defun

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; SELECT:GetSigleObj                                              ;
;;;           Date          ; Nunukan, Februari 2010                                          ;
;;;           Project       ; -                                                               ;
;;;	      File          ; polygpselev.lsp                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; memilih hanya satu objek yg type nya ditentukan dlm             ;
;;;                           "CatagorySelectionLst"                                          ;
;;;                                                                                           ;
;;;                                                                                           ;
;;;           NOTES : File di COPAS dari polyelev.lsp                                         ;
;;;********************************************************************************************
  (defun SELECT:GetSigleObj (CatagorySelectionLst / SsSet)
    (if	(setq SsSet (ssget ":S" CatagorySelectionLst))
      (list (ssname SsSet 0))		;hanya menambahkan perintah list
    ) ;_ end of if
  ) ;_ end of defun

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; POLYLINE:GetElev-Dist-List                                      ;
;;;           Date          ; Nunukan, Februari 2010                                          ;
;;;           Project       ; -                                                               ;
;;;	      File          ; polygpselev.lsp                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; mengambil '(x y z) dalam keadaan bernilai statif dari sekian    ;
;;;                           banyak titik '(x y z)                                           ;
;;;                                                                                           ;
;;;           NOTES : File di COPAS dari polyelev.lsp                                         ;
;;;********************************************************************************************
  (defun POLYLINE:GetElev-Dist-List (polypointsLst NilaiElev BasePt / OutList)
    (mapcar '(lambda (Pt)
	       (setq OutList
		      (append
			OutList
			(list (ELEVASI:Set-Elev-Dist BasePt Pt NilaiElev))
		      ) ;_ end of append
	       ) ;_ end of setq
	     ) ;_ end of lambda
	    polypointsLst
    ) ;_ end of mapcar
    OutList
  ) ;_ end of defun

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; POLYLINE:GetDist-List                                           ;
;;;           Date          ; Nunukan, Februari 2010                                          ;
;;;           Project       ; -                                                               ;
;;;	      File          ; polygpselev.lsp                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; hanya Mengambil semua nilai x dari sekian banyak titik (x y z)  ;
;;;                                                                                           ;
;;;           NOTES : File di COPAS dari polyelev.lsp                                         ;
;;;********************************************************************************************
  (defun POLYLINE:GetDist-List (polypointsLst / OutList)
    (mapcar '(lambda (Pt)
	       (setq OutList (append OutList (list (car pt)))) ;_ end of setq
	     ) ;_ end of lambda
	    polypointsLst
    ) ;_ end of mapcar
    OutList
  ) ;_ end of defun

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; POLYLINE:GetElev-List                                           ;
;;;           Date          ; Nunukan, Februari 2010                                          ;
;;;           Project       ; -                                                               ;
;;;	      File          ; polygpselev.lsp                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; hanya Mengambil semua nilai y dari sekian banyak titik (x y z)  ;
;;;                                                                                           ;
;;;           NOTES : File di COPAS dari polyelev.lsp                                         ;
;;;********************************************************************************************
  (defun POLYLINE:GetElev-List (polypointsLst / OutList)
    (mapcar '(lambda (Pt)
	       (setq OutList (append OutList (list (cadr pt)))) ;_ end of setq
	     ) ;_ end of lambda
	    polypointsLst
    ) ;_ end of mapcar
    OutList
  ) ;_ end of defun

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; POINT:PointTransition                                           ;
;;;           Date          ; Nunukan, Agustus 2006                                           ;
;;;           Project       ; -                                                               ;
;;;	      File          ; polygpselev.lsp                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; Transisi & adaptasi nilai koordinat pada keadaan                ;
;;;                           mode UCS atau WCS                                               ;
;;;                                                                                           ;
;;;           NOTES : File di COPAS dari entmake.lsp                                          ;
;;;********************************************************************************************
;;;Code | Coordinate system                                                                   ;
;;;0      World (WCS)                                                                         ;
;;;1      User (current UCS)                                                                  ;
;;;2      Display; DCS of current viewport when used with code 0 or 1,                        ;
;;;       DCS of current model space viewport when used with code 3                           ;
;;;3      Paper space DCS, PSDCS (used only with code 2)                                      ;
;;;Converted 2D point Z values  
;;;From  Filled-in Z value 
;;;WCS 0.0 
;;;UCS Current elevation 
;;;OCS 0.0 
;;;DCS Projected to the current construction plane (UCS XY plane + current elevation) 
;;;PSDCS Projected to the current construction plane (UCS XY plane + current elevation) 
  (defun POINT:PointTransition (Pt)
    (trans Pt 1 (getvar 'WORLDUCS))
  ) ;_ end of defun

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; GP:ENTMAKE-3DPOLYLINE                                           ;
;;;           Date          ; Nunukan, Agustus 2006                                           ;
;;;           Project       ; -                                                               ;
;;;	      File          ; polygpselev.lsp                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; membuat objek polyline dari sekumpulan data koordinat '(x y z)  ;
;;;                                                                                           ;
;;;           NOTES : File di COPAS dari entmake.lsp                                          ;
;;;********************************************************************************************
  (defun GP:ENTMAKE-3DPOLYLINE (ParPtLst layer)
    (entmake
      (List
	(cons 0 "POLYLINE")		; Object type
	;;(cons 62 Color)			; Color
	;;(cons 100 "AcDb3dPolyline")
	(cons 70 8)			;3D POLYLINE
	(cons 8 layer)			;modif 2019
	(cons 66 1)			; Vertices follow
      ) ;_ end of List
    ) ;_ end of entmake
    (mapcar '(lambda (Pt)
	       (entmake
		 (list
		   (cons 0 "VERTEX")	; Object type
		   (cons 70 32)		;3D Vertex
		   (cons 10 (POINT:PointTransition Pt))
		 ) ;_ end of list
	       ) ;_ end of entmake
	     ) ;_ end of lambda
	    ParPtLst
    ) ;_ end of mapcar
    (entmake (list (cons 0 "SEQEND")))
    (cdr (assoc 5 (entget (entlast))))	; handle object
  ) ;_ end of defun

(defun 3dPoly (pts lay)
  (Create-3DPolyline
    (list layer: (@ChkVar lay "0"))
    pts
  ) ;_ end of Create-3DPolyline
) ;_ end of defun

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; GP:ENTMAKE-TEXT                                                 ;
;;;           Date          ; Nunukan, Agustus 2006                                           ;
;;;           Project       ; -                                                               ;
;;;	      File          ; polygpselev.lsp                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; membuat objek text                                              ;
;;;                                                                                           ;
;;;           NOTES : File di COPAS dari entmake.lsp                                          ;
;;;********************************************************************************************
  (defun GP:ENTMAKE-TEXT (Pt TxtHeight Rotation TheString layer)
    (entmake
      (list
	(cons 0 "TEXT")
	;;(cons 62 Color)
	(cons 8 layer)			;modif 2019
	(cons 10 (POINT:PointTransition Pt))
	(cons 40 TxtHeight)
	(cons 1 TheString)
	(cons 50 Rotation)
	(cons 72 1)
	(cons 11 (POINT:PointTransition Pt))
	(cons 73 2)
      ) ;_ end of list
    ) ;_ end of entmake
    (cdr (assoc 5 (entget (entlast))))	; handle object
  ) ;_ end of defun

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; GP:CreateTxtNumber                                              ;
;;;           Date          ; Nunukan, Apri 2019                                              ;
;;;           Project       ; -                                                               ;
;;;	      File          ; polygpselev.lsp                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; membuat objek text                                              ;
;;;                                                                                           ;
;;;           NOTES : listing baru                                                            ;
;;;********************************************************************************************
  (defun GP:CreateTxtNumber (PtLst TxtHeight layer / n TheString)
    (setq n 0)
    (mapcar
      '(lambda (Pt)			;define lambda expression
	 (progn
	   (setq TheString (itoa n))
	   (GP:ENTMAKE-TEXT Pt TxtHeight 0 TheString layer)
					;place number at point pt
	   (setq n (1+ n))
	 ) ;_ end of progn
       ) ;_ end of lambda
      PtLst				;list supplied to lambda
    ) ;_ end of mapcar
  ) ;_ end of defun

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; GP:CreateTxtNumber                                              ;
;;;           Date          ; Nunukan, Apri 2019                                              ;
;;;           Project       ; -                                                               ;
;;;	      File          ; polygpselev.lsp                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; membuat objek text                                              ;
;;;                                                                                           ;
;;;           NOTES : listing baru                                                            ;
(defun GP:CreateTrack3dpoly	(PtLst / Bpt)

;;;********************************************************************************************
  (defun ConvertXYZ2LongSec (Pts Bpt / rept ct pt1 pt2 Dist DistLst)
    (if	(length Pts)
      (progn
	(setq
	  rept	  (- (length Pts) 1)
	  ct	  0
	  pt1	  (nth ct Pts)
	  DistLst (list (list (car Bpt)(caddr pt1)))	;begin point
	) ;_ end of setq
	(repeat	rept
	  (setq
	    pt1	    (nth ct Pts)
	    pt2	    (nth (+ ct 1) Pts)
	    Dist    (Distance Pt1 Pt2)
	    DistLst (append DistLst (list (list (+ (car Bpt) Dist) (caddr pt2))))
	    ct	    (1+ ct)
	  ) ;_ end of setq
	) ;_ end of repeat
	DistLst
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of defun
  (if (setq Bpt (getpoint "Get Point "))
    (ConvertXYZ2LongSec PtLst Bpt)
  ) ;_ end of if
) ;_ end of defun
;;;GP:CreateTrack3dpoly end
  
;;;=== Begin of Global Variable ===============================================================
  (if (not *HGPSPOLY)
    (progn

;;;=== setup variable==========================================================================
      (setq
	TxtHeight
				0.2
	AsTxtlayer		"astxt"
	AsPolylayer		"aspoly"
	CatagorySelectionLst-01
				(list (cons 0 "POLYLINE"))
      ) ;_ end of setq

;;;=== side out value input ===================================================================
      (if (setq ObjName (SELECT:GetSigleObj CatagorySelectionLst-01))
	(progn
;;;=== Action Proccesing ======================================================================
	  (setq
	    polypointsLst (POLYLINE:ReadBulgeData ObjName)
	    OutList	  (POLYLINE:GetPointFromLst polypointsLst)
	  ) ;_ end of setq

	  ;;3D Polyline
	  (setq *HGPSPOLY (3dPoly OutList AsPolylayer))
	  ;;(setq *HGPSPOLY (GP:ENTMAKE-3DPOLYLINE OutList AsPolylayer))
	  (GP:CreateTxtNumber OutList TxtHeight AsTxtlayer)

;;;          (foreach n
;;;	   (GP:CreateTrack3dpoly OutList)
;;;           (print n)
;;;          ) ;_ end of foreach
;;;=== Action End =============================================================================
	) ;_ end of progn
	(progn
	  (prompt "\nNothing Selection, POLYLINE Object only")
	) ;_ end of progn
      ) ;_ end of if

;;;=== copyright description===================================================================
      (prompt
	(strcat
	  "\nSource Date (01): Nunukan - 07 Februari 2010"
	  "\nSource Modified (02): Nunukan - April 2019"
	  "\ntool set : polygpselev"
	  "\nEditor & Programmer : Pahor. M | tempuyax@yahoo.com.sg | Nunukan - April 2019"
	) ;_ end of strcat
      ) ;_ end of prompt

;;;=== End of Global Variable =================================================================
    ) ;_ end of progn MAIN
    (prompt "\n***As GPS Sudah di Dibuat***")
  ) ;_ end of if MAIN
  (princ)
) ;_ end of defun
;;;=== END OF MAIN ============================================================================


;;;********************************************************************************************
;;; Main       ; c:DelVertex                                                                  #
;;; Editor     : Pahor. M                                                                     #
;;; Tanggal    : nunukan, 06 April 2019                                                       #
;;; Koreksi    : -                                                                            #
;;; DIskripsi  : Menghapus Vertex POLYLINE                                                    #
;;;********************************************************************************************
(defun c:DelVertex (/ SrcPtnLst DstPtnLst DelPtnLst)

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; GP:CreateTxtNumber                                              ;
;;;           Date          ; Nunukan, Apri 2019                                              ;
;;;           Project       ; -                                                               ;
;;;	      File          ; polygpselev.lsp                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; membuat objek text                                              ;
;;;                                                                                           ;
;;;           NOTES : listing baru                                                            ;
;;;********************************************************************************************
  (defun GP:CreateTxtNumber (PtLst TxtHeight layer / n TheString)
    (setq n 0)
    (mapcar
      '(lambda (Pt)			;define lambda expression
	 (progn
	   (setq TheString (itoa n))
	   (GP:ENTMAKE-TEXT Pt TxtHeight 0 TheString layer)
					;place number at point pt
	   (setq n (1+ n))
	 ) ;_ end of progn
       ) ;_ end of lambda
      PtLst				;list supplied to lambda
    ) ;_ end of mapcar
  ) ;_ end of defun

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; POINT:PointTransition                                           ;
;;;           Date          ; Nunukan, Agustus 2006                                           ;
;;;           Project       ; -                                                               ;
;;;	      File          ; polygpselev.lsp                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; Transisi & adaptasi nilai koordinat pada keadaan                ;
;;;                           mode UCS atau WCS                                               ;
;;;                                                                                           ;
;;;           NOTES : File di COPAS dari entmake.lsp                                          ;
;;;********************************************************************************************
  (defun POINT:PointTransition (Pt)
    (trans Pt 1 (getvar 'WORLDUCS))
  ) ;_ end of defun

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; GP:ENTMAKE-3DPOLYLINE                                           ;
;;;           Date          ; Nunukan, Agustus 2006                                           ;
;;;           Project       ; -                                                               ;
;;;	      File          ; polygpselev.lsp                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; membuat objek polyline dari sekumpulan data koordinat '(x y z)  ;
;;;                                                                                           ;
;;;           NOTES : File di COPAS dari entmake.lsp                                          ;
;;;********************************************************************************************
  (defun GP:ENTMAKE-3DPOLYLINE (ParPtLst layer)
    (entmake
      (List
	(cons 0 "POLYLINE")		; Object type
	;;(cons 62 Color)			; Color
	(cons 70 8)			;3D POLYLINE
	(cons 8 layer)			;modif 2019
	(cons 66 1)			; Vertices follow
      ) ;_ end of List
    ) ;_ end of entmake
    (mapcar '(lambda (Pt)
	       (entmake
		 (list
		   (cons 0 "VERTEX")	; Object type
		   (cons 70 32)		;3D Vertex
		   (cons 10 (POINT:PointTransition Pt))
		 ) ;_ end of list
	       ) ;_ end of entmake
	     ) ;_ end of lambda
	    ParPtLst
    ) ;_ end of mapcar
    (entmake (list (cons 0 "SEQEND")))
    (cdr (assoc 5 (entget (entlast))))	; handle object
  ) ;_ end of defun

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; GP:ENTMAKE-TEXT                                                 ;
;;;           Date          ; Nunukan, Agustus 2006                                           ;
;;;           Project       ; -                                                               ;
;;;	      File          ; polygpselev.lsp                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; membuat objek text                                              ;
;;;                                                                                           ;
;;;           NOTES : File di COPAS dari entmake.lsp                                          ;
;;;********************************************************************************************
  (defun GP:ENTMAKE-TEXT (Pt TxtHeight Rotation TheString layer)
    (entmake
      (list
	(cons 0 "TEXT")
	;;(cons 62 Color)
	(cons 8 layer)			;modif 2019
	(cons 10 (POINT:PointTransition Pt))
	(cons 40 TxtHeight)
	(cons 1 TheString)
	(cons 50 Rotation)
	(cons 72 1)
	(cons 11 (POINT:PointTransition Pt))
	(cons 73 2)
      ) ;_ end of list
    ) ;_ end of entmake
    (cdr (assoc 5 (entget (entlast))))	; handle object
  ) ;_ end of defun

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; TEXT:ReadPointData                                              ;
;;;           Date          ; Nunukan, Apri 2019                                              ;
;;;           Project       ; -                                                               ;
;;;	      File          ; polygpselev.lsp                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; membaca base point objek text yg terseleksi                     ;
;;;                                                                                           ;
;;;           NOTES : listing baru                                                            ;
;;;********************************************************************************************
  (defun TEXT:ReadPointData (SelectionNameObjLst
			     /		     ct
			     ObjLst	     PointData
			     polypointsLst
			    )
    (setq ct 0)
    (repeat
      (length
	SelectionNameObjLst
      ) ;_ end of length
       (setq
;;;============================================================================================
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 ObjLst
		       (entget
			 (nth ct SelectionNameObjLst)
		       ) ;_ end of entget
;;;============================================================================================
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 PointData
		       (atoi
			 (cdr (assoc
				1
				ObjLst
			      ) ;_ end of assoc
			 ) ;_ end of cdr
		       ) ;_ end of atoi
;;;============================================================================================
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 polypointsLst
		       (append polypointsLst
			       (list PointData)
		       ) ;_ end of append
;;;============================================================================================
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       ) ;_ end of setq
       (setq ct (1+ ct))
    ) ;_ end of repeat
    polypointsLst
  ) ;_ end of defun

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; POLYLINE:ReadDstPts                                             ;
;;;           Date          ; Nunukan, Februari 2010                                          ;
;;;           Project       ; -                                                               ;
;;;	      File          ; polygpselev.lsp                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; mengambil nilai '(x y z) Polyline dari (handent *HGPSPOLY)      ;
;;;                                                                                           ;
;;;                                                                                           ;
;;;           NOTES : File di COPAS dari polyelev.lsp                                         ;
;;;********************************************************************************************
  (defun POLYLINE:ReadDstPts
	 (/ NextObjName DXF PointData polypointsLst)
    (setq NextObjName (entnext (handent *HGPSPOLY)))
    (while
      (/=
	(cdr
	  (assoc
	    0
	    (entget
	      NextObjName
	    ) ;_ end of entget
	  ) ;_ end of assoc
	) ;_ end of cdr
	"SEQEND"
      ) ;_ end of /=
       (setq
;;;============================================================================================
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 DXF
		       (entget
			 NextObjName
		       ) ;_ end of entget
;;;============================================================================================
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 PointData
		       (cdr (assoc
			      10
			      DXF
			    ) ;_ end of assoc
		       ) ;_ end of cdr
;;;============================================================================================
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 NextObjName
		       (entnext
			 NextObjName
		       ) ;_ end of entnext
;;;============================================================================================
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 polypointsLst
		       (append polypointsLst
			       (list PointData)
		       ) ;_ end of append
;;;============================================================================================
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       ) ;_ end of setq
    ) ;_ end of while
    polypointsLst
  ) ;_ end of defun

;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Procedure     ; POLYLINE:DelPtLst                                               ;
;;;           Date          ; Nunukan, Apri 2019                                              ;
;;;           Project       ; -                                                               ;
;;;	      File          ; polygpselev.lsp                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; menghapus koordinat2 dari sumber ke tujuan                      ;
;;;                                                                                           ;
;;;           NOTES : listing baru                                                            ;
;;;********************************************************************************************
  (defun POLYLINE:DelPtLst (s d / ct tem)
    (setq ct 0)
    (repeat (length s)
      (setq tem	(subst 'nil (nth (nth ct s) d) d)
	    d	tem
	    ct	(1+ ct)
      ) ;_ end of setq
    ) ;_ end of repeat
    (vl-remove-if 'null tem)
  ) ;_ end of defun

;;;=== Begin of Global Variable ===============================================================
;;;********************************************************************************************
  (if *HGPSPOLY
    (if	(entget (handent *HGPSPOLY))	; cek entities yg terhapus
      (progn
;;;=== Action Proccesing ======================================================================
;;;********************************************************************************************
	(setq
	  TxtHeight
				  0.2
	  AsTxtlayer		  "astxt"
	  AsPolylayer		  "aspoly"
	  CatagorySelectionLst-01
				  (list (cons 8 AsTxtlayer))
	) ;_ end of setq

	(prompt "\nSelect Poin Number... ")
;;;	(PRINT:PrintLine
	  (setq	SrcPtnLst
		 (TEXT:ReadPointData
		   (SELECT-OBJ:SelectionNameObj
		     ""
		     CatagorySelectionLst-01
		   ) ;_ end of SELECT-OBJ:SelectionNameObj
		 ) ;_ end of TEXT:ReadPointData
	  ) ;_ end of setq
;;;	  "Data Text Point ..... :"
;;;	) ;_ end of PRINT:PrintLine

;;;	(PRINT:PrintLine
	  (setq DstPtnLst (POLYLINE:ReadDstPts))
;;;	  "Data As Point ..... :"
;;;	) ;_ end of PRINT:PrintLine

;;;	(PRINT:PrintLine
	  (setq DelPtnLst (POLYLINE:DelPtLst SrcPtnLst DstPtnLst))
;;;	  "Delet As Point ..... :"
;;;	) ;_ end of PRINT:PrintLine

	;;Delete & rebuild Polyline
	(entdel (handent *HGPSPOLY))
	(setq *HGPSPOLY (GP:ENTMAKE-3DPOLYLINE DelPtnLst AsPolylayer))
        ;;Delete & rebuild AS Text
	(foreach n (SELECT-OBJ:SelectionNameObj "X" (list (cons 8 AsTxtlayer))) (entdel n)) 
	(GP:CreateTxtNumber DelPtnLst TxtHeight AsTxtlayer)

	(prompt
	  (strcat
	    "\ntool set : polygpselev"
	    "\nEditor & Programmer : Pahor. M | tempuyax@yahoo.com.sg | Nunukan - April 2019"
	  ) ;_ end of strcat
	) ;_ end of prompt

;;;********************************************************************************************
;;;=== Action End ============================================================================
;;;********************************************************************************************
      ) ;_ end of progn MAIN

      (prompt
	(strcat
	  "\n***As GPS Terhapus***"
	  "\nRekonstruksi Ulang sperti di bawah ini!"
	  "\n1. Hapus dgn DelPolyGpsElev"
	  "\n2. Buat Kembali dgn PolyGpsElev"
	) ;_ end of strcat
      ) ;_ end of prompt
    ) ;_ end of if cek entities

;;;=== End of Global Variable =================================================================
    (prompt "\n***As GPS Belum di Dibuat***")
  ) ;_ end of if MAIN
  (princ)
) ;_ end of defun
;;;********************************************************************************************
;;;=== END OF =================================================================================
;;;********************************************************************************************


;;;********************************************************************************************
;;; Main       ; C:DelPolyGpsElev                                                             #
;;; Editor     : Pahor. M                                                                     #
;;; Tanggal    : nunukan, 06 April 2019                                                       #
;;; Koreksi    : -                                                                            #
;;; DIskripsi  : Menghapus variable & semua objek yg sudah dibuat "PolyGpsElev"               #
;;;********************************************************************************************
(defun C:DelPolyGpsElev	(/		 AsTxtlayer
			 AsPolylayer	 Star-AND
			 End-AND	 Star-OR
			 End-OR		 CatagorySelectionLst
			)

  (if *HGPSPOLY
    (progn
;;;=== Begin of Global Variable ===============================================================
      (setq
	AsTxtlayer
	 "astxt"
	AsPolylayer
	 "aspoly"

	Star-OR	(cons -4 "<OR")
	End-OR (cons -4 "OR>")

	Star-AND (cons -4 "<AND")
	End-AND	(cons -4 "AND>")

	CatagorySelectionLst
	 (list
	   Star-OR
	   Star-AND
	   (cons 0 "TEXT")
	   (cons 8 AsTxtlayer)
	   End-AND
	   Star-AND
	   (cons 0 "POLYLINE")
	   (cons 8 AsPolylayer)
	   End-AND
	   End-OR
	 ) ;_ end of list
      ) ;_ end of setq

      (foreach n
		 (SELECT-OBJ:SelectionNameObj "X" CatagorySelectionLst)
	(entdel n)
      ) ;_ end of foreach
      (set (read "*HGPSPOLY") nil)	;menghapus global variable

;;;********************************************************************************************
;;;=== Action End ============================================================================
;;;********************************************************************************************
      (prompt
	(strcat
	  "\ntool set : polygpselev"
	  "\nEditor & Programmer : Pahor. M | tempuyax@yahoo.com.sg | Nunukan - April 2019"
	) ;_ end of strcat
      ) ;_ end of prompt

;;;=== End of Global Variable ===============================================================
    ) ;_ end of progn
    (prompt "\n***As GPS Belum di Dibuat***")
  ) ;_ end of if
  (princ)
) ;_ end of defun

;;;********************************************************************************************
;;;=== END OF =================================================================================
;;;********************************************************************************************

;;(getvar 'LWDEFAULT)
;;(setvar 'LWDEFAULT 13)


;;;********************************************************************************************
;;;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg                          ;
;;;           Command       ; c:CrtLongSec                                                    ;
;;;           Date          ; Nunukan, Apri 2019                                              ;
;;;           Project       ; -                                                               ;
;;;	      File          ; polygpselev.lsp                                                 ;
;;;           Version       ; Betatester 02.04.190                                            ;
;;;	      Description   ; membuat objek long section                                      ;
;;;                                                                                           ;
;;;           NOTES : listing baru                                                            ;
(defun c:CrtLongSec ()

;;;	      Description   ; mengambil nilai '(x y z) Polyline dari (handent *HGPSPOLY)      ;
;;;********************************************************************************************
  (defun POLYLINE:ReadDstPts
	 (/ NextObjName DXF PointData polypointsLst)
    (setq NextObjName (entnext (handent *HGPSPOLY)))
    (while
      (/=
	(cdr
	  (assoc
	    0
	    (entget
	      NextObjName
	    ) ;_ end of entget
	  ) ;_ end of assoc
	) ;_ end of cdr
	"SEQEND"
      ) ;_ end of /=
       (setq
;;;============================================================================================
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 DXF
		       (entget
			 NextObjName
		       ) ;_ end of entget
;;;============================================================================================
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 PointData
		       (cdr (assoc
			      10
			      DXF
			    ) ;_ end of assoc
		       ) ;_ end of cdr
;;;============================================================================================
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 NextObjName
		       (entnext
			 NextObjName
		       ) ;_ end of entnext
;;;============================================================================================
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 polypointsLst
		       (append polypointsLst
			       (list PointData)
		       ) ;_ end of append
;;;============================================================================================
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       ) ;_ end of setq
    ) ;_ end of while
    polypointsLst
  ) ;_ end of defun

;;; Description   ; membuat object lwpolyline    ;
;;;********************************************************************************************
  (defun LWPoly	(pts lay)
    (Create-LWPolyline
      (list layer: (@ChkVar lay "0"))
      pts
    ) ;_ end of Create-3DPolyline
  ) ;_ end of defun

;;; Description   ; mengubah nilai '(x y z) ke '(jarak elevasi) dari (handent *HGPSPOLY)      ;
;;;********************************************************************************************
(defun ConvertLongSec (3dpts / Bpt res)

  (defun xyz->xy (pt Bpt)
    (if	Bpt
      (list (+ (cx pt) (cx Bpt)) (+ (cy pt) (cy Bpt)))
      (list (cx pt) (cy pt))
    ) ;_ end of if
  ) ;_ end of defun

  (defun xyz->z	(pt factor)
    (if	factor
      (* factor (caddr pt))
      (caddr pt)
    ) ;_ end of if
  ) ;_ end of defun


  ;;LongSec-vertex convertion
  ;;merubah (xyz) ke (jarak zenit)
  (defun LongSec-vertex	(vertices tem is2dpt)
    (if	(null (cdr vertices))
      (list)
      (cons
	(list
	  (if is2dpt
	    ;;Horizontal Distance (jarak datar gunakan 2dpt)
	    (setq tem (+
			(distance (xyz->xy (car vertices) nil)
				  (xyz->xy (cadr vertices) nil)
			) ;_ end of distance
			;;  2dpts
			tem
		      ) ;_ end of +
	    ) ;_ end of setq
	    ;;Slove Distance (jarak miring gunakan 3dpt)
	    (setq tem (+
			(distance (car vertices)
				  (cadr vertices)
			) ;_ end of distance
			;;  3dpts
			tem
		      ) ;_ end of +
	    ) ;_ end of setq
	  ) ;_ end of if
	  (xyz->z (cadr vertices) (/ 0.3048 4.0))
	  ;;koreksi gps 4m maka 0.3048m / 4.0m
	  ;;0.3048 m = 1 feet
	  ;; convert feet ke meter
	) ;_ end of list
	(LongSec-vertex (cdr vertices) tem is2dpt)
      ) ;_ end of cons
    ) ;_ end of if
  ) ;_ end of defun

  (defun dz->dRz (pt baseZenith)
    (list (cx pt) (- (cy pt) baseZenith))
  ) ;_ end of defun

  ;;Zenith convertion to Relatif Zenith
  (defun dzTodRz (vertices baseZenith)
    (if	(null vertices)
      (list)
      (cons
	;;calc to zenith
	(dz->dRz (car vertices) baseZenith)
	(dzTodRz (cdr vertices) baseZenith)
      ) ;_ end of cons
    ) ;_ end of if
  ) ;_ end of defun

  ;;LongSec-vertex convertion
  (defun xyWithBasept (vertices Bpt)
    (if	(null vertices)
      (list)
      (cons
	;;calc to base point
	(xyz->xy (car vertices) Bpt)
	(xyWithBasept (cdr vertices) Bpt)
      ) ;_ end of +
    ) ;_ end of if
  ) ;_ end of defun

  (if (setq Bpt (getpoint "Get BasePoint : "))
    (progn
      (setq
	res
	 (list (append res
		       (list 0.0 (xyz->z (car 3dpts) (/ 0.3048 4.0)))
	       ) ;_ end of append
	 ) ;_ end of list
      ) ;_ end of setq
      ;;setup awal titik relatif
      (setq res (append res (LongSec-vertex 3dpts 0 t)))
      ;;3dpt=nil 2dpt=t
      ;;(dist zenith)
      (setq res (dzTodRz res (cy (car res))))
      ;;result
      (setq res (xyWithBasept res Bpt));;result
    ) ;_ end of progn
  ) ;_ end of if
) ;_ end of defun
;;;********************************************************************************************
*****************************************************************************

;;;=== Begin of Global Variable ===============================================================
(if *HGPSPOLY
  (progn
;;;=== Begin of Global Variable ===============================================================

     (LWPoly (ConvertLongSec (POLYLINE:ReadDstPts)) nil)
;;;    (foreach n
;;;	       (ConvertLongSec (POLYLINE:ReadDstPts))
;;;      (print n)
;;;    ) ;_ end of foreach

;;;********************************************************************************************
;;;=== Action End ============================================================================
;;;********************************************************************************************
    (prompt
      (strcat
	"\ntool set : polygpselev"
	"\nEditor & Programmer : Pahor. M | tempuyax@yahoo.com.sg | Nunukan - September 2021"
      ) ;_ end of strcat
    ) ;_ end of prompt

;;;=== End of Global Variable ===============================================================
  ) ;_ end of progn
  (prompt "\n***As GPS Belum di Dibuat***")
) ;_ end of if
(princ)
) ;_ end of defun==================================================================================================================defun==================================================================================================================efun==================================================================================================================defun================================================================================================================================================================================================================================defun==================================================================================================================efun==================================================================================================================defun==================================================================================================================