(defun c:use (/ mdir dir fnl)
  (setq
    mdir "D:/DATA-pahor/Lisp Excel"
    dir	 "TOOL GAMBAR"
    fnl	 (list
	   "LWPOLYLINE ELEV.LSP"
	   "POLYLINE ELEV.LSP"
	   "POLYLINEGPS ELEV.LSP"
	   "selection.LSP"
	   "object2016.LSP"
	   "Matrix2D.lsp"
	   "Until.lsp"
	 ) ;_ end of list
  ) ;_ end of setq
  (foreach fn fnl
    (load
      (strcat
	mdir "/" dir "/" fn) 
    ) ;_ end of load
  ) ;_ end of foreach
) ;_ end of defun
