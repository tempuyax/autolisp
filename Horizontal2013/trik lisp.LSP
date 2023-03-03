(setq cen (mapcar 'list '(10 10)
		  (list 5 5 ) ;_ end of list
	  ) ;_ end of mapcar
) ;_ end of setq
(setq lst (mapcar
	    '(lambda (a) (cons (mapcar 'apply a cen) '(42 . 0.0)))
	    '((- -) (+ -) (+ +) (- +))
	  ) ;_ end of mapcar
) ;_ end of setq
(apply 'append
       (mapcar
	 '(lambda (x)
	    (list (cons 10 '(10 0)) (cdr x))
	  ) ;_ end of lambda
	 lst
       ) ;_ end of mapcar
) ;_ end of apply

((10 5) (10 5)) 
(((5 5) 42 . 0.0) ((15 5) 42 . 0.0) ((15 15) 42 . 0.0) ((5 15) 42 . 0.0)) 
((10 10 0) (42 . 0.0) (10 10 0) (42 . 0.0) (10 10 0) (42 . 0.0) (10 10 0) (42 . 0.0)) 

Vport dxflist
(-1 . <Entity name: 7ed3f508>)
(0 . "VIEWPORT")
(330 . <Entity name: 7ed3f448>)
(5 . "8889")
(100 . "AcDbEntity")
(67 . 1)
(410 . "hal")
(8 . "0")
(390 . <Entity name: 7effb470>)
(100 . "AcDbViewport")
(10 184.64 211.616 0.0)
(40 . 320.0) ;; Lebar  x
(41 . 142.0) ;; tinggi y
(68 . 6)
(69 . 7)
(12 0.0 0.0 0.0)
(13 0.0 0.0 0.0)
(14 0.5 0.5 0.0)
(15 0.5 0.5 0.0)
(16 0.0 0.0 1.0)
(17 356131.0 431628.0 480.358)
(42 . 50.0)
(43 . -480.358)
(44 . -480.358)
(45 . 142.0)
(50 . 0.0)
(51 . 0.339082)
(72 . 1000)
(90 . 32864)
(281 . 0)
(71 . 1)
(74 . 0)
(110 0.0 0.0 0.0)
(111 1.0 0.0 0.0)
(112 0.0 1.0 0.0)
(79 . 0)
(146 . 0.0)
(170 . 0)
