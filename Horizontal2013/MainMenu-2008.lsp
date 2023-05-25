  ;********************************************************************************************
  ;	      Programmer    ; Pahor. M ,E-mail tempuyax@yahoo.com.sg
  ;
  ;           Date          ; Nunukan, Agustus 2006
  ;
  ;           Project       ; Horizontal Alignment Lips
  ;
  ;	      File          ; menu.lsp
  ;
  ;           Version       ; Betatester 00.01.070
  ;
  ;	      Description   ; Main Menu, Sentral Pengendali & Mengarahkan Rutin-rutin fungsi
  ;                           program lips tertentu
  ;
  ;           NOTES :
  ;           Program ini dibuat hanya untuk kalangan sendiri. Tidak diperjual belikan dan tidak
  ;           dipublikasikan. Semua listing Program ditulis & dirancang sendiri tanpa melibatkan
  ;           pihak mana pun.
  ;
  ;           Awal Source Program Ditulis & diperbaiki secara kontinu, sekaligus uji coba selama dua
  ;           minggu penuh.
  ;
  ;           Sebagian Informasi & Semua Perhitungan menggunakan rumus-rumus yang diambil dari
  ;           beberapa referensi buku, antara lain ;
  ;                          - Kumpulan Rumus Teknik
  ;                          - Teknik Sipil  
  ;                          - Pedoman perencanaan & Giomentrik Jalan Raya
  ;                          - Lisp 2004 
  ;                          - Autocad 2004
  ;           Dilarang keras menjiplak atau merubah sedikit untuk memperbanyak karya ini tanpa
  ;           seijin kami, atas nama Penulis & Perancang Program Lisp ini.
  ;
  ;********************************************************************************************

(defun C:Horizontal (/ num)
  (if (/= (getvar 'ATTREQ) 1)
    (setvar "ATTREQ" 1)
  ) ;_ end of if
  (if (/= (getvar 'AUPREC) 4)
    (setvar "AUPREC" 4)
  ) ;_ end of if
  (if (/= (getvar 'DIMZIN) 1)
    (setvar "DIMZIN" 1)
  ) ;_ end of if
  (setq SF 1)

  (initget 1 "C S P")
  (setq	num
	 (strcase
	   (getstring
	     "\nAlingment Horizontal Selected (full-(C)ircle/(S)pi-circle-spi/s(P)i-spi) : "
	   ) ;_ end of getstring
	 ) ;_ end of strcase
  ) ;_ end of setq
  (cond
    ((eq num "C")
     (MAIN:MainFC SF)
    )
    ((eq num "S")
     (MAIN:MainSCS SF)
    )
    ((eq num "P")
     (MAIN:MainSS SF)
    )
    (T num)
  ) ;_ end of cond
  (prompt
    (strcat
      "\nSource Date (01): Nunukan - Agustus 2006"
      "\nSource Modified (02): Nunukan - Mei 2008"
      "\nSource Modified (03): Nunukan - Nopember 2013"
      "\nEditor & Programmer : Pahor. M | tempuyax@yahoo.com.sg | Nunukan - Nopember 2015"
    ) ;_ end of strcat
  ) ;_ end of prompt
  (princ)
) ;_ end of defun
					;==============================================
