(defun vert (/		 filterlist  vla-obj-list
	     lwlist	 2dlist	     ptlist	 vlist1
	     vlist2	 vlist3
	    )
  (vl-load-com)
  (setq	filterlist   (make-filter)
	vla-obj-list (get-objects filterlist)
	lwlist	     (nth 0 vla-obj-list)
	2dlist	     (nth 1 vla-obj-list)
	ptlist	     (nth 2 vla-obj-list)
	vlist1	     nil
	vlist2	     nil
	vlist3	     nil
  ) ;_ end-of setq
  (if lwlist
    (setq vlist1 (make-list lwlist 2))
  ) ;_ end of if
  (if 2dlist
    (setq vlist2 (make-list 2dlist 3))
  ) ;_ end of if
  (if ptlist
    (setq vlist3 (make-list ptlist 3))
  ) ;_ end of if
  (write-text vlist1 vlist2 vlist3)
  (princ)
) ;_ end of vert

(defun make-list (p-list n / i vlist obj coords ca j x y z xy)
  (setq	i (- 1)
	vlist nil
  ) ;_ end of setq
  (repeat (length p-list)
    (setq obj	 (nth (setq i (1+ i)) p-list)
	  coords (vlax-get-property obj "coordinates")
	  ca	 (vlax-variant-value coords)
	  j	 (- 1)
    ) ;_ end-of setq
    (repeat (/ (length (vlax-safearray->list ca)) n)
      (setq x (vlax-safearray-get-element ca (setq j (1+ j))))
      (setq y (vlax-safearray-get-element ca (setq j (1+ j))))
      (if (= n 2)
	(setq xy (list x y))
	(progn
	  (setq z (vlax-safearray-get-element ca (setq j (1+ j))))
	  (setq xy (list x y z))
	) ;_ end of progn
      ) ;_ end of if
      (setq vlist (append vlist (list xy)))
    ) ;_ end-of repeat
  ) ;_ end-of repeat
) ;_ end-of make-list

(defun make-filter (/ filter)
  (setq	filter '((-4 . "<OR")
		 (0 . "LWPOLYLINE")
		 (0 . "POLYLINE")
		 (-4 . "OR>")
		)
  ) ;_ end of setq
) ;_ end of make-filter

(defun get-objects (filter  /	    ss	    k	    lwp-list
		    2dp-list	    pt-list no-ent  obj	    pl
		    2d	    pt
		   )
  (setq no-ent 1)
  (while no-ent
    (setq ss	   (ssget ":S" filter)
	  k	   (- 1)
	  lwp-list nil
	  2dp-list nil
	  pt-list  nil
	  obj	   nil
	  pl	   "AcDbPolyline"
	  2d	   "AcDb2dPolyline"
	  pt	   "AcDbPoint"
    ) ;_ end-of setq
    (if	ss
      (progn
	(setq no-ent nil)
	(repeat	(sslength ss)
	  (setq	ent (ssname ss (setq k (1+ k)))
		obj (vlax-ename->vla-object ent)
	  ) ;_ end-of setq
	  (cond
	    ((= (vlax-get-property obj "ObjectName") pl)
	     (setq lwp-list (append lwp-list (list obj)))
	    )
	    ((= (vlax-get-property obj "ObjectName") 2d)
	     (setq 2dp-list (append 2dp-list (list obj)))
	    )
	    ((= (vlax-get-property obj "ObjectName") pt)
	     (setq pt-list (append pt-list (list obj)))
	    )
	  ) ;_ end-of cond
	) ;_ end-of repeat
      ) ;_ end-of progn
      (prompt "\nNo polylines or points selected, try again.")
    ) ;_ end-of if
  ) ;_ end-of while
  (list lwp-list 2dp-list pt-list)
) ;_ end-of get-objects

(defun setClipText(str / html result)
(if (= 'STR (type str))
  (progn
  (setq html   (vlax-create-object "htmlfile")
        result (vlax-invoke (vlax-get (vlax-get html 'ParentWindow) 'ClipBoardData) 'setData "Text" str)
  )
  (vlax-release-object html)
   str
   )
 );end if
)

(defun write-text (vl1 vl2 vl3)
  ;(setq	fn (getfiled "Text File 2018" "" "txt" 1)) 
  ;(setq f (close (open fn "w")))
  ;(setq msg "Points from 2d-Polylines")
  ;(do-points fn vl2 msg 3)
  ;(setq msg "\nPoints from Point entities")
  ;(do-points fn vl3 msg 3)
  ;(setq msg "\nPoints from LW-Polylines")
  (do-points fn vl1 msg 2)
  ;(princ)
) ;_ end of write-text

(defun do-points (fn vl msg n)
  (setq stri "")
  ;(setq f (open fn "a"))
  ;(write-line msg f)
  ;(write-line "  x,  y,  z" f)
  ;(write-line "" f)
  (foreach point vl
    (setq x (nth 0 point)
	  y (nth 1 point)
    ) ;_ end of setq
    (if	(= n 2)
      (setq str (strcat "x=" (rtos x) " y=" (rtos y)))
      (progn
	(setq z (nth 2 point))
	(setq str (strcat (rtos x) "," (rtos y) "," (rtos z)))
      ) ;_ end of progn
    ) ;_ end of if
    ;(write-line str f)
	(if (= stri "")
	  (setq stri (strcat stri str))
	  (setq stri (strcat stri "\n" str))
	)
  ) ;_ end of foreach
  (setClipText stri)
  (prompt "Coordinates copied to clipboard.")
  ;(setq f (close f))
  ;(princ)
) ;_ end of defun

(defun c:pts ()
  (princ "Point List Export Is Activated. Please Select A Polygon:")
  (princ)

  (vert)
  (princ)
) ;_ end-of defun
(princ)
(prompt "Point List Clipboard Export loaded by T.Hotchkiss/A.Samiee/Mss  - enter PTS to start ")
(princ)