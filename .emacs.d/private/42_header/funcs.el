;******************************************************************************;
;                                                                              ;
;               list.el for list                                               ;
;               Created on : Thu Oct 20 10:02:03 2011                          ;
;               Made by : David "Thor" GIRON <thor@epitech.net>                ;
;                                                                              ;
;******************************************************************************;



(defun list-map (f l)
  "Map function 'f' over list 'l' and returns the result list."
  (mapcar f l
   )
  )

(defun list-iter (f l)
  "Map procedure 'f' over list 'l' and returns nil."
  (mapc f l)
  nil
  )

(defun list-fold (f s l)
  "Returns f (... (f (f s e1) e2) ...) en."
  (reduce f l :initial-value s)
  )



;******************************************************************************;
;******************************************************************************;
;                                                                              ;
;               string.el for string                                           ;
;               Created on : Thu Oct 20 15:36:57 2011                          ;
;               Made by : David "Thor" GIRON <thor@epitech.net>                ;
;                                                                              ;
;******************************************************************************;




(defun string-reverse (s)
  "Returns the reversed string of 's'."
  (concat (reverse (string-to-list s)))
)

(defun string-length (s)
  "Returns the length of string 's'."
  (length s)
)

(defun string-fill (len)
  (make-string len ? )
)

(defun string-compare (s1 s2)
  "Compares 's1' and 's2'. Returns :
    - -1 if 's1' is shorter than 's2'
    - 0 if 's1' and 's2' are equal
    - 1 if 's1' is longer than 's2'"
  (let ((len1 (string-length s1))
	(len2 (string-length s2)))
    (cond
     ((= len1 len2) 0)
     ((<  len1 len2) -1)
     ((>  len1 len2) 1)
     )
    )
  )

(defun string-longest (s1 s2)
  "Returns the length of the longest string"
  (let ((res (string-compare s1 s2)))
    (cond
     ((= res 1 ) (string-length s1))
     ((= res 0 ) (string-length s1))
     ((= res -1) (string-length s2))
     )
    )
  )

(defun string-pick-longest (s1 s2)
  "Returns the longest string between 's1' and 's2', or 's1' otherwise."
  (let ((res (string-compare s1 s2)))
    (cond
     ((= res 1 ) s1)
     ((= res 0 ) s1)
     ((= res -1) s2)
     )
    )
  )

(defun string-longest-from-list (l)
  "Returns the length of longest string of the strings list 'l'."
  (list-fold
   (lambda (acc s) (if (> acc (string-length s)) acc (string-length s)))
   0
   l)
  )


(defun string-pick-longest-from-list (l)
  "Returns the longest string of the strings list 'l'."
  (list-fold
   (lambda (s1 s2) (string-pick-longest s1 s2))
   ""
   l)
  )



;******************************************************************************;
;******************************************************************************;
;                                                                              ;
;               comments.el for automatic comments generation                  ;
;               Created on : Fri Oct 21 17:36:51 2011                          ;
;               Made by : David "Thor" GIRON <thor@epitech.net>                ;
;                                                                              ;
;******************************************************************************;





(set 'line-std-width 80)



;******************************************************************************;
;                                                                              ;
;                          Comments tokens primitives                          ;
;                                                                              ;
;******************************************************************************;

(defun comments-start-token ()
  "Returns the comment start string of the current mode"
  comment-start
  )

(defun comments-end-token ()
  "Returns the comment end string of the current mode if any, or a
reversed string of the comment start otherwise."
  (if (/= (length comment-end) 0)
      comment-end
    (string-reverse comment-start)
    )
  )

(defun comments-start-token-length ()
  "Returns the length of the comment start string of the current mode."
  (string-length (comments-start-token))
)

(defun comments-end-token-length ()
  "Returns the length of the comment end string of the current mode."
  (string-length (comments-end-token))
)

(defun comments-tokens-length ()
  "Returns the total length of the comments tokens."
  (+ (comments-start-token-length) (comments-end-token-length))
)



;******************************************************************************;
;                                                                              ;
;                              Padding primitives                              ;
;                                                                              ;
;******************************************************************************;

(defun comments-compute-left-padding (s-text i-offset)
  "Returns a pair (lpad . rpad) for a left padded comments line."
  (let* ((i-len (+ (string-length s-text) i-offset))
	 (i-rpad (- line-std-width i-len (comments-tokens-length))))
    (cons 0 i-rpad)
    )
  )

(defun comments-compute-center-padding (s-text)
  "Returns a pair (lpad . rpad) for a center padded comments line."
  (let* ((i-len (string-length s-text))
	 (i-tpad (- line-std-width i-len (comments-tokens-length))))
    (if (= (% i-tpad 2) 0)
	(cons (/ i-tpad 2) (/ i-tpad 2))
      (cons (/ i-tpad 2) (+ (/ i-tpad 2) 1))
      )
    )
  )

(defun comments-compute-right-padding (s-text i-offset)
  "Returns a pair (lpad . rpad) for a right padded comments line."
  (let* ((i-len (+ (string-length s-text) i-offset))
	 (i-lpad (- line-std-width i-len (comments-tokens-length))))
    (cons i-lpad 0)
    )
  )



;******************************************************************************;
;                                                                              ;
;                              Contents producers                              ;
;                                                                              ;
;******************************************************************************;

(defun comments-make-padded-line (s-text i-lpad i-rpad)
  "Returns a comments string padded on line-std-width columns."
  (concat (comments-start-token)
	  (make-string i-lpad ? )
	  s-text
	  (make-string i-rpad ? )
	  (comments-end-token)
	  "\n"
	  )
  )

(defun comments-make-left-padded-line (s-text i-offset)
  "Returns a comments string left padded on line-std-width columns."  
  (let* ((pad (comments-compute-left-padding s-text i-offset)))
    (comments-make-padded-line
     (concat (make-string i-offset ? ) s-text)
     (car pad)
     (cdr pad))
    )
  )

(defun comments-make-center-padded-line (s-text)
  "Returns a comments string center padded on line-std-width columns."  
  (let* ((pad (comments-compute-center-padding s-text)))
    (comments-make-padded-line s-text (car pad) (cdr pad))
    )
  )

(defun comments-make-right-padded-line (s-text i-offset)
  "Returns a comments string right padded on line-std-width columns."  
  (let* ((pad (comments-compute-right-padding s-text i-offset)))
    (comments-make-padded-line
     (concat s-text (make-string i-offset ? ))
     (car pad)
     (cdr pad))
    )
  )

(defun comments-make-bar ()
  "Returns as a string a full comments bar of line-std-width."
  (concat (comments-start-token) 
	  (make-string (- line-std-width (comments-tokens-length)) ?*)
	  (comments-end-token) "\n")
  )

(defun comments-make-empty-line ()
  "Returns as a string an empty comments line of line-std-width."
  (concat (comments-start-token) 
	  (make-string (- line-std-width (comments-tokens-length)) ? )
	  (comments-end-token) "\n")
  )



;******************************************************************************;
;                                                                              ;
;                            Interactives functions                            ;
;                                                                              ;
;******************************************************************************;

(defun comments-insert-left-padded-line (s-text i-offset)
  "Inserts in the current buffer a comments string left padded on
   line-std-width columns."
  (interactive)
  (insert (comments-make-left-padded-line s-text i-offset))
  )

(defun comments-insert-center-padded-line (s-text)
  "Inserts in the current buffer a comments string center padded on
   line-std-width columns."
  (interactive)
  (insert (comments-make-center-padded-line s-text))
  )

(defun comments-insert-right-padded-line (s-text i-offset)
  "Inserts in the current buffer a comments string right padded on
   line-std-width columns."
  (interactive)
  (insert (comments-make-right-padded-line s-text i-offset))
  )

(defun comments-insert-bar ()
  "Inserts in the current buffer a line-std-width comments bar."
  (interactive)
  (insert (comments-make-bar))
  )

(defun comments-insert-empty-line ()
  "Inserts in the current buffer a line-std-width comments empty line."
  (interactive)
  (insert (comments-make-empty-line))
  )

(defun comments-insert-box (s-text)
  "Inserts a box of std-width with center padded 's-text'."
  (interactive "sBox content: ")
  (comments-insert-bar)
  (comments-insert-empty-line)
  (comments-insert-center-padded-line s-text)
  (comments-insert-empty-line)
  (comments-insert-bar)
  )

(defun comments-insert-small-box (s-text)
  "Inserts a small box of std-width with center padded 's-text'."
  (interactive "sBox content: ")
  (comments-insert-bar)
  (comments-insert-center-padded-line s-text)
  (comments-insert-bar)
  )



;******************************************************************************;
;*******************************************************************************;
;                                                                               ;
;                   42_header.el for 42 Emacs header                            ;
;                   Created on : Tue Jun 18 10:46:22 2013                       ;
;                   Made by : David "Thor" GIRON <thor@42.fr>                   ;
;                                                                               ;
;*******************************************************************************;






;******************************************************************************;
;                                                                              ;
;                                                         :::      ::::::::    ;
;    filename_____________________________.ext          :+:      :+:    :+:    ;
;                                                     +:+ +:+         +:+      ;
;    By: login____ <mail_______@student.42.fr>      +#+  +:+       +#+         ;
;                                                 +#+#+#+#+#+   +#+            ;
;    Created: yyyy/mm/dd 15:27:11 by login____         #+#    #+#              ;
;    Updated: 2018/01/12 17:15:55 by suvitiel         ###   ########.fr        ;
;                                                                              ;
;******************************************************************************;



(global-set-key (kbd "C-c h") 'header-insert)
(setq write-file-hooks (cons 'header-update write-file-hooks))


(set 'user-login "mrodrigu")




(set 'user-mail (let ((mail (getenv "MAIL")))
				   (if (string= mail nil)
					   "mrodrigu@student.42.fr"
					 mail)
				   )
	 )


(set 'left-std-margin 5)
(set 'right-std-margin 5)
(set 'info-std-width 41)


(set 'ft-1 "        :::      ::::::::")
(set 'ft-2 "      :+:      :+:    :+:")
(set 'ft-3 "    +:+ +:+         +:+  ")
(set 'ft-4 "  +#+  +:+       +#+     ")
(set 'ft-5 "+#+#+#+#+#+   +#+        ")
(set 'ft-6 "     #+#    #+#          ")
(set 'ft-7 "    ###   ########.fr    ")
(set 'ft-std-width 25)



;*******************************************************************************;


(defun header-chop-str (str n)
  (if (> (length str) n)
	  (let* ((max (- n 3))
	  		(new (substring str 0 max)))
	  	(concat new "..."))
	str)
  )

(defun header-make-left-margin ()
  "Creates the header comments start token and left margin"
  (let ((fill (string-fill (- left-std-margin (comments-start-token-length)))))
	(concat (comments-start-token) fill))
  )

(defun header-make-right-margin ()
  "Creates the header right margin and comments end token"
  (let ((fill (string-fill (- right-std-margin (comments-end-token-length)))))
	(concat fill (comments-end-token)))
  )

(defun header-make-central-gap (left-chunk right-chunk)
  "Creates the gap between the left infos block and the right logo"
  (string-fill (- line-std-width
				  (string-length left-chunk)
				  (string-length right-chunk)))
  )

(defun header-make-file-name ()
  "Creates the 'file.ext' entry of the header."
  (let* ((filename (header-chop-str (file-name-nondirectory (buffer-file-name))
									info-std-width))
		 (fill (string-fill (- info-std-width (string-length filename)))))
	(concat filename fill))
  )

(defun header-make-by ()
  "Creates the 'By: login <mail>' entry of the header."
  (let* ((mail-span (- info-std-width (+ (length user-login) 7)))
		 (by (concat "By: " user-login " <" (header-chop-str user-mail mail-span) ">"))
		 (fill (string-fill (- info-std-width (string-length by)))))
	(concat by fill))
  )

(defun header-make-creation-date ()
  "Creates the 'Created: yyyy/mm/dd hh:mm:ss' entry of the header.'"
  (concat "Created: " (format-time-string "%Y/%m/%d %T") " by " user-login)
  )

(defun header-make-update-date ()
  "Creates the 'Updated: yyyy/mm/dd hh:mm:ss' entry of the header.'"
  (concat "Updated: " (format-time-string "%Y/%m/%d %T") " by " user-login)
  )


;*******************************************************************************;



(defun header-insert-line-01 ()
  "Line 1 of the header"
  (comments-insert-bar)
  )

(defun header-insert-line-02 ()
  "Line 2 of the header"
  (comments-insert-empty-line)
  )

(defun header-insert-line-03 ()
  "Line 3 of the header"
  (let* ((left-margin (header-make-left-margin))
		 (right-margin (header-make-right-margin))
		 (central-gap (header-make-central-gap
					   left-margin
					   (concat ft-1 right-margin))))
	(insert (concat left-margin central-gap ft-1 right-margin))
	)
  )

(defun header-insert-line-04 ()
  "Line 4 of the header"
  (let* ((left-margin (header-make-left-margin))
		 (right-margin (header-make-right-margin))
		 (filename (header-make-file-name))
		 (central-gap (header-make-central-gap (concat left-margin filename)
											   (concat ft-2 right-margin))))
	(insert (concat left-margin filename central-gap ft-2 right-margin))
	)
  )

(defun header-insert-line-05 ()
  "Line 5 of the header"
  (let* ((left-margin (header-make-left-margin))
		 (right-margin (header-make-right-margin))
		 (central-gap (header-make-central-gap left-margin
											   (concat ft-3 right-margin))))
	(insert (concat left-margin central-gap ft-3 right-margin))
	)
  )

(defun header-insert-line-06 ()
  "Line 6 of the header"
  (let* ((left-margin (header-make-left-margin))
		 (right-margin (header-make-right-margin))
		 (by (header-make-by))
		 (central-gap (header-make-central-gap (concat left-margin by)
											   (concat ft-4 right-margin))))
	(insert (concat left-margin by central-gap ft-4 right-margin))
	)
  )

(defun header-insert-line-07 ()
  "Line 7 of the header"
  (let* ((left-margin (header-make-left-margin))
		 (right-margin (header-make-right-margin))
		 (central-gap (header-make-central-gap left-margin
											   (concat ft-5 right-margin))))
	(insert (concat left-margin central-gap ft-5 right-margin))
	)
  )

(defun header-insert-line-08 ()
  "Line 8 of the header"
  (let* ((left-margin (header-make-left-margin))
		 (right-margin (header-make-right-margin))
		 (created (header-make-creation-date))
		 (central-gap (header-make-central-gap (concat left-margin created)
											   (concat ft-6 right-margin))))
	(insert (concat left-margin created central-gap ft-6 right-margin))
	)
  )

(defun header-insert-line-09 ()
  "Line 9 of the header"
  (let* ((left-margin (header-make-left-margin))
		 (right-margin (header-make-right-margin))
		 (updated (header-make-update-date))
		 (central-gap (header-make-central-gap (concat left-margin updated)
											   (concat ft-7 right-margin))))
	(insert (concat left-margin updated central-gap ft-7 right-margin))
	)
  )

(defun header-insert-line-10 ()
  "Line 10 of the header"
  (comments-insert-empty-line)
  )

(defun header-insert-line-11 ()
  "Line 11 of the header"
  (comments-insert-bar)
  )



;*******************************************************************************;



(defun header-insert ()
  "Creates a header for the current source file."
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(header-insert-line-01)
	(header-insert-line-02)
	(header-insert-line-03) (newline)
	(header-insert-line-04) (newline)
	(header-insert-line-05) (newline)
	(header-insert-line-06) (newline)
	(header-insert-line-07) (newline)
	(header-insert-line-08) (newline)
	(header-insert-line-09) (newline)
	(header-insert-line-10)
	(header-insert-line-11)
	)
  )


(defun header-update ()
  "Updates the header for the current source file."
  (interactive)
  (save-excursion
    (if (buffer-modified-p)
        (progn
          (goto-char (point-min))
          (if (search-forward "Updated" nil t)
              (progn
                (delete-region
                 (progn (beginning-of-line) (point))
                 (progn (end-of-line) (point)))
				(header-insert-line-09)
                (message "Header up to date."))))))
  nil)



;******************************************************************************;
