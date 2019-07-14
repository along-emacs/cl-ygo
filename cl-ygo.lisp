;;;; cl-ygo.lisp -*- coding: utf-8-unix; -*-

(in-package #:cl-ygo)

(defparameter *deck-dir* "deck"
  "directory of your deck.")

(defparameter *cards-db* "cards.cdb"
  "sqlite3 database file")

(defparameter *cards-cache* nil
  "cache of all cards")

(defparameter *card-lists* '(:deck ()      :hand () :extra ()
			     :monster ()   :spell&trap ()
			     :graveyard () :banished ()
			     :field ()     :pendulum ())
  "Structure:
'(:deck (card1 card2...)
  :hand (card3 card4...))
")

(defclass player ()
  ((name :accessor player-name
	 :initarg :name)
   (lp :accessor player-lp
       :initarg :lp
       :initform 8000)))

(defclass card ()
  ((id          :accessor card-id
		:initarg  :id)
   (category    :accessor card-cate)
   (name        :accessor card-name
		:initarg  :name)
   (attribute   :accessor card-attr)
   (rank        :accessor card-rank)
   (type        :accessor card-type)
   (description :accessor card-desc
		:initarg  :desc)
   (functions   :accessor card-func)))

(defun runsql (sql)
  "For test"
  (with-connection
      (con :sqlite3
	   :database-name "D:/Programs/YGOPro/cards.cdb" )
    (let* ((query  (prepare con sql))
	   (result (execute query)))
      (fetch-all result))))

(defmethod print-object ((cd card) stream)
  "Pretty print for instance of card"
  (format stream "#<~A> "
	  ;; (card-id cd)
	  (card-name cd)))

(defun get-dir-of (&rest paths)
  (apply #'concatenate 'string
	 (namestring *default-pathname-defaults*)
	 paths))

(defun get-card-by-id (id)
  (let* ((sql (concatenate 'string "select
texts.id, texts.name, texts.desc,
datas.atk, datas.def, datas.level
from texts
join datas on datas.id = texts.id
where texts.id = " (write-to-string id) ";"))
	 (card-info (car (runsql sql))))
    (make-instance 'card
		   :id   (getf card-info :|id|)
		   :name (getf card-info :|name|)
		   :desc (getf card-info :|desc|))))

(defun empty-cache ()
  (setq *cards-cache* nil))

(defun empty-deck ()
  (loop for (zone list) on *card-lists* by #'cddr do
       (setf (getf *card-lists* zone) nil)))

(defun parse-deck (name)
"Format of ydk file:
#created by ...
#main
card id 1
card id 2
#extra
card id 3
card id 4
!side
card id 5
"
  (let ((deck-list nil)
	(deck-path (get-dir-of *deck-dir* "/" name ".ydk"))
	(card-id 0) (main-p nil) (extra-p nil) (side-p nil))
    (with-open-file (deck deck-path :direction :input)
      (loop for line = (read-line deck nil nil) while line do
	   (cond ((scan-to-strings "^#main"  line) (setq main-p  t card-id 0))
		 ((scan-to-strings "^#extra" line) (setq extra-p t card-id 0))
		 ((scan-to-strings "^!side"  line) (setq side-p  t card-id 0))
		 ((scan-to-strings "^[0-9]+" line) (setq card-id (parse-integer line
										:junk-allowed t)))
		 (t                                (setq card-id 0)))
	   (when (not (zerop card-id))
	     (push card-id
		   (getf deck-list
			 (cond ((equal `(,main-p ,extra-p ,side-p) '(t  nil nil)) :deck)
			       ((equal `(,main-p ,extra-p ,side-p) '(t   t  nil)) :extra)
			       ((equal `(,main-p ,extra-p ,side-p) '(t   t   t )) :side)
			       (t                                                 :side)))))))
    deck-list))

(defun init-deck (name)
  (empty-cache)
  (empty-deck)
  (let ((deck-list (parse-deck name)))
    (map nil
	 #'(lambda (deck-name)
	     (let ((card-id-list (getf deck-list deck-name)))
	       (loop for card-id in card-id-list do
		    (let ((card-obj (get-card-by-id card-id)))
		      (push card-obj *cards-cache*)
		      (push card-obj (getf *card-lists* deck-name))))))
	 '(:deck :extra))))

(defun get-cards-from (&rest zones)
  "Get cards from specific zones.
Default is the main deck."
  (let* ((zone-list (if (null zones)
			'(:deck)
			zones))
	 (cards-list (loop for zone in zone-list collect
			  (getf *card-lists* zone))))
    (apply #'append cards-list)))

    
(defun search-cards-by-name (name &rest zones)
  "Search card by name. Default location is the main deck."
  (let* ((cards (apply #'get-cards-from zones))
	 (result
	  (loop for card in cards collect
	       (when (scan-to-strings
		      name (card-name card))
		 card))))
    (remove nil result)))
