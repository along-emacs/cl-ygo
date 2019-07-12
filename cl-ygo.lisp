;;;; cl-ygo.lisp -*- coding: utf-8-unix; -*-

(in-package #:cl-ygo)

(defparameter *deck-dir* "deck"
  "directory of your deck.")

(defparameter *cards-db* "cards.cdb"
  "sqlite3 database file")

;; '(:deck (card1 card2...)
;;   :hand (card3 card4...))
(defparameter *card-lists* '(:deck ()      :hand () :extra ()
			     :monster ()   :spell&trap ()
			     :graveyard () :banished ()
			     :field ()     :pendulum ()))

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

;; for test
(defun runsql (sql)
  (dbi:with-connection
      (con :sqlite3
	   :database-name "D:/Programs/YGOPro/cards.cdb" )
    (let* ((query  (dbi:prepare con sql))
	   (result (dbi:execute query)))
      (dbi:fetch-all result))))

;; pretty print for instance of card
(defmethod print-object ((cd card) stream)
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

(defun empty-decks ()
  (loop for (zone list) on *card-lists* by #'cddr do
       (setf (getf *card-lists* zone) nil)))

(defun init-deck (name)
  (let ((deck-path (get-dir-of *deck-dir* "/" name ".ydk"))
	(in-main t))
    (empty-decks)
    (with-open-file (deck deck-path :direction :input)
      (do ((l (read-line deck)
    	      (read-line deck nil 'eof)))
    	  ((or (eq l 'eof)
	       (string= l "!side")) "Inited")
	(let ((id (parse-integer l :junk-allowed t)))
	  (when (cl-ppcre:scan-to-strings "side" l)
	      (setq in-main nil))
	  (when id
	    (push (get-card-by-id id)
		  (getf *card-lists* (if in-main :deck :extra)))))))))

(defun get-cards-from (&rest zones)
  (apply #'append
	 (loop for zone in zones collect
	      (if (keywordp zone)
		  (getf *card-lists* zone)
		  zone))))
    
(defun search-cards-by-name (name &rest zones)
  (let ((result
	 (mapcar #'(lambda (zone)
		     (mapcar #'(lambda (card)
				 (when (cl-ppcre:scan-to-strings
					name (card-name card))
				   card))
			     (getf *card-lists* zone)))
		 zones)))
    (remove nil (car result))))

