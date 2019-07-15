;;;; cl-ygo.lisp -*- coding: utf-8-unix; -*-

(in-package #:cl-ygo)

(defparameter *deck-dir* "deck"
  "directory of your deck.")

(defparameter *cards-db* "cards.cdb"
  "sqlite3 database file")

(defparameter *cards-index* nil
  "Index of all cards")

(defparameter *zone-list* '(:deck      :hand  :extra 
			    :monster   :spell&trap 
			    :graveyard :banished 
			    :field     :pendulum))

(defparameter *card-lists* (apply #' append
				     (loop for zone in *zone-list* collect
					  `(,zone nil))))

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
   (functions   :accessor card-func)
   (flags       :accessor card-flags)
   (effects     :accessor card-effects)))

(defun empty-index ()
  (setq *cards-index* nil))

(defun empty-deck ()
  (loop for zone in *zone-list* do
       (setf (getf *card-lists* zone) nil)))

(defun decks (&rest zones)
  (let* ((zone-list (if (null zones)
			'(:deck) zones))
	 (card-list (loop for zone in zone-list collect
			 `(,zone ,(getf *card-lists* zone))))
	 (decks (apply #'append card-list)))
    decks))

(defun cards (&rest zones)
  (let* ((zone-list (if (null zones)
			'(:deck) zones))
	 (deck-list (apply #'decks zone-list))
	 (card-list (remove-if #'keywordp deck-list))
	 (cards (apply #'append card-list)))
    cards))

(defun runsql (sql)
  "For test"
  (with-connection
      (con :sqlite3
	   :database-name "D:/Programs/YGOPro/cards.cdb" )
    (let* ((query  (prepare con sql))
	   (result (execute query)))
      (fetch-all result))))

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

(defun parse-deck (name)
  (with-open-file (deck (get-dir-of *deck-dir* "/" name ".ydk"))      
    (let* ((raw-string (loop for line = (read-line deck nil nil) while line collect
			    (cond ((scan-to-strings "created by" line) "((")
				  ((or (eq (char line 0) #\#)
				       (eq (char line 0) #\!))
				   (setf (char line 0) #\:)
				   (concatenate 'string ")" line "("))
				  (t line))))
	   (deck-string (regex-replace-all
			 "" (apply #'concatenate 'string
				     (append raw-string '("))")))" "))
	   (deck-list (read-from-string deck-string nil t)))
      (remove nil deck-list))))

(defun fill-deck (id &optional (zone :deck))
  (let ((card-obj (get-card-by-id id)))
    (push card-obj *cards-index*)
    (push card-obj (getf *card-lists* zone))))

(defun init-deck (name)
  (empty-index)
  (empty-deck)
  (let ((deck-id-list (parse-deck name)))
    (loop for (zone id-list) on deck-id-list by #'cddr do
	 (loop for id in id-list do
	      (fill-deck id zone)))))

(defun search-cards-by-name (name &rest zones)
  (apply #'append
	 (loop for (zone cards) on (apply #'decks zones) by #'cddr collect
	      `(,zone ,(remove-if-not #'(lambda (card)
					  (scan-to-strings name
							   (card-name card)))
				      cards)))))

(defun search-cards-by-sequence (number &rest zones)
  (apply #'append
	 (loop for (zone cards) on (apply #'decks zones) by #'cddr collect
	      `(,zone ,(loop
			  for card in cards
			  for ctr from 1 to number collect
			  card)))))

(defun Fisher-Yates-Shuffle (zone)	;(random n) => [0, n)
  (let* ((len (length (cards zone)))
	 (new-cards-list
	  (loop for i from len downto 1 collect
	       (nth (random (1+ i)) (cards zone)))))
    (setf (getf *card-lists* zone) new-cards-list)))

(defun move-cards (cards-lists-with-zone dest-zone &optional (shuffle t))
  (pprint cards-lists-with-zone)
  (when shuffle
    (fisher-yates-shuffle dest-zone)))

