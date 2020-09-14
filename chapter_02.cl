(defun mappend (fn the-list)
    "Apply fn to each element of list and append the results."
    (apply #'append (mapcar fn the-list)))

(defun random-elt (choices)
    "Choose an element from a list at random."
    (elt choices (random (length choices))))

(defun one-of (set)
    "Pick one element of set, and make a list of it."
    (list (random-elt set)))

(defparameter *simple-grammar*
    '((sentence -> (noun-phrase verb-phrase))
      (noun-phrase -> (Article Noun))
      (verb-phrase -> (Verb noun-phrase))
      (Article -> the a)
      (Noun -> man ball woman table)
      (Verb -> hit took saw liked))
    "A grammar for a trivial subset of English.")

(defparameter *bigger-grammar*
    '((sentence -> (noun-phrase verb-phrase))
      (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
      (verb-phrase -> (Verb noun-phrase PP*))
      (PP* -> () (PP PP*))
      (Adj* -> () (Adj Adj*))
      (PP -> (Prep noun-phrase))
      (Prep -> to in by with on)
      (Adj -> big little blue green adiabatic)
      (Article -> the a)
      (Name -> Pat Kim Lee Terry Robin)
      (Noun -> man ball woman table)
      (Verb -> hit took saw liked)
      (Pronoun -> he she it these those that)))


(defvar *grammar* *simple-grammar*)
(setf *grammar* *bigger-grammar*)

(defun rule-lhs (rule)
    "The left-hand side of a rule"
    (first rule))

(defun rule-rhs (rule)
    "The right-hand side of a rule"
    (rest (rest rule)))

(defun rewrites (category)
    "Return a list of the possible rewrites for this category."
    (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
    "Generate a random sentence or phrase"
    (let ((choices nil))
        (cond ((listp phrase)
               (mappend #'generate phrase))
              ((setf choices (rewrites phrase))
               (generate (random-elt choices)))
              (t (list phrase)))))

