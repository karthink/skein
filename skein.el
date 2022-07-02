;; -*- lexical-binding: t; -*-
(require 'seq)
(require 'map)

;; pick - pick a nested element from the collection
;; each - pick a nested element from each member of the collection
;;        apply a function to each member of the collection and return the result
;; only - filter the collection on the existence of keys, or
;;        filter the collection by a predicate
;; sort - sort the collection by a predicate
;; into - convert the collection to a list or vector
;; conc - concatenate the collection's elements to a list or vector
;; uniq - retain only unique members of the collection
;; keys - return the keys of the collection
;; vals - return the values of the collection
;; fold - reduce the collection with a function
;; merge - merge the collection with others

;; Signatures

;; (pick :key1 :key2 :key3)
;; (each :key1 :key2 :key3)
;; (each MAP-FN)        - MAP-FN takes one variable, a collection element
;; (only :key1 :key2 :key3)
;; (only PREDICATE)     - PREDICATE takes one variable, a collection element
;; (sort PREDICATE)     - PREDICATE takes two variables, collection elements
;; (into TYPE)          - TYPE is list or vector
;; (conc TYPE)          - TYPE is list or vector
;; (uniq)
;; (keys)
;; (vals)
;; (fold FOLD-FN INIT)  - FOLD-FN takes two variables, collection elements.
;;                        INIT is the initial value
;; (merge COLL)         - COLl is a collection to be merged
;; (merge MERGE-FN)     - MERGE-FUN takes one variable, the collection, and
;;                        returns the collection to be merged

(defmacro skein (plst &rest forms)
  (declare (indent defun))
  `(cl-letf*
       (;; (pick (make-symbol "pick"))
        ;; (sort (make-symbol "sort"))
        ;; (each  (make-symbol "each"))
        ;; (uniq  (make-symbol "uniq"))
        ;; (vals  (make-symbol "vals"))
        ;; (keys  (make-symbol "keys"))
        ;; (conc  (make-symbol "conc"))
        ;; (fold  (make-symbol "fold"))
        ;; (merge (make-symbol "merge"))
        ;; (only  (make-symbol "only"))
        ;; (into  (make-symbol "into"))

        ((symbol-function '~pick)
         (lambda (map &rest keys)
           (seq-reduce (lambda (result next)
                         (cond
                          ((functionp next) (funcall next result))
                          (;; (or
                           ;;  (numberp next)
                           ;;  (keywordp next)
                           ;;  (stringp )
                           ;;  )
                           t
                           (map-elt result next))
                          ;; (t result)
                          ))
            keys
            map)))

        ((symbol-function '~sort)
         (lambda (map cmp &rest accessors)
           (if accessors
               (seq-sort
                (lambda (a b)
                  (funcall cmp
                   (apply #'~pick a accessors)
                   (apply #'~pick b accessors)))
                map)
             (seq-sort cmp map))))

        ((symbol-function '~each)
         (lambda (map func &rest accessors)
           (if (or accessors (not (functionp func)))
               (seq-map (lambda (el) (apply #'~pick el func accessors)) map)
             (if (= (car (func-arity func)) 1)
                 (seq-map func map)
               (map-apply func map)))))

        ((symbol-function '~uniq) #'seq-uniq)
        ((symbol-function '~vals) #'map-values)
        ((symbol-function '~keys) #'map-keys)

        ((symbol-function '~conc)
         (lambda (map &optional type)
           (apply #'seq-concatenate
            (or type 'list)
            (~into map 'list))))

        ((symbol-function '~fold)
         (lambda (map func init)
           (seq-reduce func map init)))

        ((symbol-function '~merge)
         (lambda (map &rest maps)
           (apply #'map-merge 'plist map maps)))

        ((symbol-function '~only)
         (lambda (map func &rest accessors)
           (if (or accessors (not (functionp func)))
               (seq-filter (lambda (el) (apply #'~pick map func accessors))
                map)
             (seq-filter func map))))

        ((symbol-function '~into)
         (lambda (map &optional type)
           (pcase type
            ('vector (seq-into map 'vector))
            ('plist (map-into map 'plist))
            ;; ('list (seq-into map 'list))
            ('alist (map-into map 'alist))
            ('hash-table (map-into map '(hash-table :test eql)))
            (_ (seq-into map 'list)))))

        ((symbol-function '~freq)
         (lambda (map &optional cmp)
           (let ((freqs) (cmp (or cmp 'equal)))
            (seq-do (lambda (el) (incf (alist-get el freqs 0 nil cmp))) map)
            freqs)))

        ((symbol-function '~freqs) #'~freq))

     ,(cl-loop for form in forms with calls do
       (cond
        ((atom form) (push form calls))
        ((memq (car form)
          '(~pick ~only ~each
            ~conc ~sort ~conc
            ~vals ~key ~into
            ~freq ~freqs
            ~merge ~uniq ~fold))
         (push
          `(funcall (lambda (m) (funcall ',(car form) m ,@(cdr form))))
          calls))
        (t (push form calls)))
       finally return
       `(thread-last ,plst
         ,@(reverse calls)))))

(defalias '~>> 'skein)

;; (ps-acc dat2
;;   (~pick :data :user :repositories :nodes)
;;   ;; (~only :stargazerCount)
;;   (~each (lambda (el) `(:langs ,(~vals (~conc (~pick el :languages :nodes) 'list)))))
;;   ;; (~conc)
;;   ;; (~sort #'string< :name)
;;   ;; (~conc)
;;   ;; (~vals)
;;   ;; (seq-map #'length)
;;   )

(provide 'skein)
