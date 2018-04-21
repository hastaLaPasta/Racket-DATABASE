#lang racket

(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

;= Funcțiile de acces
(define init-database
  (λ ()
    '()))

(define create-table
  (λ (table columns-name)
    (reverse (cons columns-name (cons table (init-database))))))

(define get-name
  (λ (table)
    (car table)))

(define get-columns
  (λ (table)
    (car (cdr table))))

(define get-tables
  (λ (db)
    db))

(define get-table
  (λ (db table-name)
    (car (filter
          (λ (lst) (if (equal? (car lst) table-name) #t #f)) db))))

(define add-table
  (λ (db table)
    (cons table db)))

(define remove-table
  (λ (db table-name)
    (filter (λ (lst) (if (equal? (car lst) table-name) #f #t)) db)))


;============================================================================================
;=                         Table Students                                                   =
;= +----------------+-----------+------------+-------+--------+                                  =
;= |Registration nr | Last name | First name | Group | Grades |                                  =
;= +----------------+-----------+------------+-------+--------+                                  =
;= |            123 | Ionescu   | Gigel      | 321CA |  9.82  |                                  =
;= |            124 | Popescu   | Maria      | 321CB |  9.91  |                                  =
;= |            125 | Popa      | Ionel      | 321CC |  9.99  |                                  =
;= |            126 | Georgescu | Ioana      | 321CD |  9.87  |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;=                                                                                           =
;=                                         Table Courses                                    =
;= +------+----------+-------------------------------------+---------------+------------+      =
;= | Year | Semester |            Discipline               | Nr of credits | Homeworks  |      =
;= +------+----------+-------------------------------------+---------------+------------+      =
;= | I    | I        | Computer programming                |             5 |          2 |      =
;= | II   | II       | Programming paradigms               |             6 |          3 |      =
;= | III  | I        | Parallel and distributed algorithms |             5 |          3 |      =
;= | IV   | I        | Artificial intelligence             |             6 |          3 |      =
;= | I    | II       | Data structures                     |             5 |          3 |      =
;= | III  | II       | Data bases                          |             5 |          0 |      =
;= +------+----------+-------------------------------------+---------------+------------+      =
;============================================================================================
(define db '(("Students" ("Registration number" "Last name" "First name" "Group" "Grades")
                         (123 "Ionescu" "Gigel" "321CA" 9.82)
                         (124 "Popescu" "Maria" "321CB" 9.91)
                         (125 "Popa" "Ionel" "321CC" 9.99)
                         (126 "Georgescu" "Ioana" "321CD" 9.87))
             ("Courses" ("Year" "Semester" "Discipline" "Number of credits" "Homeworks")
                        ("I" "I" "Programarea calculatoarelor" 5 2)
                        ("II" "II" "Paradigme de programare" 6 3)
                        ("III" "I" "Algoritmi paraleli și distribuiți" 5 3)
                        ("IV" "I" "Inteligență artificială" 6 3)
                        ("I" "II" "Structuri de date" 5 3)
                        ("III" "II" "Baze de date" 5 0))))



;Make a list of indexes to find the positions you need
;extracted data.
; table-columns = list column names in the table
; columns = columns to be selected
(define get-all-index
  (λ (table-columns columns)
    (if (null? columns) '()
        (append (list (index-of table-columns (car columns)))
                (get-all-index table-columns (cdr columns))))))


;  Extracts data from the table based on an index list.
; table = table to extract from
; index = list of indexes (where should we extract)
(define get-data-with-index
  (λ (tabel index)
    (if (null? index) '()
        (cons
         (map (λ(lst) (list-ref lst (car index))) tabel)
         (get-data-with-index tabel (cdr index))
         )
        )
    )
  )


;  Returns a list to be inserted into the table
; entry = inital list
; index = list of indexes where we will insert
; lst = list of values which will be added
(define compute-entry
  (λ (entry index lst)
    (if (null? index) entry
        (let loop ([first-lst (take entry (car index))]
                   [second-lst (drop entry (car index))])
          (compute-entry
             (append (append first-lst (list (car lst))) (cdr second-lst))
             (cdr index)
             (cdr lst)
          )
       )
    )
  )
)

;  Inserts the new entry table, deletes the table from the base
;data and reintroduce the new one.

(define insert
  (λ (db table-name record)
    (letrec ([columns-to-add (map car record)]
             [index (get-all-index (get-columns (get-table db table-name)) columns-to-add)]
             [table (get-table db table-name)]
             [new-entry (make-list (length (get-columns table)) NULL)])
      
      (remove-table db table-name)
      (add-table db (append table (list (compute-entry new-entry index (map cdr record)))))
      )
    )
  )


; Select data using the implemented get-data-with-index
; above. If all selections are null, returns the empty list.
(define simple-select
  (λ (db table-name columns)
    (if (null? (get-table db table-name)) NULL
        (filter
           (λ(elem)
             (if (not (null? elem)) #t #f)
           )
           (get-data-with-index
             (cdr (cdr (get-table db table-name)))
             (get-all-index (get-columns (get-table db table-name)) columns)
           )
        )
    )
  )
)

;  Returns a table with the entries that they complete
; conditions.
; db = data base
; table-name = tables name
; conditions = list of conditions

(define filtered-table
  (λ (db table-name conditions)
    (let loop ([index (get-all-index
                       (get-columns (get-table db table-name))
                       (map (λ(elem) (car (cdr elem))) conditions)
                      )]
               [aux (cdr (get-table db table-name))]
               [first-cond conditions]
              )
      (if (or (null? first-cond) (null? index)) aux
         (loop
            (cdr index)
            (append
               (list (car aux))
               (filter (λ(elem)
                          (if ((car (car first-cond))
                                 (list-ref elem (car index))
                                 (last (car first-cond))
                              ) #t  #f
                          )
                       )
                       (cdr aux)
               )
            )
            (cdr conditions)
         )
      )
    )
  )
)

; Count all items that appear once
; in a list.
(define unique-count
  (λ (lst)
    (cond [(equal? (length lst) 1) 1]
          [(equal? (member (car lst) (cdr lst)) #f) (+ 1 (unique-count (cdr lst)))]
          [else (unique-count (cdr lst))])))

; Apply all the operations in the list
; columns and returns the resulting list.
; columns = list of columns and operations
(define compute-table
  (λ (table columns)
    (if (null? columns) '()
        (cond [(not (pair? (car columns)))
                 (append
                    (get-data-with-index
                       (cdr table)
                       (get-all-index (car table) (list (car columns)))
                    )
                    (compute-table table (cdr columns))
                  )]

              [(and (pair? (car columns)) (equal? (car (car columns)) 'min))
               (append
                  (list (apply min
                            (car (get-data-with-index
                                    (cdr table)
                                    (get-all-index
                                       (car table)
                                       (list (cdr (car columns)))
                                     )
                                  )
                            )
                        )
                  )
                  (compute-table table (cdr columns))
               )]
              
              [(and (pair? (car columns)) (equal? (car (car columns)) 'max))
               (append
                  (list
                     (apply
                        max
                        (car
                           (get-data-with-index
                              (cdr table)
                              (get-all-index (car table) (list (cdr (car columns))))
                           )
                        )
                     )
                  )
                  (compute-table table (cdr columns))
               )]
              
              [(and (pair? (car columns)) (equal? (car (car columns)) 'count))
               (append
                  (list
                     (unique-count
                        (car
                           (get-data-with-index
                              (cdr table)
                              (get-all-index (car table) (list (cdr (car columns))))
                           )
                        )
                     )
                  )
                  (compute-table table (cdr columns))
               )]
              
              [(and (pair? (car columns)) (equal? (car (car columns)) 'avg))
               (append
                  (list
                     (/
                       (foldr + 0
                          (car
                             (get-data-with-index
                                (cdr table)
                                (get-all-index
                                   (car table)
                                   (list (cdr (car columns)))
                                )
                             )
                          )
                       )
                       (length
                          (car
                             (get-data-with-index
                                (cdr table)
                                (get-all-index
                                   (car table)
                                   (list (cdr (car columns)))
                                )
                             )
                          )
                       )
                     )
                  )
                  (compute-table table (cdr columns))
               )]
              
              [(and (pair? (car columns)) (equal? (car (car columns)) 'sum))
               (append
                  (list
                     (foldr + 0
                            (car
                              (get-data-with-index
                                 (cdr table)
                                 (get-all-index
                                    (car table)
                                    (list (cdr (car columns)))
                                 )
                              )
                            )
                     )
                  )
                  (compute-table table (cdr columns))
               )]
              
              [(and (pair? (car columns)) (equal? (car (car columns)) 'sort-asc))
               (append
                  (list
                     (sort
                        (car
                           (get-data-with-index
                              (cdr table)
                              (get-all-index
                                 (car table)
                                 (list (cdr (car columns)))
                              )
                           )
                        )
                        <
                     )
                  )
                  (compute-table table (cdr columns))
               )]
              
              [(and (pair? (car columns)) (equal? (car (car columns)) 'sort-desc))
               (append
                  (list
                     (sort
                        (car
                           (get-data-with-index
                              (cdr table)
                              (get-all-index
                                 (car table)
                                 (list (cdr (car columns)))
                              )
                           )
                        )
                        >
                     )
                  )
                  (compute-table table (cdr columns))
               )]
              
        )
    )
  )
)


(define select
  (λ (db table-name columns conditions)
    (compute-table (filtered-table db table-name conditions) columns)))


;Select from the table all entries that meet the conditions
;and input values ​​in the list of values. Returns a new one
;table.
(define update-filtered-table
  (λ (db table-name values condition)
    (let loop ([table (filtered-table db table-name condition)]
               [valaux values])
      (if (or
             (null? valaux)
             (equal?
                (car (get-all-index
                        (car (cdr (get-table db table-name)))
                        (list (car (car valaux)))
                      )
                )
                #f
             )
          )
          table
          (loop
             (append
                (list (car (cdr (get-table db table-name))))
                (map (λ (elem)
                        (append
                           (append
                              (take
                                 elem
                                 (car (get-all-index
                                         (car (cdr (get-table db table-name)))
                                         (list (car (car valaux)))
                                      )
                                 )
                              )
                              (list(cdr (car valaux))))
                           (cdr (drop elem (car (get-all-index
                                                (car (cdr (get-table db table-name)))
                                                (list (car (car valaux)))
                                                )))
                           )
                        )
                     )
                     (cdr table)
                )
             )
            (cdr valaux)
         )
      )
    )
  )
)


; Lists the indices to be inserted
; in the table
(define index-to-insert
  (λ (db table-name condition)
    (let loop ([filtered-table (filtered-table db table-name condition)]
               [initial-table (cdr (get-table db table-name))]
               [return '()])
      (if (null? filtered-table) return
          (loop
             (cdr filtered-table)
             initial-table
             (append
                return
                  (list
                     (index-of
                        initial-table
                        (car filtered-table)
                     )
                  )
             )
          )
      )
    )
  )
)

(define update
  (λ (db table-name values conditions)
    (let loop ([initial-table (cdr (get-table db table-name))]
               [replace-table (update-filtered-table db table-name values conditions)]
               [index-lst (index-to-insert db table-name conditions)])
      
      (if (or (null? replace-table) (null? index-lst) (equal? (car index-lst) #f))
          (add-table
             (remove-table db table-name)
             (append (list (car (get-table db table-name))) initial-table)
          )
          (loop
             (append
                (append (take initial-table (car index-lst)) (list (car replace-table)))
                (cdr (drop initial-table (car index-lst)))
             )
             (cdr replace-table)
             (cdr index-lst)
          )
      )
    )
  )
)


(define delete
  (λ (db table-name conditions)
    (let loop ([initial-table (cdr (get-table db table-name))]
               [index-lst (index-to-insert db table-name conditions)])
      (cond [(null? conditions) (add-table
                                   (remove-table db table-name)
                                   (take initial-table 2)
                                )]

            [(or (null? index-lst) (equal? (car index-lst) #f))
             (add-table
                (remove-table db table-name)
                (append (list (car (get-table db table-name)))
                   (append
                      (list (car (cdr (get-table db table-name))))
                      (filter
                         (λ(elem) (if (not (equal? elem NULL)) #t #f))
                         initial-table
                      )
                   )
                )
             )]
            
            [else
              (loop
                 (append
                    (append
                       (take initial-table (car index-lst))
                       (list NULL)
                    )
                    (cdr (drop initial-table (car index-lst)))
                 )
                 (cdr index-lst)
              )]
      )
    )
  )
)