(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
       collect line)))

(defun find-passports (lines)
  (loop
     for line in lines
     for n from 0

     when (= 0 n)
     collect n

     when (string= "" line)
     collect (1+ n)
  ))

(defun extract-passport (lines)
  (format nil "~{~A~^ ~}" 
          (loop
             for line in lines
             while (string/= "" line)
             collect line
               )))

(defun split-passports (lines)
  (loop
     for n in (find-passports lines)
     collect (extract-passport (subseq lines n))))

(defun all-true-p (my-list)
  (loop for item in my-list always item))

(defun count-true (my-list)
  (loop
     for value in my-list
     sum (if (null value) 0 1)))

(defun get-value (record key num-digits)
  (let* ((start (+ (length key) (search key record))))
    (subseq record start (min (+ start num-digits) (length record)))))

(defun get-value-from-key (text key)
  (handler-case
      (format
       nil
       "~{~A~^~}"
       (loop
          with start = (search key text :test #'string=)
          for n from (+ start (length key)) below (length text)
          for c = (char text n)
          while (string/= c #\Space)
          collect c))
    (t nil)))

(defun byr-p (record)
  (handler-case
      (<=
       1920
       (parse-integer (get-value-from-key record "byr:"))
       2002)
    (t nil)))


(defun iyr-p (record)
  (handler-case
      (<=
       2010
       (parse-integer (get-value-from-key record "iyr:"))
       2020)
    (t nil)))

(defun eyr-p (record)
  (handler-case
      (<=
       2020
       (parse-integer (get-value-from-key record "eyr:"))
       2030)
    (t nil)))

(defun hgt-p (record)
  (let* ((value (get-value-from-key record "hgt:"))
         (cm-pos (search "cm" value :test #'string=))
         (in-pos (search "in" value :test #'string=))
         )
    (cond ((not(null cm-pos))
           (<= 150 (parse-integer (subseq value 0 cm-pos)) 193))
          ((not (null in-pos))
           (<= 59 (parse-integer (subseq value 0 in-pos)) 76))
          (t nil))))

(defun hcl-p (record)
  (let* ((value (get-value-from-key record "hcl:")))
    (and
     (= (length value) 7)
     (string= (char value 0) #\#)
     (loop
        for c across (subseq value 1)
        always (not
                (null
                 (position
                  c
                  (list #\a #\b #\c #\d #\e #\f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))))))                      

(defun ecl-p (record)
  (let* ((value (get-value-from-key record "ecl:")))
    (or
     (string= "amb" value)
     (string= "blu" value)
     (string= "brn" value)
     (string= "gry" value)
     (string= "grn" value)
     (string= "hzl" value)
     (string= "oth" value))))

(defun pid-p (record)
  (let* ((value (get-value-from-key record "pid:")))
    (and
     (= (length value) 9)
     (loop for c across value always (digit-char-p c)))))

(defun passport-valid-simple-p (text)
  (all-true-p
   (list
    (not (null (search "byr:" text)))
    (not (null (search "iyr:" text)))
    (not (null (search "eyr:" text)))
    (not (null (search "hgt:" text)))
    (not (null (search "hcl:" text)))
    (not (null (search "ecl:" text)))
    (not (null (search "pid:" text))))))
    

(defun passport-valid-complex-p (text)
  (all-true-p
   (list
    (byr-p text)
    (iyr-p text)
    (eyr-p text)
    (hgt-p text)
    (hcl-p text)
    (ecl-p text)
    (pid-p text)
    )))

(defun count-valid-passports-simple (passports)
  (count-true (mapcar 'passport-valid-simple-p passports)))

(defun count-valid-passports-complex (passports)
  (count-true (mapcar 'passport-valid-complex-p passports)))

(print
 (count-valid-passports-simple
  (split-passports
   (get-file "puzzle_input.txt"))))

(print
 (count-valid-passports-complex
  (split-passports
   (get-file "puzzle_input.txt"))))
