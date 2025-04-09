
;;; ChatGPT assisted throughout
;;; Convert any input to uppercase using built-in string-upcase
(defun to-upper (text)
  (string-upcase text))

;;; Encrypts a string using a Caesar cipher in a functional manner
(defun caesar-encrypt (text shift)
  (let ((text (to-upper text)))  ; Convert the input to uppercase.
    (map 'string
         (lambda (c)
           (if (and (>= (char-code c) (char-code #\A))
                    (<= (char-code c) (char-code #\Z)))
               (code-char (+ (mod (+ (- (char-code c) (char-code #\A))
                                        shift)
                                    26)
                             (char-code #\A)))
               c))
         text)))

;;; Decrypts a string using a Caesar cipher in a functional manner
(defun caesar-decrypt (text shift)
  (map 'string
       (lambda (c)
         (if (and (>= (char-code c) (char-code #\A))
                  (<= (char-code c) (char-code #\Z)))
             (code-char (+ (mod (+ (- (char-code c) (char-code #\A))
                                      (- 26 shift))
                                  26)
                           (char-code #\A)))
             c))
       text))

;;; Solves (breaks) a Caesar cipher by printing encryptions for each possible shift
(defun solve (text max-shift)
  (labels ((solve-rec (s)
             (when (>= s 0)
               (format t "Caesar ~a: ~a~%" s (caesar-encrypt text s))
               (solve-rec (1- s)))))
    (solve-rec max-shift)))

;;; Test function
(defun test-caesar-cipher ()
  (let ((original "This is a test string")
        (shift 8))
    (let* ((encrypted (caesar-encrypt original shift))
           (decrypted (caesar-decrypt encrypted shift)))
      (format t "Original: ~a~%" original)
      (format t "Encrypted: ~a~%" encrypted)
      (format t "Decrypted: ~a~%" decrypted)
      (format t "~%SOLVE for \"HAL\":~%")
      (solve "HAL" 26))))

;;; To run the test:
(test-caesar-cipher)
