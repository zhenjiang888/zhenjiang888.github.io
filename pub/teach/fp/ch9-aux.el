；このプログラムは理学部情報科学科の萩谷昌己先生のものを
；参考して作ったものです．

１ 盤面の表現

(defun at (b i j)
   (elt (elt b i) j))

(defun elt (x i)
  (cond ((= i 0) (car x))
        ((= i 1) (car (cdr x)))
        ((= i 2) (car (cdr (cdr x))))))


２ 主関数

(defun max-move (p b i j)
  (if (= i 3) 
      '(-999 . (0 . 0))    ; 古い版では，) が一個余っていた．
      (if (eq (at b i j) 'b) 
	(if (= j 2)        ; 古い版では，(= j 3)でした．
	    (max-move-2 (cons (eval-board p (move p b i j)) (cons i j))
			(max-move p b (+ i 1) 0))
	    (max-move-2 (cons (eval-board p (move p b i j)) (cons i j))
		      (max-move p b i (+ j 1))))
        (if (= j 2)        ; 古い版では，(= j 3)でした．
	    (max-move p b (+ i 1) 0)
	    (max-move p b i (+ j 1))))))

(defun eval-board (p b)
   (cond ((winp p b) 1)
         ((winp (opp p) b) -1)
         ((drawp b) 0)
         (t (- (car (max-move (opp p) b 0 0))))))

３ 補助関数

(defun winp (p b)
  (or (win-line p b 0 0 0 1)
      (win-line p b 1 0 0 1)
      (win-line p b 2 0 0 1)
      (win-line p b 0 0 1 0)
      (win-line p b 0 1 1 0)
      (win-line p b 0 2 1 0)
      (win-line p b 0 0 1 1)
      (win-line p b 2 0 -1 1)))

(defun win-line (p b i j di dj)
  (and (eq (at b i j) p)
       (eq (at b (+ i di) (+ j dj)) p)
       (eq (at b (+ i di di) (+ j dj dj)) p)))

(defun drawp (b)
 (and (draw-line b 0 0 0 1)
      (draw-line b 1 0 0 1)
      (draw-line b 2 0 0 1)
      (draw-line b 0 0 1 0)
      (draw-line b 0 1 1 0)
      (draw-line b 0 2 1 0)
      (draw-line b 0 0 1 1)
      (draw-line b 2 0 -1 1)))

(defun draw-line (b i j di dj)
  (and (or (eq (at b i j) 'o)
           (eq (at b (+ i di) (+ j dj)) 'o)
           (eq (at b (+ i di di) (+ j dj dj)) 'o))
       (or (eq (at b i j) 'x)
           (eq (at b (+ i di) (+ j dj)) 'x)
           (eq (at b (+ i di di) (+ j dj dj)) 'x))))

(defun opp (p)
  (if (eq p 'o) 'x 'o))

(defun max-move-2 (m1 m2)
   (if (< (car m1) (car m2)) m2 m1))

(defun move (p b i j)
  (rep-elt b i (rep-elt (elt b i) j p)))

(defun rep-elt (x i e)
   (cond ((= i 0) (cons e (cdr x)))
         ((= i 1) (cons (car x) (cons e (cdr (cdr x)))))
         ((= i 2) (cons (car x) (cons (car (cdr x)) (cons e ()))))))

４ テスト

(at '((x b o) (o o x) (b b x)) 2 1)
b

(at '((x b o) (o o x) (b b x)) 1 2)
x

(move 'x  '((x b o) (o o x) (b b x)) 2 1)
((x b o) (o o x) (b x x))

(drawp '((o x x) (b o b) (b b o)))
nil

(drawp '((o x o) (o x x) (x o b)))
t

(eval-board 'o '((x b b) (b o x) (o b o)))
1

(eval-board 'x '((o x o) (o x x) (x o b)))
0

(max-move 'o '((b x x) (b o b) (b b o)) 0 0)
(1 0 . 0)

(max-move 'o '((x b b) (b o x) (b b o)) 0 0)
(1 2 . 0)

(max-move 'x '((o x o) (o x b) (x o b)) 0 0)
(0 1 . 2)

(max-move 'o '((b b b) (b b b) (b b b)) 0 0)
(0 0 . 0)     ; この計算は20分ぐらいかかりました．



