;定义一个全局变量
(defvar *db* nil)

;创建cd记录函数
(defun make-cd(title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

;添加cd记录函数
(defun add-record(cd)
  (push cd *db*))

;人性化显示函数
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a: ~10t~a~%~}~%" cd)))

;交互提示和读取用户输入
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;交互式输入cd记录函数
(defun prompt-for-cd()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

;循环交互
(defun add-cds()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: "))(return))))

;以标准语法保存数据库到文件
(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

;用相同的语法标准读取数据库文件到全局变量
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))
;查询数据库-----------------------------
;通用选择器（select）函数生成器：根据传递给他的参数，生成用于不同字段的选择器函数。
;该函数返回的是匿名函数函数体，甚至是可以是一种组合
(defun whereis (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd) 
      (and
       (if title (equal (getf cd :title) title) t)
       (if artist (equal (getf cd :artist) artist) t)
       (if rating (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t) )))
       ;只判断真有该数据段传入时

;使用宏功能，修改where功能,使更通用，以及减少代码消耗，提高代码效率------
;构建表达式list
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

;根据输入的fields内容，数量来收集所有生成的list
(defun make-comparison-list (fields)
  (loop while fields
       collecting (make-comparison-expr (pop fields) (pop fields))))

;将收集到的所有list放在AND和一个匿名函数里
;这里定义了一个宏，宏的参数列表，这里是一些有语义的列表
(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparison-list clauses))))
;---

;选择查询函数，部分函数体来自上面函数
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;更新数据库----------------------------------
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
	(mapcar
	 #'(lambda (row)
	     (when (funcall selector-fn row)
	       (if title (setf (getf row :title) title))
	       (if artist (setf (getf row :artist) artist))
	       (if rating (setf (getf row :rating) rating))
	       (if ripped-p (setf (getf row :ripped) ripped)))
	     row) *db*)))
;删除选定的数据记录
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

