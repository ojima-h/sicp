(load "amb/parsing_natural_language.scm")

(batch-exec
 (parse '(the professor lectures to the student in the class with the cat))
 try-again
 try-again
 try-again
 try-again
 try-again)



;>>> <<< (parse '(the professor lectures to the student in the class with the cat))
;>>> ;;; Starting a new problem 
;>>> ;;; Amb-Eval value:
;>>> (sentence
;>>>  (simple-noun-phrase
;>>>   (article the)
;>>>   (noun professor))
;>>>  (verb-phrase
;>>>   (verb-phrase
;>>>    (verb-phrase
;>>>     (verb lectures)
;>>>     (prep-phrase
;>>>      (prep to)
;>>>      (simple-noun-phrase
;>>>       (article the)
;>>>       (noun student))))
;>>>    (prep-phrase
;>>>     (prep in)
;>>>     (simple-noun-phrase
;>>>      (article the)
;>>>      (noun class))))
;>>>   (prep-phrase (prep with)
;>>> 	       (simple-noun-phrase
;>>> 		(article the)
;>>> 		(noun cat)))))
;>>> <<< try-again
;>>> ;;; Amb-Eval value:
;>>> (sentence
;>>>  (simple-noun-phrase
;>>>   (article the)
;>>>   (noun professor))
;>>>  (verb-phrase
;>>>   (verb-phrase
;>>>    (verb lectures)
;>>>    (prep-phrase
;>>>     (prep to)
;>>>     (simple-noun-phrase
;>>>      (article the)
;>>>      (noun student))))
;>>>   (prep-phrase
;>>>    (prep in)
;>>>    (noun-phrase
;>>>     (simple-noun-phrase
;>>>      (article the)
;>>>      (noun class))
;>>>     (prep-phrase
;>>>      (prep with)
;>>>      (simple-noun-phrase
;>>>       (article the)
;>>>       (noun cat)))))))
;>>> <<< try-again
;>>> ;;; Amb-Eval value:
;>>> (sentence
;>>>  (simple-noun-phrase
;>>>   (article the)
;>>>   (noun professor))
;>>>  (verb-phrase
;>>>   (verb-phrase
;>>>    (verb lectures)
;>>>    (prep-phrase
;>>>     (prep to)
;>>>     (noun-phrase
;>>>      (simple-noun-phrase
;>>>       (article the)
;>>>       (noun student))
;>>>      (prep-phrase
;>>>       (prep in)
;>>>       (simple-noun-phrase
;>>>        (article the)
;>>>        (noun class))))))
;>>>   (prep-phrase
;>>>    (prep with)
;>>>    (simple-noun-phrase
;>>>     (article the)
;>>>     (noun cat)))))
;>>> <<< try-again
;>>> ;;; Amb-Eval value:
;>>> (sentence
;>>>  (simple-noun-phrase
;>>>   (article the)
;>>>   (noun professor))
;>>>  (verb-phrase
;>>>   (verb lectures)
;>>>   (prep-phrase
;>>>    (prep to)
;>>>    (noun-phrase
;>>>     (noun-phrase
;>>>      (simple-noun-phrase
;>>>       (article the)
;>>>       (noun student))
;>>>      (prep-phrase
;>>>       (prep in)
;>>>       (simple-noun-phrase
;>>>        (article the)
;>>>        (noun class))))
;>>>     (prep-phrase
;>>>      (prep with)
;>>>      (simple-noun-phrase
;>>>       (article the)
;>>>       (noun cat)))))))
;>>> <<< try-again
;>>> ;;; Amb-Eval value:
;>>> (sentence
;>>>  (simple-noun-phrase
;>>>   (article the)
;>>>   (noun professor))
;>>>  (verb-phrase
;>>>   (verb lectures)
;>>>   (prep-phrase
;>>>    (prep to)
;>>>    (noun-phrase
;>>>     (simple-noun-phrase
;>>>      (article the)
;>>>      (noun student))
;>>>     (prep-phrase
;>>>      (prep in)
;>>>      (noun-phrase
;>>>       (simple-noun-phrase
;>>>        (article the)
;>>>        (noun class))
;>>>       (prep-phrase
;>>>        (prep with)
;>>>        (simple-noun-phrase
;>>> 	(article the)
;>>> 	(noun cat)))))))))
;>>> <<< try-again
;>>> ;;; There are no more values of
;>>> (parse '(the professor lectures to the student in the class with the cat))#t
