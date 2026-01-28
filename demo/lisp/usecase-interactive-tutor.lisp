;; usecase-interactive-tutor.lisp
;;
;; INTERACTIVE PROGRAMMING TUTOR
;; Teaches concepts, asks questions, tracks progress, adapts to student
;;
;; Run: omega --file demo/lisp/usecase-interactive-tutor.lisp --caps infer

;; ============================================================================
;; STUDENT STATE
;; ============================================================================

(define student-state
  (list
    (cons 'topics-covered '())
    (cons 'correct-answers 0)
    (cons 'wrong-answers 0)
    (cons 'current-topic #f)
    (cons 'difficulty 'beginner)
    (cons 'history '())))

(define (set-student! key value)
  (set! student-state
    (map (lambda (pair)
           (if (equal? (car pair) key)
               (cons key value)
               pair))
         student-state)))

(define (get-student key)
  (cdr (assoc key student-state)))

(define (add-to-history! entry)
  (set-student! 'history (cons entry (get-student 'history))))

;; ============================================================================
;; CURRICULUM
;; ============================================================================

(define curriculum
  (list
    (list 'variables "Variables store values" "What does `(define x 5)` do?")
    (list 'functions "Functions are reusable code" "How do you define a function that adds two numbers?")
    (list 'recursion "Recursion is a function calling itself" "Write factorial using recursion")
    (list 'lists "Lists are sequences of values" "How do you get the first element of a list?")
    (list 'higher-order "Functions can take functions as arguments" "What does `map` do?")
    (list 'effects "Effects are side effects like LLM calls" "How do you call an LLM in OmegaLLM?")))

(define (get-topic-info topic)
  (assoc topic curriculum))

(define (get-next-topic)
  (let ((covered (get-student 'topics-covered)))
    (let loop ((topics curriculum))
      (if (null? topics)
          #f
          (let ((topic (caar topics)))
            (if (member topic covered)
                (loop (cdr topics))
                topic))))))

;; ============================================================================
;; TEACHING
;; ============================================================================

;; Explain a topic at current difficulty
(define (explain-topic topic)
  (let* ((info (get-topic-info topic))
         (basic-explanation (cadr info))
         (difficulty (get-student 'difficulty))
         (explanation
           (effect infer.op
             (list "Explain this programming concept for a " (symbol->string difficulty) " student:\n"
                   "Topic: " (symbol->string topic) "\n"
                   "Basic idea: " basic-explanation "\n"
                   "Give a clear explanation with a simple example."))))
    (set-student! 'current-topic topic)
    (add-to-history! (list 'explanation topic explanation))
    explanation))

;; Generate a practice question
(define (generate-question topic)
  (let* ((info (get-topic-info topic))
         (sample-question (caddr info))
         (difficulty (get-student 'difficulty))
         (question
           (effect infer.op
             (list "Generate a " (symbol->string difficulty)
                   " level practice question about " (symbol->string topic)
                   " in Lisp/Scheme.\n"
                   "Example question type: " sample-question "\n"
                   "Include what the expected answer should be."))))
    (add-to-history! (list 'question topic question))
    question))

;; Check an answer
(define (check-answer student-answer)
  (let* ((topic (get-student 'current-topic))
         (last-question (cadr (car (filter (lambda (h) (equal? (car h) 'question))
                                           (get-student 'history)))))
         (evaluation
           (effect infer.op
             (list "Evaluate this student answer:\n"
                   "Question context: " (symbol->string topic) "\n"
                   "Student answer: " student-answer "\n\n"
                   "Is it correct? Explain what's right/wrong. Be encouraging.\n"
                   "Start with CORRECT or INCORRECT."))))
    (if (string-contains? evaluation "CORRECT")
        (begin
          (set-student! 'correct-answers (+ 1 (get-student 'correct-answers)))
          (maybe-level-up!))
        (set-student! 'wrong-answers (+ 1 (get-student 'wrong-answers))))
    (add-to-history! (list 'answer student-answer evaluation))
    evaluation))

;; ============================================================================
;; ADAPTIVE LEARNING
;; ============================================================================

;; Maybe increase difficulty
(define (maybe-level-up!)
  (let ((correct (get-student 'correct-answers))
        (wrong (get-student 'wrong-answers)))
    (if (and (>= correct 3)
             (> correct (* 2 wrong)))
        (let ((current (get-student 'difficulty)))
          (cond
            ((equal? current 'beginner) (set-student! 'difficulty 'intermediate))
            ((equal? current 'intermediate) (set-student! 'difficulty 'advanced))
            (else #f))))))

;; Mark topic as covered
(define (complete-topic! topic)
  (set-student! 'topics-covered
    (cons topic (get-student 'topics-covered))))

;; Get personalized hint
(define (get-hint)
  (let* ((topic (get-student 'current-topic))
         (history-str (format-recent-history))
         (hint
           (effect infer.op
             (list "The student is stuck on " (symbol->string topic) ".\n"
                   "Recent interaction:\n" history-str "\n"
                   "Give a helpful hint without giving away the answer."))))
    (add-to-history! (list 'hint hint))
    hint))

(define (format-recent-history)
  (let ((recent (take (get-student 'history) 5)))
    (string-join
      (map (lambda (h) (string-append (symbol->string (car h)) ": " (substring (cadr h) 0 100)))
           recent)
      "\n")))

;; ============================================================================
;; TUTORING SESSION
;; ============================================================================

;; Start a tutoring session
(define (start-session)
  (let ((next-topic (get-next-topic)))
    (if next-topic
        (string-append
          "Welcome! Let's learn about " (symbol->string next-topic) ".\n\n"
          (explain-topic next-topic)
          "\n\nReady for a question? Call (generate-question '" (symbol->string next-topic) ")")
        "Congratulations! You've covered all topics!")))

;; Full interaction loop (for REPL)
(define (tutor-respond input)
  (cond
    ((string-contains? input "explain")
     (let ((topic (get-student 'current-topic)))
       (if topic
           (explain-topic topic)
           "No current topic. Call (start-session)")))

    ((string-contains? input "question")
     (let ((topic (get-student 'current-topic)))
       (if topic
           (generate-question topic)
           "No current topic. Call (start-session)")))

    ((string-contains? input "hint")
     (get-hint))

    ((string-contains? input "next")
     (let ((current (get-student 'current-topic)))
       (if current (complete-topic! current))
       (start-session)))

    ((string-contains? input "progress")
     (format-progress))

    (else
     (check-answer input))))

;; ============================================================================
;; PROGRESS TRACKING
;; ============================================================================

(define (format-progress)
  (string-append
    "=== Student Progress ===\n"
    "Difficulty: " (symbol->string (get-student 'difficulty)) "\n"
    "Topics covered: " (number->string (length (get-student 'topics-covered))) "/" (number->string (length curriculum)) "\n"
    "Correct answers: " (number->string (get-student 'correct-answers)) "\n"
    "Wrong answers: " (number->string (get-student 'wrong-answers)) "\n"))

;; ============================================================================
;; USAGE
;; ============================================================================

;; Start tutoring:
;; (start-session)

;; Get a question:
;; (generate-question 'variables)

;; Submit answer:
;; (check-answer "(define x 5) creates a variable x with value 5")

;; Get help:
;; (get-hint)

;; Check progress:
;; (format-progress)

;; Move to next topic:
;; (complete-topic! 'variables)
;; (start-session)
