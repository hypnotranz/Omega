;; usecase-data-pipeline.lisp
;;
;; DATA PROCESSING PIPELINE with LLM augmentation
;; Process CSV/JSON files, enrich with LLM, output results
;;
;; Demonstrates: streams + file I/O + LLM enrichment + lazy evaluation
;;
;; Run: omega --file demo/lisp/usecase-data-pipeline.lisp --caps shell,file.read,file.write,infer

;; ============================================================================
;; DATA LOADING
;; ============================================================================

;; Parse a CSV line into fields (simple split on comma)
(define (parse-csv-line line)
  (string-split line ","))

;; Load CSV file as list of rows
(define (load-csv filepath)
  (let* ((content (effect file.read.op filepath))
         (lines (string-split content "\n"))
         (non-empty (filter (lambda (l) (not (equal? l ""))) lines)))
    (map parse-csv-line non-empty)))

;; Load CSV as stream (lazy - for large files)
(define (stream-csv filepath)
  (let* ((content (effect file.read.op filepath))
         (lines (string-split content "\n")))
    (stream-map parse-csv-line
                (stream-filter (lambda (l) (not (equal? l "")))
                               (list->stream lines)))))

;; ============================================================================
;; LLM ENRICHMENT
;; ============================================================================

;; Classify a text into categories
(define (classify text categories)
  (effect infer.op
    (list "Classify this text into exactly one of these categories: "
          (string-join categories ", ")
          "\n\nText: " text
          "\n\nRespond with just the category name.")))

;; Extract entities from text
(define (extract-entities text)
  (effect infer.op
    (list "Extract named entities (people, places, organizations) from:\n"
          text
          "\n\nFormat as: TYPE: name, TYPE: name, ...")))

;; Summarize text
(define (summarize text max-words)
  (effect infer.op
    (list "Summarize in " (number->string max-words) " words or less:\n" text)))

;; Sentiment analysis
(define (sentiment text)
  (effect infer.op
    (list "What is the sentiment? positive/negative/neutral:\n" text)))

;; ============================================================================
;; PIPELINE OPERATIONS
;; ============================================================================

;; Enrich a customer support ticket
(define (enrich-ticket row)
  (let* ((id (car row))
         (text (cadr row))
         (category (classify text (list "billing" "technical" "shipping" "general")))
         (sentiment-val (sentiment text))
         (priority (if (equal? sentiment-val "negative") "high" "normal")))
    (list id text category sentiment-val priority)))

;; Process tickets pipeline
(define (process-tickets input-file output-file)
  (let* ((tickets (cdr (load-csv input-file)))  ; skip header
         (enriched (map enrich-ticket tickets))
         (header "id,text,category,sentiment,priority")
         (rows (map (lambda (r) (string-join r ",")) enriched))
         (output (string-append header "\n" (string-join rows "\n"))))
    (effect file.write.op output-file output)
    (string-append "Processed " (number->string (length enriched)) " tickets")))

;; ============================================================================
;; STREAMING PIPELINE (memory efficient)
;; ============================================================================

;; Process stream lazily
(define (stream-process-tickets input-file output-file)
  (let ((header "id,text,category,sentiment,priority\n"))
    ;; Write header
    (effect file.write.op output-file header)
    ;; Process stream
    (stream-for-each
      (lambda (row)
        (let* ((enriched (enrich-ticket row))
               (line (string-append (string-join enriched ",") "\n")))
          ;; Append to file
          (effect shell.op (string-append "echo '" line "' >> " output-file))))
      (stream-cdr (stream-csv input-file)))))  ; skip header

;; ============================================================================
;; FILTERING WITH SEMANTIC PREDICATES
;; ============================================================================

;; Filter rows where LLM thinks content is urgent
(define (is-urgent? text)
  (equal? "yes"
    (effect infer.op
      (list "Is this message urgent and needs immediate attention? yes/no:\n" text))))

;; Get urgent tickets only
(define (get-urgent-tickets input-file)
  (let ((tickets (cdr (load-csv input-file))))
    (filter (lambda (row) (is-urgent? (cadr row))) tickets)))

;; ============================================================================
;; AGGREGATION WITH LLM
;; ============================================================================

;; Group tickets by category and summarize each group
(define (summarize-by-category input-file)
  (let* ((tickets (cdr (load-csv input-file)))
         (enriched (map enrich-ticket tickets))
         (categories (list "billing" "technical" "shipping" "general")))
    (map (lambda (cat)
           (let* ((cat-tickets (filter (lambda (t) (equal? (caddr t) cat)) enriched))
                  (texts (map cadr cat-tickets))
                  (combined (string-join texts "\n---\n"))
                  (summary (if (> (length cat-tickets) 0)
                              (summarize combined 50)
                              "No tickets")))
             (list cat (length cat-tickets) summary)))
         categories)))

;; ============================================================================
;; EXAMPLE: MULTI-STEP PIPELINE
;; ============================================================================

;; Complex pipeline: load -> filter urgent -> enrich -> summarize -> save
(define (urgent-summary-pipeline input-file output-file)
  (let* ((all-tickets (cdr (load-csv input-file)))
         (urgent (filter (lambda (row) (is-urgent? (cadr row))) all-tickets))
         (enriched (map enrich-ticket urgent))
         (summary (effect infer.op
                    (list "Summarize these urgent issues in a brief report:\n"
                          (string-join (map cadr urgent) "\n\n"))))
         (report (string-append
                   "# Urgent Tickets Report\n\n"
                   "Total urgent: " (number->string (length urgent)) "\n\n"
                   "## Summary\n" summary "\n\n"
                   "## Details\n"
                   (string-join
                     (map (lambda (t)
                            (string-append "- [" (car t) "] " (caddr t) ": " (cadr t)))
                          enriched)
                     "\n"))))
    (effect file.write.op output-file report)
    report))

;; ============================================================================
;; USAGE
;; ============================================================================

;; Create sample data first:
;; (effect file.write.op "tickets.csv"
;;   "id,text\n1,My payment failed twice\n2,Great service thanks\n3,URGENT shipping delayed 2 weeks\n4,How do I reset password")

;; Then run pipeline:
;; (process-tickets "tickets.csv" "enriched.csv")
;; (urgent-summary-pipeline "tickets.csv" "urgent-report.md")
