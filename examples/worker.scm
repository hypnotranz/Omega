;; Worker: reads task from session, does work, writes result back
(begin
  (define task (effect file.read.op "examples/task.txt"))
  (define result (string-append "processed: " task))
  (effect file.write.op "examples/result.txt" result)
  result)
