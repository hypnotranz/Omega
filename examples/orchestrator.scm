;; Orchestrator: spawns workers for tasks
(begin
  ;; Write task for worker
  (effect file.write.op "examples/task.txt" "build the feature")
  
  ;; Spawn worker
  (effect shell.op "node dist/omega-repl.mjs -c \"(load \\\"examples/worker.scm\\\")\"")
  
  ;; Read result
  (effect file.read.op "examples/result.txt"))
