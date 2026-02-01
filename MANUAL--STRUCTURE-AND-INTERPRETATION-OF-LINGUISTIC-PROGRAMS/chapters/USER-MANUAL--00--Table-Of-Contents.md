# OmegaLLM User Manual

*A Functional Language for Composable AI*

---

## Table of Contents

### Part I: Foundations

- [Chapter 1: Getting Started](USER-MANUAL--01--Getting-Started.md)
- [Chapter 2: LLM Calls as Functions](USER-MANUAL--02--Llm-Calls-As-Functions.md)
- [Chapter 3: Functional Composition](USER-MANUAL--03--Functional-Composition.md)
- [Chapter 4: Higher-Order LLM Functions](USER-MANUAL--04--Higher-Order-Llm-Functions.md)
- [Chapter 5: Nondeterministic Search](USER-MANUAL--05--Nondeterministic-Search.md)
- [Chapter 6: Multi-Shot Sampling](USER-MANUAL--06--Multi-Shot-Sampling.md)
- [Chapter 7: Lazy Streams](USER-MANUAL--07--Lazy-Streams.md)
- [Chapter 8: The Debugger](USER-MANUAL--08--The-Debugger.md)
- [Chapter 9: The Agentic REPL](USER-MANUAL--09--The-Agentic-Repl.md)
- [Chapter 10: Full API Reference](USER-MANUAL--10--Full-Api-Reference.md)

### Part II: Structure and Interpretation of Inference Programs

**SICP Chapter 1: Building Abstractions with Procedures**
- [Chapter 11: Semantic Procedures as Black Boxes](USER-MANUAL--11--Semantic-Procedures-As-Black-Boxes.md) *(SICP 1.1)*
- [Chapter 12: Inference Processes](USER-MANUAL--12--Inference-Processes-Recursion-And-Iteration-In-Semantic-Space.md) *(SICP 1.2)*
- [Chapter 13: Higher-Order Inference](USER-MANUAL--13--Higher-Order-Inference.md) *(SICP 1.3)*

**SICP Chapter 2: Building Abstractions with Data**
- [Chapter 14: Semantic Data Abstraction](USER-MANUAL--14--Semantic-Data-Abstraction.md) *(SICP 2.1)*
- [Chapter 15: Sequences as Semantic Interfaces](USER-MANUAL--15--Sequences-As-Semantic-Interfaces.md) *(SICP 2.2)*
- [Chapter 16: Symbolic Semantic Data](USER-MANUAL--16--Symbolic-Semantic-Data.md) *(SICP 2.3)*
- [Chapter 17: Multiple Representations of Meaning](USER-MANUAL--17--Multiple-Representations-Of-Meaning.md) *(SICP 2.4)*
- [Chapter 18: Generic Semantic Operations](USER-MANUAL--18--Generic-Semantic-Operations.md) *(SICP 2.5)*

**SICP Chapter 3: Modularity, Objects, and State**
- [Chapter 19: Conversational State and Memory](USER-MANUAL--19--Conversational-State-And-Memory.md) *(SICP 3.1)*
- [Chapter 20: The Semantic Environment Model](USER-MANUAL--20--The-Semantic-Environment-Model.md) *(SICP 3.2)*
- [Chapter 21: Mutable Semantic Structures](USER-MANUAL--21--Mutable-Semantic-Structures.md) *(SICP 3.3)*
- [Chapter 22: Concurrent Inference](USER-MANUAL--22--Concurrent-Inference.md) *(SICP 3.4)*
- [Chapter 23: Streams of Inference](USER-MANUAL--23--Streams-Of-Inference.md) *(SICP 3.5)*

**SICP Chapter 4: Metalinguistic Abstraction**
- [Chapter 24: Metalinguistic Abstraction](USER-MANUAL--24--Metalinguistic-Abstraction-The-Oracle-In-The-Evaluator.md) *(SICP 4.1)*
- [Chapter 25: Lazy Semantic Evaluation](USER-MANUAL--25--Lazy-Semantic-Evaluation.md) *(SICP 4.2)*
- [Chapter 26: The AMB Inference Engine](USER-MANUAL--26--The-Amb-Inference-Engine.md) *(SICP 4.3)*
- [Chapter 27: Logic Programming with Semantic Facts](USER-MANUAL--27--Logic-Programming-With-Semantic-Facts.md) *(SICP 4.4)*

**SICP Chapter 5: Computing with Register Machines & Advanced Topics**
- [Chapter 28: The Substitution Model For Semantic Evaluation](USER-MANUAL--28--The-Substitution-Model-For-Semantic-Evaluation.md) *(SICP 1.1.5 - Pedagogical)*
- [Chapter 29: Iterative Semantic Refinement](USER-MANUAL--29--Iterative-Semantic-Refinement.md) *(SICP 1.2.1 - Iteration)*
- [Chapter 30: Tree Recursion With Semantic Branching](USER-MANUAL--30--Tree-Recursion-With-Semantic-Branching.md) *(SICP 1.2.2 - Tree Recursion)*
- [Chapter 31: Orders Of Growth Semantic Cost Analysis](USER-MANUAL--31--Orders-Of-Growth-Semantic-Cost-Analysis.md) *(SICP 1.2.3 - Cost Analysis)*
- [Chapter 32: General Methods Fixpoint And Root Finding](USER-MANUAL--32--General-Methods-Fixpoint-And-Root-Finding.md) *(SICP 1.3.3 - General Methods)*
- [Chapter 33: Hierarchical Semantic Structures](USER-MANUAL--33--Hierarchical-Semantic-Structures.md) *(SICP 2.2.2 - Hierarchical Data)*
- [Chapter 34: Symbolic Semantic Data](USER-MANUAL--34--Symbolic-Semantic-Data.md) *(SICP 2.3 - Symbolic Data)*
- [Chapter 35: Tagged Data With Type Dispatch](USER-MANUAL--35--Tagged-Data-With-Type-Dispatch.md) *(SICP 2.4 - Multiple Representations)*
- [Chapter 36: Type Coercion Towers](USER-MANUAL--36--Type-Coercion-Towers.md) *(SICP 2.5 - Coercion)*
- [Chapter 37: Mutable Queues And Tables](USER-MANUAL--37--Mutable-Queues-And-Tables.md) *(SICP 3.3 - Mutable Data)*
- [Chapter 38: Constraint Propagation Networks](USER-MANUAL--38--Constraint-Propagation-Networks.md) *(SICP 3.3.5 - Constraint Propagation)*
- [Chapter 39: Serializers For Concurrent LLM Calls](USER-MANUAL--39--Serializers-For-Concurrent-LLM-Calls.md) *(SICP 3.4 - Concurrency)*
- [Chapter 40: Data-Directed Evaluation](USER-MANUAL--40--Data-Directed-Evaluation.md) *(SICP 4.1 - Metacircular Evaluator)*
- [Chapter 41: Unification And Pattern Matching](USER-MANUAL--41--Unification-And-Pattern-Matching.md) *(SICP 4.4.4 - Unification)*
- [Chapter 42: Query Systems With Semantic Facts](USER-MANUAL--42--Query-Systems-With-Semantic-Facts.md) *(SICP 4.4 - Logic Programming)*
- [Chapter 43: Analyzing Evaluator](USER-MANUAL--43--Analyzing-Evaluator.md) *(SICP 4.1.7 - Analyzing Evaluator)*
- [Chapter 44: Compiler Optimizations](USER-MANUAL--44--Compiler-Optimizations.md) *(SICP 5.5 - Compilation)*
- [Chapter 45: Bytecode Execution](USER-MANUAL--45--Bytecode-Execution.md) *(SICP 5.4 - Explicit Control)*
- [Chapter 46: OPR Multi-Kernel Execution](USER-MANUAL--46--OPR-Multi-Kernel-Execution.md) *(OmegaLLM-Specific)*
- [Chapter 47: Provenance And Evidence Chains](USER-MANUAL--47--Provenance-And-Evidence-Chains.md) *(OmegaLLM-Specific)*
- [Chapter 48: Budget Management](USER-MANUAL--48--Budget-Management.md) *(OmegaLLM-Specific)*
- [Chapter 49: Semantic Caching Strategies](USER-MANUAL--49--Semantic-Caching-Strategies.md) *(OmegaLLM-Specific)*

### Appendices

- [Appendix A: Configuration](USER-MANUAL--91--Appendix-A-Configuration.md)
- [Appendix B: Design Philosophy](USER-MANUAL--92--Appendix-B-Design-Philosophy.md)

### Epilogue

- [Epilogue: The Structure of Understanding](USER-MANUAL--99--Epilogue-The-Structure-Of-Understanding.md)

---

*OmegaLLM: Where functions meet understanding.*
