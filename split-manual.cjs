const fs = require('fs');
const path = require('path');

const content = fs.readFileSync(path.join(__dirname, 'USER-MANUAL.md'), 'utf8');
const docsDir = path.join(__dirname, 'docs');

// Create docs directory if needed
if (!fs.existsSync(docsDir)) {
  fs.mkdirSync(docsDir);
}

// Clear existing chapter files
fs.readdirSync(docsDir).forEach(file => {
  if (file.startsWith('USER-MANUAL--')) {
    fs.unlinkSync(path.join(docsDir, file));
  }
});

// Extract intro
const introMatch = content.match(/^# OmegaLLM[^]*?(?=---\s*\n\n## Part I:)/);
const intro = introMatch ? introMatch[0].trim() : '';

// Split by chapter/section headers
const sections = [];

// Get introduction section
const introSectionMatch = content.match(/## Introduction\n[^]*?(?=---\s*\n\n## Chapter 1:)/);
const introSection = introSectionMatch ? introSectionMatch[0].trim() : '';

// Extract chapters
const chapterRegex = /## Chapter (\d+): ([^\n]+)\n([\s\S]*?)(?=## Chapter \d+:|## Part [IV]+:|## Epilogue:|## Appendix [AB]:|$)/g;
let match;
while ((match = chapterRegex.exec(content)) !== null) {
  const [full, num, title, body] = match;
  sections.push({
    num: parseInt(num),
    title: title.trim(),
    content: `## Chapter ${num}: ${title}\n${body.trim()}`
  });
}

// Extract epilogue
const epilogueMatch = content.match(/## Epilogue:[^\n]*\n([\s\S]*?)(?=## Appendix [AB]:|$)/);
const epilogue = epilogueMatch ? `## Epilogue:${epilogueMatch[0].split('\n').slice(0, 1)[0].replace('## Epilogue:', '')}\n${epilogueMatch[1].trim()}` : '';

// Extract appendices
const appendixAMatch = content.match(/## Appendix A:[^\n]*\n([\s\S]*?)(?=## Appendix B:|$)/);
const appendixBMatch = content.match(/## Appendix B:[^\n]*\n([\s\S]*?)$/);

function toFilename(title) {
  return title
    .replace(/[—–:]/g, '-')
    .replace(/\s+/g, '-')
    .split('-')
    .map(word => word.charAt(0).toUpperCase() + word.slice(1).toLowerCase())
    .join('-')
    .replace(/-+/g, '-')
    .replace(/[^a-zA-Z0-9-]/g, '')
    .replace(/-+$/, '');
}

function writeChapterFile(filename, content) {
  const header = `# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

`;
  fs.writeFileSync(path.join(docsDir, filename), header + content);
  console.log(`Created: ${filename}`);
}

// Write introduction
writeChapterFile('USER-MANUAL--00--Introduction.md', introSection);

// Write chapters
sections.forEach(section => {
  const numStr = section.num.toString().padStart(2, '0');
  const filename = `USER-MANUAL--${numStr}--${toFilename(section.title)}.md`;
  writeChapterFile(filename, section.content);
});

// Write epilogue
if (epilogue) {
  writeChapterFile('USER-MANUAL--99--Epilogue-The-Structure-Of-Understanding.md', epilogue);
}

// Write appendices
if (appendixAMatch) {
  writeChapterFile('USER-MANUAL--91--Appendix-A-Configuration.md',
    `## Appendix A: Configuration\n${appendixAMatch[1].trim()}`);
}
if (appendixBMatch) {
  writeChapterFile('USER-MANUAL--92--Appendix-B-Design-Philosophy.md',
    `## Appendix B: Design Philosophy\n${appendixBMatch[1].trim()}`);
}

// Create Table of Contents
const toc = `# OmegaLLM User Manual

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

### Appendices

- [Appendix A: Configuration](USER-MANUAL--91--Appendix-A-Configuration.md)
- [Appendix B: Design Philosophy](USER-MANUAL--92--Appendix-B-Design-Philosophy.md)

### Epilogue

- [Epilogue: The Structure of Understanding](USER-MANUAL--99--Epilogue-The-Structure-Of-Understanding.md)

---

*OmegaLLM: Where functions meet understanding.*
`;

fs.writeFileSync(path.join(docsDir, 'USER-MANUAL--00--Table-Of-Contents.md'), toc);
console.log('Created: USER-MANUAL--00--Table-Of-Contents.md');

console.log('\n✓ Manual split into ' + (sections.length + 4) + ' files');
