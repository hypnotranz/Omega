// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-3.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import type { Job } from "./types";

export interface Frontier {
  push(job: Job): void;
  pop(): Job | undefined;
  size(): number;
  clear(): void;
}

export class StackFrontier implements Frontier {
  private xs: Job[] = [];
  push(job: Job) { this.xs.push(job); }
  pop() { return this.xs.pop(); }
  size() { return this.xs.length; }
  clear() { this.xs = []; }
}

export class QueueFrontier implements Frontier {
  private xs: Job[] = [];
  private head = 0;
  push(job: Job) { this.xs.push(job); }
  pop() {
    if (this.head >= this.xs.length) return undefined;
    const j = this.xs[this.head];
    this.head += 1;
    // compact occasionally
    if (this.head > 1024 && this.head * 2 > this.xs.length) {
      this.xs = this.xs.slice(this.head);
      this.head = 0;
    }
    return j;
  }
  size() { return this.xs.length - this.head; }
  clear() { this.xs = []; this.head = 0; }
}

export class PriorityFrontier implements Frontier {
  // Max-heap by job.score (reference grade)
  private heap: Job[] = [];
  push(job: Job) { heapPush(this.heap, job); }
  pop() { return heapPop(this.heap); }
  size() { return this.heap.length; }
  clear() { this.heap = []; }
}

function heapPush(heap: Job[], job: Job) {
  heap.push(job);
  let i = heap.length - 1;
  while (i > 0) {
    const p = Math.floor((i - 1) / 2);
    if (heap[p].score >= heap[i].score) break;
    [heap[p], heap[i]] = [heap[i], heap[p]];
    i = p;
  }
}

function heapPop(heap: Job[]): Job | undefined {
  if (heap.length === 0) return undefined;
  const top = heap[0];
  const last = heap.pop()!;
  if (heap.length > 0) {
    heap[0] = last;
    let i = 0;
    while (true) {
      const l = 2 * i + 1, r = 2 * i + 2;
      let m = i;
      if (l < heap.length && heap[l].score > heap[m].score) m = l;
      if (r < heap.length && heap[r].score > heap[m].score) m = r;
      if (m === i) break;
      [heap[i], heap[m]] = [heap[m], heap[i]];
      i = m;
    }
  }
  return top;
}

export class BeamFrontier implements Frontier {
  private heap: Job[] = [];
  constructor(private readonly width: number) {}
  push(job: Job) {
    heapPush(this.heap, job);
    if (this.heap.length > this.width) {
      // drop worst by rebuilding heap after removing min:
      // reference-grade: sort descending, truncate, rebuild
      this.heap.sort((a, b) => b.score - a.score);
      this.heap.length = this.width;
      this.heap = this.heap.slice();
      // rebuild heap structure
      const xs = this.heap.slice().sort((a, b) => a.score - b.score);
      this.heap = [];
      for (const j of xs) heapPush(this.heap, j);
    }
  }
  pop() { return heapPop(this.heap); }
  size() { return this.heap.length; }
  clear() { this.heap = []; }
}