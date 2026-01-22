# JOB-019e4-timeline-navigator: Enhanced Time Travel

## Context

This job implements the `TimelineNavigator` class that provides enhanced checkpoint navigation with annotations and tagging.

## Goal

Create a `TimelineNavigator` class that:
1. Annotates checkpoints with labels and descriptions
2. Supports tagging for organization
3. Generates visual timeline representations
4. Enables quick navigation to annotated points

## Dependencies

- `019-types` (for CheckpointAnnotation interface)

## Blockers

- None (can run in parallel with other Layer 5 jobs)

## Files to Create

1. `server/timeline-navigator.ts` - Timeline navigation

## Implementation

```typescript
// server/timeline-navigator.ts
import type { CheckpointAnnotation } from './types';

export interface CheckpointInfo {
  seq: number;
  reason: string;
  stateId: string;
  byteOffset?: number;
}

export interface AnnotatedCheckpoint extends CheckpointInfo {
  annotation: CheckpointAnnotation | null;
}

export class TimelineNavigator {
  private annotations: Map<string, Map<number, CheckpointAnnotation>> = new Map();

  addAnnotation(
    sessionName: string,
    checkpointSeq: number,
    label: string,
    description: string = '',
    tags: string[] = []
  ): CheckpointAnnotation {
    if (!this.annotations.has(sessionName)) {
      this.annotations.set(sessionName, new Map());
    }

    const annotation: CheckpointAnnotation = {
      checkpointSeq,
      label,
      description,
      tags,
      created: new Date(),
    };

    this.annotations.get(sessionName)!.set(checkpointSeq, annotation);

    return annotation;
  }

  getAnnotation(sessionName: string, checkpointSeq: number): CheckpointAnnotation | null {
    const sessionAnnotations = this.annotations.get(sessionName);
    if (!sessionAnnotations) return null;
    return sessionAnnotations.get(checkpointSeq) || null;
  }

  getSessionAnnotations(sessionName: string): CheckpointAnnotation[] {
    const sessionAnnotations = this.annotations.get(sessionName);
    if (!sessionAnnotations) return [];
    return Array.from(sessionAnnotations.values());
  }

  getAnnotationsByTag(sessionName: string, tag: string): CheckpointAnnotation[] {
    return this.getSessionAnnotations(sessionName)
      .filter(a => a.tags.includes(tag));
  }

  removeAnnotation(sessionName: string, checkpointSeq: number): boolean {
    const sessionAnnotations = this.annotations.get(sessionName);
    if (!sessionAnnotations) return false;
    return sessionAnnotations.delete(checkpointSeq);
  }

  updateAnnotation(
    sessionName: string,
    checkpointSeq: number,
    updates: Partial<Omit<CheckpointAnnotation, 'checkpointSeq' | 'created'>>
  ): CheckpointAnnotation | null {
    const existing = this.getAnnotation(sessionName, checkpointSeq);
    if (!existing) return null;

    const updated: CheckpointAnnotation = {
      ...existing,
      ...updates,
    };

    this.annotations.get(sessionName)!.set(checkpointSeq, updated);
    return updated;
  }

  generateTimeline(
    sessionName: string,
    checkpoints: CheckpointInfo[]
  ): AnnotatedCheckpoint[] {
    return checkpoints.map(cp => ({
      ...cp,
      annotation: this.getAnnotation(sessionName, cp.seq),
    }));
  }

  findByLabel(sessionName: string, labelPattern: string): CheckpointAnnotation[] {
    const pattern = new RegExp(labelPattern, 'i');
    return this.getSessionAnnotations(sessionName)
      .filter(a => pattern.test(a.label));
  }

  getAllTags(sessionName: string): string[] {
    const tags = new Set<string>();
    for (const annotation of this.getSessionAnnotations(sessionName)) {
      for (const tag of annotation.tags) {
        tags.add(tag);
      }
    }
    return Array.from(tags).sort();
  }

  clearSessionAnnotations(sessionName: string): void {
    this.annotations.delete(sessionName);
  }

  // Serialize annotations for persistence
  exportAnnotations(sessionName: string): CheckpointAnnotation[] {
    return this.getSessionAnnotations(sessionName);
  }

  // Import annotations from persistence
  importAnnotations(sessionName: string, annotations: CheckpointAnnotation[]): void {
    if (!this.annotations.has(sessionName)) {
      this.annotations.set(sessionName, new Map());
    }

    const sessionAnnotations = this.annotations.get(sessionName)!;
    for (const annotation of annotations) {
      sessionAnnotations.set(annotation.checkpointSeq, annotation);
    }
  }
}
```

## Testing

```typescript
// server/timeline-navigator.spec.ts
import { TimelineNavigator } from './timeline-navigator';

describe('TimelineNavigator', () => {
  let navigator: TimelineNavigator;

  beforeEach(() => {
    navigator = new TimelineNavigator();
  });

  describe('addAnnotation', () => {
    it('creates annotation with all fields', () => {
      const annotation = navigator.addAnnotation(
        'session1',
        5,
        'Important point',
        'This is where the bug was fixed',
        ['bugfix', 'important']
      );

      expect(annotation.checkpointSeq).toBe(5);
      expect(annotation.label).toBe('Important point');
      expect(annotation.description).toBe('This is where the bug was fixed');
      expect(annotation.tags).toEqual(['bugfix', 'important']);
      expect(annotation.created).toBeInstanceOf(Date);
    });
  });

  describe('getAnnotation', () => {
    it('retrieves existing annotation', () => {
      navigator.addAnnotation('session1', 5, 'Test');

      const annotation = navigator.getAnnotation('session1', 5);
      expect(annotation?.label).toBe('Test');
    });

    it('returns null for non-existent annotation', () => {
      expect(navigator.getAnnotation('session1', 999)).toBeNull();
    });
  });

  describe('getAnnotationsByTag', () => {
    it('filters annotations by tag', () => {
      navigator.addAnnotation('session1', 1, 'A', '', ['tag1']);
      navigator.addAnnotation('session1', 2, 'B', '', ['tag2']);
      navigator.addAnnotation('session1', 3, 'C', '', ['tag1', 'tag2']);

      const tag1Annotations = navigator.getAnnotationsByTag('session1', 'tag1');
      expect(tag1Annotations).toHaveLength(2);
    });
  });

  describe('generateTimeline', () => {
    it('combines checkpoints with annotations', () => {
      navigator.addAnnotation('session1', 2, 'Annotated');

      const checkpoints = [
        { seq: 1, reason: 'auto', stateId: 's1' },
        { seq: 2, reason: 'manual', stateId: 's2' },
        { seq: 3, reason: 'auto', stateId: 's3' },
      ];

      const timeline = navigator.generateTimeline('session1', checkpoints);

      expect(timeline).toHaveLength(3);
      expect(timeline[0].annotation).toBeNull();
      expect(timeline[1].annotation?.label).toBe('Annotated');
      expect(timeline[2].annotation).toBeNull();
    });
  });

  describe('updateAnnotation', () => {
    it('updates existing annotation', () => {
      navigator.addAnnotation('session1', 5, 'Original');

      const updated = navigator.updateAnnotation('session1', 5, {
        label: 'Updated',
        tags: ['new-tag'],
      });

      expect(updated?.label).toBe('Updated');
      expect(updated?.tags).toEqual(['new-tag']);
    });

    it('returns null for non-existent annotation', () => {
      const result = navigator.updateAnnotation('session1', 999, { label: 'X' });
      expect(result).toBeNull();
    });
  });

  describe('findByLabel', () => {
    it('finds annotations matching pattern', () => {
      navigator.addAnnotation('session1', 1, 'Bug fix #123');
      navigator.addAnnotation('session1', 2, 'Feature add');
      navigator.addAnnotation('session1', 3, 'Bug fix #456');

      const bugs = navigator.findByLabel('session1', 'bug fix');
      expect(bugs).toHaveLength(2);
    });
  });

  describe('getAllTags', () => {
    it('returns unique sorted tags', () => {
      navigator.addAnnotation('session1', 1, 'A', '', ['z', 'a']);
      navigator.addAnnotation('session1', 2, 'B', '', ['m', 'a']);

      const tags = navigator.getAllTags('session1');
      expect(tags).toEqual(['a', 'm', 'z']);
    });
  });

  describe('import/export', () => {
    it('exports and imports annotations', () => {
      navigator.addAnnotation('session1', 1, 'Test', 'Desc', ['tag']);

      const exported = navigator.exportAnnotations('session1');

      const newNavigator = new TimelineNavigator();
      newNavigator.importAnnotations('session1', exported);

      const annotation = newNavigator.getAnnotation('session1', 1);
      expect(annotation?.label).toBe('Test');
    });
  });
});
```

## Success Criteria

1. Annotations are created and retrieved correctly
2. Tag-based filtering works
3. Timeline generation works
4. All tests pass

## Estimated Effort

- Implementation: 45 minutes
- Testing: 45 minutes
