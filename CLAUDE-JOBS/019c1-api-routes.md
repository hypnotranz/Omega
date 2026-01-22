# JOB-019c1-api-routes: REST Endpoint Definitions

## Context

This job implements the API routes for the Session Server, defining all REST endpoints for client communication.

## Goal

Create API route handlers that:
1. Define all REST endpoints for session operations
2. Handle request validation
3. Return proper error responses
4. Integrate with SessionManager

## Dependencies

- `019-types` (for request/response types)
- `019b2-session-manager` (for ISessionManager)

## Blockers

- `019b2-session-manager` must be complete

## Files to Create

1. `server/api-routes.ts` - Express router with all endpoints

## Implementation

```typescript
// server/api-routes.ts
import { Router, Request, Response, NextFunction } from 'express';
import type { 
  ISessionManager, 
  ApiExecuteRequest, 
  ApiExecuteResponse,
  ApiSessionListResponse,
  ApiErrorResponse 
} from './types';

export function createApiRouter(sessionManager: ISessionManager): Router {
  const router = Router();

  // Health check endpoint
  router.get('/health', (_req: Request, res: Response) => {
    res.json({ status: 'ok', timestamp: new Date().toISOString() });
  });

  // Execute command in session
  router.post('/sessions/:name/execute', async (req: Request, res: Response, next: NextFunction) => {
    try {
      const { name } = req.params;
      const { command } = req.body as ApiExecuteRequest;

      if (!command || typeof command !== 'string') {
        return res.status(400).json({ error: 'Missing or invalid command' } as ApiErrorResponse);
      }

      const result = await sessionManager.executeCommand(name, command);

      const response: ApiExecuteResponse = {
        output: result.output,
        shouldExit: result.shouldExit,
      };

      res.json(response);
    } catch (error) {
      next(error);
    }
  });

  // List all sessions
  router.get('/sessions', async (_req: Request, res: Response, next: NextFunction) => {
    try {
      const sessions = sessionManager.listSessions();

      const response: ApiSessionListResponse = { sessions };

      res.json(response);
    } catch (error) {
      next(error);
    }
  });

  // Get session details
  router.get('/sessions/:name', async (req: Request, res: Response, next: NextFunction) => {
    try {
      const { name } = req.params;
      const session = await sessionManager.getSession(name);

      if (!session) {
        return res.status(404).json({ error: 'Session not found' } as ApiErrorResponse);
      }

      res.json(session.getMetadata());
    } catch (error) {
      next(error);
    }
  });

  // Force save session
  router.post('/sessions/:name/save', async (req: Request, res: Response, next: NextFunction) => {
    try {
      const { name } = req.params;
      const session = await sessionManager.getSession(name);

      if (!session) {
        return res.status(404).json({ error: 'Session not found' } as ApiErrorResponse);
      }

      await sessionManager.saveDirtySessions();

      res.json({ success: true });
    } catch (error) {
      next(error);
    }
  });

  // Error handling middleware
  router.use((error: Error, _req: Request, res: Response, _next: NextFunction) => {
    console.error('API Error:', error);

    const response: ApiErrorResponse = {
      error: error.message || 'Internal server error',
    };

    res.status(500).json(response);
  });

  return router;
}
```

## Testing

```typescript
// server/api-routes.spec.ts
import express from 'express';
import request from 'supertest';
import { createApiRouter } from './api-routes';
import type { ISessionManager, ISessionInstance, SessionMetadata, ExecuteResult } from './types';

describe('API Routes', () => {
  let app: express.Application;
  let mockSessionManager: ISessionManager;
  let mockSession: ISessionInstance;

  beforeEach(() => {
    mockSession = {
      execute: jest.fn(async () => ({ output: 'result', shouldExit: false })),
      getState: jest.fn(),
      getMetadata: () => ({
        id: 'test',
        name: 'test',
        created: new Date(),
        lastAccessed: new Date(),
        eventCount: 5,
        checkpointCount: 2,
        size: 1024,
      }),
      isDirty: () => false,
      markClean: jest.fn(),
      getLastAccessTime: () => Date.now(),
      getChangesSinceLastSave: () => [],
      updateSavePoint: jest.fn(),
    };

    mockSessionManager = {
      getSession: jest.fn(async () => mockSession),
      executeCommand: jest.fn(async () => ({ output: 'executed', shouldExit: false })),
      listSessions: jest.fn(() => [mockSession.getMetadata()]),
      saveDirtySessions: jest.fn(async () => {}),
      shutdown: jest.fn(async () => {}),
    };

    app = express();
    app.use(express.json());
    app.use('/api', createApiRouter(mockSessionManager));
  });

  describe('GET /api/health', () => {
    it('returns ok status', async () => {
      const response = await request(app).get('/api/health');

      expect(response.status).toBe(200);
      expect(response.body.status).toBe('ok');
    });
  });

  describe('POST /api/sessions/:name/execute', () => {
    it('executes command and returns result', async () => {
      const response = await request(app)
        .post('/api/sessions/test/execute')
        .send({ command: '(+ 1 2)' });

      expect(response.status).toBe(200);
      expect(response.body.output).toBe('executed');
      expect(mockSessionManager.executeCommand).toHaveBeenCalledWith('test', '(+ 1 2)');
    });

    it('returns 400 for missing command', async () => {
      const response = await request(app)
        .post('/api/sessions/test/execute')
        .send({});

      expect(response.status).toBe(400);
      expect(response.body.error).toContain('command');
    });
  });

  describe('GET /api/sessions', () => {
    it('returns list of sessions', async () => {
      const response = await request(app).get('/api/sessions');

      expect(response.status).toBe(200);
      expect(response.body.sessions).toHaveLength(1);
      expect(response.body.sessions[0].name).toBe('test');
    });
  });

  describe('GET /api/sessions/:name', () => {
    it('returns session metadata', async () => {
      const response = await request(app).get('/api/sessions/test');

      expect(response.status).toBe(200);
      expect(response.body.name).toBe('test');
      expect(response.body.eventCount).toBe(5);
    });
  });

  describe('POST /api/sessions/:name/save', () => {
    it('triggers save and returns success', async () => {
      const response = await request(app).post('/api/sessions/test/save');

      expect(response.status).toBe(200);
      expect(response.body.success).toBe(true);
      expect(mockSessionManager.saveDirtySessions).toHaveBeenCalled();
    });
  });
});
```

## Success Criteria

1. All endpoints respond correctly
2. Error handling works properly
3. Request validation is robust
4. All tests pass

## Estimated Effort

- Implementation: 1 hour
- Testing: 1 hour
