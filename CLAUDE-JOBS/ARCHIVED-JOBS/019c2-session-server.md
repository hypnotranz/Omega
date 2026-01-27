# JOB-019c2-session-server: Express Application

## Context

This job implements the main Session Server Express application that ties together all components.

## Goal

Create the main server application that:
1. Initializes all components (SessionManager, etc.)
2. Sets up Express with middleware
3. Mounts API routes
4. Handles graceful shutdown

## Dependencies

- `019-types` (for interfaces)
- `019a1-config` (for ServerConfig)
- `019b2-session-manager` (for SessionManager)
- `019c1-api-routes` (for API router)

## Blockers

- `019b2-session-manager` must be complete
- `019c1-api-routes` must be complete

## Files to Create

1. `server/session-server.ts` - Main Express application

## Implementation

```typescript
// server/session-server.ts
import express, { Application } from 'express';
import { loadConfig, validateConfig } from './config';
import { SessionManager } from './session-manager';
import { createApiRouter } from './api-routes';
import type { ServerConfig } from './types';

// Import from existing omega-repl.ts - these will be injected
import { processReplCommand, initReplState } from '../omega-repl';

export class SessionServer {
  private app: Application;
  private server: ReturnType<Application['listen']> | null = null;
  private sessionManager: SessionManager;
  private config: ServerConfig;

  constructor(config?: Partial<ServerConfig>) {
    this.config = { ...loadConfig(), ...config };

    // Validate configuration
    const errors = validateConfig(this.config);
    if (errors.length > 0) {
      throw new Error(`Invalid configuration: ${errors.join(', ')}`);
    }

    // Initialize session manager with command processor
    this.sessionManager = new SessionManager(
      this.config,
      processReplCommand,
      initReplState
    );

    // Create Express app
    this.app = express();
    this.setupMiddleware();
    this.setupRoutes();
  }

  private setupMiddleware(): void {
    this.app.use(express.json());

    // Request logging
    if (this.config.logLevel === 'debug') {
      this.app.use((req, _res, next) => {
        console.log(`${req.method} ${req.path}`);
        next();
      });
    }
  }

  private setupRoutes(): void {
    const apiRouter = createApiRouter(this.sessionManager);
    this.app.use('/api', apiRouter);
  }

  start(): Promise<void> {
    return new Promise((resolve, reject) => {
      try {
        this.server = this.app.listen(this.config.port, () => {
          console.log(`Session server running on port ${this.config.port}`);
          resolve();
        });

        this.server.on('error', (error: NodeJS.ErrnoException) => {
          if (error.code === 'EADDRINUSE') {
            reject(new Error(`Port ${this.config.port} is already in use`));
          } else {
            reject(error);
          }
        });
      } catch (error) {
        reject(error);
      }
    });
  }

  async stop(): Promise<void> {
    // Shutdown session manager first
    await this.sessionManager.shutdown();

    // Close HTTP server
    if (this.server) {
      return new Promise((resolve, reject) => {
        this.server!.close((error) => {
          if (error) {
            reject(error);
          } else {
            console.log('Session server stopped');
            resolve();
          }
        });
      });
    }
  }

  getApp(): Application {
    return this.app;
  }
}

// Main entry point when run directly
async function main(): Promise<void> {
  const server = new SessionServer();

  // Handle graceful shutdown
  const shutdown = async (signal: string) => {
    console.log(`\nReceived ${signal}, shutting down...`);
    await server.stop();
    process.exit(0);
  };

  process.on('SIGINT', () => shutdown('SIGINT'));
  process.on('SIGTERM', () => shutdown('SIGTERM'));

  try {
    await server.start();
  } catch (error) {
    console.error('Failed to start server:', error);
    process.exit(1);
  }
}

// Run if executed directly
if (require.main === module) {
  main();
}

export { main };
```

## Testing

```typescript
// server/session-server.spec.ts
import request from 'supertest';
import { SessionServer } from './session-server';

// Mock the omega-repl imports
jest.mock('../omega-repl', () => ({
  processReplCommand: jest.fn(async (cmd, state) => ({
    replState: state,
    output: `Executed: ${cmd}`,
    shouldExit: false,
  })),
  initReplState: jest.fn(async () => ({
    defs: [],
    debugMode: false,
    stepCount: 0,
    trace: [],
  })),
}));

describe('SessionServer', () => {
  let server: SessionServer;

  beforeEach(() => {
    server = new SessionServer({
      port: 0, // Use random available port
      sessionDir: '/tmp/test-sessions',
      saveIntervalMs: 10000,
    });
  });

  afterEach(async () => {
    await server.stop();
  });

  describe('getApp', () => {
    it('returns Express application', () => {
      const app = server.getApp();
      expect(app).toBeDefined();
    });
  });

  describe('API integration', () => {
    it('responds to health check', async () => {
      const app = server.getApp();

      const response = await request(app).get('/api/health');

      expect(response.status).toBe(200);
      expect(response.body.status).toBe('ok');
    });

    it('executes commands', async () => {
      const app = server.getApp();

      const response = await request(app)
        .post('/api/sessions/test/execute')
        .send({ command: '(+ 1 2)' });

      expect(response.status).toBe(200);
      expect(response.body.output).toContain('Executed');
    });

    it('lists sessions', async () => {
      const app = server.getApp();

      // Create a session first
      await request(app)
        .post('/api/sessions/test/execute')
        .send({ command: '(+ 1 2)' });

      const response = await request(app).get('/api/sessions');

      expect(response.status).toBe(200);
      expect(response.body.sessions).toBeDefined();
    });
  });

  describe('start and stop', () => {
    it('starts and stops cleanly', async () => {
      await server.start();
      await server.stop();
      // Should not throw
    });
  });
});
```

## Success Criteria

1. Server starts and listens on configured port
2. All API endpoints work correctly
3. Graceful shutdown saves all data
4. All tests pass

## Estimated Effort

- Implementation: 1 hour
- Testing: 1 hour
