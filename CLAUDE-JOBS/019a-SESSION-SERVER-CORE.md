# JOB-019a: Session Server Core

## Context & Motivation

As outlined in the main architecture document (JOB-019), the OmegaLLM REPL needs a stateful Session Server to improve performance. This sub-job focuses on implementing the core server functionality that will serve as the foundation for the entire system.

## Goal

Implement the core Session Server with basic API endpoints and process management. This includes:

1. Creating the server application
2. Implementing core API endpoints
3. Setting up process management
4. Establishing the basic communication protocol

## Technical Approach

### Server Implementation

We'll build a Node.js server using Express.js for the API layer. The server will:

1. Handle incoming HTTP requests
2. Support basic session operations
3. Implement proper error handling
4. Include logging for debugging

### Core API Endpoints

Implement the following minimal set of endpoints:

```
POST /sessions/:name/execute
  - Execute command in session context
  - Returns command result

GET /sessions
  - List available sessions

GET /sessions/:name
  - Get session details
```

These endpoints provide the minimal functionality needed for basic REPL operations.

### Process Management

Implement a process management system that:

1. Allows the server to run as a background process
2. Provides a way to start/stop the server
3. Handles graceful shutdown
4. Implements proper error handling

## Files to Create

1. `session-server.ts` - Main server application
2. `process-manager.ts` - Process management utilities
3. `api-routes.ts` - API endpoint definitions
4. `session-service.ts` - Core session operations logic
5. `server-config.ts` - Configuration settings

## Code Structure

```typescript
// session-server.ts (Main server file)
import express from 'express';
import { apiRouter } from './api-routes';

const app = express();
const PORT = process.env.PORT || 3000;

app.use(express.json());
app.use('/api', apiRouter);

app.listen(PORT, () => {
  console.log(`Session server running on port ${PORT}`);
});

// Graceful shutdown handling
process.on('SIGINT', () => {
  console.log('Shutting down session server...');
  // Clean up resources
  process.exit(0);
});
```

```typescript
// api-routes.ts
import express from 'express';
import { SessionService } from './session-service';

const router = express.Router();
const sessionService = new SessionService();

// Execute command in session
router.post('/sessions/:name/execute', async (req, res) => {
  try {
    const { name } = req.params;
    const { command } = req.body;
    
    const result = await sessionService.executeCommand(name, command);
    res.json(result);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// List sessions
router.get('/sessions', async (req, res) => {
  try {
    const sessions = await sessionService.listSessions();
    res.json(sessions);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// Get session details
router.get('/sessions/:name', async (req, res) => {
  try {
    const { name } = req.params;
    const session = await sessionService.getSession(name);
    
    if (!session) {
      return res.status(404).json({ error: 'Session not found' });
    }
    
    res.json(session);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

export const apiRouter = router;
```

```typescript
// session-service.ts (Basic skeleton)
export class SessionService {
  private sessions = new Map();
  
  async executeCommand(sessionName: string, command: string) {
    // Get or create session
    const session = await this.getOrCreateSession(sessionName);
    
    // Execute command in session context
    const result = await session.execute(command);
    
    // Queue save (to be implemented in JOB-019c)
    
    return result;
  }
  
  async listSessions() {
    // List available sessions (minimal implementation)
    // Will be enhanced in JOB-019b
    return [];
  }
  
  async getSession(name: string) {
    // Get session details (minimal implementation)
    // Will be enhanced in JOB-019b
    return null;
  }
  
  private async getOrCreateSession(name: string) {
    // Minimal implementation
    // Will be enhanced in JOB-019b
    return {
      execute: async (command) => {
        // Placeholder for command execution
        return { output: 'Command executed' };
      }
    };
  }
}
```

```typescript
// process-manager.ts
import { spawn } from 'child_process';
import * as fs from 'fs';
import * as path from 'path';

export class ProcessManager {
  static async startServer(options = {}) {
    const serverPath = path.resolve(__dirname, 'session-server.js');
    
    // Check if server is already running
    if (await ProcessManager.isServerRunning()) {
      console.log('Server is already running');
      return;
    }
    
    // Start server as detached process
    const child = spawn('node', [serverPath], {
      detached: true,
      stdio: 'ignore',
      env: { ...process.env, ...options }
    });
    
    child.unref();
    
    // Write PID file
    const pidFile = path.resolve(__dirname, '.server.pid');
    fs.writeFileSync(pidFile, child.pid.toString());
    
    return child.pid;
  }
  
  static async stopServer() {
    const pid = await ProcessManager.getServerPid();
    
    if (!pid) {
      console.log('Server is not running');
      return;
    }
    
    try {
      process.kill(pid, 'SIGINT');
      console.log(`Sent SIGINT to server process ${pid}`);
      
      // Clean up PID file
      const pidFile = path.resolve(__dirname, '.server.pid');
      fs.unlinkSync(pidFile);
    } catch (error) {
      console.error(`Failed to stop server: ${error.message}`);
    }
  }
  
  static async isServerRunning() {
    const pid = await ProcessManager.getServerPid();
    
    if (!pid) {
      return false;
    }
    
    try {
      // Check if process is running
      process.kill(pid, 0);
      return true;
    } catch (error) {
      // Process is not running
      return false;
    }
  }
  
  static async getServerPid() {
    const pidFile = path.resolve(__dirname, '.server.pid');
    
    if (!fs.existsSync(pidFile)) {
      return null;
    }
    
    const pid = parseInt(fs.readFileSync(pidFile, 'utf8'), 10);
    return isNaN(pid) ? null : pid;
  }
}
```

```typescript
// server-config.ts
export const ServerConfig = {
  port: process.env.SESSION_SERVER_PORT || 3000,
  sessionDir: process.env.SESSION_DIR || '.omega-session',
  maxCachedSessions: 10,
  saveInterval: 5000, // ms
  logLevel: 'info',
};
```

## Testing Strategy

1. Unit tests for each component
2. API tests using a test client
3. Process management tests
4. End-to-end tests with simple commands

## Success Criteria

1. Server can start and stop cleanly
2. API endpoints respond correctly
3. Basic session operations work
4. Error handling is robust

## Dependencies

- Express.js for HTTP API
- TypeScript (existing in the project)

## Next Steps

After completing this job, the next steps will be:

1. JOB-019b: Session Manager and Pool
2. JOB-019c: Optimized Persistence
3. JOB-019d: REPL Client Adaptation
