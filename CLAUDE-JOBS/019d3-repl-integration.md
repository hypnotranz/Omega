# JOB-019d3-repl-integration: Adapt omega-repl.ts

## Context

This job adapts the existing omega-repl.ts to use the Session Server for session operations instead of direct file access.

## Goal

Modify omega-repl.ts to:
1. Use SessionClient for session operations when server is available
2. Auto-start server via ClientProcessManager
3. Maintain backward compatibility with existing CLI interface
4. Fall back to direct file access if server unavailable

## Dependencies

- `019d1-session-client` (for SessionClient)
- `019d2-client-process-manager` (for ClientProcessManager)

## Blockers

- `019d1-session-client` must be complete
- `019d2-client-process-manager` must be complete

## Files to Modify

1. `bin/omega-repl.ts` - Integrate with Session Server

## Implementation

The changes to omega-repl.ts are minimal and focused:

```typescript
// Add imports at top of omega-repl.ts
import { SessionClient } from '../server/session-client';
import { ClientProcessManager } from '../server/client-process-manager';

// Add client initialization
let sessionClient: SessionClient | null = null;
let clientProcessManager: ClientProcessManager | null = null;

async function initializeClient(): Promise<SessionClient | null> {
  if (sessionClient) return sessionClient;

  try {
    clientProcessManager = new ClientProcessManager();
    await clientProcessManager.ensureServerRunning();

    sessionClient = new SessionClient();
    await sessionClient.init();

    return sessionClient;
  } catch (error) {
    console.error('Failed to connect to session server, using direct mode');
    return null;
  }
}

// Modify session command handling in processReplCommand
// Replace the `:session list` handler:

if (trimmed === ":session list") {
  const client = await initializeClient();
  
  if (client) {
    // Use server
    try {
      const sessions = await client.listSessions();
      if (sessions.length === 0) {
        log("No saved sessions.");
      } else {
        log("Saved sessions:");
        for (const s of sessions) {
          log(`  ${s.name} (${s.eventCount ?? 0} events, ${s.checkpointCount ?? 0} checkpoints)`);
        }
      }
      return { replState, output: output.join("\n"), shouldExit };
    } catch (error) {
      log(`Error: ${error.message}`);
      return { replState, output: output.join("\n"), shouldExit };
    }
  }
  
  // Fall back to direct file access (existing code)
  // ... existing implementation ...
}

// Similar pattern for other session commands:
// - :session save
// - :session load
// - :session fork
// - :session goto
// - :session trace
// - :session checkpoints
// - :session resume
```

## Detailed Changes

### 1. Add Server Mode Detection

```typescript
// Near top of file, after imports
const USE_SERVER = process.env.OMEGA_USE_SERVER !== 'false';

async function getSessionClient(): Promise<SessionClient | null> {
  if (!USE_SERVER) return null;
  return initializeClient();
}
```

### 2. Modify Main Function for Server Mode

```typescript
// In main() function, modify the session handling:

async function main() {
  const args = parseArgs();

  // Try to use server for session operations
  if (args.session && USE_SERVER) {
    const client = await getSessionClient();
    
    if (client && args.cmd) {
      try {
        const result = await client.executeCommand(args.session, args.cmd);
        
        if (args.json) {
          console.log(JSON.stringify({
            session: args.session,
            command: args.cmd,
            output: result.output,
          }, null, 2));
        } else {
          if (result.output) console.log(result.output);
        }
        return;
      } catch (error) {
        console.error(`Server error: ${error.message}`);
        // Fall through to direct mode
      }
    }
  }

  // ... rest of existing main() implementation ...
}
```

### 3. Add Server Control Commands

```typescript
// Add new REPL commands for server management

if (trimmed === ":server status") {
  const client = await getSessionClient();
  if (client) {
    const healthy = await client.healthCheck();
    log(healthy ? "Server is running" : "Server is not responding");
  } else {
    log("Server mode disabled");
  }
  return { replState, output: output.join("\n"), shouldExit };
}

if (trimmed === ":server stop") {
  if (clientProcessManager) {
    await clientProcessManager.stopServer();
    log("Server stopped");
  } else {
    log("No server to stop");
  }
  return { replState, output: output.join("\n"), shouldExit };
}
```

## Testing

```typescript
// test/repl-integration.spec.ts
import { processReplCommand, initReplState } from '../bin/omega-repl';

// Mock the server modules
jest.mock('../server/session-client');
jest.mock('../server/client-process-manager');

describe('REPL Server Integration', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('with server available', () => {
    it('uses server for :session list', async () => {
      const mockClient = {
        listSessions: jest.fn().mockResolvedValue([
          { name: 'test', eventCount: 5, checkpointCount: 2 }
        ]),
        healthCheck: jest.fn().mockResolvedValue(true),
      };

      // Setup mocks...

      const replState = await initReplState();
      const result = await processReplCommand(':session list', replState);

      expect(result.output).toContain('test');
      expect(result.output).toContain('5 events');
    });
  });

  describe('with server unavailable', () => {
    it('falls back to direct file access', async () => {
      // Mock server as unavailable
      // Verify direct file access is used
    });
  });

  describe('server control commands', () => {
    it(':server status reports server state', async () => {
      const replState = await initReplState();
      const result = await processReplCommand(':server status', replState);

      expect(result.output).toMatch(/running|disabled/i);
    });
  });
});
```

## Migration Notes

1. **Environment Variable**: Set `OMEGA_USE_SERVER=false` to disable server mode
2. **Backward Compatibility**: All existing commands work unchanged
3. **Graceful Degradation**: Falls back to direct mode if server unavailable

## Success Criteria

1. Session commands work via server when available
2. Falls back gracefully to direct mode
3. No breaking changes to existing CLI
4. All tests pass

## Estimated Effort

- Implementation: 2 hours
- Testing: 1.5 hours
