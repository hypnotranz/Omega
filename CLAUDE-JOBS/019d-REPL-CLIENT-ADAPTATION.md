# JOB-019d: REPL Client Adaptation

## Context & Motivation

Building on the server implementation in JOB-019a through JOB-019c, this job focuses on adapting the existing REPL CLI client to communicate with the Session Server instead of directly accessing files. This is a critical part of the architecture that ensures users experience improved performance without any change to the CLI interface they're already familiar with.

## Goal

Modify the existing omega-repl.ts CLI client to:

1. Communicate with the Session Server for all session operations
2. Auto-start the server when needed
3. Maintain the exact same CLI interface and functionality
4. Handle communication errors gracefully
5. Provide a seamless user experience

## Technical Approach

### Client Adaptation

The adaptation strategy focuses on minimal changes to the existing codebase:

1. Create a SessionClient class for server communication
2. Replace file operations with API calls
3. Implement auto-start functionality
4. Handle connection and communication errors

### Implementation Details

The main implementation tasks are:

1. Create API client for server communication
2. Add server process management
3. Modify existing REPL code to use the client
4. Implement proper error handling

## Files to Create/Modify

1. `session-client.ts` - API client for server communication
2. `client-process-manager.ts` - Manages server process from client side
3. `omega-repl.ts` (modify) - Adapt to use SessionClient instead of direct file access

## Code Structure

```typescript
// session-client.ts
import axios from 'axios';
import { ClientProcessManager } from './client-process-manager';

export class SessionClient {
  private baseUrl: string;
  private initialized: boolean = false;
  private processManager: ClientProcessManager;
  
  constructor(port: number = 3000) {
    this.baseUrl = `http://localhost:${port}/api`;
    this.processManager = new ClientProcessManager();
  }
  
  async init(): Promise<void> {
    if (this.initialized) return;
    
    // Check if server is running, start if needed
    const serverRunning = await this.processManager.isServerRunning();
    
    if (!serverRunning) {
      console.log('Starting session server...');
      await this.processManager.startServer();
      
      // Wait for server to be ready
      await this.waitForServer();
    }
    
    this.initialized = true;
  }
  
  async executeCommand(sessionName: string, command: string): Promise<{ output: string, shouldExit: boolean }> {
    await this.init();
    
    try {
      const response = await axios.post(`${this.baseUrl}/sessions/${sessionName}/execute`, {
        command
      });
      
      return response.data;
    } catch (error) {
      if (axios.isAxiosError(error)) {
        if (error.code === 'ECONNREFUSED') {
          throw new Error('Failed to connect to session server. Server may be down.');
        }
        
        if (error.response) {
          throw new Error(`Server error: ${error.response.data.error || error.message}`);
        }
      }
      
      throw new Error(`Error executing command: ${error.message}`);
    }
  }
  
  async listSessions(): Promise<any[]> {
    await this.init();
    
    try {
      const response = await axios.get(`${this.baseUrl}/sessions`);
      return response.data;
    } catch (error) {
      console.error('Error listing sessions:', error.message);
      return [];
    }
  }
  
  async getSession(name: string): Promise<any> {
    await this.init();
    
    try {
      const response = await axios.get(`${this.baseUrl}/sessions/${name}`);
      return response.data;
    } catch (error) {
      if (axios.isAxiosError(error) && error.response?.status === 404) {
        return null;
      }
      
      console.error(`Error getting session ${name}:`, error.message);
      return null;
    }
  }
  
  async forceSave(sessionName: string): Promise<void> {
    await this.init();
    
    try {
      await axios.post(`${this.baseUrl}/sessions/${sessionName}/save`);
    } catch (error) {
      console.error(`Error saving session ${sessionName}:`, error.message);
    }
  }
  
  private async waitForServer(retries = 10, delay = 300): Promise<void> {
    for (let i = 0; i < retries; i++) {
      try {
        await axios.get(`${this.baseUrl}/health`);
        return; // Server is up
      } catch (error) {
        if (i === retries - 1) {
          throw new Error('Server failed to start or is unresponsive');
        }
        
        // Wait and retry
        await new Promise(resolve => setTimeout(resolve, delay));
      }
    }
  }
}
```

```typescript
// client-process-manager.ts
import { spawn, ChildProcess } from 'child_process';
import * as path from 'path';
import * as fs from 'fs';
import axios from 'axios';

export class ClientProcessManager {
  private serverPidFile: string;
  private serverScriptPath: string;
  
  constructor() {
    // PID file in user's home directory to track server process
    this.serverPidFile = path.join(process.env.HOME || process.env.USERPROFILE || '.', '.omega-session-server.pid');
    
    // Path to server script relative to this file
    this.serverScriptPath = path.resolve(__dirname, 'session-server.js');
  }
  
  async isServerRunning(): Promise<boolean> {
    // First check if we can connect to the server
    try {
      await axios.get('http://localhost:3000/api/health', { timeout: 500 });
      return true;
    } catch (error) {
      // Server not responding, check if process exists
      const pid = this.getServerPid();
      if (!pid) return false;
      
      try {
        // Check if process is running
        process.kill(pid, 0);
        return true;
      } catch (error) {
        // Process not running, clean up stale PID file
        this.cleanupStalePid();
        return false;
      }
    }
  }
  
  async startServer(): Promise<number> {
    // Check if TypeScript or JavaScript
    const scriptExt = fs.existsSync(this.serverScriptPath) ? 'js' : 'ts';
    const scriptPath = scriptExt === 'js' ? this.serverScriptPath : this.serverScriptPath.replace('.js', '.ts');
    
    const executor = scriptExt === 'js' ? 'node' : 'tsx';
    
    // Start server as detached process
    const child = spawn(executor, [scriptPath], {
      detached: true,
      stdio: 'ignore',
      env: {
        ...process.env,
        NODE_ENV: 'production'
      }
    });
    
    // Unref to allow parent to exit independently
    child.unref();
    
    // Save PID
    fs.writeFileSync(this.serverPidFile, child.pid.toString());
    
    return child.pid;
  }
  
  stopServer(): boolean {
    const pid = this.getServerPid();
    if (!pid) return false;
    
    try {
      process.kill(pid, 'SIGTERM');
      this.cleanupStalePid();
      return true;
    } catch (error) {
      console.error('Failed to stop server:', error.message);
      return false;
    }
  }
  
  private getServerPid(): number | null {
    if (!fs.existsSync(this.serverPidFile)) {
      return null;
    }
    
    const pid = parseInt(fs.readFileSync(this.serverPidFile, 'utf8'), 10);
    return isNaN(pid) ? null : pid;
  }
  
  private cleanupStalePid(): void {
    if (fs.existsSync(this.serverPidFile)) {
      try {
        fs.unlinkSync(this.serverPidFile);
      } catch (error) {
        console.error('Failed to clean up PID file:', error.message);
      }
    }
  }
}
```

Now let's modify the existing `omega-repl.ts` file to use our new client:

```typescript
// omega-repl.ts (partial modifications)

// Import our new client
import { SessionClient } from './session-client';

// Create client instance
const sessionClient = new SessionClient();

// Replace loadSession function
async function loadSession(name: string): Promise<ReplState | null> {
  try {
    const session = await sessionClient.getSession(name);
    
    if (!session) {
      return null;
    }
    
    // We need to convert the session data from the server to ReplState format
    // This is a placeholder implementation - the actual one would map properly
    let replState = await initReplState();
    
    // Apply session data to replState
    // This would need to be implemented based on the server response format
    
    return replState;
  } catch (error) {
    console.error(`Error loading session ${name}:`, error.message);
    return null;
  }
}

// Replace saveSession function
function saveSession(replState: ReplState, name: string): void {
  // We'll do this asynchronously but won't wait for it
  sessionClient.forceSave(name).catch(err => {
    console.error(`Error saving session ${name}:`, err.message);
  });
}

// Modify main function to use client for session commands
async function main() {
  const args = parseArgs();
  
  // Session mode: run command(s) against a named session
  if (args.session) {
    try {
      // Execute command via session client
      const { output, shouldExit } = await sessionClient.executeCommand(
        args.session,
        args.cmd || ':info'
      );
      
      // Output result
      if (args.json) {
        const result = {
          session: args.session,
          command: args.cmd,
          output,
        };
        console.log(JSON.stringify(result, null, 2));
      } else {
        if (output) console.log(output);
      }
      
      if (shouldExit) {
        return;
      }
    } catch (error) {
      console.error(`Session error: ${error.message}`);
      process.exit(1);
    }
    
    return;
  }
  
  // Rest of the function remains the same...
}

// Modify processReplCommand to use client for session commands
async function processReplCommand(
  trimmed: string,
  replState: ReplState
): Promise<{ replState: ReplState; output: string; shouldExit: boolean }> {
  // For session-specific commands, use the client
  if (trimmed === ":session list") {
    try {
      const sessions = await sessionClient.listSessions();
      return {
        replState,
        output: formatSessionList(sessions),
        shouldExit: false
      };
    } catch (error) {
      return {
        replState,
        output: `Error listing sessions: ${error.message}`,
        shouldExit: false
      };
    }
  }
  
  // Handle other session commands similarly...
  
  // For non-session commands, use the existing implementation
  // Rest of the function remains the same...
}

// Helper function to format session list
function formatSessionList(sessions: any[]): string {
  if (sessions.length === 0) {
    return "No saved sessions.";
  }
  
  const lines = ["Saved sessions:"];
  
  for (const session of sessions) {
    lines.push(`  ${session.name} (${session.eventCount ?? 0} events, ${session.checkpointCount ?? 0} checkpoints)`);
  }
  
  return lines.join("\n");
}
```

## Testing Strategy

1. Test all session commands through the client
2. Verify auto-start functionality
3. Test error handling scenarios
4. Ensure backward compatibility

## Success Criteria

1. All session commands work correctly through the client
2. Same CLI interface is maintained
3. Performance is significantly improved
4. Server auto-start works reliably
5. Error handling is robust

## Dependencies

1. JOB-019a (Session Server Core)
2. JOB-019b (Session Manager and Pool)
3. JOB-019c (Optimized Persistence)
4. Axios or similar HTTP client library

## Next Steps

After completing this job, the final step will be:

1. JOB-019e: Advanced Features
