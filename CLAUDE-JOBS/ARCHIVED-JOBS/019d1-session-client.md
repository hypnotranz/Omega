# JOB-019d1-session-client: HTTP Client for Server Communication

## Context

This job implements the `SessionClient` class that provides the client-side API for communicating with the Session Server.

## Goal

Create a `SessionClient` class that:
1. Communicates with Session Server via HTTP
2. Handles connection errors gracefully
3. Implements retry logic
4. Implements `ISessionClient` interface

## Dependencies

- `019-types` (for interfaces)

## Blockers

- None (can develop against API contract before server is complete)

## Files to Create

1. `server/session-client.ts` - HTTP client implementation

## Implementation

```typescript
// server/session-client.ts
import type { 
  ISessionClient, 
  ExecuteResult, 
  SessionMetadata,
  ApiExecuteResponse,
  ApiSessionListResponse 
} from './types';

export interface SessionClientOptions {
  baseUrl?: string;
  port?: number;
  timeout?: number;
  retries?: number;
  retryDelay?: number;
}

export class SessionClient implements ISessionClient {
  private baseUrl: string;
  private timeout: number;
  private retries: number;
  private retryDelay: number;
  private initialized = false;

  constructor(options: SessionClientOptions = {}) {
    const port = options.port || 3000;
    this.baseUrl = options.baseUrl || `http://localhost:${port}/api`;
    this.timeout = options.timeout || 30000;
    this.retries = options.retries || 3;
    this.retryDelay = options.retryDelay || 300;
  }

  async init(): Promise<void> {
    if (this.initialized) return;

    // Verify server is reachable
    const healthy = await this.healthCheck();
    if (!healthy) {
      throw new Error('Session server is not reachable');
    }

    this.initialized = true;
  }

  async executeCommand(sessionName: string, command: string): Promise<ExecuteResult> {
    const response = await this.request<ApiExecuteResponse>(
      `/sessions/${encodeURIComponent(sessionName)}/execute`,
      {
        method: 'POST',
        body: JSON.stringify({ command }),
      }
    );

    return {
      output: response.output,
      shouldExit: response.shouldExit,
    };
  }

  async listSessions(): Promise<SessionMetadata[]> {
    const response = await this.request<ApiSessionListResponse>('/sessions');
    return response.sessions;
  }

  async getSession(name: string): Promise<SessionMetadata | null> {
    try {
      const response = await this.request<SessionMetadata>(
        `/sessions/${encodeURIComponent(name)}`
      );
      return response;
    } catch (error: any) {
      if (error.status === 404) {
        return null;
      }
      throw error;
    }
  }

  async forceSave(sessionName: string): Promise<void> {
    await this.request(
      `/sessions/${encodeURIComponent(sessionName)}/save`,
      { method: 'POST' }
    );
  }

  async healthCheck(): Promise<boolean> {
    try {
      await this.request('/health', { timeout: 1000 });
      return true;
    } catch {
      return false;
    }
  }

  private async request<T>(
    path: string,
    options: RequestInit & { timeout?: number } = {}
  ): Promise<T> {
    const url = `${this.baseUrl}${path}`;
    const timeout = options.timeout || this.timeout;

    let lastError: Error | null = null;

    for (let attempt = 0; attempt <= this.retries; attempt++) {
      try {
        const controller = new AbortController();
        const timeoutId = setTimeout(() => controller.abort(), timeout);

        const response = await fetch(url, {
          ...options,
          headers: {
            'Content-Type': 'application/json',
            ...options.headers,
          },
          signal: controller.signal,
        });

        clearTimeout(timeoutId);

        if (!response.ok) {
          const error = new Error(`HTTP ${response.status}: ${response.statusText}`) as any;
          error.status = response.status;
          
          try {
            const body = await response.json();
            error.message = body.error || error.message;
          } catch {}

          throw error;
        }

        return await response.json();
      } catch (error: any) {
        lastError = error;

        // Don't retry on client errors (4xx)
        if (error.status && error.status >= 400 && error.status < 500) {
          throw error;
        }

        // Don't retry on last attempt
        if (attempt < this.retries) {
          await new Promise(r => setTimeout(r, this.retryDelay * (attempt + 1)));
        }
      }
    }

    throw lastError || new Error('Request failed');
  }
}
```

## Testing

```typescript
// server/session-client.spec.ts
import { SessionClient } from './session-client';

// Mock fetch globally
const mockFetch = jest.fn();
global.fetch = mockFetch;

describe('SessionClient', () => {
  let client: SessionClient;

  beforeEach(() => {
    mockFetch.mockReset();
    client = new SessionClient({ port: 3000, retries: 1, retryDelay: 10 });
  });

  describe('healthCheck', () => {
    it('returns true when server responds ok', async () => {
      mockFetch.mockResolvedValueOnce({
        ok: true,
        json: async () => ({ status: 'ok' }),
      });

      const result = await client.healthCheck();

      expect(result).toBe(true);
    });

    it('returns false when server is unreachable', async () => {
      mockFetch.mockRejectedValueOnce(new Error('Connection refused'));

      const result = await client.healthCheck();

      expect(result).toBe(false);
    });
  });

  describe('executeCommand', () => {
    it('sends command and returns result', async () => {
      mockFetch.mockResolvedValueOnce({
        ok: true,
        json: async () => ({ output: '=> 3', shouldExit: false }),
      });

      const result = await client.executeCommand('test', '(+ 1 2)');

      expect(result.output).toBe('=> 3');
      expect(result.shouldExit).toBe(false);
      expect(mockFetch).toHaveBeenCalledWith(
        'http://localhost:3000/api/sessions/test/execute',
        expect.objectContaining({
          method: 'POST',
          body: JSON.stringify({ command: '(+ 1 2)' }),
        })
      );
    });

    it('retries on network error', async () => {
      mockFetch
        .mockRejectedValueOnce(new Error('Network error'))
        .mockResolvedValueOnce({
          ok: true,
          json: async () => ({ output: 'ok', shouldExit: false }),
        });

      const result = await client.executeCommand('test', 'cmd');

      expect(result.output).toBe('ok');
      expect(mockFetch).toHaveBeenCalledTimes(2);
    });
  });

  describe('listSessions', () => {
    it('returns list of sessions', async () => {
      const sessions = [
        { id: 'test', name: 'test', eventCount: 5 },
      ];

      mockFetch.mockResolvedValueOnce({
        ok: true,
        json: async () => ({ sessions }),
      });

      const result = await client.listSessions();

      expect(result).toEqual(sessions);
    });
  });

  describe('getSession', () => {
    it('returns session metadata', async () => {
      const session = { id: 'test', name: 'test', eventCount: 5 };

      mockFetch.mockResolvedValueOnce({
        ok: true,
        json: async () => session,
      });

      const result = await client.getSession('test');

      expect(result).toEqual(session);
    });

    it('returns null for 404', async () => {
      const error = { status: 404, ok: false, statusText: 'Not Found' };
      mockFetch.mockResolvedValueOnce({
        ...error,
        json: async () => ({ error: 'Not found' }),
      });

      const result = await client.getSession('nonexistent');

      expect(result).toBeNull();
    });
  });

  describe('forceSave', () => {
    it('sends save request', async () => {
      mockFetch.mockResolvedValueOnce({
        ok: true,
        json: async () => ({ success: true }),
      });

      await client.forceSave('test');

      expect(mockFetch).toHaveBeenCalledWith(
        'http://localhost:3000/api/sessions/test/save',
        expect.objectContaining({ method: 'POST' })
      );
    });
  });
});
```

## Success Criteria

1. All API methods work correctly
2. Retry logic handles transient failures
3. Error handling is robust
4. All tests pass

## Estimated Effort

- Implementation: 1 hour
- Testing: 1 hour
