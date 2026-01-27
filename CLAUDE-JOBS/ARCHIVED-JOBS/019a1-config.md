# JOB-019a1-config: Server Configuration

## Context

This job implements the server configuration module that provides environment-based configuration for the Session Server.

## Goal

Create a configuration module that:
1. Loads configuration from environment variables
2. Provides sensible defaults
3. Validates configuration values
4. Exports a typed configuration object

## Dependencies

- `019-types` (for `ServerConfig` interface)

## Blockers

- None

## Files to Create

1. `server/config.ts` - Configuration module

## Implementation

```typescript
// server/config.ts
import type { ServerConfig } from './types';

function getEnvNumber(key: string, defaultValue: number): number {
  const value = process.env[key];
  if (value === undefined) return defaultValue;
  const parsed = parseInt(value, 10);
  return isNaN(parsed) ? defaultValue : parsed;
}

function getEnvString(key: string, defaultValue: string): string {
  return process.env[key] || defaultValue;
}

function getEnvLogLevel(key: string, defaultValue: 'debug' | 'info' | 'warn' | 'error'): 'debug' | 'info' | 'warn' | 'error' {
  const value = process.env[key];
  if (value === 'debug' || value === 'info' || value === 'warn' || value === 'error') {
    return value;
  }
  return defaultValue;
}

export function loadConfig(): ServerConfig {
  return {
    port: getEnvNumber('SESSION_SERVER_PORT', 3000),
    sessionDir: getEnvString('SESSION_DIR', '.omega-session'),
    maxCachedSessions: getEnvNumber('MAX_CACHED_SESSIONS', 10),
    saveIntervalMs: getEnvNumber('SAVE_INTERVAL_MS', 5000),
    logLevel: getEnvLogLevel('LOG_LEVEL', 'info'),
  };
}

export function validateConfig(config: ServerConfig): string[] {
  const errors: string[] = [];
  
  if (config.port < 1 || config.port > 65535) {
    errors.push(`Invalid port: ${config.port}`);
  }
  
  if (config.maxCachedSessions < 1) {
    errors.push(`maxCachedSessions must be at least 1`);
  }
  
  if (config.saveIntervalMs < 100) {
    errors.push(`saveIntervalMs must be at least 100ms`);
  }
  
  return errors;
}

// Default export for convenience
export const config = loadConfig();
```

## Testing

```typescript
// server/config.spec.ts
import { loadConfig, validateConfig } from './config';

describe('Config', () => {
  const originalEnv = process.env;

  beforeEach(() => {
    process.env = { ...originalEnv };
  });

  afterAll(() => {
    process.env = originalEnv;
  });

  describe('loadConfig', () => {
    it('returns default values when no env vars set', () => {
      delete process.env.SESSION_SERVER_PORT;
      delete process.env.SESSION_DIR;
      
      const config = loadConfig();
      
      expect(config.port).toBe(3000);
      expect(config.sessionDir).toBe('.omega-session');
      expect(config.maxCachedSessions).toBe(10);
      expect(config.saveIntervalMs).toBe(5000);
      expect(config.logLevel).toBe('info');
    });

    it('reads values from environment', () => {
      process.env.SESSION_SERVER_PORT = '4000';
      process.env.SESSION_DIR = '/custom/path';
      process.env.MAX_CACHED_SESSIONS = '20';
      process.env.LOG_LEVEL = 'debug';
      
      const config = loadConfig();
      
      expect(config.port).toBe(4000);
      expect(config.sessionDir).toBe('/custom/path');
      expect(config.maxCachedSessions).toBe(20);
      expect(config.logLevel).toBe('debug');
    });

    it('handles invalid number values gracefully', () => {
      process.env.SESSION_SERVER_PORT = 'not-a-number';
      
      const config = loadConfig();
      
      expect(config.port).toBe(3000); // Falls back to default
    });
  });

  describe('validateConfig', () => {
    it('returns empty array for valid config', () => {
      const config = loadConfig();
      const errors = validateConfig(config);
      expect(errors).toEqual([]);
    });

    it('returns error for invalid port', () => {
      const config = { ...loadConfig(), port: 0 };
      const errors = validateConfig(config);
      expect(errors).toContain('Invalid port: 0');
    });

    it('returns error for invalid maxCachedSessions', () => {
      const config = { ...loadConfig(), maxCachedSessions: 0 };
      const errors = validateConfig(config);
      expect(errors.some(e => e.includes('maxCachedSessions'))).toBe(true);
    });
  });
});
```

## Success Criteria

1. Configuration loads from environment variables
2. Sensible defaults are provided
3. Validation catches invalid values
4. All tests pass

## Estimated Effort

- Implementation: 30 minutes
- Testing: 30 minutes
