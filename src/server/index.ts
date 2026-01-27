/**
 * @package OmegaLLM Debug Server
 *
 * This is the PUBLIC API for the debug server.
 * Everything exported here is part of the stable contract.
 *
 * PACKAGE BOUNDARY - Only these exports are public:
 *
 * TYPES (for consumers):
 *   - IDebugService         - The service interface contract
 *   - MachineSnapshot       - Full machine state at a point in time
 *   - StepResult            - Result of stepping execution
 *   - Breakpoint            - Breakpoint configuration
 *   - SessionConfig         - Session creation options
 *   - SerializedValue       - JSON-safe value representation
 *   - SerializedFrame       - JSON-safe stack frame
 *   - SerializedControl     - JSON-safe control register
 *   - SerializedBinding     - JSON-safe environment binding
 *   - ServerEvent           - WebSocket events (server -> client)
 *   - ClientCommand         - WebSocket commands (client -> server)
 *
 * IMPLEMENTATION (for running the server):
 *   - DebugServer           - The HTTP/WebSocket server
 *   - startDebugServer()    - Quick start function
 */

// ============================================================
// PUBLIC TYPE EXPORTS - The Contract
// ============================================================

export type {
  // Core service interface
  IDebugService,

  // Session types
  SessionConfig,

  // Machine state types
  MachineSnapshot,
  SerializedControl,
  SerializedBinding,
  SerializedValue,
  SerializedFrame,
  SerializedHandler,
  SerializedStoreEntry,

  // Execution types
  StepResult,
  Breakpoint,

  // WebSocket protocol
  ServerEvent,
  ClientCommand,
} from './debugService';

// ============================================================
// PUBLIC CLASS EXPORTS - The Implementation
// ============================================================

export { DebugServer } from './debugServer';

// ============================================================
// CONVENIENCE FUNCTIONS
// ============================================================

import { DebugServer } from './debugServer';

/**
 * Start a debug server on the specified port.
 *
 * @example
 * ```typescript
 * import { startDebugServer } from 'omegallm/server';
 *
 * const server = await startDebugServer(3456);
 * // Server now running at http://localhost:3456
 * // WebSocket at ws://localhost:3456/ws?session=<id>
 * ```
 */
export async function startDebugServer(port = 3456): Promise<DebugServer> {
  const server = new DebugServer(port);
  await server.start();
  return server;
}

// ============================================================
// API DOCUMENTATION
// ============================================================

/**
 * # OmegaLLM Debug Server API
 *
 * ## REST Endpoints
 *
 * ### Session Management
 * - POST   /session              - Create new session
 * - GET    /sessions             - List all sessions
 * - GET    /session/:id          - Get session info
 * - DELETE /session/:id          - Close session
 *
 * ### Code Execution
 * - POST   /session/:id/load     - Load code (body: { code: string })
 * - POST   /session/:id/step     - Execute single step
 * - POST   /session/:id/step/:n  - Execute N steps
 * - POST   /session/:id/continue - Run until breakpoint/effect/done
 * - POST   /session/:id/run      - Run to completion
 * - POST   /session/:id/resume   - Resume from effect (body: { value: any })
 *
 * ### State Inspection
 * - GET    /session/:id/snapshot     - Get current machine state
 * - GET    /session/:id/stack        - Get call stack
 * - GET    /session/:id/binding/:name - Get specific binding
 * - POST   /session/:id/eval         - Evaluate expression (body: { expr: string })
 *
 * ### Breakpoints
 * - POST   /session/:id/breakpoint      - Add breakpoint
 * - DELETE /session/:id/breakpoint/:id  - Remove breakpoint
 * - GET    /session/:id/breakpoints     - List breakpoints
 *
 * ### Time Travel
 * - POST   /session/:id/jump/:step  - Jump to step
 * - GET    /session/:id/history     - Get execution history
 *
 * ### OPR Kernels
 * - GET    /kernels                - List available kernels
 * - POST   /kernel/:id/execute     - Execute kernel (body: { program: any })
 *
 * ## WebSocket Protocol
 *
 * Connect to: ws://localhost:PORT/ws?session=SESSION_ID
 *
 * ### Server Events (ServerEvent)
 * - { type: 'snapshot', snapshot: MachineSnapshot }
 * - { type: 'breakpointHit', breakpointId: string, snapshot: MachineSnapshot }
 * - { type: 'effectPending', effect: { op, args }, snapshot: MachineSnapshot }
 * - { type: 'done', result: SerializedValue, snapshot: MachineSnapshot }
 * - { type: 'error', error: { message, stack? }, snapshot: MachineSnapshot }
 *
 * ### Client Commands (ClientCommand)
 * - { type: 'step' }
 * - { type: 'stepN', n: number }
 * - { type: 'continue' }
 * - { type: 'run', maxSteps?: number }
 * - { type: 'pause' }
 * - { type: 'resume', value: any }
 * - { type: 'evaluate', expr: string }
 * - { type: 'addBreakpoint', breakpoint: {...} }
 * - { type: 'removeBreakpoint', breakpointId: string }
 * - { type: 'jumpToStep', step: number }
 */
