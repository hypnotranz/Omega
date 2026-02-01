# 1110: Server Refactor

## Status: COMPLETE ✅
- **Completed**: 2026-01-30
- **Location**: OmegaLLM-3/src/server/debugSession.ts (523 lines)
- **Reduction**: 1180 → 523 lines (56% reduction)

## Purpose
Refactor debugSession.ts to use OmegaRuntime, enabling clean Debug Adapter Protocol implementation.

## Dependencies
- 1000-runtime-assembly.md ✅

## Source References
- src/server/debugSession.ts (current implementation)
- VS Code Debug Adapter Protocol specification
- Actual reduction: 1180 → 523 lines

---

## Current State

The current `debugSession.ts` contains:
- Duplicated evaluation logic
- Manual state management
- Inline breakpoint handling
- Direct CESK manipulation

---

## Target Architecture

```typescript
// src/server/debugSession.ts (simplified)

import {
  DebugSession,
  InitializedEvent,
  StoppedEvent,
  TerminatedEvent,
  BreakpointEvent,
  OutputEvent,
  Thread,
  StackFrame,
  Scope,
  Variable
} from '@vscode/debugadapter';
import { DebugProtocol } from '@vscode/debugprotocol';
import { createRuntime, OmegaRuntime } from '../runtime';

export class OmegaDebugSession extends DebugSession {
  private runtime: OmegaRuntime;

  constructor() {
    super();
  }

  protected async initializeRequest(
    response: DebugProtocol.InitializeResponse
  ): Promise<void> {
    // Report capabilities from runtime
    const capabilities = this.runtime?.debug.getCapabilities() ?? {};

    response.body = {
      supportsConfigurationDoneRequest: true,
      supportsFunctionBreakpoints: capabilities.supportsFunctionBreakpoints,
      supportsConditionalBreakpoints: capabilities.supportsConditionalBreakpoints,
      supportsHitConditionalBreakpoints: capabilities.supportsHitConditionalBreakpoints,
      supportsEvaluateForHovers: capabilities.supportsEvaluateForHovers,
      supportsStepBack: capabilities.supportsStepBack,
      supportsSetVariable: capabilities.supportsSetVariable,
      supportsRestartFrame: capabilities.supportsRestartFrame,
      supportsGotoTargetsRequest: capabilities.supportsGotoTargetsRequest,
      supportsCompletionsRequest: capabilities.supportsCompletionsRequest,
    };

    this.sendResponse(response);
    this.sendEvent(new InitializedEvent());
  }

  protected async launchRequest(
    response: DebugProtocol.LaunchResponse,
    args: DebugProtocol.LaunchRequestArguments
  ): Promise<void> {
    // Create runtime with debug preset
    this.runtime = await createRuntime({ preset: 'debug' });

    // Wire events
    this.wireEvents();

    // Launch debug session
    await this.runtime.debug.launch({
      program: args.program,
      stopOnEntry: args.stopOnEntry
    });

    this.sendResponse(response);
  }

  private wireEvents(): void {
    this.runtime.on('stopped', (event) => {
      this.sendEvent(new StoppedEvent(
        event.reason,
        event.threadId,
        event.text
      ));
    });

    this.runtime.on('breakpoint', (event) => {
      if (event.type === 'hit') {
        this.sendEvent(new StoppedEvent(
          'breakpoint',
          1,
          `Breakpoint ${event.breakpoint.id}`
        ));
      }
    });

    this.runtime.on('output', (event) => {
      this.sendEvent(new OutputEvent(
        event.output,
        event.category
      ));
    });

    this.runtime.on('terminated', () => {
      this.sendEvent(new TerminatedEvent());
    });
  }

  protected async setBreakPointsRequest(
    response: DebugProtocol.SetBreakpointsResponse,
    args: DebugProtocol.SetBreakpointsArguments
  ): Promise<void> {
    const source = args.source.path!;
    const breakpoints = args.breakpoints ?? [];

    // Use runtime's breakpoint manager
    const results = this.runtime.debug.setBreakpoints(
      source,
      breakpoints.map(bp => ({
        line: bp.line,
        condition: bp.condition,
        hitCondition: bp.hitCondition,
        logMessage: bp.logMessage
      }))
    );

    response.body = {
      breakpoints: results.map(bp => ({
        id: parseInt(bp.id),
        verified: bp.verified,
        line: bp.line,
        source: { path: source }
      }))
    };

    this.sendResponse(response);
  }

  protected async continueRequest(
    response: DebugProtocol.ContinueResponse
  ): Promise<void> {
    await this.runtime.debug.continue();
    this.sendResponse(response);
  }

  protected async nextRequest(
    response: DebugProtocol.NextResponse
  ): Promise<void> {
    await this.runtime.debug.stepOver();
    this.sendResponse(response);
  }

  protected async stepInRequest(
    response: DebugProtocol.StepInResponse
  ): Promise<void> {
    await this.runtime.debug.stepInto();
    this.sendResponse(response);
  }

  protected async stepOutRequest(
    response: DebugProtocol.StepOutResponse
  ): Promise<void> {
    await this.runtime.debug.stepOut();
    this.sendResponse(response);
  }

  protected async stepBackRequest(
    response: DebugProtocol.StepBackResponse
  ): Promise<void> {
    await this.runtime.debug.stepBack();
    this.sendResponse(response);
  }

  protected async stackTraceRequest(
    response: DebugProtocol.StackTraceResponse,
    args: DebugProtocol.StackTraceArguments
  ): Promise<void> {
    const frames = this.runtime.debug.getStackTrace({
      startFrame: args.startFrame,
      levels: args.levels
    });

    response.body = {
      stackFrames: frames.map(f => new StackFrame(
        f.id,
        f.name,
        f.source ? { path: f.source } : undefined,
        f.line,
        f.column
      )),
      totalFrames: frames.length
    };

    this.sendResponse(response);
  }

  protected async scopesRequest(
    response: DebugProtocol.ScopesResponse,
    args: DebugProtocol.ScopesArguments
  ): Promise<void> {
    const scopes = this.runtime.debug.getScopes(args.frameId);

    response.body = {
      scopes: scopes.map(s => new Scope(
        s.name,
        s.variablesReference,
        s.expensive
      ))
    };

    this.sendResponse(response);
  }

  protected async variablesRequest(
    response: DebugProtocol.VariablesResponse,
    args: DebugProtocol.VariablesArguments
  ): Promise<void> {
    const variables = this.runtime.debug.getVariables(args.variablesReference);

    response.body = {
      variables: variables.map(v => new Variable(
        v.name,
        v.value,
        v.variablesReference
      ))
    };

    this.sendResponse(response);
  }

  protected async evaluateRequest(
    response: DebugProtocol.EvaluateResponse,
    args: DebugProtocol.EvaluateArguments
  ): Promise<void> {
    const result = this.runtime.debug.evaluate(
      args.expression,
      args.frameId,
      args.context
    );

    response.body = {
      result: result.result,
      type: result.type,
      variablesReference: result.variablesReference
    };

    this.sendResponse(response);
  }

  protected async disconnectRequest(
    response: DebugProtocol.DisconnectResponse
  ): Promise<void> {
    await this.runtime.debug.disconnect();
    await this.runtime.dispose();
    this.sendResponse(response);
  }
}
```

---

## Migration Steps

1. Replace inline evaluation with `runtime.eval()`
2. Replace breakpoint management with `runtime.debug.setBreakpoints()`
3. Replace state inspection with `runtime.inspector.*`
4. Replace stepping logic with `runtime.debug.step*()`
5. Wire runtime events to DAP events

---

## Test Requirements

### Unit Tests
- [ ] InitializeRequest returns capabilities
- [ ] LaunchRequest creates runtime
- [ ] SetBreakpointsRequest configures breakpoints
- [ ] ContinueRequest resumes execution
- [ ] StepRequests work correctly
- [ ] StackTraceRequest returns frames
- [ ] VariablesRequest returns variables
- [ ] EvaluateRequest evaluates expressions

### Integration Tests
- [ ] Full debug session lifecycle
- [ ] Breakpoints trigger stopped events
- [ ] Variable inspection works
- [ ] Expression evaluation in context works

---

## Acceptance Criteria
1. All DAP operations delegate to OmegaRuntime
2. Code significantly reduced
3. All debug features work
4. VS Code extension functions correctly
5. Tests pass
