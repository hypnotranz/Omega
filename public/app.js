/**
 * OmegaLLM Debugger - Inline Results UI
 */

// LLM-focused examples
const EXAMPLES = {
  sentiment: `; SENTIMENT ANALYSIS
(define (analyze-sentiment text)
  (effect infer.op
    (list "Classify sentiment as positive/negative/neutral. One word:" text)))

(analyze-sentiment "I love this debugger!")`,

  summarize: `; SUMMARIZATION
(define (summarize text)
  (effect infer.op
    (list "Summarize in 10 words or less:" text)))

(summarize "The OmegaLLM debugger lets you step through code and inspect LLM calls before they happen.")`,

  translate: `; TRANSLATION
(define (translate text lang)
  (effect infer.op
    (list (string-append "Translate to " lang ":") text)))

(translate "Hello, world!" "Spanish")`,

  qa: `; Q&A
(define (ask context question)
  (effect infer.op
    (list "Answer based on context."
          (string-append "Context: " context)
          (string-append "Question: " question))))

(ask "OmegaLLM uses a CESK machine." "What machine does it use?")`,

  chain: `; CHAINED CALLS
(define (analyze-and-respond text)
  (let ((mood (effect infer.op (list "Is this positive or negative?" text))))
    (effect infer.op (list (string-append "Write a " mood " reply to:") text))))

(analyze-and-respond "I'm confused about how this works.")`
};

class DebuggerApp {
  constructor() {
    this.sessionId = null;
    this.ws = null;
    this.snapshot = null;
    this.lastSnapshotId = null; // Prevent duplicate processing
    this._stepInProgress = false; // Lock to prevent concurrent steps
    this.executionItems = []; // Track inline execution items
    this.baseUrl = window.location.origin;

    this.initElements();
    this.bindEvents();
    this.bindKeyboard();
  }

  initElements() {
    // Status
    this.statusBadge = document.getElementById('status-badge');
    this.wsStatus = document.getElementById('ws-status');

    // Code
    this.codeEditor = document.getElementById('code-editor');

    // Controls
    this.btnLoad = document.getElementById('btn-load');
    this.btnClear = document.getElementById('btn-clear');
    this.btnStep = document.getElementById('btn-step');
    this.btnStep10 = document.getElementById('btn-step-10');
    this.btnContinue = document.getElementById('btn-continue');
    this.btnRun = document.getElementById('btn-run');

    // Step count
    this.stepCount = document.getElementById('step-count');

    // Execution flow (inline results)
    this.executionFlow = document.getElementById('execution-flow');

    // State tabs
    this.controlDisplay = document.getElementById('control-display');
    this.environment = document.getElementById('environment');
    this.callStack = document.getElementById('call-stack');
    this.handlers = document.getElementById('handlers');
    this.trace = document.getElementById('trace');
    this.breakpoints = document.getElementById('breakpoints');

    // Trace/breakpoint controls
    this.btnBack = document.getElementById('btn-back');
    this.gotoStep = document.getElementById('goto-step');
    this.btnGoto = document.getElementById('btn-goto');
    this.breakType = document.getElementById('break-type');
    this.breakCondition = document.getElementById('break-condition');
    this.btnAddBreak = document.getElementById('btn-add-break');

    // Hidden compatibility elements
    this.sessionIdEl = document.getElementById('session-id');
    this.btnNewSession = document.getElementById('btn-new-session');
    this.btnPause = document.getElementById('btn-pause');
    this.btnResume = document.getElementById('btn-resume');
    this.resumeValue = document.getElementById('resume-value');
    this.effectPanel = document.getElementById('effect-panel');
    this.pendingEffect = document.getElementById('pending-effect');
    this.resultPanel = document.getElementById('result-panel');
    this.result = document.getElementById('result');
    this.errorPanel = document.getElementById('error-panel');
    this.error = document.getElementById('error');
    this.history = document.getElementById('history');
  }

  // Helper to enable/disable step buttons during async operations
  setStepButtonsEnabled(enabled) {
    const buttons = [this.btnStep, this.btnStep10, this.btnContinue, this.btnRun];
    buttons.forEach(btn => {
      if (btn) btn.disabled = !enabled;
    });
  }

  bindEvents() {
    // Load button removed - using Ctrl+Enter now
    this.btnLoad?.addEventListener('click', () => this.loadCode());
    this.btnClear?.addEventListener('click', () => this.clearCode());
    this.btnStep.addEventListener('click', () => this.step());
    this.btnStep10.addEventListener('click', () => this.stepN(10));
    this.btnContinue.addEventListener('click', () => this.continue());
    this.btnRun.addEventListener('click', () => this.run());

    // Tab switching
    document.querySelectorAll('.tab').forEach(tab => {
      tab.addEventListener('click', () => {
        // Remove active from all tabs and panes
        document.querySelectorAll('.tab').forEach(t => t.classList.remove('active'));
        document.querySelectorAll('.tab-pane').forEach(p => p.classList.remove('active'));
        // Activate clicked tab and corresponding pane
        tab.classList.add('active');
        const pane = document.getElementById(`tab-${tab.dataset.tab}`);
        if (pane) pane.classList.add('active');
      });
    });

    // Example buttons
    document.querySelectorAll('.btn-example').forEach(btn => {
      btn.addEventListener('click', () => {
        this.loadExample(btn.dataset.example);
      });
    });

    // Trace controls
    this.btnBack?.addEventListener('click', () => this.goBack());
    this.btnGoto?.addEventListener('click', () => {
      const step = parseInt(this.gotoStep?.value || '0');
      if (!isNaN(step)) this.jumpToStep(step);
    });

    // Breakpoint controls
    this.btnAddBreak?.addEventListener('click', () => this.addBreakpoint());

    // LLM Resume - use event delegation on the execution flow container
    this.executionFlow.addEventListener('click', (e) => {
      console.log('Execution flow click:', e.target.className, e.target);

      // Handle Resume button (manual value)
      if (e.target.classList.contains('llm-resume-btn')) {
        console.log('Resume button clicked');
        const card = e.target.closest('.llm-call-card');
        const input = card?.querySelector('.llm-resume-input');
        if (input) {
          let value;
          try {
            value = JSON.parse(input.value || '""');
          } catch {
            value = input.value;
          }
          console.log('Resuming with value:', value);
          e.target.disabled = true;
          e.target.textContent = 'Resuming...';
          this.resumeWithValue(value).finally(() => {
            e.target.disabled = false;
            e.target.textContent = 'Resume';
          });
        }
      }

      // Handle "Call LLM" button (auto-call)
      if (e.target.classList.contains('llm-auto-btn')) {
        console.log('Auto LLM button clicked');
        e.target.disabled = true;
        e.target.textContent = 'Calling...';
        this.callLLMAndContinue().finally(() => {
          e.target.disabled = false;
          e.target.textContent = 'Call LLM';
        });
      }
    });

    // Also handle Enter key in resume input
    this.executionFlow.addEventListener('keydown', (e) => {
      if (e.key === 'Enter' && e.target.classList.contains('llm-resume-input')) {
        const card = e.target.closest('.llm-call-card');
        const btn = card?.querySelector('.llm-resume-btn');
        btn?.click();
      }
    });

    // Trace click - jump to step
    this.trace.addEventListener('click', (e) => {
      const row = e.target.closest('[data-step]');
      if (row) {
        const step = parseInt(row.dataset.step);
        if (!isNaN(step)) this.jumpToStep(step);
      }
    });

    // Breakpoint delete
    this.breakpoints.addEventListener('click', (e) => {
      if (e.target.classList.contains('bp-delete')) {
        const bpId = e.target.dataset.bpId;
        if (bpId) this.deleteBreakpoint(bpId);
      }
    });
  }

  bindKeyboard() {
    document.addEventListener('keydown', (e) => {
      // Ctrl+Enter or Cmd+Enter in code editor = Load & Run
      if (e.target === this.codeEditor && (e.ctrlKey || e.metaKey) && e.key === 'Enter') {
        e.preventDefault();
        this.loadCode();
        return;
      }

      // Shift+Enter in code editor = Load only (pause at start)
      if (e.target === this.codeEditor && e.shiftKey && e.key === 'Enter') {
        e.preventDefault();
        this.loadCode();
        return;
      }

      if (e.target === this.codeEditor) return;

      switch (e.key) {
        case 'F10':
          e.preventDefault();
          this.step();
          break;
        case 'F5':
          e.preventDefault();
          this.continue();
          break;
        case 'F8':
          e.preventDefault();
          this.run();
          break;
      }
    });
  }

  // ──────────────────────────────────────────────────────────
  // API
  // ──────────────────────────────────────────────────────────

  async api(method, path, body = null) {
    const options = {
      method,
      headers: { 'Content-Type': 'application/json' },
    };
    if (body) options.body = JSON.stringify(body);

    const res = await fetch(`${this.baseUrl}${path}`, options);
    if (!res.ok) {
      const error = await res.json().catch(() => ({ error: res.statusText }));
      throw new Error(error.error || 'Request failed');
    }
    return res.json();
  }

  async createSession() {
    try {
      console.log('Creating session...');
      const data = await this.api('POST', '/session');
      console.log('Session created:', data);
      this.sessionId = data.sessionId;
      if (this.sessionIdEl) this.sessionIdEl.textContent = this.sessionId;
      this.updateStatus('paused');
      this.connectWebSocket();
    } catch (e) {
      console.error('Create session error:', e);
      this.addErrorToFlow('Failed to create session: ' + e.message);
    }
  }

  async loadCode() {
    console.log('loadCode called, current sessionId:', this.sessionId);
    if (!this.sessionId) {
      await this.createSession();
    }

    const code = this.codeEditor.value.trim();
    if (!code) {
      console.log('No code to load');
      return;
    }

    // Clear execution flow for new code
    this.executionItems = [];
    this.lastSnapshotId = null; // Reset to allow new snapshots
    this.renderExecutionFlow();

    try {
      console.log('Loading code, sessionId:', this.sessionId);
      const data = await this.api('POST', `/session/${this.sessionId}/load`, { code });
      console.log('Load result:', data);
      if (!data.success) {
        this.addErrorToFlow(data.error || 'Failed to load code');
      } else {
        // Add initial code expression
        this.addExpressionToFlow(code, true);
        await this.refreshSnapshot();
      }
    } catch (e) {
      console.error('Load code error:', e);
      this.addErrorToFlow(e.message);
    }
  }

  clearCode() {
    this.codeEditor.value = '';
    this.executionItems = [];
    this.renderExecutionFlow();
  }

  async loadExample(name) {
    console.log('loadExample called:', name);
    if (EXAMPLES[name]) {
      this.codeEditor.value = EXAMPLES[name];
      // Auto-load the code
      console.log('Calling loadCode...');
      await this.loadCode();
      console.log('loadCode complete, sessionId:', this.sessionId);
    }
  }

  async step() {
    if (!this.sessionId) {
      this.addErrorToFlow('No session - click an example or press Ctrl+Enter first');
      return;
    }
    // Prevent concurrent steps (race condition fix)
    if (this._stepInProgress) {
      console.log('Step blocked - already in progress');
      return;
    }
    this._stepInProgress = true;
    this.setStepButtonsEnabled(false);
    try {
      console.log('Step called, sessionId:', this.sessionId);
      // Use step-async to auto-handle LLM effects
      const data = await this.api('POST', `/session/${this.sessionId}/step-async`);
      console.log('Step result:', data);
      this.handleStepResult(data);
    } catch (e) {
      console.error('Step error:', e);
      this.addErrorToFlow(e.message);
    } finally {
      this._stepInProgress = false;
      this.setStepButtonsEnabled(true);
    }
  }

  async stepN(n) {
    if (!this.sessionId) {
      this.addErrorToFlow('No session - click an example or press Ctrl+Enter first');
      return;
    }
    if (this._stepInProgress) {
      console.log('StepN blocked - already in progress');
      return;
    }
    this._stepInProgress = true;
    this.setStepButtonsEnabled(false);
    try {
      // Step N times using step-async to handle LLM effects
      for (let i = 0; i < n; i++) {
        const data = await this.api('POST', `/session/${this.sessionId}/step-async`);
        this.handleStepResult(data);
        // Stop early if done, error, OR effect (effect means LLM call is pending)
        if (data.outcome === 'done' || data.outcome === 'error' || data.outcome === 'effect') break;
      }
    } catch (e) {
      this.addErrorToFlow(e.message);
    } finally {
      this._stepInProgress = false;
      this.setStepButtonsEnabled(true);
    }
  }

  async continue() {
    if (!this.sessionId) {
      this.addErrorToFlow('No session - click an example or press Ctrl+Enter first');
      return;
    }
    if (this._stepInProgress) {
      console.log('Continue blocked - already in progress');
      return;
    }
    this._stepInProgress = true;
    this.setStepButtonsEnabled(false);
    try {
      console.log('Continue called, sessionId:', this.sessionId);
      const data = await this.api('POST', `/session/${this.sessionId}/continue`);
      console.log('Continue result:', data);
      this.handleStepResult(data);
    } catch (e) {
      console.error('Continue error:', e);
      this.addErrorToFlow(e.message);
    } finally {
      this._stepInProgress = false;
      this.setStepButtonsEnabled(true);
    }
  }

  async run() {
    if (!this.sessionId) {
      this.addErrorToFlow('No session - click an example or press Ctrl+Enter first');
      return;
    }
    if (this._stepInProgress) {
      console.log('Run blocked - already in progress');
      return;
    }
    this._stepInProgress = true;
    this.setStepButtonsEnabled(false);
    try {
      console.log('Run called, sessionId:', this.sessionId);
      // Use run-async to auto-handle LLM effects with real LLM calls
      const data = await this.api('POST', `/session/${this.sessionId}/run-async`);
      console.log('Run result:', data);
      this.handleStepResult(data);
    } catch (e) {
      console.error('Run error:', e);
      this.addErrorToFlow(e.message);
    } finally {
      this._stepInProgress = false;
      this.setStepButtonsEnabled(true);
    }
  }

  async resumeWithValue(value) {
    if (!this.sessionId) {
      this.addErrorToFlow('No session');
      return;
    }
    try {
      console.log('Resume called with value:', value, 'sessionId:', this.sessionId);
      const data = await this.api('POST', `/session/${this.sessionId}/resume`, { value });
      console.log('Resume result:', data);
      // Add the LLM response as a result
      this.addResultToFlow(JSON.stringify(value));
      this.handleStepResult(data);
    } catch (e) {
      console.error('Resume error:', e);
      this.addErrorToFlow(e.message);
    }
  }

  async callLLMAndContinue() {
    if (!this.sessionId) {
      this.addErrorToFlow('No session');
      return;
    }
    try {
      console.log('Calling LLM and continuing, sessionId:', this.sessionId);
      // Use approve-llm which calls the LLM for the pending effect and resumes
      const data = await this.api('POST', `/session/${this.sessionId}/approve-llm`);
      console.log('Approve-LLM result:', data);
      this.handleStepResult(data);
    } catch (e) {
      console.error('Call LLM error:', e);
      this.addErrorToFlow(e.message);
    }
  }

  async refreshSnapshot() {
    if (!this.sessionId) return;
    try {
      const snapshot = await this.api('GET', `/session/${this.sessionId}/snapshot`);
      this.updateSnapshot(snapshot);
    } catch (e) {
      this.addErrorToFlow(e.message);
    }
  }

  // ──────────────────────────────────────────────────────────
  // WebSocket
  // ──────────────────────────────────────────────────────────

  connectWebSocket() {
    if (this.ws) this.ws.close();

    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    const wsUrl = `${protocol}//${window.location.host}/ws?session=${this.sessionId}`;

    this.ws = new WebSocket(wsUrl);

    this.ws.onopen = () => {
      this.wsStatus.textContent = 'connected';
      this.wsStatus.className = 'ws-status ws-connected';
    };

    this.ws.onclose = () => {
      this.wsStatus.textContent = 'disconnected';
      this.wsStatus.className = 'ws-status ws-disconnected';
    };

    this.ws.onerror = (e) => console.error('WebSocket error:', e);

    this.ws.onmessage = (event) => {
      try {
        const data = JSON.parse(event.data);
        if (data.snapshot) this.updateSnapshot(data.snapshot);
      } catch (e) {
        console.error('WS parse error:', e);
      }
    };
  }

  // ──────────────────────────────────────────────────────────
  // Snapshot & Rendering
  // ──────────────────────────────────────────────────────────

  handleStepResult(data) {
    if (data.snapshot) {
      this.updateSnapshot(data.snapshot);
    }
    // Show effect log if present (from run-async)
    if (data.effectLog && data.effectLog.length > 0) {
      data.effectLog.forEach(effect => {
        this.addStepToFlow(effect.step, `LLM: ${effect.op}`, effect.result, true);
      });
    }
  }

  updateSnapshot(snapshot) {
    // Skip if we've already processed this exact snapshot
    if (snapshot.snapshotId && snapshot.snapshotId === this.lastSnapshotId) {
      return;
    }
    this.lastSnapshotId = snapshot.snapshotId;
    this.snapshot = snapshot;

    // Update step count
    this.stepCount.textContent = snapshot.step;

    // Update status badge
    this.updateStatus(snapshot.status);

    // Update state tabs
    this.renderStateTabs(snapshot);

    // Track the current step in execution flow
    if (snapshot.control && snapshot.status !== 'done') {
      const expr = snapshot.control.exprSummary || snapshot.control.valSummary || '';
      if (expr && snapshot.status === 'paused') {
        this.addStepToFlow(snapshot.step, expr, null, false);
      }
    }

    // Handle different states - render inline
    if (snapshot.status === 'effect' && snapshot.pendingEffect) {
      this.showLLMCallInline(snapshot.pendingEffect);
    } else if (snapshot.status === 'done' && snapshot.result) {
      this.showFinalResult(snapshot.result);
    } else if (snapshot.error) {
      this.addErrorToFlow(snapshot.error.message);
    } else if (snapshot.control) {
      // Show current expression being evaluated
      this.updateCurrentExpression(snapshot.control);
    }
  }

  updateStatus(status) {
    this.statusBadge.textContent = status;
    this.statusBadge.className = `status-badge status-${status}`;
  }

  // ──────────────────────────────────────────────────────────
  // Inline Execution Flow
  // ──────────────────────────────────────────────────────────

  addExpressionToFlow(expr, isCurrent = false) {
    this.executionItems.push({
      type: 'expr',
      content: expr,
      current: isCurrent
    });
    this.renderExecutionFlow();
  }

  addStepToFlow(stepNum, expr, result = null, isLLM = false) {
    // Don't add duplicates
    const existing = this.executionItems.find(item => item.type === 'step' && item.stepNum === stepNum);
    if (existing) return;

    this.executionItems.push({
      type: 'step',
      stepNum,
      expr,
      result,
      isLLM
    });
    this.renderExecutionFlow();
  }

  addResultToFlow(result) {
    this.executionItems.push({
      type: 'result',
      content: result
    });
    this.renderExecutionFlow();
  }

  addErrorToFlow(message) {
    this.executionItems.push({
      type: 'error',
      content: message
    });
    this.renderExecutionFlow();
  }

  showLLMCallInline(effect) {
    // Remove any existing LLM call cards
    this.executionItems = this.executionItems.filter(item => item.type !== 'llm-call');

    // Add the LLM call card
    const isLLM = effect.op === 'infer.op' || effect.op.startsWith('opr.');
    let promptParts = [];
    if (effect.args && effect.args.length > 0) {
      promptParts = effect.args.map(a => a.summary || JSON.stringify(a));
    }

    this.executionItems.push({
      type: 'llm-call',
      op: effect.op,
      prompt: promptParts.join('\n'),
      isLLM
    });
    this.renderExecutionFlow();
  }

  showFinalResult(result) {
    // Remove any LLM call cards and existing final results (prevent duplicates)
    this.executionItems = this.executionItems.filter(item => item.type !== 'llm-call' && item.type !== 'final');

    this.executionItems.push({
      type: 'final',
      content: result.summary || JSON.stringify(result)
    });
    this.renderExecutionFlow();
  }

  updateCurrentExpression(control) {
    // Update the "current" flag on expressions
    const summary = control.exprSummary || control.valSummary || '';
    if (summary) {
      // Mark all as not current, then add new current
      this.executionItems.forEach(item => {
        if (item.type === 'expr') item.current = false;
      });
    }
  }

  renderExecutionFlow() {
    if (this.executionItems.length === 0) {
      this.executionFlow.innerHTML = '<div class="execution-empty">Click an example or press Ctrl+Enter</div>';
      return;
    }

    this.executionFlow.innerHTML = this.executionItems.map((item, i) => {
      switch (item.type) {
        case 'expr':
          return `<div class="exec-expr ${item.current ? 'current' : ''}">${this.escapeHtml(item.content)}</div>`;

        case 'step':
          const stepClass = item.isLLM ? 'exec-step llm' : 'exec-step';
          const stepIcon = item.isLLM ? '🤖' : '▸';
          let stepHtml = `<div class="${stepClass}">
            <span class="step-num">${stepIcon} [${item.stepNum}]</span>
            <span class="step-expr">${this.escapeHtml(item.expr)}</span>`;
          if (item.result) {
            stepHtml += `<div class="step-result">=> ${this.escapeHtml(String(item.result).slice(0, 100))}</div>`;
          }
          stepHtml += `</div>`;
          return stepHtml;

        case 'result':
          return `<div class="exec-result"><span class="exec-result-value">${this.escapeHtml(item.content)}</span></div>`;

        case 'error':
          return `<div class="exec-error">${this.escapeHtml(item.content)}</div>`;

        case 'llm-call':
          return this.renderLLMCallCard(item, i);

        case 'final':
          return `
            <div class="exec-final-result">
              <div class="exec-final-label">Result</div>
              <div class="exec-final-value">${this.escapeHtml(item.content)}</div>
            </div>
          `;

        default:
          return '';
      }
    }).join('');

    // Scroll to bottom
    this.executionFlow.scrollTop = this.executionFlow.scrollHeight;
  }

  renderLLMCallCard(item, index) {
    return `
      <div class="llm-call-card">
        <div class="llm-call-header">
          <span class="llm-badge">LLM CALL</span>
          <span class="llm-op">${this.escapeHtml(item.op)}</span>
        </div>
        <div class="llm-call-body">
          <div class="llm-prompt-label">Prompt</div>
          <div class="llm-prompt-content">${this.escapeHtml(item.prompt)}</div>
        </div>
        <div class="llm-call-actions">
          <button class="btn btn-run llm-auto-btn" style="margin-right:8px">Call LLM</button>
          <span style="color:var(--text-muted);margin-right:8px">or</span>
          <input type="text" class="llm-resume-input" placeholder='Manual response' data-index="${index}">
          <button class="btn btn-resume llm-resume-btn">Resume</button>
        </div>
      </div>
    `;
  }

  // ──────────────────────────────────────────────────────────
  // State Tabs
  // ──────────────────────────────────────────────────────────

  renderStateTabs(snapshot) {
    // Debug: log snapshot to see what we're getting
    console.log('Snapshot received:', snapshot);

    // Control - Rich display
    if (snapshot.control) {
      const c = snapshot.control;
      const tag = c.tag || c.type || 'unknown';
      const expr = c.exprSummary || c.valSummary || c.summary || (typeof c === 'string' ? c : JSON.stringify(c, null, 2));
      const phase = snapshot.status === 'effect' ? '⚡ EFFECT PENDING' :
                    snapshot.status === 'done' ? '✓ COMPLETE' : '▶ EVALUATING';

      let controlHtml = `<div style="margin-bottom:8px;color:var(--accent-cyan)">${phase}</div>`;
      controlHtml += `<div style="margin-bottom:4px"><strong style="color:var(--accent-purple)">Tag:</strong> ${this.escapeHtml(tag)}</div>`;
      controlHtml += `<div style="margin-bottom:4px"><strong style="color:var(--accent-orange)">Expression:</strong></div>`;
      controlHtml += `<div style="padding:4px;background:var(--bg-tertiary);border-radius:4px;white-space:pre-wrap">${this.escapeHtml(expr)}</div>`;

      if (c.fullExpr) {
        controlHtml += `<div style="margin-top:8px"><strong style="color:var(--text-muted)">Full:</strong></div>`;
        controlHtml += `<div style="padding:4px;background:var(--bg-tertiary);border-radius:4px;font-size:10px;max-height:60px;overflow-y:auto">${this.escapeHtml(c.fullExpr)}</div>`;
      }
      this.controlDisplay.innerHTML = controlHtml;
    } else {
      // Show raw snapshot for debugging
      const rawKeys = Object.keys(snapshot || {}).join(', ') || 'empty';
      this.controlDisplay.innerHTML = `<div style="color:var(--text-muted)">No control field</div><div style="font-size:10px;margin-top:8px;color:var(--text-muted)">Snapshot keys: ${rawKeys}</div><pre style="font-size:9px;max-height:100px;overflow:auto">${this.escapeHtml(JSON.stringify(snapshot, null, 2))}</pre>`;
    }

    // Environment - Grouped by depth with better formatting
    if (snapshot.environment && snapshot.environment.length > 0) {
      const userBindings = snapshot.environment.filter(b => !this.isPrimitive(b.name));
      if (userBindings.length > 0) {
        let envHtml = '';
        const grouped = {};
        userBindings.forEach(b => {
          const depth = b.depth || 0;
          if (!grouped[depth]) grouped[depth] = [];
          grouped[depth].push(b);
        });

        Object.keys(grouped).sort((a, b) => a - b).forEach(depth => {
          const label = depth === '0' ? 'Global' : `Scope ${depth}`;
          envHtml += `<div style="color:var(--accent-cyan);margin:4px 0;font-weight:600">${label}</div>`;
          grouped[depth].slice(0, 15).forEach(b => {
            const valStr = b.value?.summary || JSON.stringify(b.value);
            const type = this.getValueType(b.value);
            envHtml += `<div style="padding:2px 0;border-bottom:1px solid var(--border-default)">`;
            envHtml += `<span style="color:var(--accent-purple)">${this.escapeHtml(b.name)}</span>`;
            envHtml += `<span style="color:var(--text-muted);font-size:10px"> : ${type}</span>`;
            envHtml += ` = <span style="color:var(--accent-green)">${this.escapeHtml(valStr)}</span>`;
            envHtml += `</div>`;
          });
        });
        this.environment.innerHTML = envHtml;
      } else {
        this.environment.innerHTML = '<div style="color:var(--text-muted)">No user bindings</div>';
      }
    } else {
      this.environment.innerHTML = '<div style="color:var(--text-muted)">No bindings</div>';
    }

    // Call stack - Show continuation types with colors
    if (snapshot.callStack && snapshot.callStack.length > 0) {
      let stackHtml = `<div style="color:var(--text-muted);margin-bottom:8px">Depth: ${snapshot.callStack.length}</div>`;
      snapshot.callStack.forEach((f, i) => {
        const isTop = i === 0;
        const bgColor = isTop ? 'rgba(63, 185, 80, 0.1)' : 'transparent';
        const borderColor = isTop ? 'var(--accent-green)' : 'var(--border-default)';
        stackHtml += `<div style="padding:4px;margin:2px 0;background:${bgColor};border-left:2px solid ${borderColor}">`;
        stackHtml += `<span style="color:var(--accent-cyan)">[${f.index}]</span> `;
        stackHtml += `<span style="color:var(--accent-purple);font-weight:600">${this.escapeHtml(f.tag)}</span>`;
        if (f.description) {
          stackHtml += `<div style="margin-left:16px;color:var(--text-secondary);font-size:11px">${this.escapeHtml(f.description)}</div>`;
        }
        stackHtml += `</div>`;
      });
      this.callStack.innerHTML = stackHtml;
    } else {
      this.callStack.innerHTML = '<div style="color:var(--text-muted)">Empty call stack</div>';
    }

    // Handlers - Show operations they handle
    if (snapshot.handlers && snapshot.handlers.length > 0) {
      let handlersHtml = `<div style="color:var(--text-muted);margin-bottom:8px">Active handlers: ${snapshot.handlers.length}</div>`;
      snapshot.handlers.forEach((h, i) => {
        handlersHtml += `<div style="padding:4px;margin:2px 0;background:var(--bg-tertiary);border-radius:4px">`;
        handlersHtml += `<span style="color:var(--accent-purple);font-weight:600">Handler ${i + 1}</span>`;
        handlersHtml += `<div style="margin-top:4px">`;
        (h.operations || []).forEach(op => {
          const isLLM = op === 'infer.op' || op.startsWith('opr.');
          const color = isLLM ? 'var(--accent-purple)' : 'var(--accent-blue)';
          const icon = isLLM ? '🤖' : '⚙️';
          handlersHtml += `<span style="display:inline-block;padding:2px 6px;margin:2px;background:var(--bg-elevated);border-radius:3px;color:${color};font-size:11px">${icon} ${this.escapeHtml(op)}</span>`;
        });
        handlersHtml += `</div></div>`;
      });
      this.handlers.innerHTML = handlersHtml;
    } else {
      this.handlers.innerHTML = '<div style="color:var(--text-muted)">No effect handlers installed</div>';
    }

    // Trace - Fetch from server and show clickable entries
    const step = snapshot.step || 0;
    let traceHtml = `<div style="color:var(--accent-cyan);margin-bottom:8px">Current step: <strong>${step}</strong></div>`;

    // Fetch history from server
    this.fetchHistory().then(history => {
      if (history && history.length > 0) {
        let histHtml = `<div style="color:var(--accent-cyan);margin-bottom:8px">Current step: <strong>${step}</strong> / ${history.length - 1}</div>`;
        history.slice(-30).forEach(t => {
          const isCurrent = t.step === step;
          const bg = isCurrent ? 'rgba(63, 185, 80, 0.15)' : 'transparent';
          const border = isCurrent ? 'var(--accent-green)' : 'var(--border-default)';
          histHtml += `<div style="padding:2px 4px;margin:1px 0;background:${bg};border-left:2px solid ${border};cursor:pointer" data-step="${t.step}">`;
          histHtml += `<span style="color:var(--accent-cyan);min-width:30px;display:inline-block">[${t.step}]</span> `;
          histHtml += `<span style="color:var(--text-secondary)">${this.escapeHtml(t.control || '')}</span>`;
          if (isCurrent) histHtml += ` <span style="color:var(--accent-green)">◀ current</span>`;
          histHtml += `</div>`;
        });
        this.trace.innerHTML = histHtml;
      }
    }).catch(() => {});

    // Show placeholder while loading
    traceHtml += '<div style="color:var(--text-muted)">Loading history...</div>';
    this.trace.innerHTML = traceHtml;

    // Breakpoints - With delete buttons
    if (snapshot.breakpoints && snapshot.breakpoints.length > 0) {
      let bpHtml = `<div style="color:var(--text-muted);margin-bottom:8px">Active breakpoints: ${snapshot.breakpoints.length}</div>`;
      snapshot.breakpoints.forEach(bp => {
        const enabledColor = bp.enabled ? 'var(--accent-red)' : 'var(--text-muted)';
        const icon = bp.type === 'step' ? '🔢' : bp.type === 'expr' ? '📝' : '⚡';
        bpHtml += `<div style="padding:4px;margin:2px 0;background:var(--bg-tertiary);border-radius:4px;display:flex;justify-content:space-between;align-items:center">`;
        bpHtml += `<span>`;
        bpHtml += `<span style="color:${enabledColor}">●</span> `;
        bpHtml += `${icon} <span style="color:var(--accent-purple)">#${bp.id}</span> `;
        bpHtml += `<span style="color:var(--text-muted)">[${bp.type}]</span> `;
        bpHtml += `<span style="color:var(--accent-orange)">${this.escapeHtml(bp.condition)}</span>`;
        bpHtml += `</span>`;
        bpHtml += `<button class="btn btn-sm btn-ghost bp-delete" data-bp-id="${bp.id}" style="padding:2px 6px">×</button>`;
        bpHtml += `</div>`;
      });
      this.breakpoints.innerHTML = bpHtml;
    } else {
      this.breakpoints.innerHTML = '<div style="color:var(--text-muted)">No breakpoints set</div><div style="margin-top:8px;font-size:11px;color:var(--text-muted)">Use controls above to add:<br>• Step # - break at step N<br>• Expression - break on pattern<br>• Effect - break on effect type</div>';
    }
  }

  getValueType(val) {
    if (!val) return 'nil';
    if (val.type) return val.type;
    if (val.tag) return val.tag;
    if (typeof val === 'string') return 'string';
    if (typeof val === 'number') return 'number';
    if (typeof val === 'boolean') return 'boolean';
    if (Array.isArray(val)) return 'list';
    return 'object';
  }

  async goBack() {
    if (!this.sessionId) return;
    try {
      const data = await this.api('POST', `/session/${this.sessionId}/back`);
      this.handleStepResult(data);
    } catch (e) {
      // Back might not be implemented, just log
      console.log('Back not available:', e.message);
    }
  }

  async addBreakpoint() {
    if (!this.sessionId) return;
    const type = this.breakType?.value || 'step';
    const condition = this.breakCondition?.value || '';
    if (!condition) return;

    try {
      await this.api('POST', `/session/${this.sessionId}/breakpoint`, { type, condition });
      await this.refreshSnapshot();
      this.breakCondition.value = '';
    } catch (e) {
      console.log('Add breakpoint failed:', e.message);
    }
  }

  async jumpToStep(step) {
    if (!this.sessionId) return;
    try {
      const data = await this.api('POST', `/session/${this.sessionId}/jump/${step}`);
      this.handleStepResult(data);
    } catch (e) {
      console.log('Jump to step failed:', e.message);
    }
  }

  async fetchHistory() {
    if (!this.sessionId) return [];
    try {
      const history = await this.api('GET', `/session/${this.sessionId}/history`);
      return history || [];
    } catch (e) {
      return [];
    }
  }

  async deleteBreakpoint(bpId) {
    if (!this.sessionId) return;
    try {
      await this.api('DELETE', `/session/${this.sessionId}/breakpoint/${bpId}`);
      await this.refreshSnapshot();
    } catch (e) {
      console.log('Delete breakpoint failed:', e.message);
    }
  }

  // ──────────────────────────────────────────────────────────
  // Legacy compat methods
  // ──────────────────────────────────────────────────────────

  showEffectPanel() {}
  hideEffectPanel() {}
  showResult() {}
  hideResult() {}
  showError(msg) { this.addErrorToFlow(msg); }
  hideError() {}
  clearDisplays() {
    this.executionItems = [];
    this.renderExecutionFlow();
  }
  renderControl() {}
  renderCallStack() {}
  renderHandlers() {}
  renderEnvironment() {}
  renderHistory() {}

  // ──────────────────────────────────────────────────────────
  // Helpers
  // ──────────────────────────────────────────────────────────

  escapeHtml(str) {
    if (typeof str !== 'string') return String(str);
    return str
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#039;');
  }

  isPrimitive(name) {
    const primitives = [
      '+', '-', '*', '/', '<', '>', '<=', '>=', '=', 'eq?',
      'car', 'cdr', 'cons', 'list', 'null?', 'pair?', 'list?',
      'number?', 'string?', 'symbol?', 'boolean?', 'procedure?',
      'not', 'and', 'or', 'display', 'newline', 'error',
      'string-append', 'number->string', 'apply', 'map', 'filter'
    ];
    return primitives.includes(name);
  }
}

// Initialize
document.addEventListener('DOMContentLoaded', () => {
  window.app = new DebuggerApp();
});
