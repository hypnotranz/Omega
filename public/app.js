/**
 * OmegaLLM Debugger - Web UI Application
 */

class DebuggerApp {
  constructor() {
    this.sessionId = null;
    this.ws = null;
    this.snapshot = null;
    this.baseUrl = window.location.origin;

    this.initElements();
    this.bindEvents();
    this.bindKeyboard();
  }

  initElements() {
    // Session info
    this.sessionIdEl = document.getElementById('session-id');
    this.statusBadge = document.getElementById('status-badge');
    this.wsStatus = document.getElementById('ws-status');

    // Code editor
    this.codeEditor = document.getElementById('code-editor');

    // Controls
    this.btnNewSession = document.getElementById('btn-new-session');
    this.btnLoad = document.getElementById('btn-load');
    this.btnClear = document.getElementById('btn-clear');
    this.btnStep = document.getElementById('btn-step');
    this.btnStep10 = document.getElementById('btn-step-10');
    this.btnContinue = document.getElementById('btn-continue');
    this.btnRun = document.getElementById('btn-run');
    this.btnPause = document.getElementById('btn-pause');
    this.btnResume = document.getElementById('btn-resume');
    this.resumeValue = document.getElementById('resume-value');

    // Displays
    this.stepCount = document.getElementById('step-count');
    this.controlDisplay = document.getElementById('control-display');
    this.callStack = document.getElementById('call-stack');
    this.handlers = document.getElementById('handlers');
    this.environment = document.getElementById('environment');
    this.history = document.getElementById('history');

    // Panels
    this.effectPanel = document.getElementById('effect-panel');
    this.pendingEffect = document.getElementById('pending-effect');
    this.resultPanel = document.getElementById('result-panel');
    this.result = document.getElementById('result');
    this.errorPanel = document.getElementById('error-panel');
    this.error = document.getElementById('error');
  }

  bindEvents() {
    this.btnNewSession.addEventListener('click', () => this.createSession());
    this.btnLoad.addEventListener('click', () => this.loadCode());
    this.btnClear.addEventListener('click', () => this.clearCode());
    this.btnStep.addEventListener('click', () => this.step());
    this.btnStep10.addEventListener('click', () => this.stepN(10));
    this.btnContinue.addEventListener('click', () => this.continue());
    this.btnRun.addEventListener('click', () => this.run());
    this.btnPause.addEventListener('click', () => this.pause());
    this.btnResume.addEventListener('click', () => this.resumeWithValue());
  }

  bindKeyboard() {
    document.addEventListener('keydown', (e) => {
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
  // API Calls
  // ──────────────────────────────────────────────────────────

  async api(method, path, body = null) {
    const options = {
      method,
      headers: { 'Content-Type': 'application/json' },
    };
    if (body) {
      options.body = JSON.stringify(body);
    }

    const res = await fetch(`${this.baseUrl}${path}`, options);
    if (!res.ok) {
      const error = await res.json().catch(() => ({ error: res.statusText }));
      throw new Error(error.error || 'Request failed');
    }
    return res.json();
  }

  async createSession() {
    try {
      const data = await this.api('POST', '/session');
      this.sessionId = data.sessionId;
      this.sessionIdEl.textContent = this.sessionId;
      this.updateStatus('paused');
      this.connectWebSocket();
      this.clearDisplays();
    } catch (e) {
      this.showError(e.message);
    }
  }

  async loadCode() {
    if (!this.sessionId) {
      await this.createSession();
    }

    const code = this.codeEditor.value.trim();
    if (!code) return;

    try {
      const data = await this.api('POST', `/session/${this.sessionId}/load`, { code });
      if (!data.success) {
        this.showError(data.error || 'Failed to load code');
      } else {
        await this.refreshSnapshot();
      }
    } catch (e) {
      this.showError(e.message);
    }
  }

  clearCode() {
    this.codeEditor.value = '';
  }

  async step() {
    if (!this.sessionId) return;

    try {
      const data = await this.api('POST', `/session/${this.sessionId}/step`);
      this.handleStepResult(data);
    } catch (e) {
      this.showError(e.message);
    }
  }

  async stepN(n) {
    if (!this.sessionId) return;

    try {
      const data = await this.api('POST', `/session/${this.sessionId}/step/${n}`);
      this.handleStepResult(data);
    } catch (e) {
      this.showError(e.message);
    }
  }

  async continue() {
    if (!this.sessionId) return;

    try {
      const data = await this.api('POST', `/session/${this.sessionId}/continue`);
      this.handleStepResult(data);
    } catch (e) {
      this.showError(e.message);
    }
  }

  async run() {
    if (!this.sessionId) return;

    try {
      const data = await this.api('POST', `/session/${this.sessionId}/run`);
      this.handleStepResult(data);
    } catch (e) {
      this.showError(e.message);
    }
  }

  async pause() {
    // Pause is typically used for long-running operations
    // For now, we don't have background execution, so this is a no-op
    console.log('Pause requested');
  }

  async resumeWithValue() {
    if (!this.sessionId) return;

    let value;
    const input = this.resumeValue.value.trim();

    try {
      value = input ? JSON.parse(input) : null;
    } catch (e) {
      this.showError(`Invalid JSON: ${e.message}`);
      return;
    }

    try {
      const data = await this.api('POST', `/session/${this.sessionId}/resume`, { value });
      this.handleStepResult(data);
      this.resumeValue.value = '';
    } catch (e) {
      this.showError(e.message);
    }
  }

  async refreshSnapshot() {
    if (!this.sessionId) return;

    try {
      const snapshot = await this.api('GET', `/session/${this.sessionId}/snapshot`);
      this.updateSnapshot(snapshot);
    } catch (e) {
      this.showError(e.message);
    }
  }

  async jumpToStep(step) {
    if (!this.sessionId) return;

    try {
      const snapshot = await this.api('POST', `/session/${this.sessionId}/jump/${step}`);
      this.updateSnapshot(snapshot);
    } catch (e) {
      this.showError(e.message);
    }
  }

  async fetchHistory() {
    if (!this.sessionId) return;

    try {
      const history = await this.api('GET', `/session/${this.sessionId}/history`);
      this.renderHistory(history);
    } catch (e) {
      console.error('Failed to fetch history:', e);
    }
  }

  // ──────────────────────────────────────────────────────────
  // WebSocket
  // ──────────────────────────────────────────────────────────

  connectWebSocket() {
    if (this.ws) {
      this.ws.close();
    }

    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    const wsUrl = `${protocol}//${window.location.host}/ws?session=${this.sessionId}`;

    this.ws = new WebSocket(wsUrl);

    this.ws.onopen = () => {
      console.log('WebSocket connected');
      this.wsStatus.textContent = 'Connected';
      this.wsStatus.className = 'ws-status ws-connected';
    };

    this.ws.onclose = () => {
      console.log('WebSocket disconnected');
      this.wsStatus.textContent = 'Disconnected';
      this.wsStatus.className = 'ws-status ws-disconnected';
    };

    this.ws.onerror = (e) => {
      console.error('WebSocket error:', e);
    };

    this.ws.onmessage = (event) => {
      try {
        const data = JSON.parse(event.data);
        this.handleWebSocketEvent(data);
      } catch (e) {
        console.error('Failed to parse WebSocket message:', e);
      }
    };
  }

  handleWebSocketEvent(event) {
    switch (event.type) {
      case 'snapshot':
        this.updateSnapshot(event.snapshot);
        break;

      case 'breakpointHit':
        this.updateSnapshot(event.snapshot);
        console.log('Breakpoint hit:', event.breakpointId);
        break;

      case 'effectPending':
        this.updateSnapshot(event.snapshot);
        break;

      case 'done':
        this.updateSnapshot(event.snapshot);
        break;

      case 'error':
        this.updateSnapshot(event.snapshot);
        break;
    }
  }

  // ──────────────────────────────────────────────────────────
  // UI Updates
  // ──────────────────────────────────────────────────────────

  handleStepResult(data) {
    if (data.snapshot) {
      this.updateSnapshot(data.snapshot);
    }
    this.fetchHistory();
  }

  updateSnapshot(snapshot) {
    this.snapshot = snapshot;

    // Update step count
    this.stepCount.textContent = snapshot.step;

    // Update status
    this.updateStatus(snapshot.status);

    // Update control
    this.renderControl(snapshot.control);

    // Update call stack
    this.renderCallStack(snapshot.callStack);

    // Update handlers
    this.renderHandlers(snapshot.handlers);

    // Update environment
    this.renderEnvironment(snapshot.environment);

    // Handle pending effect
    if (snapshot.status === 'effect' && snapshot.pendingEffect) {
      this.showEffectPanel(snapshot.pendingEffect);
    } else {
      this.hideEffectPanel();
    }

    // Handle result
    if (snapshot.status === 'done' && snapshot.result) {
      this.showResult(snapshot.result);
    } else {
      this.hideResult();
    }

    // Handle error
    if (snapshot.error) {
      this.showError(snapshot.error.message);
    } else {
      this.hideError();
    }
  }

  updateStatus(status) {
    this.statusBadge.textContent = status;
    this.statusBadge.className = `status-badge status-${status}`;
  }

  renderControl(control) {
    if (!control) {
      this.controlDisplay.innerHTML = '<div class="control-empty">No code loaded</div>';
      return;
    }

    const tag = control.tag;
    const type = control.exprType || control.valType || '';
    const summary = control.exprSummary || control.valSummary || '';

    this.controlDisplay.innerHTML = `
      <span class="control-tag">${tag}</span>
      <span class="control-type">${type}</span>
      <span class="control-summary">${this.escapeHtml(summary)}</span>
    `;
  }

  renderCallStack(stack) {
    if (!stack || stack.length === 0) {
      this.callStack.innerHTML = '<div class="stack-empty">Empty</div>';
      return;
    }

    // Show most recent first (reversed)
    const frames = [...stack].reverse();

    this.callStack.innerHTML = frames.map((frame, i) => `
      <div class="stack-frame">
        <span class="frame-index">${frame.index}</span>
        <div class="frame-info">
          <span class="frame-tag">${frame.tag}</span>
          <div class="frame-desc">${this.escapeHtml(frame.description)}</div>
        </div>
      </div>
    `).join('');
  }

  renderHandlers(handlers) {
    if (!handlers || handlers.length === 0) {
      this.handlers.innerHTML = '<div class="handlers-empty">No handlers</div>';
      return;
    }

    this.handlers.innerHTML = handlers.map(h => `
      <div class="handler-item">
        <span class="handler-ops">${h.operations.join(', ')}</span>
        ${h.hasReturn ? ' [ret]' : ''}
        ${h.hasFinally ? ' [fin]' : ''}
      </div>
    `).join('');
  }

  renderEnvironment(bindings) {
    if (!bindings || bindings.length === 0) {
      this.environment.innerHTML = '<div class="env-empty">No bindings</div>';
      return;
    }

    // Group by depth and filter out primitives (depth > 1 or known names)
    const userBindings = bindings.filter(b => b.depth === 0 || !this.isPrimitive(b.name));

    if (userBindings.length === 0) {
      this.environment.innerHTML = '<div class="env-empty">No user bindings</div>';
      return;
    }

    this.environment.innerHTML = userBindings.slice(0, 30).map(b => `
      <div class="env-binding">
        <span class="binding-name">${this.escapeHtml(b.name)}</span>
        <span class="binding-equals">=</span>
        <span class="binding-value">${this.escapeHtml(b.value.summary)}</span>
        ${b.depth > 0 ? `<span class="binding-depth">${b.depth}</span>` : ''}
      </div>
    `).join('');
  }

  renderHistory(history) {
    if (!history || history.length === 0) {
      this.history.innerHTML = '<div class="history-empty">No history</div>';
      return;
    }

    // Show most recent first
    const items = [...history].reverse().slice(0, 50);

    this.history.innerHTML = items.map(h => `
      <div class="history-item" data-step="${h.step}">
        <span class="history-step">#${h.step}</span>
        <span class="history-control">${this.escapeHtml(h.control)}</span>
      </div>
    `).join('');

    // Bind click events for time travel
    this.history.querySelectorAll('.history-item').forEach(el => {
      el.addEventListener('click', () => {
        const step = parseInt(el.dataset.step, 10);
        this.jumpToStep(step);
      });
    });
  }

  showEffectPanel(effect) {
    this.effectPanel.classList.remove('hidden');
    this.pendingEffect.innerHTML = `
      <div class="effect-op">${this.escapeHtml(effect.op)}</div>
      <div class="effect-args">Args: ${this.escapeHtml(JSON.stringify(effect.args.map(a => a.summary)))}</div>
    `;
  }

  hideEffectPanel() {
    this.effectPanel.classList.add('hidden');
  }

  showResult(result) {
    this.resultPanel.classList.remove('hidden');
    this.result.textContent = result.summary || JSON.stringify(result);
  }

  hideResult() {
    this.resultPanel.classList.add('hidden');
  }

  showError(message) {
    this.errorPanel.classList.remove('hidden');
    this.error.textContent = message;
  }

  hideError() {
    this.errorPanel.classList.add('hidden');
  }

  clearDisplays() {
    this.stepCount.textContent = '0';
    this.controlDisplay.innerHTML = '<div class="control-empty">No code loaded</div>';
    this.callStack.innerHTML = '<div class="stack-empty">Empty</div>';
    this.handlers.innerHTML = '<div class="handlers-empty">No handlers</div>';
    this.environment.innerHTML = '<div class="env-empty">No bindings</div>';
    this.history.innerHTML = '<div class="history-empty">No history</div>';
    this.hideEffectPanel();
    this.hideResult();
    this.hideError();
  }

  // ──────────────────────────────────────────────────────────
  // Helpers
  // ──────────────────────────────────────────────────────────

  escapeHtml(str) {
    if (typeof str !== 'string') return str;
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
      'vector', 'vector-ref', 'vector-set!', 'vector-length',
      'make-vector', 'string-append', 'string-length', 'substring',
      'number->string', 'string->number', 'symbol->string', 'string->symbol',
      'apply', 'map', 'filter', 'foldl', 'foldr',
      'call/cc', 'call-with-current-continuation',
    ];
    return primitives.includes(name);
  }
}

// Initialize app
document.addEventListener('DOMContentLoaded', () => {
  window.app = new DebuggerApp();
});
