# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Appendix A: Configuration
### API Keys

OmegaLLM looks for API keys in this order:

1. `OPENAI_API_KEY` environment variable
2. `ANTHROPIC_API_KEY` environment variable
3. `../LambdaRLM/config.yaml` file

### The .env File

Create a `.env` file in the OmegaLLM directory:

```
OPENAI_API_KEY=sk-proj-your-key-here
```

The REPL loads this automatically.

---