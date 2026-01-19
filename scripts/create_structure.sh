#!/bin/bash
# SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-1.md (Section 124)
# AUTO-GENERATED - Creates the OmegaLLM directory structure

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OMEGA_ROOT="$(dirname "$SCRIPT_DIR")"

echo "Creating OmegaLLM directory structure..."

mkdir -p "$OMEGA_ROOT/src/adapters/oracle/engines"
mkdir -p "$OMEGA_ROOT/src/adapters/tools"
mkdir -p "$OMEGA_ROOT/src/core"
mkdir -p "$OMEGA_ROOT/src/core/artifacts"
mkdir -p "$OMEGA_ROOT/src/core/boot/omega0"
mkdir -p "$OMEGA_ROOT/src/core/cli/commands"
mkdir -p "$OMEGA_ROOT/src/core/effects"
mkdir -p "$OMEGA_ROOT/src/core/eval"
mkdir -p "$OMEGA_ROOT/src/core/expand"
mkdir -p "$OMEGA_ROOT/src/core/obligations"
mkdir -p "$OMEGA_ROOT/src/core/syntax"
mkdir -p "$OMEGA_ROOT/test/differential"
mkdir -p "$OMEGA_ROOT/test/effects"
mkdir -p "$OMEGA_ROOT/test/helpers"
mkdir -p "$OMEGA_ROOT/test/hermetic"
mkdir -p "$OMEGA_ROOT/test/macros"
mkdir -p "$OMEGA_ROOT/test/nondet"

echo "Directory structure created."
echo ""
echo "Next steps:"
echo "  1. Run the extraction script to populate with code"
echo "  2. Run 'npm init' to create package.json"
echo "  3. Install dependencies: npm install typescript vitest"
