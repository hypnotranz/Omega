#!/usr/bin/env python3
"""
closeitem.py - Agent-facing "close" script

This script is what agents call when they think they're done.
It does NOT actually close anything - it just signals to the harness
that the agent believes the work is complete.

The harness will:
1. Capture all file changes
2. Run CI (build, test)
3. If pass: merge and actually close the bead
4. If fail: spawn a fresh agent to continue

Usage (from agent prompt):
    python closeitem.py

That's it. No arguments needed. The harness knows what task this is.
"""

import os
import sys
from pathlib import Path
from datetime import datetime

DONE_MARKER = ".agent-done"

def main():
    # Find worktree root (where .git is)
    cwd = Path.cwd()
    root = cwd
    while root != root.parent:
        if (root / ".git").exists():
            break
        root = root.parent
    
    # Write the done marker
    marker_path = root / DONE_MARKER
    marker_path.write_text(f"Agent signaled completion at {datetime.now().isoformat()}\n")
    
    print("=" * 60)
    print("TASK SUBMITTED FOR REVIEW")
    print("=" * 60)
    print()
    print("Your work has been submitted. The build system will now:")
    print("  1. Commit your changes")
    print("  2. Run build verification")
    print("  3. Run tests")
    print("  4. Merge if everything passes")
    print()
    print("You may now exit. Thank you for your contribution.")
    print("=" * 60)
    
    return 0

if __name__ == "__main__":
    sys.exit(main())
