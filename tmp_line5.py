from pathlib import Path
line = Path("docs/USER-MANUAL--00--Quick-Reference.md").read_text(encoding="utf-8").splitlines()[18]
print(line.encode("unicode_escape"))
