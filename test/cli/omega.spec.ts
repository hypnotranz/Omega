// test/cli/omega.spec.ts
// Tests for the unified omega CLI command
// Following TDD - write tests FIRST

import { describe, it, expect } from "vitest";
import {
  parseCliArgs,
  getHelpText,
  getVersion,
  detectMode,
  buildConfig,
  type CliArgs,
  type CliConfig,
} from "../../bin/omega-cli-lib";

describe("Unified omega CLI", () => {
  describe("Command-line argument parsing", () => {
    it("should parse --help flag", () => {
      const args = ["--help"];
      const parsed = parseCliArgs(args);
      expect(parsed.help).toBe(true);
    });

    it("should parse --version flag", () => {
      const args = ["--version"];
      const parsed = parseCliArgs(args);
      expect(parsed.version).toBe(true);
    });

    it("should parse --eval with code", () => {
      const args = ["--eval", "(+ 1 2)"];
      const parsed = parseCliArgs(args);
      expect(parsed.eval).toBe("(+ 1 2)");
    });

    it("should parse file argument", () => {
      const args = ["example.omega"];
      const parsed = parseCliArgs(args);
      expect(parsed.file).toBe("example.omega");
    });

    it("should parse --debug flag", () => {
      const args = ["--debug", "example.omega"];
      const parsed = parseCliArgs(args);
      expect(parsed.debug).toBe(true);
      expect(parsed.file).toBe("example.omega");
    });

    it("should parse --verbose flag", () => {
      const args = ["--verbose"];
      const parsed = parseCliArgs(args);
      expect(parsed.verbose).toBe(true);
    });

    it("should default to REPL mode with no arguments", () => {
      const args: string[] = [];
      const parsed = parseCliArgs(args);
      expect(parsed.mode).toBe("repl");
    });

    it("should set exec mode when --eval is provided", () => {
      const args = ["--eval", "(+ 1 2)"];
      const parsed = parseCliArgs(args);
      expect(parsed.mode).toBe("exec");
    });

    it("should set exec mode when file is provided", () => {
      const args = ["example.omega"];
      const parsed = parseCliArgs(args);
      expect(parsed.mode).toBe("exec");
    });

    it("should handle --session argument", () => {
      const args = ["--session", "my-session"];
      const parsed = parseCliArgs(args);
      expect(parsed.session).toBe("my-session");
    });
  });

  describe("Help text formatting", () => {
    it("should generate help text with all options", () => {
      const help = getHelpText();
      expect(help).toContain("omega");
      expect(help).toContain("--help");
      expect(help).toContain("--version");
      expect(help).toContain("--eval");
      expect(help).toContain("--debug");
      expect(help).toContain("--verbose");
      expect(help).toContain("--session");
    });

    it("should include REPL commands in help", () => {
      const help = getHelpText();
      expect(help).toContain(":step");
      expect(help).toContain(":run");
      expect(help).toContain(":state");
      expect(help).toContain(":quit");
    });

    it("should include debugger commands in help", () => {
      const help = getHelpText();
      expect(help).toContain(":break");
      expect(help).toContain(":trace");
      expect(help).toContain(":goto");
    });
  });

  describe("Version display", () => {
    it("should return version string", () => {
      const version = getVersion();
      expect(version).toMatch(/\d+\.\d+\.\d+/); // semver pattern
    });

    it("should include package name", () => {
      const version = getVersion();
      expect(version).toContain("omega-llm");
    });
  });

  describe("Mode detection", () => {
    it("should detect REPL mode for no arguments", () => {
      const mode = detectMode({ mode: "repl" });
      expect(mode).toBe("repl");
    });

    it("should detect exec mode for --eval", () => {
      const mode = detectMode({ mode: "exec", eval: "(+ 1 2)" });
      expect(mode).toBe("exec");
    });

    it("should detect exec mode for file", () => {
      const mode = detectMode({ mode: "exec", file: "test.omega" });
      expect(mode).toBe("exec");
    });

    it("should enable debug features when --debug is set", () => {
      const config = buildConfig({ debug: true, mode: "repl" });
      expect(config.debugEnabled).toBe(true);
    });
  });

  describe("Configuration building", () => {
    it("should build default REPL config", () => {
      const config = buildConfig({ mode: "repl" });
      expect(config.mode).toBe("repl");
      expect(config.debugEnabled).toBe(true); // debugger merged into REPL
      expect(config.verbose).toBe(false);
    });

    it("should build exec config with code", () => {
      const config = buildConfig({ mode: "exec", eval: "(+ 1 2)" });
      expect(config.mode).toBe("exec");
      expect(config.code).toBe("(+ 1 2)");
    });

    it("should build exec config with file", () => {
      const config = buildConfig({ mode: "exec", file: "test.omega" });
      expect(config.mode).toBe("exec");
      expect(config.file).toBe("test.omega");
    });

    it("should enable verbose logging when flag is set", () => {
      const config = buildConfig({ mode: "repl", verbose: true });
      expect(config.verbose).toBe(true);
    });

    it("should set session name", () => {
      const config = buildConfig({ mode: "repl", session: "test-session" });
      expect(config.session).toBe("test-session");
    });
  });

  describe("Edge cases", () => {
    it("should handle empty --eval gracefully", () => {
      const parsed = parseCliArgs(["--eval", ""]);
      expect(parsed.eval).toBe("");
    });

    it("should handle missing file argument", () => {
      const parsed = parseCliArgs([]);
      expect(parsed.file).toBeUndefined();
    });

    it("should handle multiple flags", () => {
      const parsed = parseCliArgs(["--debug", "--verbose", "file.omega"]);
      expect(parsed.debug).toBe(true);
      expect(parsed.verbose).toBe(true);
      expect(parsed.file).toBe("file.omega");
    });

    it("should handle invalid arguments gracefully", () => {
      const parsed = parseCliArgs(["--unknown-flag"]);
      // Should not crash, can ignore or warn
      expect(parsed).toBeDefined();
    });
  });
});
