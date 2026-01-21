import type { ClockPort } from "./clock";
import type { OraclePort } from "./oracle";
import type { RngPort } from "./rng";
import type { SinkPort } from "./sink";
import type { SourcePort } from "./source";
import type { StorePort } from "./store";
import type { ToolPort } from "./tool";

/**
 * Complete set of ports for execution.
 */
export interface PortSet {
  oracle: OraclePort;
  tool: ToolPort;
  store: StorePort;
  sink: SinkPort;
  source: SourcePort;
  clock: ClockPort;
  rng: RngPort;
}

/**
 * Create a port set with all required ports.
 */
export function createPortSet(ports: PortSet): PortSet {
  return ports;
}
