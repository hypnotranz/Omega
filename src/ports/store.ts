import type { ExecContext } from "./types";

/**
 * Store port interface.
 * Persistent key-value storage.
 */
export interface StorePort {
  /**
   * Get value by key.
   * @returns value or null if not found
   */
  get(storeId: string, key: string, ctx: ExecContext): Promise<unknown | null>;

  /**
   * Put value by key.
   */
  put(storeId: string, key: string, value: unknown, ctx: ExecContext): Promise<void>;

  /**
   * Check if key exists.
   */
  has(storeId: string, key: string, ctx: ExecContext): Promise<boolean>;

  /**
   * Delete key.
   */
  delete(storeId: string, key: string, ctx: ExecContext): Promise<void>;
}

/**
 * Validate store operation against capabilities.
 */
export function validateStoreCap(storeId: string, op: "get" | "put" | "delete", ctx: ExecContext): void {
  const cap = ctx.caps.storeCap;
  if (!cap) {
    throw new Error("No StoreCap in context");
  }
  if (!cap.allowedStores.has(storeId)) {
    throw new Error(`Store ${storeId} not allowed by capability`);
  }
  if (cap.readOnly && (op === "put" || op === "delete")) {
    throw new Error(`Store ${storeId} is read-only`);
  }
}
