import type { Outcome, Done, Fail, Pause } from "./outcome";
import { isDone, isFail, isPause } from "./outcome";

export function match<A, R>(
  outcome: Outcome<A>,
  handlers: {
    done: (d: Done<A>) => R;
    fail: (f: Fail) => R;
    pause: (p: Pause<A>) => R;
  }
): R {
  switch (outcome.tag) {
    case "Done":
      return handlers.done(outcome);
    case "Fail":
      return handlers.fail(outcome);
    case "Pause":
      return handlers.pause(outcome);
  }
}

export function mapOutcome<A, B>(o: Outcome<A>, fn: (a: A) => B): Outcome<B> {
  if (isDone(o)) {
    return { ...o, value: fn(o.value) };
  }
  return o as unknown as Outcome<B>;
}

export async function flatMapOutcome<A, B>(
  o: Outcome<A>,
  fn: (a: A) => Promise<Outcome<B>> | Outcome<B>
): Promise<Outcome<B>> {
  if (isDone(o)) {
    return await fn(o.value);
  }
  return o as unknown as Outcome<B>;
}

export function unwrap<A>(o: Outcome<A>): A {
  if (isDone(o)) {
    return o.value;
  }
  if (isFail(o)) {
    throw new Error(o.failure.message);
  }
  throw new Error("Cannot unwrap paused outcome");
}

export function unwrapOr<A>(o: Outcome<A>, defaultValue: A): A {
  return isDone(o) ? o.value : defaultValue;
}
