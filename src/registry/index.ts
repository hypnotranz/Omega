export * from "./types";
export * from "./registry";
export * from "./query";
export { generateMarkdown, generateJSON } from "./docgen";
export * from "./validate";

import { defaultRegistry } from "./registry";
import { frameLispDescriptors } from "./descriptors/framelisp";
import { lambdaLLMDescriptors } from "./descriptors/lambdallm";
import { lambdaRLMDescriptors } from "./descriptors/lambdarlm";
import { omegaLLMDescriptors } from "./descriptors/omegallm";

const seeds = [
  ...frameLispDescriptors,
  ...lambdaLLMDescriptors,
  ...lambdaRLMDescriptors,
  ...omegaLLMDescriptors,
];

for (const desc of seeds) {
  defaultRegistry.register(desc);
}

export {
  frameLispDescriptors,
  lambdaLLMDescriptors,
  lambdaRLMDescriptors,
  omegaLLMDescriptors,
  defaultRegistry,
};
