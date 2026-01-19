# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 6: Multi-Shot Sampling
### The search.op Effect

For probabilistic reasoning, use `search.op` to sample multiple responses:

```lisp
Ω> (effect search.op "Pick a random fruit: apple, banana, or cherry")
```

This returns a *distribution*—a collection of samples with weights:

```json
{
  "tag": "Dist",
  "support": [
    {"v": {"denotation": "apple"}, "w": 1},
    {"v": {"denotation": "apple"}, "w": 1},
    {"v": {"denotation": "banana"}, "w": 1},
    {"v": {"denotation": "cherry"}, "w": 1},
    ...
  ],
  "meta": {"kind": "search", "note": "n=8"}
}
```

By default, it takes 8 samples. You can see which answers the LLM favors.

### Controlling Sample Count

Pass a map with an "n" key to control sampling:

```lisp
; This is the payload format for specifying sample count
(effect search.op (map ("n" 16) ("prompt" "Pick a color")))
```