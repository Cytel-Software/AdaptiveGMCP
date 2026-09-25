---
description: Required commenting standards when creating or modifying R source and test code.
applyTo: "**/*.R"
---

# Good development practices

## Write good code comments

- Every newly created function MUST have a header describing its purpose,
  parameters, and return value. This includes exported functions, internal
  helpers, nested functions, callbacks, and test helpers.
- Add brief inline comments before non-obvious algorithmic steps, assumptions,
  transformations, or domain decisions.
- Do not comment trivial assignments or restate the code.
- When substantially modifying an undocumented function, add the required
  header as part of the change.
