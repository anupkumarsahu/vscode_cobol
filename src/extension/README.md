# Extension Runtime Layer

This folder hosts activation/lifecycle oriented modules and orchestration code.

Current direction:
- Keep `src/extension.ts` as the public VS Code entrypoint.
- Move lifecycle orchestration and startup logic into focused modules in this folder.
- Keep feature implementations in feature/provider folders.

## Responsibility Boundaries

- `src/extension.ts`
  - Thin entrypoint and compatibility facade.
- `src/config/*`
  - Configuration acquisition, reloading, and validation.
- `src/features/*`
  - Feature registration and lazy/deferred initialization.
- `src/language/*`
  - Language IDs, constants, and grammar-related runtime constants.

This structure is intentionally incremental to avoid changing user-visible behavior.
