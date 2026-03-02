# Architecture Overview

This extension is being refactored toward a layered, modular architecture that preserves existing behavior while reducing startup cost and coupling.

## Target Layout

- `src/extension/` — activation and lifecycle orchestration
- `src/config/` — centralized settings access and validation
- `src/language/` — language IDs and grammar/runtime constants
- `src/features/` — feature registration and lazy loading
- `src/providers/` — VS Code provider implementations
- `src/utils/` — shared helpers/logging primitives

## Incremental Changes Introduced

1. **Configuration service boundary**
   - New `ConfigurationService` centralizes access to workspace/resource settings.

2. **Language constants boundary**
   - New `languageConstants` module removes repeated language string literals.

3. **Lazy feature registration**
   - New `LazyLanguageFeatures` defers semantic tokens + code lens registration until COBOL documents are actually opened/active.

4. **Activation cleanup**
   - Duplicate reference provider registration removed.

5. **Desktop command extraction**
   - Desktop-only command wiring moved to `src/extension/desktopCommands.ts`.

6. **Configuration change extraction**
   - Scoped configuration reaction logic moved to `src/extension/configurationChangeHandler.ts`.

7. **Provider registry extraction**
   - Language provider registration orchestration moved to `src/extension/providerRegistry.ts`.

8. **Editor lifecycle extraction**
   - Editor/document event orchestration and debounced decoration updates moved to `src/extension/editorLifecycle.ts`.

9. **Feature-flag gates centralized**
   - Activation toggle checks moved behind `src/config/featureFlags.ts` for explicit, testable feature gating.

10. **Providers module added**
   - Provider entrypoints grouped under `src/providers/` and routed through a dedicated registry.
   - Provider implementations relocated under `src/providers/intellisense`, `src/providers/navigation`, and `src/providers/language`.

11. **Workspace metadata commands extracted**
   - Metadata scan command wiring moved to `src/extension/workspaceMetadataCommands.ts`.

12. **Editor and tree features relocated**
   - Margin decorations and color comments moved to `src/features/editor/`.
   - Source tree view moved to `src/features/tree/`.

13. **Advanced language providers relocated**
   - Semantic tokens and code lens providers moved to `src/providers/language/`.
   - Call hierarchy provider moved to `src/providers/navigation/`.

14. **Command helper cluster relocated**
   - Diagnostic symbol dump, dot-graph markdown, make-dependency, and copybook drag/drop helpers moved to `src/extension/commands/`.
   - `vscommon_commands.ts` now imports these helpers from the extension command module area.

15. **Workspace utility cluster relocated**
   - Workspace folders, VS file utilities, and scanner helper tools moved to `src/features/workspace/`.
   - Core/feature/provider consumers now reference the workspace feature module paths.

16. **Runtime helper pair relocated**
   - Console external-features adapter and terminal profile provider moved to `src/features/runtime/`.
   - Scanner and extension entrypoint imports now reference runtime feature module paths.

17. **VS external-features runtime module relocated**
   - `vsexternalfeatures.ts` moved to `src/features/runtime/` and all consumers rewired to the runtime path.
   - Runtime module now consumes workspace helpers from `src/features/workspace/`.

18. **External-features interface/types relocated**
   - `externalfeatures.ts` moved to `src/features/runtime/` and all interface/type consumers rewired.
   - Runtime implementations now import interface/types locally from the runtime domain.

19. **Workspace scanner orchestrator relocated**
   - `vscobscanner.ts` moved to `src/features/workspace/` as part of workspace/metadata scanning concerns.
   - Extension command modules now import scanner orchestration from the workspace feature domain.

20. **Help and feedback tree view relocated**
   - `feedbacktree.ts` moved to `src/features/tree/vsfeedbacktree.ts` as part of tree/UI feature domain.
   - Both desktop and web extension entrypoints now import feedback tree from the tree feature module.

21. **Tab stop/indentation utility relocated**
   - `tabstopper.ts` moved to `src/features/editor/tabstopper.ts` as part of editor formatting features.
   - Importers in `vscommon_commands.ts` and `configurationChangeHandler.ts` updated to reference editor feature module.

22. **Custom intellisense rules relocated**
   - `vscustomrules.ts` moved to `src/providers/intellisense/vscustomrules.ts` as part of intellisense provider domain.
   - All 5 consumers (IDE configuration handler, core utils, and 3 intellisense providers) rewired to provider intellisense path.

23. **Documentation comment handler relocated**
   - `doccomment.ts` moved to `src/features/editor/doccomment.ts` as part of editor formatting features.
   - Importer in `vsformatter.ts` (language provider) updated to reference editor feature module.

24. **Comment toggle utility relocated**
   - `commenter.ts` moved to `src/features/editor/commenter.ts` as part of editor formatting features.
   - Importers in extension entrypoints (`src/extension.ts`, `src/web/extension.ts`) updated to reference editor feature module.

25. **Compiler directive conversion utility relocated**
   - `vsdirectivesconv.ts` moved to `src/features/workspace/vsdirectivesconv.ts` as part of workspace source analysis.
   - Importers in core scanner modules (`src/features/workspace/cobolsourcescanner.ts`, `src/features/workspace/icobolsourcescanner.ts`) updated to reference workspace feature module.

26. **Case formatter utility relocated**
   - `caseformatter.ts` moved to `src/providers/language/caseformatter.ts` as part of language formatting providers.
   - Importer in `src/providers/language/vsformatter.ts` updated to reference local language provider module.

27. **String builder utility relocated**
   - `stringutils.ts` moved to `src/utils/stringutils.ts` as part of utility/helper module domain.
   - Importers in source handler modules (`src/features/workspace/filesourcehandler.ts`, `src/features/workspace/vscodesourcehandler.ts`) updated to reference utils module.

28. **Make dependency utility relocated**
   - `makedeps.ts` moved to `src/extension/commands/makedeps.ts` as part of extension command helpers.
   - Importers in `src/extension/commands/vsmakedep.ts` and `src/features/workspace/vscobscanner.ts` updated to reference extension command module.

29. **Copybook definition provider relocated**
   - `opencopybook.ts` moved to `src/features/workspace/opencopybook.ts` as part of workspace source/navigation features.
   - Importers in runtime, navigation, workspace, and provider registry modules updated to reference workspace feature module.

30. **Source tree item model relocated**
   - `sourceItem.ts` moved to `src/features/tree/sourceItem.ts` as part of tree/view feature domain.
   - Importers in tree view provider and desktop command modules updated to reference tree feature module.

31. **Split tokenizer utility relocated**
   - `splittoken.ts` moved to `src/utils/splittoken.ts` as part of utility/helper module domain.
   - Importers in `src/features/workspace/cobolsourcescanner.ts` and `src/utils/vscobolutils.ts` updated to reference utils module.

32. **Source format detector fully relocated**
   - Legacy top-level `sourceformat.ts` import path was removed, standardizing on `src/features/workspace/sourceformat.ts`.
   - Importers in scanner and editor margin modules now reference the workspace feature module path directly.

33. **Program navigation command utility relocated**
   - `cobolprogram.ts` moved to `src/extension/commands/cobolprogram.ts` as part of extension command helpers.
   - Importer in `src/vscommon_commands.ts` updated to reference extension command module.

34. **Build task provider relocated**
   - `bldTaskProvider.ts` moved to `src/extension/bldTaskProvider.ts` as part of extension activation/runtime wiring.
   - Importer in `src/extension.ts` updated to reference the extension module path.

35. **File source handler relocated**
   - `filesourcehandler.ts` moved to `src/features/workspace/filesourcehandler.ts` as part of workspace source ingestion.
   - Core scanner and test suite importers updated to reference workspace feature module path.

36. **VS Code source handler relocated**
   - `vscodesourcehandler.ts` moved to `src/features/workspace/vscodesourcehandler.ts` as part of workspace/editor source ingestion.
   - Importers in scanner and editor margin modules updated to reference workspace feature module path.

37. **File utilities relocated**
   - `fileutils.ts` moved to `src/utils/fileutils.ts` as part of shared utility/helper domain.
   - All runtime, workspace, extension command, and core consumers updated to reference the utils module path.

38. **Logger utilities relocated**
   - `vslogger.ts` moved to `src/utils/vslogger.ts` as part of shared utility/helper domain.
   - Extension, runtime, provider, workspace, and test consumers updated to reference the utils module path.

39. **Extension utility helpers relocated**
   - `vsextutis.ts` moved to `src/utils/vsextutis.ts` as part of shared utility/helper domain.
   - All extension lifecycle, provider, editor feature, web entrypoint, linter, and utility consumers updated to reference the utils module path.

40. **Global cache helper relocated**
   - `globalcachehelper.ts` moved to `src/features/workspace/globalcachehelper.ts` as part of workspace cache/source index features.
   - All 9 consumers across core scanner, workspace metadata commands, intellisense/navigation providers, and utility modules updated to reference workspace feature module path.

41. **Source handler interfaces relocated**
   - `isourcehandler.ts` moved to `src/features/workspace/isourcehandler.ts` as part of workspace source ingestion contracts.
   - All 10 consumers across scanner core, runtime external-features, editor comments, and workspace source handlers updated to reference workspace feature module path.

42. **Scanner interface contracts relocated**
   - `icobolsourcescanner.ts` moved to `src/features/workspace/icobolsourcescanner.ts` as part of workspace scanner contracts.
   - All 20 consumers across scanner core, linter, providers, workspace helpers, and command modules updated to reference workspace feature module path.

43. **Extension defaults relocated**
   - `extensionDefaults.ts` moved to `src/config/extensionDefaults.ts` as part of configuration/constants boundaries.
   - All extension lifecycle, scanner, provider, editor, runtime, and command consumers updated to reference the config module path.

44. **Scanner message/data model relocated**
   - `cobscannerdata.ts` moved to `src/features/workspace/cobscannerdata.ts` as part of workspace scanner orchestration.
   - Scanner core, worker, symbol event helper, and workspace scanner orchestrator consumers updated to reference workspace feature module path.

45. **Global symbol/cache model relocated**
   - `cobolglobalcache.ts` moved to `src/features/workspace/cobolglobalcache.ts` as part of workspace symbol/cache domain modeling.
   - All 9 consumers across scanner core, workspace cache helpers, and navigation/provider modules updated to reference workspace feature module path.

46. **Workspace symbol cache helper relocated**
   - `cobolworkspacecache.ts` moved to `src/features/workspace/cobolworkspacecache.ts` as part of workspace cache orchestration.
   - All 6 consumers across scanner core, extension activation/config handlers, and workspace scanner orchestration updated to reference workspace feature module path.

47. **Symbol table event helper relocated**
   - `cobolsymboltableeventhelper.ts` moved to `src/features/workspace/cobolsymboltableeventhelper.ts` as part of workspace scanning/event dispatch.
   - Scanner consumer updated to reference workspace feature module path.

48. **COBOL linter module relocated**
   - `cobollinter.ts` moved to `src/providers/language/cobollinter.ts` as part of language diagnostics/code-action providers.
   - Extension activation and provider lifecycle consumers updated to reference provider-language module path.

49. **Common command orchestration relocated**
   - `vscommon_commands.ts` moved to `src/extension/commands/vscommon_commands.ts` as part of extension command wiring domain.
   - Desktop/web extension entrypoints and configuration change handler updated to reference extension-command module path.

50. **VS Code source scanner wrapper relocated**
   - `vscobolscanner.ts` moved to `src/features/workspace/vscobolscanner.ts` as part of workspace scanner integration.
   - All extension, provider, runtime/editor feature, and utility consumers updated to reference workspace feature module path.

51. **Configuration runtime module relocated**
   - `vsconfiguration.ts` moved to `src/config/vsconfiguration.ts` as part of configuration domain consolidation.
   - All extension, provider, feature, and utility consumers updated to reference config module path.

52. **Core COBOL utility module relocated**
   - `vscobolutils.ts` moved to `src/utils/vscobolutils.ts` as part of shared utility/helper domain.
   - All extension entrypoints, command/language/intellisense providers, and workspace/tree feature consumers updated to reference the utils module path.

53. **Configuration contract module relocated**
   - `iconfiguration.ts` moved to `src/config/iconfiguration.ts` as part of configuration domain consolidation.
   - All core scanner files, extension/runtime modules, feature/provider/test consumers updated to reference `src/config` paths.

54. **Scanner worker module relocated**
   - `cobscanner_worker.ts` moved to `src/features/workspace/cobscanner_worker.ts` as part of workspace scanner execution features.
   - Worker-local imports were normalized to workspace/runtime/config/utils module boundaries with no behavior changes.

55. **Core source scanner module relocated**
   - `cobolsourcescanner.ts` moved to `src/features/workspace/cobolsourcescanner.ts` as part of workspace scanner parsing/domain logic.
   - All scanner consumers across providers, extension commands, tests, utilities, and workspace helpers updated to reference the workspace feature path.

56. **Scanner orchestration module relocated**
   - `cobscanner.ts` moved to `src/features/workspace/cobscanner.ts` as part of workspace scanner orchestration/runtime execution.
   - Worker scanner import and webpack scanner entry were updated to the new workspace module path while preserving `dist/cobscanner.js` output naming.

## Why this improves performance

- Avoids provider registration work before any COBOL file is in context.
- Reduces activation-time object construction and registration overhead.
- Keeps frequently used startup path focused on essential commands/config setup.

## Compatibility guarantees

- No intentional user-visible behavior change.
- Existing command IDs and configuration keys are preserved.
- Existing provider implementations remain intact and are only registration-orchestrated differently.

## Next refactor slices

- Continue shrinking top-level `src` by moving remaining integration helpers (scanner/config bridge utilities) into feature domains.
- Add barrel exports for extension command modules to reduce deep relative imports.
- Continue consolidating duplicated keyword/grammar constants under `src/language/`.
