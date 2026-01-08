# Black Box Testing - Visual Structure

## 🎨 Test Suite Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                  VS CODE COBOL EXTENSION                         │
│                     (bitlang.cobol)                              │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ Tests
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                      TEST SUITES                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌────────────────────────────────────────────────────────┐   │
│  │  EXISTING TESTS (White Box / Unit)                     │   │
│  ├────────────────────────────────────────────────────────┤   │
│  │  • extension.test.ts         (Core parsing)            │   │
│  │  • tandem-cobol.test.ts      (Tandem dialect)          │   │
│  │  • library-navigation.test.ts (Library features)       │   │
│  │  • issues.test.ts            (Bug regression)          │   │
│  └────────────────────────────────────────────────────────┘   │
│                                                                  │
│  ┌────────────────────────────────────────────────────────┐   │
│  │  BLACK BOX TESTS (User-Facing Features) ★ NEW         │   │
│  ├────────────────────────────────────────────────────────┤   │
│  │                                                          │   │
│  │  📋 blackbox.master.test.ts                            │   │
│  │     ├── BBT-000: Overview                              │   │
│  │     ├── BBT-001: Environment                           │   │
│  │     ├── BBT-002: Extension check                       │   │
│  │     ├── BBT-003: Workspace                             │   │
│  │     ├── BBT-004: Test files                            │   │
│  │     └── BBT-005: Summary                               │   │
│  │                                                          │   │
│  │  💡 blackbox.intellisense.test.ts (6 tests)           │   │
│  │     ├── BBT-IS-001: Code completion                    │   │
│  │     ├── BBT-IS-002: Hover info                         │   │
│  │     ├── BBT-IS-003: Document symbols                   │   │
│  │     ├── BBT-IS-004: Go to definition                   │   │
│  │     ├── BBT-IS-005: Find references                    │   │
│  │     └── BBT-IS-006: Snippets                           │   │
│  │                                                          │   │
│  │  🧭 blackbox.navigation.test.ts (7 tests)             │   │
│  │     ├── BBT-NAV-001: Workspace symbols                 │   │
│  │     ├── BBT-NAV-002: Symbol hierarchy                  │   │
│  │     ├── BBT-NAV-003: Call hierarchy                    │   │
│  │     ├── BBT-NAV-004: Peek definition                   │   │
│  │     ├── BBT-NAV-005: Breadcrumbs                       │   │
│  │     ├── BBT-NAV-006: Type definition                   │   │
│  │     └── BBT-NAV-007: Implementations                   │   │
│  │                                                          │   │
│  │  ✏️  blackbox.formatting.test.ts (7 tests)            │   │
│  │     ├── BBT-FMT-001: Document format                   │   │
│  │     ├── BBT-FMT-002: Range format                      │   │
│  │     ├── BBT-FMT-003: On-type format                    │   │
│  │     ├── BBT-FMT-004: Code actions                      │   │
│  │     ├── BBT-FMT-005: Selection range                   │   │
│  │     ├── BBT-FMT-006: Folding                           │   │
│  │     └── BBT-FMT-007: Comments                          │   │
│  │                                                          │   │
│  │  🌐 blackbox.dialect.test.ts (10 tests)               │   │
│  │     ├── BBT-DIALECT-001: NonStop detection             │   │
│  │     ├── BBT-DIALECT-002: Tandem directives             │   │
│  │     ├── BBT-DIALECT-003: Computer names                │   │
│  │     ├── BBT-DIALECT-004: NonStop symbols               │   │
│  │     ├── BBT-DIALECT-005: ACU COBOL                     │   │
│  │     ├── BBT-DIALECT-006: RM COBOL                      │   │
│  │     ├── BBT-DIALECT-007: IBM i COBOL                   │   │
│  │     ├── BBT-DIALECT-008: Reference format              │   │
│  │     ├── BBT-DIALECT-009: Format detection              │   │
│  │     └── BBT-DIALECT-010: Free format                   │   │
│  │                                                          │   │
│  │  ⚙️  blackbox.configuration.test.ts (15 tests)        │   │
│  │     ├── BBT-CONFIG-001 to 015: Settings validation     │   │
│  │     └── (Copybooks, formats, scanner, logging, etc.)   │   │
│  │                                                          │   │
│  │  🔄 blackbox.lifecycle.test.ts (15 tests)             │   │
│  │     ├── BBT-LIFECYCLE-001 to 015: Extension lifecycle  │   │
│  │     └── (Activation, commands, conflicts, cleanup)     │   │
│  │                                                          │   │
│  └────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
                    ┌──────────────────┐
                    │   TEST RESULTS   │
                    └──────────────────┘
```

## 📊 Test Coverage Map

```
┌─────────────────────────────────────────────────────────────┐
│           COBOL EXTENSION FEATURES                           │
├──────────────────────┬──────────────────────────────────────┤
│ FEATURE              │ BLACK BOX TEST COVERAGE              │
├──────────────────────┼──────────────────────────────────────┤
│ Code Completion      │ ███████████████████ 🟢 High (6)     │
│ Navigation           │ ███████████████████ 🟢 High (7)     │
│ Formatting           │ ███████████████████ 🟢 High (7)     │
│ Dialects             │ ███████████████████ 🟢 High (10)    │
│ Configuration        │ ███████████████████ 🟢 High (15)    │
│ Extension Lifecycle  │ ███████████████████ 🟢 High (15)    │
└──────────────────────┴──────────────────────────────────────┘

Total: 60+ black box tests
```

## 🎯 Test Execution Flow

```
npm test
    │
    ├─→ Compile TypeScript (.ts → .js)
    │
    ├─→ Launch VS Code Extension Host
    │
    ├─→ Load Extension (bitlang.cobol)
    │
    ├─→ Run Test Suite (Mocha)
    │       │
    │       ├─→ blackbox.master.test.js
    │       │       └─→ Show overview & validate environment
    │       │
    │       ├─→ blackbox.intellisense.test.js
    │       │       └─→ Test IntelliSense features
    │       │
    │       ├─→ blackbox.navigation.test.js
    │       │       └─→ Test navigation features
    │       │
    │       ├─→ blackbox.formatting.test.js
    │       │       └─→ Test formatting features
    │       │
    │       ├─→ blackbox.dialect.test.js
    │       │       └─→ Test dialect support
    │       │
    │       ├─→ blackbox.configuration.test.js
    │       │       └─→ Test configuration
    │       │
    │       ├─→ blackbox.lifecycle.test.js
    │       │       └─→ Test extension lifecycle
    │       │
    │       └─→ [Existing test files...]
    │
    └─→ Report Results
            │
            ├─→ Console output with ✓/✗
            ├─→ Summary statistics
            └─→ Exit code (0 = pass, 1 = fail)
```

## 🗂️ File Organization

```
vscode_cobol/
│
├── BLACKBOX-TESTING-SUMMARY.md          ← 📄 Overview document
│
├── TEST-PLAN.md                          ← 📋 Original test plan
│
└── src/test/
    ├── suite/
    │   │
    │   ├── 📚 DOCUMENTATION
    │   │   ├── BLACKBOX-TESTING-README.md       (Comprehensive guide)
    │   │   ├── BLACKBOX-QUICKSTART.md           (Quick commands)
    │   │   ├── BLACKBOX-INTEGRATION-NOTES.md    (Integration info)
    │   │   └── BLACKBOX-VISUAL-STRUCTURE.md     (This file)
    │   │
    │   ├── 🧪 BLACK BOX TESTS
    │   │   ├── blackbox.master.test.ts           (5 tests)
    │   │   ├── blackbox.intellisense.test.ts     (6 tests)
    │   │   ├── blackbox.navigation.test.ts       (7 tests)
    │   │   ├── blackbox.formatting.test.ts       (7 tests)
    │   │   ├── blackbox.dialect.test.ts          (10 tests)
    │   │   ├── blackbox.configuration.test.ts    (15 tests)
    │   │   └── blackbox.lifecycle.test.ts        (15 tests)
    │   │
    │   ├── 🔬 EXISTING TESTS
    │   │   ├── extension.test.ts
    │   │   ├── tandem-cobol.test.ts
    │   │   ├── library-navigation.test.ts
    │   │   └── issues.test.ts
    │   │
    │   ├── 📁 TEST DATA
    │   │   ├── test.cbl
    │   │   ├── B30DIRb.cob
    │   │   └── test-copybook.cpy
    │   │
    │   └── index.ts                      ← Test runner
    │
    └── runTest.ts                        ← Test harness
```

## 🎮 Command Shortcuts

```
┌─────────────────────────────────────────────────────────┐
│ COMMAND                          │ WHAT IT DOES          │
├──────────────────────────────────┼───────────────────────┤
│ npm test                         │ Run ALL tests         │
│ npm test -- --grep "Black Box"   │ Black box only        │
│ npm test -- --grep "BBT-IS"      │ IntelliSense only     │
│ npm test -- --grep "BBT-NAV"     │ Navigation only       │
│ npm test -- --grep "BBT-FMT"     │ Formatting only       │
│ npm test -- --grep "BBT-DIALECT" │ Dialect only          │
│ npm test -- --grep "BBT-CONFIG"  │ Configuration only    │
│ npm test -- --grep "BBT-LIFECYCLE"│ Lifecycle only       │
│ npm run compile                  │ Compile TypeScript    │
│ F5 in VS Code                    │ Debug tests           │
└──────────────────────────────────┴───────────────────────┘
```

## 🔍 Test ID Legend

```
BBT-000 to BBT-005         → Master Suite (Environment)
BBT-IS-001 to BBT-IS-006   → IntelliSense Tests
BBT-NAV-001 to BBT-NAV-007 → Navigation Tests
BBT-FMT-001 to BBT-FMT-007 → Formatting Tests
BBT-DIALECT-001 to 010     → Dialect Tests
BBT-CONFIG-001 to 015      → Configuration Tests
BBT-LIFECYCLE-001 to 015   → Lifecycle Tests
```

## 📈 Test Categories Visual

```
              BLACK BOX TESTS (60+ total)
                      │
        ┌─────────────┼─────────────┐
        │             │             │
    ┌───▼───┐     ┌───▼───┐    ┌───▼────┐
    │Master │     │Feature│    │System  │
    │Tests  │     │Tests  │    │Tests   │
    │(5)    │     │(27)   │    │(30)    │
    └───┬───┘     └───┬───┘    └───┬────┘
        │             │             │
        │         ┌───┴───┬────┐    │
        │         │       │    │    │
        │    ┌────▼──┐ ┌─▼─┐ ┌▼──┐ │
        │    │Intel- │ │Nav│ │Fmt│ │
        │    │Sense  │ │(7)│ │(7)│ │
        │    │(6)    │ └───┘ └───┘ │
        │    └───────┘              │
        │                           │
        │                      ┌────┴───┬────┐
        │                      │        │    │
        │                  ┌───▼──┐ ┌──▼─┐ ┌▼──────┐
        │                  │Dialect│ │Conf│ │Life-  │
        │                  │(10)   │ │(15)│ │cycle  │
        │                  └───────┘ └────┘ │(15)   │
        │                                   └───────┘
        │
    Environment Validation
    Overview & Summary
```

## 🎨 Color-Coded Results

When tests run, you'll see:
```
✓ = Green  = Test Passed
✗ = Red    = Test Failed
⊘ = Yellow = Test Skipped
ℹ = Blue   = Information
```

## 🌊 Test Data Flow

```
Test File (.ts)
    │
    ├─→ Compile to JavaScript (.js)
    │
    ├─→ Load in VS Code Extension Host
    │
    ├─→ Execute Test
    │       │
    │       ├─→ Call VS Code API
    │       │   (vscode.commands.executeCommand)
    │       │
    │       ├─→ Extension Handles Request
    │       │   (Providers respond)
    │       │
    │       └─→ Return Results
    │
    ├─→ Assert Results
    │   (assert.ok, assert.strictEqual)
    │
    └─→ Log Output
        (console.log)
```

## 📊 Coverage Visualization

```
IntelliSense:     [████████████████████] 100%  (6/6 tests)
Navigation:       [████████████████████] 100%  (7/7 tests)
Formatting:       [████████████████████] 100%  (7/7 tests)
Dialects:         [████████████████████] 100% (10/10 tests)
Configuration:    [████████████████████] 100% (15/15 tests)
Lifecycle:        [████████████████████] 100% (15/15 tests)
────────────────────────────────────────────────────────────
Overall:          [████████████████████] 100% (60+/60+ tests)
```

## 🎯 Quick Reference

| Symbol | Meaning |
|--------|---------|
| 🟢 | High coverage / Working |
| 🟡 | Medium coverage / Partial |
| 🔴 | Low coverage / Issue |
| ✅ | Complete / Ready |
| ⚠️  | Warning / Attention needed |
| 📋 | Master/Overview |
| 💡 | IntelliSense |
| 🧭 | Navigation |
| ✏️  | Formatting |
| 🌐 | Dialects |
| ⚙️  | Configuration |
| 🔄 | Lifecycle |

---

**Created**: January 7, 2026  
**Purpose**: Visual guide to black box test structure  
**Status**: ✅ Ready to use
