# Black Box Testing - Integration Notes

## ✅ Tests Are Ready to Run

All black box tests are compatible with the existing test infrastructure and will be automatically discovered by the test runner.

## 🔄 Test Discovery

The test runner (`src/test/suite/index.ts`) uses glob pattern `*.test.js` which will automatically include all compiled black box test files:

```typescript
const files = glob.sync("*.test.js", { cwd: testsRoot, absolute: true });
```

This means:
- `blackbox.master.test.ts` → `blackbox.master.test.js` ✅
- `blackbox.intellisense.test.ts` → `blackbox.intellisense.test.js` ✅
- `blackbox.navigation.test.ts` → `blackbox.navigation.test.js` ✅
- `blackbox.formatting.test.ts` → `blackbox.formatting.test.js` ✅
- `blackbox.dialect.test.ts` → `blackbox.dialect.test.js` ✅
- `blackbox.configuration.test.ts` → `blackbox.configuration.test.js` ✅
- `blackbox.lifecycle.test.ts` → `blackbox.lifecycle.test.js` ✅

## 🚀 Running Tests

### Compile TypeScript First
```bash
npm run compile
```

### Run All Tests (Including Black Box)
```bash
npm test
```

### Run Only Black Box Tests
```bash
npm test -- --grep "Black Box"
```

### Run Specific Category
```bash
npm test -- --grep "BBT-IS"        # IntelliSense only
npm test -- --grep "BBT-NAV"       # Navigation only
npm test -- --grep "BBT-DIALECT"   # Dialect tests only
```

## 📋 Existing Test Files

The black box tests complement these existing tests:

| Existing Test | Focus | Compatibility |
|---------------|-------|---------------|
| `extension.test.ts` | Core parsing logic | ✅ Compatible |
| `tandem-cobol.test.ts` | Tandem dialect parsing | ✅ Complementary |
| `library-navigation.test.ts` | Library navigation | ✅ Compatible |
| `issues.test.ts` | Bug regression | ✅ Compatible |

## 🎯 Test Execution Order

Mocha will execute tests in this order:
1. `blackbox.master.test.ts` - Shows overview and validates environment
2. Other test files in alphabetical order
3. Black box tests intermixed with existing tests

### To Run Master Suite First
```bash
npm test -- --grep "BBT-000"
```

## 🔍 Debug Configuration

The existing `.vscode/launch.json` "Extension Tests" configuration works with black box tests:

```json
{
    "name": "Extension Tests",
    "type": "extensionHost",
    "request": "launch",
    "runtimeExecutable": "${execPath}",
    "args": [
        "--extensionDevelopmentPath=${workspaceFolder}",
        "--extensionTestsPath=${workspaceFolder}/out/test/suite/index"
    ],
    "outFiles": ["${workspaceFolder}/out/test/**/*.js"],
    "preLaunchTask": "npm: compile"
}
```

**To debug black box tests:**
1. Open any `blackbox.*.test.ts` file
2. Set breakpoints as needed
3. Press F5
4. Select "Extension Tests"

## 📊 Test Output Integration

Black box tests use the same Mocha test framework and will integrate seamlessly:

```
Extension Test Suite
  Core Extension Test Suite
    ✓ Read file [basic] (test.cbl)
    ✓ Parse file for constants/paragraphs/sections (test.cbl)
    ✓ Verify outline view for B30DIRb.cob

  Black Box: Master Test Suite
    ✓ BBT-000: Black Box Test Suite Overview
    ✓ BBT-001: Verify test environment
    ✓ BBT-002: Verify extension installation

  Black Box: IntelliSense Tests
    ✓ BBT-IS-001: Code completion provides COBOL keywords
    ✓ BBT-IS-002: Hover provides information for COBOL symbols
    ...

  Black Box: Navigation Tests
    ...

  Black Box: Dialect-Specific Tests
    ...

  Black Box: Configuration Tests
    ...

  Black Box: Extension Lifecycle Tests
    ...
```

## 🔧 No Configuration Changes Required

**Good news**: No changes needed to existing files!

The black box tests:
- ✅ Use existing test infrastructure
- ✅ Follow existing patterns (Mocha, TDD style)
- ✅ Compatible with existing test runner
- ✅ Use same debug configuration
- ✅ Integrate with existing npm scripts

## 📝 Test File Naming Convention

Black box tests follow a clear naming pattern:
- `blackbox.*.test.ts` - All black box test files
- `blackbox.master.test.ts` - Entry point with overview
- `blackbox.<category>.test.ts` - Category-specific tests

This makes them easy to:
- Identify visually
- Run as a group (`--grep "Black Box"`)
- Maintain separately

## 🎨 Test Style Consistency

Black box tests match existing test style:
- ✅ Use Mocha `suite()` and `test()` functions (TDD style)
- ✅ Use `assert` module for assertions
- ✅ Import VS Code types from `vscode`
- ✅ Use `async/await` for asynchronous operations
- ✅ Include descriptive test names
- ✅ Add console.log for diagnostic output

Example matching existing style:
```typescript
suite("Black Box: IntelliSense Tests", () => {
    test("BBT-IS-001: Code completion provides COBOL keywords", async () => {
        const document = await openCobolDocument("test.cbl");
        const completions = await vscode.commands.executeCommand(...);
        assert.ok(completions, "Completion list should be returned");
    });
});
```

## 🚦 CI/CD Integration

If you have CI/CD pipelines, black box tests work with existing setup:

```yaml
# Example GitHub Actions (if applicable)
- name: Run tests
  run: npm test
  # Black box tests included automatically

# Run only black box tests in separate job
- name: Run black box tests
  run: npm test -- --grep "Black Box"
```

## 📈 Test Coverage Reports

If using code coverage tools (Istanbul, nyc, etc.), black box tests will be included in coverage reports since they exercise the extension through VS Code APIs.

## 🔄 Test Maintenance

When updating extension features:
1. **Existing tests** - Update as before
2. **Black box tests** - Update corresponding `blackbox.*.test.ts` file
3. **Both types complement each other** - Unit tests for logic, black box for user experience

## 📚 Documentation Structure

```
Project Root
├── BLACKBOX-TESTING-SUMMARY.md          ← High-level overview (this file's companion)
└── src/test/suite/
    ├── BLACKBOX-TESTING-README.md       ← Comprehensive guide
    ├── BLACKBOX-QUICKSTART.md           ← Quick commands
    ├── BLACKBOX-INTEGRATION-NOTES.md    ← This file
    ├── blackbox.master.test.ts          ← Entry point
    ├── blackbox.intellisense.test.ts    ← IntelliSense tests
    ├── blackbox.navigation.test.ts      ← Navigation tests
    ├── blackbox.formatting.test.ts      ← Formatting tests
    ├── blackbox.dialect.test.ts         ← Dialect tests
    ├── blackbox.configuration.test.ts   ← Configuration tests
    └── blackbox.lifecycle.test.ts       ← Lifecycle tests
```

## ✅ Checklist for First Run

- [ ] Run `npm install` (if not done recently)
- [ ] Run `npm run compile` to compile TypeScript
- [ ] Run `npm test` to execute all tests
- [ ] Check output for black box test results
- [ ] Review `BLACKBOX-TESTING-SUMMARY.md` for overview
- [ ] Read `BLACKBOX-QUICKSTART.md` for commands
- [ ] Optionally run specific categories: `npm test -- --grep "BBT-IS"`

## 🎯 Success Criteria

You'll know black box tests are working when:
- ✅ `npm test` runs without errors
- ✅ You see "Black Box:" test suites in output
- ✅ Master suite shows overview and summary
- ✅ Tests complete with passing/failing status
- ✅ Console shows diagnostic information

## 📞 Support

- **Integration Issues**: Check this file
- **Running Tests**: See `BLACKBOX-QUICKSTART.md`
- **Understanding Tests**: See `BLACKBOX-TESTING-README.md`
- **Test Results**: See `BLACKBOX-TESTING-SUMMARY.md`

---

**Status**: ✅ Integrated and ready to run  
**Changes Required**: None - works with existing setup  
**First Command**: `npm run compile && npm test`
