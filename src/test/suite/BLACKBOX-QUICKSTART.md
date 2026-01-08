# Quick Start: Running Black Box Tests

## Prerequisites

Ensure you have:
- Node.js and npm installed
- VS Code extension development environment set up
- Test workspace files present (test.cbl, B30DIRb.cob, etc.)

## Quick Commands

### Run All Tests
```bash
npm test
```

### Run Only Black Box Tests
```bash
npm test -- --grep "Black Box"
```

### Run Specific Test Suite
```bash
# IntelliSense tests
npm test -- --grep "BBT-IS"

# Navigation tests
npm test -- --grep "BBT-NAV"

# Formatting tests
npm test -- --grep "BBT-FMT"

# Dialect tests
npm test -- --grep "BBT-DIALECT"

# Configuration tests
npm test -- --grep "BBT-CONFIG"

# Lifecycle tests
npm test -- --grep "BBT-LIFECYCLE"
```

### Run Single Test
```bash
npm test -- --grep "BBT-IS-001"
```

## Debug in VS Code

1. Open any test file (e.g., `blackbox.intellisense.test.ts`)
2. Press **F5** or click "Run and Debug"
3. Select **"Extension Tests"** configuration
4. Tests will run in new VS Code window

## Test Results

### Expected Output

```
Black Box: IntelliSense Tests
  ✓ BBT-IS-001: Code completion provides COBOL keywords (2045ms)
  ✓ BBT-IS-002: Hover provides information for COBOL symbols (312ms)
  ✓ BBT-IS-003: Document symbols are provided for outline view (189ms)
  ✓ BBT-IS-004: Go to Definition works for paragraphs (234ms)
  ✓ BBT-IS-005: Find References works for variables (298ms)
  ✓ BBT-IS-006: Snippet completion available for COBOL structures (176ms)

6 passing (3.5s)
```

### Understanding Results

- ✓ **Passing**: Feature works as expected
- ✗ **Failing**: Feature not working or test needs update
- **Skipped**: Test not applicable in current environment

## Common Issues

### Issue: "Extension not found"
**Solution**: Ensure extension is installed or run `npm run compile` first.

### Issue: Tests timeout
**Solution**: Increase timeout in test or check that COBOL files exist.

### Issue: "Cannot find module"
**Solution**: Run `npm install` to install dependencies.

## Test File Locations

```
src/test/suite/
├── blackbox.intellisense.test.ts  ← IntelliSense features
├── blackbox.navigation.test.ts     ← Navigation features
├── blackbox.formatting.test.ts     ← Formatting features
├── blackbox.dialect.test.ts        ← Dialect support
├── blackbox.configuration.test.ts  ← Configuration
├── blackbox.lifecycle.test.ts      ← Extension lifecycle
└── BLACKBOX-TESTING-README.md      ← Full documentation
```

## Next Steps

1. **Read full documentation**: See [BLACKBOX-TESTING-README.md](./BLACKBOX-TESTING-README.md)
2. **Review test plan**: See [TEST-PLAN.md](../../../TEST-PLAN.md)
3. **Check NonStop tests**: See [NONSTOP-COBOL-TEST.md](../../../NONSTOP-COBOL-TEST.md)

## Quick Test Examples

### Test a Specific Feature
```typescript
test("BBT-IS-001: Code completion provides COBOL keywords", async () => {
    const document = await openCobolDocument("test.cbl");
    const completions = await vscode.commands.executeCommand(
        "vscode.executeCompletionItemProvider",
        document.uri,
        new vscode.Position(10, 4)
    );
    assert.ok(completions, "Should return completions");
});
```

### Check Configuration
```typescript
test("BBT-CONFIG-002: Copybook directories configuration", () => {
    const config = vscode.workspace.getConfiguration("coboleditor");
    const copybookDirs = config.get("copybookdirs");
    assert.ok(copybookDirs !== undefined, "Setting should exist");
});
```

## Support

- **Documentation**: [BLACKBOX-TESTING-README.md](./BLACKBOX-TESTING-README.md)
- **Issues**: GitHub repository issues
- **Test Plan**: [TEST-PLAN.md](../../../TEST-PLAN.md)
