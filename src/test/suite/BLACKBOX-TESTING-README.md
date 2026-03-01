# Black Box Testing Suite

## Overview

This directory contains comprehensive black box tests for the VS Code COBOL extension (`bitlang.cobol`). Black box testing validates the extension from the user's perspective, testing inputs and outputs without requiring knowledge of internal implementation.

## Test Structure

### Test Files

| Test Suite | File | Focus Area | Test Count |
|------------|------|------------|------------|
| **IntelliSense** | `blackbox.intellisense.test.ts` | Code completion, hover, symbols | 6 tests |
| **Navigation** | `blackbox.navigation.test.ts` | Go-to-definition, references, breadcrumbs | 7 tests |
| **Formatting** | `blackbox.formatting.test.ts` | Document formatting, code actions, folding | 7 tests |
| **Dialects** | `blackbox.dialect.test.ts` | NonStop/Tandem, ACU, RM, IBM i COBOL | 10 tests |
| **Configuration** | `blackbox.configuration.test.ts` | Settings, preferences, problem matchers | 15 tests |
| **Lifecycle** | `blackbox.lifecycle.test.ts` | Activation, commands, extension management | 15 tests |

**Total: 60+ Black Box Tests**

## Test Categories

### 1. IntelliSense Tests (`BBT-IS-###`)

Tests user-facing IntelliSense features:
- **BBT-IS-001**: Code completion provides COBOL keywords
- **BBT-IS-002**: Hover information for symbols
- **BBT-IS-003**: Document symbols for outline view
- **BBT-IS-004**: Go to Definition for paragraphs
- **BBT-IS-005**: Find References for variables
- **BBT-IS-006**: Snippet completion availability

**Key Validation**: Language service integration, symbol resolution, completion quality

### 2. Navigation Tests (`BBT-NAV-###`)

Tests code navigation features:
- **BBT-NAV-001**: Workspace symbol search
- **BBT-NAV-002**: Document symbol hierarchy (outline)
- **BBT-NAV-003**: Call hierarchy for paragraphs
- **BBT-NAV-004**: Peek definition functionality
- **BBT-NAV-005**: Breadcrumb navigation support
- **BBT-NAV-006**: Type definition provider
- **BBT-NAV-007**: Implementation provider for copybooks

**Key Validation**: Symbol navigation, cross-reference tracking, hierarchy visualization

### 3. Formatting Tests (`BBT-FMT-###`)

Tests code formatting and editing:
- **BBT-FMT-001**: Document formatting provider
- **BBT-FMT-002**: Range formatting
- **BBT-FMT-003**: On-type formatting (e.g., period triggers)
- **BBT-FMT-004**: Code actions for COBOL issues
- **BBT-FMT-005**: Selection range for smart selection
- **BBT-FMT-006**: Folding range provider
- **BBT-FMT-007**: Comment toggling

**Key Validation**: Format quality, indentation rules, code action availability

### 4. Dialect Tests (`BBT-DIALECT-###`)

Tests COBOL dialect support:
- **BBT-DIALECT-001**: NonStop COBOL file detection
- **BBT-DIALECT-002**: Tandem directive syntax (? directives)
- **BBT-DIALECT-003**: Tandem computer name detection
- **BBT-DIALECT-004**: Symbol recognition in NonStop COBOL
- **BBT-DIALECT-005-007**: ACU, RM, IBM i COBOL support
- **BBT-DIALECT-008**: Tandem reference format margins
- **BBT-DIALECT-009**: Source format detection
- **BBT-DIALECT-010**: Free format COBOL support

**Key Validation**: Multi-dialect support, language ID switching, dialect-specific syntax

### 5. Configuration Tests (`BBT-CONFIG-###`)

Tests extension settings and configuration:
- **BBT-CONFIG-001**: Extension configuration accessibility
- **BBT-CONFIG-002**: Copybook directories configuration
- **BBT-CONFIG-003**: File format configuration
- **BBT-CONFIG-004**: Source scanner enable/disable
- **BBT-CONFIG-005**: Valid COBOL language IDs
- **BBT-CONFIG-006**: Logging level configuration (with precedence)
- **BBT-CONFIG-007**: Copybook extensions
- **BBT-CONFIG-008**: ANSI/TANDEM language ID configuration
- **BBT-CONFIG-009-015**: Linter, cache, margins, IntelliSense, problem matchers, updates, language contributions

**Key Validation**: Settings schema, default values, configuration updates

### 6. Lifecycle Tests (`BBT-LIFECYCLE-###`)

Tests extension activation and management:
- **BBT-LIFECYCLE-001**: Extension installation and active state
- **BBT-LIFECYCLE-002**: Activation on COBOL file open
- **BBT-LIFECYCLE-003**: Command contributions
- **BBT-LIFECYCLE-004**: Language-switching commands
- **BBT-LIFECYCLE-005**: Copybook navigation commands
- **BBT-LIFECYCLE-006**: Diagnostic commands
- **BBT-LIFECYCLE-007**: Extension command registry health
- **BBT-LIFECYCLE-008**: Output channel creation
- **BBT-LIFECYCLE-009**: Web extension support (vscode.dev)
- **BBT-LIFECYCLE-010-015**: Metadata, activation events, dependencies, task providers, configuration schema, cleanup

**Key Validation**: Extension lifecycle, command registration, conflict handling

## Running Black Box Tests

### Prerequisites

1. VS Code Extension Test environment
2. Test workspace with COBOL files:
   - `test.cbl` - Basic COBOL test file
   - `B30DIRb.cob` - Advanced test file with libraries
   - `test-complete-nonstop-cobol.cbl` - NonStop COBOL
   - `test-tandem-issue.cbl` - Tandem dialect
   - `test-copybook.cpy` - Copybook test file

### Run All Tests

```bash
npm test
```

### Run Specific Test Suite

```bash
npm test -- --grep "Black Box: IntelliSense"
npm test -- --grep "Black Box: Dialect"
npm test -- --grep "BBT-CONFIG"
```

### Debug Tests in VS Code

1. Open test file (e.g., `blackbox.intellisense.test.ts`)
2. Press F5 or select "Extension Tests" debug configuration
3. Set breakpoints as needed

## Test Design Principles

### Black Box Approach

✅ **DO TEST:**
- User-visible behavior
- VS Code API interactions
- Configuration effects
- Command execution results
- Language service responses
- Extension metadata

❌ **DON'T TEST:**
- Internal implementation details
- Private methods or classes
- Internal data structures
- Specific algorithm implementations

### Test Independence

Each test should:
- Run independently without dependencies
- Not modify global state (or clean up after itself)
- Use descriptive test IDs (BBT-XXX-###)
- Log diagnostic information for debugging
- Handle graceful failures (test environment variations)

### Graceful Failure Handling

Tests use try-catch patterns to handle:
- Features not yet activated
- Language services not ready
- Test environment limitations
- Optional features not configured

Example:
```typescript
try {
    const result = await vscode.commands.executeCommand(...);
    assert.ok(result, "Feature should respond");
} catch (error) {
    console.log("Test result:", error);
}
```

## Test Output

### Console Logging

Each test logs diagnostic information:
```
BBT-IS-001: Code completion provides COBOL keywords
  ✓ Completion list should be returned
  ℹ Found 42 completion items
  ℹ Keywords found: perform, move, display
```

### Assertion Strategy

Tests use flexible assertions:
- **Hard Assertions**: Core functionality that must work
- **Soft Assertions**: Features that may be context-dependent
- **Logging Only**: Diagnostic checks for optional features

## Integration with Existing Tests

This black box suite complements existing tests:

| Test Type | Location | Focus |
|-----------|----------|-------|
| **Black Box** | `blackbox.*.test.ts` | User-facing features |
| **Unit Tests** | `extension.test.ts` | Core parsing logic |
| **Integration** | `library-navigation.test.ts` | Specific features |
| **Dialect** | `tandem-cobol.test.ts` | Tandem dialect |
| **Issues** | `issues.test.ts` | Bug regression |

## Test Coverage

### Feature Coverage Matrix

| Feature Area | Black Box Tests | Coverage |
|--------------|----------------|----------|
| IntelliSense | 6 tests | 🟢 High |
| Navigation | 7 tests | 🟢 High |
| Formatting | 7 tests | 🟢 High |
| Dialects | 10 tests | 🟢 High |
| Configuration | 15 tests | 🟢 High |
| Lifecycle | 15 tests | 🟢 High |

### Dialect Coverage

| COBOL Dialect | Tests | Status |
|---------------|-------|--------|
| Standard COBOL | ✅ | Covered |
| NonStop/Tandem | ✅ | 4 dedicated tests |
| ACU COBOL | ✅ | Configuration test |
| RM COBOL | ✅ | Configuration test |
| IBM i COBOL | ✅ | Configuration test |
| Non-ANSI/Tandem dialects | ❌ | Out of scope |

## Maintenance Guidelines

### Adding New Tests

1. **Choose appropriate test file** based on feature area
2. **Follow naming convention**: `BBT-XXX-###` where XXX is category code
3. **Include descriptive test name**: Clear, action-oriented description
4. **Add documentation**: Update this README with new test description
5. **Log diagnostic info**: Help future debugging with console.log
6. **Handle async properly**: Use async/await with proper error handling

### Test ID Allocation

| Category | ID Range | Available |
|----------|----------|-----------|
| IntelliSense | BBT-IS-001 to BBT-IS-999 | 007+ |
| Navigation | BBT-NAV-001 to BBT-NAV-999 | 008+ |
| Formatting | BBT-FMT-001 to BBT-FMT-999 | 008+ |
| Dialect | BBT-DIALECT-001 to BBT-DIALECT-999 | 011+ |
| Configuration | BBT-CONFIG-001 to BBT-CONFIG-999 | 016+ |
| Lifecycle | BBT-LIFECYCLE-001 to BBT-LIFECYCLE-999 | 016+ |

### Updating Tests

When features change:
1. **Review affected tests** - Search for related BBT-XXX tests
2. **Update test logic** - Adjust expectations to match new behavior
3. **Update documentation** - Keep README in sync
4. **Run full test suite** - Verify no regressions

## Troubleshooting

### Common Issues

**Issue**: Tests timeout waiting for language features
- **Solution**: Increase `waitForLanguageFeatures()` delay
- **Why**: Extension may need more time to activate in test environment

**Issue**: Completion/Hover providers return undefined
- **Solution**: Verify test file has proper COBOL syntax
- **Why**: Language services may not activate for invalid files

**Issue**: Configuration tests fail
- **Solution**: Check VS Code version compatibility
- **Why**: Configuration schema may vary by version

**Issue**: Dialect tests don't find expected content
- **Solution**: Verify test files exist and contain expected code
- **Why**: Tests depend on specific test file content

### Debug Tips

1. **Run single test**: Use `.only()` - `test.only("BBT-IS-001", ...)`
2. **Add breakpoints**: Debug in VS Code with F5
3. **Check output channel**: View "COBOL" output channel for logs
4. **Verify extension active**: Check extension is loaded in test environment
5. **Review console logs**: Tests log diagnostic information

## Future Enhancements

### Planned Test Areas

- [ ] **Performance Tests**: Measure responsiveness with large files
- [ ] **Copybook Resolution**: Test copybook search and loading
- [ ] **Linter Integration**: Test problem detection and reporting
- [ ] **Semantic Tokens**: Test syntax highlighting accuracy
- [ ] **Code Lens**: Test inline action availability
- [ ] **Task Integration**: Test compiler integration
- [ ] **Workspace Trust**: Test trusted/untrusted workspace behavior
- [ ] **Multi-root Workspace**: Test multi-folder workspace support

### Test Automation

- [ ] **CI/CD Integration**: Run tests on GitHub Actions
- [ ] **Coverage Reporting**: Generate coverage reports
- [ ] **Performance Benchmarks**: Track test execution time
- [ ] **Flaky Test Detection**: Identify unreliable tests

## References

- [TEST-PLAN.md](../../../TEST-PLAN.md) - Overall test plan
- [NONSTOP-COBOL-TEST.md](../../../NONSTOP-COBOL-TEST.md) - NonStop dialect testing
- [VS Code Testing API](https://code.visualstudio.com/api/working-with-extensions/testing-extension)
- [Mocha Testing Framework](https://mochajs.org/)

## Contact

For test-related questions:
1. Review existing test patterns
2. Check TEST-PLAN.md for requirements
3. Open issue on GitHub repository

---

**Last Updated**: January 7, 2026  
**Test Suite Version**: 1.0  
**Total Tests**: 60+
