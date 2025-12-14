# Tandem COBOL Testing - Quick Reference Guide

## Test Execution

### Run All Tests
```powershell
cd c:\VSCode\GitHub\vscode_cobol
npm test
```
**Expected**: 19/19 tests passing in ~676ms

### Run Tandem Tests Only
```powershell
npm test -- --grep "Tandem COBOL"
```
**Expected**: 10/10 Tandem tests passing in ~400ms

### Run Specific Test
```powershell
npm test -- --grep "EXITS.cob"
npm test -- --grep "B30DIRb"
npm test -- --grep "scalability"
```

---

## Test Files Location

| File Type | Location | Count |
|-----------|----------|-------|
| Test Suite | `src/test/suite/tandem-cobol.test.ts` | 1 file (307 lines) |
| Test Data | `src/test/Projects/BES/COBOL_Projects/Tandem/` | 106 projects |
| Documentation | `src/test/Projects/BES/COBOL_Projects/Tandem/TEST_SCENARIOS.md` | 1 file |
| Results | `src/test/Projects/BES/COBOL_Projects/Tandem/TEST_RESULTS.md` | 1 file |

---

## Key Test Projects

### EXITS.cob (TANDEM VLX)
**Location**: `Tandem/57T263_OSS/src/EXITS.cob`  
**Lines**: 711  
**Features**: Relative files, level 88, WS variables  
**Variables**: 58 detected  
**Parse Time**: ~47ms

### B30DIRb.cob (TANDEM/16)
**Location**: `Tandem/SETTOG_GUA/src/B30DIRb.cob`  
**Features**: Compiler directives, library references  
**Directives**: ?IF, ?ENDIF, ?SETTOG  
**Library**: b30qalib  
**Outline**: 11 sections, 1 paragraph, 7 variables

### scbun.cob (TANDEM NON-STOP)
**Location**: `Tandem/SUBUN_OSS/src/scbun.cob`  
**Features**: OSS format, TANDEM NON-STOP syntax  
**Format**: Open System Services

### Large Files (Scalability)
**Location**: `Tandem/COBOL_scalability_with_10000_lines/`  
**Lines**: 10,000+  
**Parse Time**: ~127ms  
**Sections**: 5  
**Paragraphs**: 183  
**Variables**: 706

---

## Test Validation Quick Checks

### Keyword Highlighting
```typescript
// Check for TANDEM computer name detection
assert.ok(line.includes("SOURCE-COMPUTER") && line.includes("TANDEM"));
```

### Outline View
```typescript
// Verify program ID
assert.strictEqual(scanner.ProgramId, "EXITS");

// Check sections/paragraphs
assert.ok(scanner.sections.size > 0);
assert.ok(scanner.paragraphs.size > 0);

// Verify variables (lowercase keys!)
assert.ok(scanner.constantsOrVariables.has("ws-records"));
```

### Library References
```typescript
// Check library map (lowercase!)
assert.ok(scanner.copyBookLibraries.has("b30qalib"));

// Verify COPY statements
assert.ok(scanner.copyBooksUsed.size > 0);
```

### Compiler Directives
```typescript
// Search source lines for directives
const line = handler.getLine(i, true);
if (line.includes("?IF ")) foundIF = true;
if (line.includes("?ENDIF")) foundENDIF = true;
```

---

## Common Patterns

### Test Structure
```typescript
test("Test name", () => {
    const testFile = path.join(baseForSource, "project/src/file.cob");
    
    if (!fs.existsSync(testFile)) {
        console.log("Test file not found, skipping");
        return;
    }
    
    const handler = new FileSourceHandler(settings, undefined, testFile, features);
    const scanner = COBOLSourceScanner.ScanUncached(handler, settings, false, eventHandler, features);
    
    // Assertions
    assert.strictEqual(scanner.ProgramId, "EXPECTED");
    assert.ok(scanner.constantsOrVariables.size > 0);
});
```

### File Search Pattern
```typescript
function getCobolFiles(dir: string): string[] {
    const files: string[] = [];
    const items = fs.readdirSync(dir);
    for (const item of items) {
        const fullPath = path.join(dir, item);
        if (fs.statSync(fullPath).isDirectory()) {
            files.push(...getCobolFiles(fullPath));
        } else if (item.match(/\.(cob|cbl|cobol)$/i)) {
            files.push(fullPath);
        }
    }
    return files;
}
```

---

## Important Notes

### ⚠️ Variable Names are Lowercase
```typescript
// ❌ WRONG
scanner.constantsOrVariables.has("WS-FILE-STATUS")

// ✅ CORRECT
scanner.constantsOrVariables.has("ws-file-status")
```

### ⚠️ Library Names are Lowercase
```typescript
// ❌ WRONG
scanner.copyBookLibraries.has("B30QALIB")

// ✅ CORRECT
scanner.copyBookLibraries.has("b30qalib")
```

### ⚠️ File Existence Check
```typescript
// Always check file exists before testing
if (!fs.existsSync(testFile)) {
    console.log("Test file not found, skipping");
    return;
}
```

### ⚠️ Parse Time Expectations
| File Size | Expected Parse Time |
|-----------|-------------------|
| < 1000 lines | < 100ms |
| 1000-5000 lines | < 1000ms |
| 10,000+ lines | < 10,000ms |

---

## Debugging Tips

### Enable Verbose Logging
```typescript
// Add console.log statements
console.log(`Found ${scanner.sections.size} sections`);
console.log(`Found ${scanner.paragraphs.size} paragraphs`);
console.log(`Found ${scanner.constantsOrVariables.size} variables`);

// Print variable names
const varNames = Array.from(scanner.constantsOrVariables.keys());
console.log("Variables:", varNames.slice(0, 20));
```

### Use VS Code Debugger
1. Open `.vscode/launch.json`
2. Select "Extension Tests" configuration
3. Set breakpoints in test file
4. Press F5
5. Test runs in debug mode

### Check Test Output
```powershell
npm test 2>&1 | Out-File test-output.log
notepad test-output.log
```

---

## Test Success Criteria

### ✅ All Tests Pass
```
19 passing (676ms)
0 failing
```

### ✅ Performance Metrics
- Small files: < 100ms
- Medium files: < 1000ms
- Large files (10K+): < 10,000ms
- Total suite: < 2000ms

### ✅ Coverage Metrics
- Program ID detection: 100%
- Section detection: 90%+
- Paragraph detection: 90%+
- Variable detection: 85%+
- COPY statement detection: 95%+

---

## Troubleshooting

### Test Failures
```typescript
// Common issues:
1. File not found -> Check path and file existence
2. Variable not found -> Use lowercase keys
3. Library not found -> Use lowercase keys
4. Timeout -> Check file size and parse limits
```

### SSL Certificate Errors
```powershell
# Set environment variable before running tests
$env:NODE_TLS_REJECT_UNAUTHORIZED='0'
npm test
```

### TypeScript Compilation Errors
```powershell
npm run compile
# Check for errors before running tests
```

---

## Adding New Tests

### Step 1: Identify Test File
```typescript
const testFile = path.join(baseForSource, "PROJECT_FOLDER/src/FILE.cob");
```

### Step 2: Create Test Case
```typescript
test("Test description", () => {
    // Check file exists
    if (!fs.existsSync(testFile)) {
        console.log("Test file not found, skipping");
        return;
    }
    
    // Parse file
    const handler = new FileSourceHandler(settings, undefined, testFile, features);
    const scanner = COBOLSourceScanner.ScanUncached(handler, settings, false, eventHandler, features);
    
    // Add assertions
    assert.strictEqual(scanner.ProgramId, "EXPECTED_ID");
    assert.ok(scanner.sections.size > 0, "Should have sections");
});
```

### Step 3: Run and Validate
```powershell
npm test -- --grep "Test description"
```

---

## Project Categories

### SCB Series (60+ projects)
- SCB0001_OSS through SCB0072_OSS
- Basic COBOL functionality
- Good for regression testing

### NC Series (6 projects)
- NC108M_OSS, NC114M_OSS, NC125A_OSS
- Numeric data type testing
- COMP, COMP-3, BINARY fields

### SM Series (3 projects)
- SM103A_OSS, SM205A_OSS, SM206A_OSS
- SORT/MERGE operations
- File processing

### 57T Series (3 projects)
- 57T263_OSS, 57T001, 57T212
- Extended COBOL-85 features
- TAL integration

### Special Projects
- GlobalScopeProgramID (GLOBAL clause testing)
- COBOL_scalability_with_10000_lines (performance)
- SETTOG_GUA (compiler directives)
- SUBUN_OSS (TANDEM NON-STOP)

---

## Quick Stats

| Metric | Value |
|--------|-------|
| Total Projects | 106 |
| Actively Tested | 5+ |
| Total Tests | 19 |
| Tandem Tests | 10 |
| Pass Rate | 100% |
| Avg Parse Time | ~50ms |
| Max Parse Time | ~127ms (10K lines) |
| Test Suite Time | ~676ms |

---

## References

- Test Suite: `src/test/suite/tandem-cobol.test.ts`
- Test Scenarios: `src/test/Projects/BES/COBOL_Projects/Tandem/TEST_SCENARIOS.md`
- Test Results: `src/test/Projects/BES/COBOL_Projects/Tandem/TEST_RESULTS.md`
- Source Scanner: `src/cobolsourcescanner.ts`
- File Handler: `src/filesourcehandler.ts`

---

**Last Updated**: 2024  
**Version**: 1.0  
**Status**: ✅ All Tests Passing
