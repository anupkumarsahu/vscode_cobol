# COBOL Extension Testing Framework

This directory contains test fixtures and utilities for black box testing the COBOL extension.

## Directory Structure

```
test-framework/
├── README.md                    # This file
├── test-fixtures/               # Sample COBOL files for testing
│   ├── sample-program.cbl      # Main test program (fixed format)
│   ├── free-format.cbl         # Free format test
│   ├── tandem-format.cob       # Tandem format test
│   ├── copylib-standard-data.cpy  # Copybook
│   ├── copylib-open-file.cpy   # Copybook
│   └── b30qalib                # Library file (Tandem SECTION format)
├── test-config/                # Test workspace configuration
│   └── .vscode/
│       └── settings.json       # Test settings
└── test-results/               # Test execution results (created during testing)
```

## Test Fixtures

### sample-program.cbl
Main test program demonstrating:
- Fixed format COBOL (columns 7-11)
- All major divisions (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
- File handling (OPEN, READ, CLOSE)
- Section and paragraph structure
- COPY statements with library references
- SQL embedded blocks (EXEC SQL...END-EXEC)
- Conditional logic and loops
- Comment tags (TODO, !, ?)
- Level 88 conditions
- String manipulation
- Arithmetic operations

**Features tested:**
- Syntax highlighting
- Go to definition
- Find references
- Outline view
- IntelliSense
- Hover information
- CodeLens
- Linting
- Copybook resolution

### free-format.cbl
Free format COBOL (no margins):
- `>>SOURCE FORMAT IS FREE` directive
- Modern COBOL style
- Tests format detection

### tandem-format.cob
Tandem/NonStop COBOL:
- Reference format (columns 1-132)
- `TANDEM/T16` computer name (tests `/` highlighting fix)
- No sequence area
- Tests COBOL_TANDEM language ID

### Copybooks

**copylib-standard-data.cpy**
- Standard data structures
- Date/time fields
- Constants
- Tests: COPY statement resolution

**copylib-open-file.cpy**
- File status codes
- Level 88 conditions
- Tests: Nested copybook resolution

**b30qalib** (Library file)
- Tandem SECTION-based library
- Multiple sections:
  - COPYLIB-STANDARD-DATA
  - COPYLIB-PRINT-FILE
  - COPYLIB-OPEN-FILE
- Tests: `COPY ... IN library` syntax

## How to Use

### 1. Setup Test Environment

Open the test workspace:
```powershell
code test-framework/test-fixtures
```

Or copy `test-config/.vscode/settings.json` to your test workspace.

### 2. Run Black Box Tests

Follow the checklist in `TEST-PLAN.md` using these test files.

**Example Test Workflow:**

1. **Syntax Highlighting**
   - Open `sample-program.cbl`
   - Verify keywords highlighted (PROCEDURE, DIVISION, PERFORM)
   - Check `TANDEM/16` highlighted as single unit
   - Verify comments highlighted (lines starting with `*`)
   - Check SQL blocks highlighted differently

2. **Go to Definition**
   - Click on `WS-TOTAL-RECORDS` in PROCEDURE DIVISION
   - Should jump to line 40 (WORKING-STORAGE SECTION)
   - Click on `1000-INITIALIZE` in MAIN-LOGIC
   - Should jump to section definition

3. **Find All References**
   - Right-click `WS-EOF-FLAG`
   - Should show all usages across program

4. **Copybook Navigation**
   - Click on `COPYLIB-STANDARD-DATA` in COPY statement
   - Should open library file `b30qalib`
   - Click on library name `B30QALIB`
   - Should open `b30qalib` file

5. **Hover Information**
   - Hover over `WS-TOTAL-RECORDS`
   - Should show: `PIC 9(6) VALUE ZERO`
   - Hover over `COPYLIB-OPEN-FILE`
   - Should show file path and size

6. **IntelliSense**
   - Type `MOVE` and space
   - Should suggest variables from WORKING-STORAGE
   - Type `PERFORM`
   - Should suggest section/paragraph names

7. **Outline View**
   - Open Outline panel
   - Should show:
     - IDENTIFICATION DIVISION
     - ENVIRONMENT DIVISION
     - DATA DIVISION
       - FILE SECTION
       - WORKING-STORAGE SECTION
       - LINKAGE SECTION
     - PROCEDURE DIVISION
       - MAIN-LOGIC SECTION
       - 1000-INITIALIZE SECTION
       - 2000-PROCESS-FILE SECTION
       - 3000-FINALIZE SECTION

8. **Linting**
   - Problems panel should show any issues
   - Unused sections/paragraphs flagged

9. **Format Detection**
   - Open `free-format.cbl` → Should detect free format
   - Open `tandem-format.cob` → Should detect terminal format
   - Open `sample-program.cbl` → Should detect fixed format

10. **Comment Tags**
    - Line 47: `* TODO:` should be highlighted in orange
    - Line 62: `* !` should be highlighted in red
    - Line 63: `* ?` should be highlighted in blue

### 3. Settings Variations

Test different configurations by modifying `.vscode/settings.json`:

**Fixed Format Strategy:**
```json
{
    "coboleditor.fileformat_strategy": "always_fixed"
}
```

**Tandem Format Strategy:**
```json
{
    "coboleditor.fileformat_strategy": "always_tandem"
}
```

**Disable Linting:**
```json
{
    "coboleditor.linter": false
}
```

**Enable All Logging:**
```json
{
    "coboleditor.logging_level": [
        "trace",
        "debug",
        "info",
        "warning",
        "error",
        "fatal"
    ]
}
```

### 4. Document Results

Create test report in `test-results/` directory:

```
test-results/
├── test-run-2025-12-01.md
├── screenshots/
│   ├── syntax-highlighting.png
│   ├── go-to-definition.png
│   └── hover-info.png
└── logs/
    └── cobol-output.log
```

## Expected Behavior

### Syntax Highlighting
- Keywords: Blue
- Strings: Orange
- Numbers: Light green
- Comments: Green
- SQL blocks: Different shade
- `TANDEM/16`: Single color (not split at `/`)

### Navigation
- All variables navigable to definition
- All section/paragraph names navigable
- Copybooks open on click
- Library names open library file

### IntelliSense
- Variables suggested in context
- Keywords with space if configured
- Copybook names suggested after COPY

### Diagnostics
- Unused sections flagged
- Unused paragraphs flagged
- Missing copybooks warned (if linting enabled)

### Performance
- No UI freezing on large files
- Quick response (<500ms) for completion
- Smooth scrolling and typing

## Troubleshooting

### Copybooks Not Found
Check:
1. `coboleditor.copybookdirs` includes test fixtures path
2. `coboleditor.perfile_copybookdirs` includes `${fileDirname}`
3. File extensions match `coboleditor.copybookexts`
4. Library file exists in same directory

### Syntax Highlighting Issues
Check:
1. File detected as COBOL (bottom right status bar)
2. Language ID correct (COBOL or COBOL_TANDEM)
3. Format strategy appropriate for file
4. No conflicting extensions installed

### Navigation Not Working
Check:
1. `coboleditor.enable_source_scanner: true`
2. Metadata cache built (run "Scan source files")
3. File parsed without errors (check COBOL output channel)
4. Trusted workspace (if using file system features)

### Performance Issues
Check:
1. `coboleditor.scan_line_limit` appropriate (default 20000)
2. `coboleditor.scan_time_limit` reasonable (default 4000ms)
3. Not too many files in `copybookdirs`
4. UNC paths disabled if on slow network

## Additional Test Scenarios

### Edge Cases
1. Empty file
2. File with only comments
3. Malformed COBOL
4. Very long lines (>200 chars)
5. Deeply nested COPY statements
6. Circular COPY references

### Multi-File Scenarios
1. Multiple programs referencing same copybook
2. Copybook with COPY statements
3. Cross-folder references in multi-root workspace

### Performance Tests
1. File with 50,000 lines
2. Workspace with 1,000+ files
3. Rapid typing/editing
4. Multiple files open simultaneously

## Automation Opportunities

Future enhancements could include:
- Automated test runner script
- Screenshot comparison
- Performance benchmarking
- Regression test suite
- CI/CD integration

## Contributing

When adding new test fixtures:
1. Add descriptive comments
2. Cover specific feature area
3. Update this README
4. Reference in TEST-PLAN.md
5. Keep files focused and minimal
