# Running Black Box Tests - Quick Guide

## ✅ RECOMMENDED METHOD: Use VS Code Debugger

This is the **easiest and most reliable** way to run the black box tests, especially in corporate environments with SSL certificates and OneDrive.

### Step-by-Step Instructions

#### 1. **Open VS Code**
   - Make sure you have the project open in VS Code
   - Current folder: `vscode_cobol`

#### 2. **Press F5**
   - Or click **Run > Start Debugging** from the menu
   - Or click the Run icon in the Activity Bar, then click the green play button

#### 3. **Select "Extension Tests"**
   - If prompted, choose **"Extension Tests"** from the dropdown
   - This is already configured in `.vscode/launch.json`

#### 4. **Watch Tests Run**
   - A new VS Code window will open
   - Tests execute automatically
   - View results in the **Debug Console** (Ctrl+Shift+Y)
   - Or in the **Terminal** within VS Code

### Expected Output

```
Black Box: Master Test Suite
================================================================================
BLACK BOX TEST SUITE FOR VS CODE COBOL EXTENSION
================================================================================

Extension: bitlang.cobol
Test Type: Black Box (User-Facing Features)
Test Count: 60+ tests across 6 categories
...
✓ BBT-000: Black Box Test Suite Overview
✓ BBT-001: Verify test environment
✓ BBT-002: Verify extension installation
✓ BBT-003: Verify test workspace
✓ BBT-004: Verify test files exist
✓ BBT-005: Test suite summary report

Black Box: IntelliSense Tests
  ✓ BBT-IS-001: Code completion provides COBOL keywords (2045ms)
  ✓ BBT-IS-002: Hover provides information for COBOL symbols (312ms)
  ...

65 passing (8.2s)
```

## Alternative: Command Line (Has Issues)

⚠️ **Warning**: Command line testing has issues in OneDrive folders and corporate environments.

If you still want to try:

```powershell
# Set environment variable to skip SSL verification
$env:NODE_TLS_REJECT_UNAUTHORIZED="0"

# Compile TypeScript
npm run compile

# Run tests
npm test -- --grep "Black Box"
```

**Known Issues:**
- SSL certificate errors
- OneDrive path resolution problems
- Module loading failures

## Debugging Individual Tests

### Debug a Specific Test File

1. **Open the test file** (e.g., `src/test/suite/blackbox.intellisense.test.ts`)
2. **Set breakpoints** (click left of line numbers)
3. **Press F5**
4. **Tests pause at breakpoints** - inspect variables, step through code

### Debug a Specific Test

1. Find the test you want to debug
2. Add `.only()` to focus on it:
   ```typescript
   test.only("BBT-IS-001: Code completion provides COBOL keywords", async () => {
       // Test code
   });
   ```
3. Press F5
4. Only that test will run

## Run Specific Test Categories

### Using VS Code's Run and Debug

Modify `args` in `.vscode/launch.json` temporarily:

```json
{
    "name": "Extension Tests",
    "type": "extensionHost",
    "request": "launch",
    "args": [
        "${workspaceFolder}",
        "--extensionDevelopmentPath=${workspaceFolder}",
        "--extensionTestsPath=${workspaceFolder}/out/test/suite/index",
        "--grep=BBT-IS"  // Add this line to filter tests
    ],
    ...
}
```

Then press F5.

**Filter Examples:**
- `--grep=BBT-IS` - IntelliSense tests only
- `--grep=BBT-NAV` - Navigation tests only  
- `--grep=BBT-DIALECT` - Dialect tests only
- `--grep=BBT-000` - Just the overview test

## Viewing Test Results

### Debug Console (Recommended)
- Open with **Ctrl+Shift+Y** or View > Debug Console
- Shows all test output with colors
- Shows pass/fail status
- Shows console.log messages from tests

### Terminal
- View > Terminal (Ctrl+`)
- Shows test execution progress
- May be less detailed than Debug Console

### Output Channel
- View > Output
- Select "COBOL" from dropdown
- Shows extension-specific logs

## Troubleshooting

### Tests Don't Run
- **Ensure compiled**: Run `npm run compile` first
- **Check .vscode/launch.json**: Should have "Extension Tests" configuration
- **View errors**: Check Debug Console for error messages

### Extension Not Found
- Make sure you're in the project root folder
- Verify `package.json` exists
- Recompile: `npm run compile`

### Tests Fail
- **Check test file paths**: Ensure test COBOL files exist
- **View errors**: Read test output carefully
- **Try master test**: Run BBT-000 first to verify environment

### No Output
- Open Debug Console: Ctrl+Shift+Y
- Check Terminal panel
- Ensure tests actually started

## Quick Reference Commands

| Action | Method |
|--------|--------|
| **Run all tests** | F5 → "Extension Tests" |
| **Debug specific test** | Open test file → F5 |
| **Set breakpoint** | Click left of line number |
| **View output** | Ctrl+Shift+Y (Debug Console) |
| **Stop debugging** | Shift+F5 |
| **Recompile** | `npm run compile` in terminal |

## Tips for Effective Testing

### 1. Start with Master Suite
Run `BBT-000` first to verify environment:
- Edit launch.json to add `--grep=BBT-000`
- Press F5
- Should show overview and environment check

### 2. Test One Category at a Time
Focus on one area:
- IntelliSense: `--grep=BBT-IS`
- Navigation: `--grep=BBT-NAV`
- Dialects: `--grep=BBT-DIALECT`

### 3. Use Breakpoints for Deep Debugging
- Set breakpoints in test files
- Inspect VS Code API responses
- Step through test logic

### 4. Check Console Logs
Tests log useful diagnostic information:
```
Found 42 completion items
Keywords found: perform, move, display
```

## Summary

✅ **Easiest Method**: Press F5 in VS Code  
✅ **Most Reliable**: Avoids SSL and path issues  
✅ **Best for Debugging**: Set breakpoints, inspect variables  
✅ **Corporate-Friendly**: Works in HPE environment  

**Next Steps:**
1. Press F5 now
2. Select "Extension Tests"
3. Watch tests run
4. Review results in Debug Console

---

**Need Help?**
- See [BLACKBOX-TESTING-README.md](BLACKBOX-TESTING-README.md) for full documentation
- See [TROUBLESHOOTING-SSL-TESTS.md](../../TROUBLESHOOTING-SSL-TESTS.md) for SSL issues
- Check Debug Console for error details
