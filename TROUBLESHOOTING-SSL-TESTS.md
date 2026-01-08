# Troubleshooting: SSL Certificate Error

## Problem
```
✖ Error downloading, retrying (attempt 3 of 3): unable to get local issuer certificate
✖ Error: Error: unable to get local issuer certificate
Failed to run tests
```

## Cause
The VS Code test runner (`@vscode/test-electron`) is trying to download VS Code but encounters SSL certificate validation issues, typically in corporate environments with SSL inspection.

## Solutions (Try in Order)

### Solution 1: Use Existing VS Code Installation (Recommended)

Instead of downloading VS Code, use your installed version:

```bash
# Set environment variable to skip download
$env:VSCODE_TEST_USE_INSTALLED="1"
npm test
```

Or permanently set it:
```bash
[System.Environment]::SetEnvironmentVariable('VSCODE_TEST_USE_INSTALLED', '1', 'User')
```

### Solution 2: Disable SSL Verification (Temporary)

**⚠️ Only for testing, not for production:**

```bash
# PowerShell
$env:NODE_TLS_REJECT_UNAUTHORIZED="0"
npm test
```

Or in one command:
```bash
$env:NODE_TLS_REJECT_UNAUTHORIZED="0"; npm test -- --grep "Black Box"
```

### Solution 3: Configure npm to Ignore SSL

**⚠️ Use with caution:**

```bash
npm config set strict-ssl false
npm test
npm config set strict-ssl true  # Re-enable after testing
```

### Solution 4: Use VS Code's Built-in Test Runner

Run tests directly from VS Code:

1. Press **F5**
2. Select **"Extension Tests"** from debug dropdown
3. Tests will run in a new VS Code window

This bypasses the download issue entirely.

### Solution 5: Debug Configuration (No Download)

Update `.vscode/launch.json` to use installed VS Code:

```json
{
    "name": "Extension Tests",
    "type": "extensionHost",
    "request": "launch",
    "runtimeExecutable": "${execPath}",  // Uses current VS Code
    "args": [
        "--extensionDevelopmentPath=${workspaceFolder}",
        "--extensionTestsPath=${workspaceFolder}/out/test/suite/index"
    ],
    "outFiles": ["${workspaceFolder}/out/test/**/*.js"],
    "preLaunchTask": "npm: compile"
}
```

### Solution 6: Install Corporate Certificate

If you have a corporate proxy certificate:

1. Export the certificate to a file (e.g., `corporate-cert.pem`)
2. Set Node to use it:

```bash
$env:NODE_EXTRA_CA_CERTS="C:\path\to\corporate-cert.pem"
npm test
```

## Recommended Approach for HPE Environment

Based on your HPE environment with OneDrive, I **strongly recommend**:

### **BEST SOLUTION: Use VS Code's Debugger** ✅

This avoids all SSL and path issues:

1. Open VS Code in your project
2. Press **F5** (or click Run > Start Debugging)
3. Select **"Extension Tests"** from the dropdown
4. Tests will run in a new VS Code window

**This method:**
- ✅ No SSL certificate issues
- ✅ No OneDrive path problems
- ✅ Allows debugging with breakpoints
- ✅ Shows test output in Debug Console
- ✅ Works reliably in corporate environments

### Alternative: Command Line (May have OneDrive issues)
```powershell
$env:NODE_TLS_REJECT_UNAUTHORIZED="0"; npm test -- --grep "Black Box"
```

⚠️ **Note**: Command line testing may fail in OneDrive folders due to path resolution issues. Use VS Code debugger instead.

## Alternative: Run Individual Test Files

You can also debug individual test files:

1. Open any `blackbox.*.test.ts` file
2. Press **F5**
3. Tests run in debugger (no download needed)

## Verify Solution

Once you've applied a solution, verify:

```bash
npm test -- --grep "BBT-000"  # Run just the overview test
```

If successful, run all black box tests:

```bash
npm test -- --grep "Black Box"
```

## Corporate Proxy Configuration

If you have a proxy, you may also need:

```bash
npm config set proxy http://proxy.company.com:port
npm config set https-proxy http://proxy.company.com:port
```

## Summary

**Easiest Solution**: Press **F5** in VS Code and select "Extension Tests"

**Command Line Solution**: 
```powershell
$env:NODE_TLS_REJECT_UNAUTHORIZED="0"; npm test -- --grep "Black Box"
```

---

**Last Updated**: January 7, 2026  
**Issue**: SSL certificate validation in test runner  
**Status**: Solutions provided
