# ⚡ RUN TESTS IN 3 STEPS

## Step 1: Press F5

![Press F5 Key](or click Run > Start Debugging)

```
┌─────────────────────────────────────┐
│ Keyboard Shortcut:                   │
│                                      │
│        Press  [ F5 ]                 │
│                                      │
│ Or:                                  │
│   Run > Start Debugging              │
│                                      │
└─────────────────────────────────────┘
```

## Step 2: Select "Extension Tests"

When the dropdown appears, select:
```
┌─────────────────────────────────────┐
│ Select a debug configuration:        │
│                                      │
│  ▸ Extension Tests            ← THIS │
│    Launch Extension                  │
│    Extension Tests                   │
│    Web Extension                     │
│                                      │
└─────────────────────────────────────┘
```

## Step 3: Watch Tests Run!

A new VS Code window opens and tests run automatically.

View output in **Debug Console** (Ctrl+Shift+Y):

```
┌─────────────────────────────────────────────────────────┐
│ DEBUG CONSOLE                                            │
├─────────────────────────────────────────────────────────┤
│                                                          │
│ =====================================                   │
│ BLACK BOX TEST SUITE FOR VS CODE COBOL                  │
│ =====================================                   │
│                                                          │
│ Black Box: Master Test Suite                            │
│   ✓ BBT-000: Black Box Test Suite Overview              │
│   ✓ BBT-001: Verify test environment                    │
│   ✓ BBT-002: Verify extension installation              │
│   ✓ BBT-003: Verify test workspace                      │
│   ✓ BBT-004: Verify test files exist                    │
│   ✓ BBT-005: Test suite summary report                  │
│                                                          │
│ Black Box: IntelliSense Tests                           │
│   ✓ BBT-IS-001: Code completion provides keywords       │
│   ✓ BBT-IS-002: Hover provides information              │
│   ...                                                    │
│                                                          │
│ 65 passing (8.2s)                                       │
│                                                          │
└─────────────────────────────────────────────────────────┘
```

## ✅ That's It!

Three simple steps:
1. **F5**
2. **"Extension Tests"**
3. **View Results**

## 🔍 What If It Doesn't Work?

### Tests Don't Start
1. Make sure you're in the `vscode_cobol` folder
2. Run `npm run compile` in terminal first
3. Try again with F5

### No Output Visible
- Press **Ctrl+Shift+Y** to open Debug Console
- Or check View > Debug Console

### Extension Not Found
- Verify you're in the project root
- Run: `npm install` then `npm run compile`
- Try F5 again

## 📚 Want More Details?

See full documentation:
- [HOW-TO-RUN-TESTS.md](HOW-TO-RUN-TESTS.md) - Detailed instructions
- [BLACKBOX-TESTING-README.md](BLACKBOX-TESTING-README.md) - Complete guide
- [BLACKBOX-QUICKSTART.md](BLACKBOX-QUICKSTART.md) - Quick reference

## 🎯 Run Specific Tests

To run only IntelliSense tests:
1. Open `.vscode/launch.json`
2. Find "Extension Tests" configuration
3. Add to `args` array: `"--grep=BBT-IS"`
4. Press F5

Filter options:
- `--grep=BBT-IS` - IntelliSense
- `--grep=BBT-NAV` - Navigation
- `--grep=BBT-FMT` - Formatting
- `--grep=BBT-DIALECT` - Dialects
- `--grep=BBT-CONFIG` - Configuration
- `--grep=BBT-LIFECYCLE` - Lifecycle

## 🐛 Debug Individual Tests

1. **Open test file** (e.g., `blackbox.intellisense.test.ts`)
2. **Set breakpoint** (click left of line number)
3. **Press F5**
4. **Tests pause at breakpoint**

Now you can:
- Inspect variables
- Step through code (F10, F11)
- See VS Code API responses

## ⚠️ Why Not Command Line?

Command line testing (`npm test`) has issues:
- ❌ SSL certificate errors
- ❌ OneDrive path problems
- ❌ Module loading failures

**F5 method avoids all these issues!**

---

**Ready? Press F5 now!** 🚀
