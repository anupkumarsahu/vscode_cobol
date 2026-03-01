# Black Box Testing Plan - COBOL Extension

## Version: 25.10.27
## Package: bitlang.cobol

---

## 1. Installation & Activation Tests

### 1.1 Fresh Installation
- [ ] Install extension from VSIX package
- [ ] Verify extension appears in Extensions view
- [ ] Check extension activates on startup
- [ ] Verify no errors in Output > COBOL channel
- [ ] Confirm activation events trigger correctly

### 1.2 Upgrade Testing
- [ ] Install over existing version
- [ ] Verify settings migration
- [ ] Check metadata cache preserved
- [ ] Confirm no configuration loss

### 1.3 Multi-Platform
- [ ] Test on Windows 10/11
- [ ] Test on macOS (if applicable)
- [ ] Test on Linux (if applicable)
- [ ] Test in VS Code Web (vscode.dev)

---

## 2. Language Support Tests

### 2.1 File Detection & Association
- [ ] Open `.cob` file → Detects as COBOL
- [ ] Open `.cbl` file → Detects as COBOL  
- [ ] Open `.cpy` file → Detects as COBOL
- [ ] Open `.cobol` file → Detects as COBOL
- [ ] Open file with TANDEM extensions → Detects as COBOL_TANDEM
- [ ] Verify `IDENTIFICATION DIVISION` triggers COBOL detection in plaintext

### 2.2 Syntax Highlighting
- [ ] Keywords highlighted correctly (PROCEDURE DIVISION, PERFORM, etc.)
- [ ] Comments highlighted (both `*` in column 7 and inline)
- [ ] String literals highlighted
- [ ] Numeric literals highlighted
- [ ] Variable names highlighted
- [ ] `OBJECT-COMPUTER. TANDEM/16.` → `/16` same color as `TANDEM`
- [ ] SQL blocks (EXEC SQL...END-EXEC) highlighted
- [ ] COPY statements highlighted
- [ ] Section/paragraph names highlighted

### 2.3 Source Format Detection
- [ ] Fixed format (columns 7-11) works correctly
- [ ] Free format works correctly
- [ ] Variable format works correctly
- [ ] Terminal format (ACU/RM style) works correctly
- [ ] Tandem format works correctly
- [ ] `>>SOURCE FORMAT` directives honored
- [ ] File format patterns from settings applied

---

## 3. IntelliSense & Code Completion

### 3.1 Variable Completion
- [ ] Type `MOVE` → Suggests variables from WORKING-STORAGE
- [ ] Type `ADD` → Suggests numeric variables
- [ ] Completion works across sections
- [ ] Copybook variables appear in suggestions
- [ ] `intellisense_style` setting applies (camelcase/uppercase/lowercase)

### 3.2 Keyword Completion
- [ ] Type `PERF` → Suggests `PERFORM`
- [ ] Type `ACC` → Suggests `ACCEPT`
- [ ] Keywords with required spaces honored (`intellisense_add_space_keywords`)
- [ ] Custom intellisense rules applied

### 3.3 Copybook Completion
- [ ] Type `COPY` → Suggests known copybooks
- [ ] Copybook paths resolved from `copybookdirs`
- [ ] `${fileDirname}` in `perfile_copybookdirs` works

### 3.4 Snippet Completion
- [ ] COBOL snippets work when `snippets: true`
- [ ] Snippet placeholders navigable with Tab

---

## 4. Navigation Features

### 4.1 Go to Definition
- [ ] Click variable → Jumps to definition in WORKING-STORAGE
- [ ] Click section name → Jumps to section definition
- [ ] Click paragraph name → Jumps to paragraph
- [ ] Click copybook in `COPY` statement → Opens copybook file
- [ ] Click library name in `COPY ... IN library` → Opens library file
- [ ] Ctrl+Click works same as F12

### 4.2 Find All References
- [ ] Right-click variable → Shows all usages
- [ ] Right-click section → Shows all PERFORM calls
- [ ] Right-click paragraph → Shows all PERFORM calls
- [ ] Reference count accurate

### 4.3 Outline View
- [ ] IDENTIFICATION DIVISION shown
- [ ] DATA DIVISION sections shown
- [ ] PROCEDURE DIVISION sections/paragraphs shown
- [ ] Copybooks shown (if `copybooks_nested: true`)
- [ ] Clicking item navigates to location
- [ ] `outline` setting controls detail level (on/partial/skeleton/off)

### 4.4 Breadcrumbs
- [ ] Shows current section/paragraph
- [ ] Clickable navigation works
- [ ] Updates as cursor moves

### 4.5 Quick Navigation Commands
- [ ] `COBOL: Go to Procedure Division` works
- [ ] `COBOL: Go to Working-Storage Section` works
- [ ] `COBOL: Go to Data Division` works
- [ ] `COBOL: Go to next Section/Division` works
- [ ] `COBOL: Move to Previous Section/Division` works

---

## 5. Hover Information

### 5.1 Variable Hover
- [ ] Hover over variable → Shows definition with PIC clause
- [ ] Shows level number and data type
- [ ] Shows VALUE if present
- [ ] `hover_show_variable_definition: true` required

### 5.2 Copybook Hover
- [ ] Hover over copybook name → Shows file path
- [ ] Shows file size if found
- [ ] Shows warning if not found

### 5.3 Library Hover  
- [ ] Hover over library name in `COPY ... IN lib` → Shows library file info
- [ ] Shows path and existence status

### 5.4 API Hover
- [ ] Hover over known API (CBL_ functions) → Shows description
- [ ] Shows link to documentation
- [ ] `hover_show_known_api` controls detail (off/short/long)

### 5.5 Literal Hover
- [ ] Hover over `NX"..."` hex literal → Shows decoded UTF-16
- [ ] Hover over `X"..."` hex literal → Shows decoded ASCII
- [ ] `hover_show_encoded_literals: true` required

---

## 6. Code Lens Features

### 6.1 Reference CodeLens
- [ ] Variables show reference count above definition
- [ ] Clicking shows reference list
- [ ] `enable_codelens_variable_references: true` required

### 6.2 Section/Paragraph CodeLens
- [ ] Sections show PERFORM count
- [ ] Paragraphs show PERFORM count  
- [ ] `enable_codelens_section_paragraph_references: true` required

### 6.3 Copy Replacing CodeLens
- [ ] Simple COPY REPLACING shows preview
- [ ] `enable_codelens_copy_replacing: true` required

---

## 7. Formatting & Editing

### 7.1 Tabstops
- [ ] Tab key moves to next COBOL tabstop (7, 11, 15...)
- [ ] Shift+Tab moves to previous tabstop
- [ ] `tabstops` array honored
- [ ] `enable_tabstop: true` required
- [ ] Anchor-based tabstops work (`tabstops_anchors`)

### 7.2 Comment Toggle
- [ ] `Ctrl+/` inserts `*` in column 7 (fixed format)
- [ ] `Ctrl+/` uncomments line
- [ ] Works with selections
- [ ] `line_comment: true` uses COBOL-aware comments

### 7.3 Format on Type
- [ ] Return key auto-indents continuation lines
- [ ] `format_on_return: true` required
- [ ] Constants folded to uppercase if `format_constants_to_uppercase: true`

### 7.4 Margin Coloring
- [ ] Columns 1-6 shaded (sequence area)
- [ ] Column 7 highlighted (indicator area)
- [ ] Columns 8-11 distinct (Area A)
- [ ] Columns 12-72 normal (Area B)
- [ ] Columns 73-80 shaded (identification area)
- [ ] `margin: true` required

### 7.5 Comment Tag Coloring
- [ ] `* TODO` highlighted in configured color
- [ ] `* !` highlighted
- [ ] `* ?` highlighted
- [ ] Only tag highlighted if `comment_tag_word: true`
- [ ] Entire line highlighted if `comment_tag_word: false`
- [ ] `enable_comment_tags: true` required

### 7.6 Case Transformation Commands
- [ ] `Make keywords lowercase` works
- [ ] `Make keywords uppercase` works
- [ ] `Make keywords camelcase` works
- [ ] `Make fields lowercase` works
- [ ] `Make fields uppercase` works
- [ ] `Make fields camelcase` works

### 7.7 Comment Removal Commands
- [ ] `Remove COBOL Comments` removes all comment lines
- [ ] `Remove COBOL Identification Area` clears columns 73-80
- [ ] `Remove Column Numbers` clears columns 1-6

---

## 8. Linting & Diagnostics

### 8.1 Basic Linting
- [ ] Unused sections flagged
- [ ] Unused paragraphs flagged
- [ ] `linter: true` required
- [ ] `linter_unused_sections: true` required
- [ ] `linter_unused_paragraphs: true` required

### 8.2 Copybook Warnings
- [ ] Missing copybook shows warning
- [ ] Suppressed if `linter_ignore_missing_copybook: true`

### 8.3 House Standards
- [ ] Data division order violations flagged
- [ ] `linter_house_standards: true` required
- [ ] `linter_house_standards_rules` patterns enforced

### 8.4 Problem Matchers
- [ ] Compile errors parsed and shown in Problems panel
- [ ] Click error navigates to line
- [ ] Multiple compilers supported (COBOL-IT, ACU, etc.)

---

## 9. Copybook Resolution

### 9.1 Directory Search
- [ ] Finds copybooks in `copybookdirs`
- [ ] Expands environment variables ($COBCPY, etc.)
- [ ] Searches workspace folders
- [ ] Uses `perfile_copybookdirs` with `${fileDirname}`
- [ ] Tries all `copybookexts` extensions

### 9.2 Library Support (Tandem)
- [ ] `COPY copybook IN library` resolves library file
- [ ] Library file found in source directory
- [ ] Case-insensitive search works
- [ ] Extensions tried (.cpy, .cbl, .cob, etc.)

### 9.3 Nested Copybooks
- [ ] Copybook includes another copybook
- [ ] Depth limit honored (`copybook_scan_depth: 32`)
- [ ] Circular references detected and stopped

### 9.4 Invalid Path Handling
- [ ] UNC paths blocked if `disable_unc_copybooks_directories: true`
- [ ] Invalid paths logged
- [ ] Graceful fallback on missing copybooks

---

## 10. Metadata & Caching

### 10.1 Metadata Generation
- [ ] Scan workspace for symbols (`Scan source files in workspace for metadata`)
- [ ] Global cache updated
- [ ] Symbols appear in workspace symbol search
- [ ] `maintain_metadata_cache: true` required

### 10.2 Cache Management
- [ ] `Clear metadata` command works
- [ ] Cache rebuilds on next scan
- [ ] `process_metadata_cache_on_start: true` scans on activation

### 10.3 Performance
- [ ] Large files don't freeze UI (scan_line_limit: 20000)
- [ ] Scanning timeout prevents hangs (scan_time_limit: 4000ms)
- [ ] In-memory cache limits respected (in_memory_cache_size: 6)

---

## 11. Source View Panel

### 11.1 File Display
- [ ] COBOL files shown in tree
- [ ] Grouped by type

### 11.2 Context Menu
- [ ] `Run Without Debugging` executes program
- [ ] `Start Debugging` launches debugger
- [ ] Only available in trusted workspaces

---

## 12. Settings Validation

### 12.1 Language IDs
- [ ] Only `COBOL` and `COBOL_TANDEM` accepted in `valid_cobol_language_ids`
- [ ] Invalid IDs rejected with error message

### 12.2 File Format Patterns
- [ ] `fileformat` patterns apply correctly
- [ ] Glob patterns match files
- [ ] `sourceformat` values honored (fixed/free/variable/terminal)

### 12.3 Logging
- [ ] `logging_level` array filters messages
- [ ] Precedence works (trace < debug < info < warning < error < fatal)
- [ ] COBOL output channel shows configured levels
- [ ] `cache_metadata_verbose_messages: true` shows detail

---

## 13. Workspace Trust

### 13.1 Untrusted Workspaces
- [ ] Restricted settings enforced
- [ ] No arbitrary code execution
- [ ] File system access limited
- [ ] Terminal commands disabled

### 13.2 Trusted Workspaces
- [ ] All features enabled
- [ ] Terminal integration works
- [ ] File watchers active
- [ ] Build tasks available

---

## 14. Multi-Root Workspaces

### 14.1 Multiple Folders
- [ ] Each folder has own settings
- [ ] Copybook search per folder
- [ ] Metadata cache per folder
- [ ] `workspacefolders_order` honored

### 14.2 Cross-Folder Navigation
- [ ] Go to definition across folders
- [ ] Find references across folders
- [ ] Workspace symbol search includes all folders

---

## 15. Extension Compatibility

### 15.1 Removed Extensions
- [ ] No Micro Focus COBOL references
- [ ] No Rocket COBOL conflicts
- [ ] Listfile languages removed (COBOL_MF_LISTFILE, etc.)
- [ ] COBOL_MF_PREP removed

### 15.2 Conflict Detection
- [ ] Detects conflicting COBOL extensions
- [ ] Shows warning in COBOL output channel
- [ ] Provides resolution guidance

---

## 16. Performance & Stability

### 16.1 Large Files
- [ ] 10,000+ line files parse without hanging
- [ ] Line limit enforced (`scan_line_limit: 20000`)
- [ ] Time limit enforced (`scan_time_limit: 4000`)

### 16.2 Large Workspaces
- [ ] 1000+ files indexed
- [ ] No UI freezing during metadata scan
- [ ] Memory usage reasonable (`in_memory_cache_size: 6`)

### 16.3 File Watching
- [ ] File changes detected
- [ ] Cache invalidated on edit
- [ ] No excessive CPU usage
- [ ] Debounced updates prevent thrashing

---

## 17. Error Handling

### 17.1 Malformed COBOL
- [ ] Syntax errors don't crash extension
- [ ] Partial parsing continues
- [ ] Errors logged, not thrown

### 17.2 Missing Files
- [ ] Missing copybooks logged
- [ ] Graceful degradation
- [ ] No null reference errors

### 17.3 Invalid Settings
- [ ] Bad paths ignored
- [ ] Invalid regex patterns caught
- [ ] Defaults used on error

---

## 18. Accessibility

### 18.1 Keyboard Navigation
- [ ] All features keyboard accessible
- [ ] Tab navigation works
- [ ] Screen reader compatible

### 18.2 High Contrast Themes
- [ ] Syntax highlighting visible
- [ ] Margin colors adjusted
- [ ] No color-only information

---

## 19. Localization

### 19.1 UI Text
- [ ] English strings correct
- [ ] No hardcoded locale assumptions
- [ ] Date/time formatting locale-aware

---

## 20. Regression Tests (Previous Issues)

### 20.1 Copybook Navigation
- [ ] Library name clickable in `COPY ... IN library`
- [ ] Hover works on library names
- [ ] File resolution case-insensitive

### 20.2 Syntax Highlighting
- [ ] `TANDEM/16` highlighted as single unit (not split at `/`)
- [ ] Comments in columns 1-6 ignored
- [ ] SQL blocks don't break highlighting

### 20.3 Settings Cleanup
- [ ] No orphaned Micro Focus settings
- [ ] No empty setting blocks
- [ ] JSON valid and formatted

---

## Test Environment Setup

### Required Files
1. Sample COBOL program with:
   - IDENTIFICATION DIVISION
   - WORKING-STORAGE with variables
   - PROCEDURE DIVISION with sections/paragraphs
   - COPY statements
   - SQL blocks (EXEC SQL...END-EXEC)

2. Sample copybook files (.cpy)

3. Sample library file (Tandem format)

4. Various file extensions to test detection

### Settings Variations
Test with:
- Default settings
- Fixed format strategy
- Free format strategy
- Tandem format strategy
- Linting enabled/disabled
- Various logging levels
- Trusted/untrusted workspaces

---

## Success Criteria

- [ ] All critical features work without errors
- [ ] No console errors or warnings
- [ ] Syntax highlighting accurate
- [ ] Navigation features reliable
- [ ] Performance acceptable (no UI freezing)
- [ ] Settings respected
- [ ] Copybook resolution works
- [ ] Compatible with VS Code 1.105.0+

---

## Test Report Template

```
Test Date: ___________
Tester: ___________
VS Code Version: ___________
Extension Version: 25.10.27
Platform: ___________

Critical Issues: ___________
Minor Issues: ___________
Passed Tests: _____/_____
Failed Tests: _____/_____

Notes:
___________
```
