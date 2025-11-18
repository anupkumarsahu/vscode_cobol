# NonStop COBOL Test Case - Syntax Highlighting Verification

## Test File: `test-complete-nonstop-cobol.cbl`

This test case demonstrates all the enhanced COBOL syntax highlighting features implemented in the VS Code COBOL extension.

## ✅ Features to Verify:

### 1. **NonStop COBOL Directives** 
Should be highlighted as `keyword.control.directive.nonstop.cobol`:
- `?ANSI` (line 1) - Input Format Control directive
- `?HEADING "~F~ Version: ~R~.~L~"` (line 12) - Listing Control directive

### 2. **Computer Specification (FIXED)** 
Should be highlighted as `entity.name.type.computer.cobol`:
- `TANDEM T16` (lines 30-31) - Previously incorrectly highlighted as comments
- This demonstrates the fix for OBJECT-COMPUTER and SOURCE-COMPUTER highlighting

### 3. **Section Names** 
Should be highlighted as `entity.name.function.section.cobol`:
- `WORKING-STORAGE SECTION` (line 35)
- `EXTENDED-STORAGE SECTION` (line 45) 
- `LINKAGE SECTION` (line 100)
- `MAIN SECTION` (line 109)
- `GET-SCHLUESSEL SECTION` (line 117)

### 4. **Paragraph Labels**
Should be highlighted as `entity.name.function.paragraph.cobol`:
- `MAIN-EXIT` (line 113)
- `GET-SCHLUESSEL-EXIT` (line 140)

### 5. **Comment Lines**
Should be highlighted as `comment.line.cobol.fixed`:
- All lines starting with `*` (asterisk comments)
- `* --- *` decorative comment lines
- `* --- * ------------------------------------------------>` section separators

### 6. **COBOL Keywords**
Should be highlighted as appropriate keyword types:
- Division names: `IDENTIFICATION`, `ENVIRONMENT`, `DATA`, `PROCEDURE`
- Section names: `CONFIGURATION SECTION`
- Keywords: `PROGRAM-ID`, `AUTHOR`, `DATE-WRITTEN`
- Data types: `PIC`, `COMP`, `NATIVE-2`, `NATIVE-4`
- Verbs: `MOVE`, `PERFORM`, `IF`, `COMPUTE`, `ENTER`, `ACCEPT`, `EXIT`

### 7. **Special NonStop COBOL Features**
- `ENTER TAL` statements (lines 121, 135) - NonStop TAL system calls
- `NATIVE-2`, `NATIVE-4` data types - NonStop-specific data types
- `EXTENDED-STORAGE SECTION` - NonStop memory section

## 🧪 Testing Instructions:

1. Open `test-complete-nonstop-cobol.cbl` in VS Code
2. Verify that each feature listed above has proper syntax highlighting
3. Check that:
   - `TANDEM T16` is NOT highlighted as a comment
   - Section names are properly highlighted
   - Paragraph labels are properly highlighted
   - NonStop directives are properly highlighted
   - Code folding works for sections (click/collapse ranges)

## 🔧 Recent Fixes Applied:

1. **OBJECT-COMPUTER Fix**: Separated computer specification from comment blocks
2. **Section/Paragraph Highlighting**: Added TextMate patterns for user-defined sections and paragraphs
3. **NonStop COBOL Directives**: Comprehensive support for all 6 directive categories
4. **Code Folding**: Enhanced folding markers for COBOL sections and paragraphs
5. **Paragraph Reference Fix**: Updated `PERFORM GET-SCHLUESSEL THRU GET-SCHLUESSEL-EXIT` to properly reference exit paragraph

## 📋 COBOL Code Quality Notes:

- **Paragraph References**: All defined paragraphs should be referenced by PERFORM statements
- **Exit Paragraphs**: Common pattern is `PERFORM section-name THRU section-name-EXIT`
- **Unreferenced Code**: The VS Code COBOL extension's linter can identify unreferenced sections/paragraphs

## 📝 Expected Behavior:

- ✅ All syntax elements should have distinct, appropriate colors
- ✅ No false comment highlighting on computer specifications
- ✅ Clear visual distinction between sections, paragraphs, and regular code
- ✅ Proper folding behavior for code blocks
- ✅ NonStop COBOL directives recognized and highlighted

This test case serves as a comprehensive validation of the COBOL syntax highlighting enhancements.