# NonStop COBOL Integration Summary

## Overview
The VS Code COBOL extension now provides comprehensive syntax highlighting support for NonStop COBOL question mark (`?`) directives across all major categories.

## Supported Directive Categories

### 1. Input Format Control Directives
- `?TANDEM` - Specifies Tandem source format
- `?ANSI` - Specifies ANSI source format  
- `?COLUMNS` - Specifies logical length of source lines

### 2. Listing Control Directives
- `?HEADING` - Specifies page heading text
- `?PAGE` - Page control
- `?LINES` - Specifies lines per page
- `?LIST` / `?NOLIST` - Enables/suppresses compiler listing
- `?MAP` / `?NOMAP` - Enables/suppresses symbol map
- `?FMAP` - Enables listing of source file map
- `?SHOWCOPY` / `?NOSHOWCOPY` - Controls COPY statement listing
- `?WARN` / `?NOWARN` - Enables/suppresses warnings
- `?DIAGNOSE-85` / `?NODIAGNOSE-85` - COBOL85 compatibility warnings
- `?DIAGNOSE-74` / `?NODIAGNOSE-74` - COBOL74 compatibility warnings
- `?DIAGNOSEALL` / `?NODIAGNOSEALL` - Multiple reference warnings
- `?MIGRATION-CHECK` - COBOL-2002 reserved word warnings
- `?INNERLIST` / `?NOINNERLIST` - Mnemonic code listing
- `?SUPPRESS` / `?NOSUPPRESS` - Listing suppression
- `?SUBSET` - Flags extensions/obsolete elements
- `?FIPS` / `?NOFIPS` - Federal Information Processing Standard compliance

### 3. Source Text Manipulation Directives
- `?SECTION` - Marks beginning of text portion in EDIT file
- `?SOURCE` - Reads EDIT file or sections
- `?SHOWFILE` / `?NOSHOWFILE` - Source file identification control
- `?SETTOG` - Turns toggles on
- `?RESETTOG` - Turns toggles off

### 4. Code-Generation Control Directives
- `?COMPILE` - Generates object code
- `?SYNTAX` - Suppresses object code generation
- `?RUNNABLE` - Produces a loadfile
- `?OPTIMIZE` - Specifies optimization level
- `?SYMBOLS` / `?NOSYMBOLS` - Controls symbol table inclusion
- `?INSPECT` / `?NOINSPECT` - Debugger selection
- `?SAVEABEND` / `?NOSAVEABEND` - Abnormal termination state saving
- `?CALL-SHARED` - Generates shared code (PIC)
- `?SHARED` / `?NOSHARED` - DLL shared code generation
- `?CHECK` - Run-time checking level
- `?BLANK` / `?NOBLANK` - Default initialization control
- `?CANCEL` / `?NOCANCEL` - Data initialization for non-INITIAL programs
- `?CODECOV` - Code coverage instrumentation
- `?GLOBALIZED` - Preemptable object code generation
- `?OBJEXTENT` - Object file extent size (Guardian only)
- `?PORT` / `?NOPORT` - X/Open and XPG4 compatibility
- `?UL` - User library object code

### 5. Resolution and Binding Control Directives
- `?CONSULT` / `?NOCONSULT` - Specifies object files for external reference resolution (no binding)
- `?SEARCH` / `?NOSEARCH` - Specifies object files for external reference resolution and binding
- `?ELD` - Passes linker options to the eld utility
- `?XLD` - Passes linker options to the xld utility
- `?LD` - Ignored (warning issued)
- `?LIBRARY` - Ignored (warning issued)
- `?NLD` - Ignored (warning issued)

### 6. Conditional Compilation Directives
- `?IF` - Conditional compilation if toggle set
- `?IFNOT` - Conditional compilation if toggle not set
- `?IFDEF` - Conditional compilation if symbol defined
- `?IFNDEF` - Conditional compilation if symbol not defined
- `?ELSE` - Alternative conditional compilation
- `?ENDIF` - End conditional compilation block

## Implementation Details

### TextMate Grammar
- Added comprehensive regex patterns in `COBOL.tmLanguage.json`
- Question mark directives are scoped as `keyword.control.directive.nonstop.cobol`
- Conditional directives use `keyword.control.directive.conditional.nonstop.cobol`
- Parameters are scoped as `entity.other.attribute-name.preprocessor.nonstop.cobol`

### File Support
- All NonStop COBOL directives work with existing COBOL file extensions (`.cbl`, `.cobol`, etc.)
- Example file created: `examples/nonstop-cobol-example.cbl`

### Documentation
- Updated README.md to mention NonStop COBOL support
- Added comprehensive directive category documentation

## Testing
The integration has been tested with:
- TypeScript compilation (`npm run compile`)
- Webpack build process (`npm run webpack`)
- Example file demonstrating all directive categories

## Usage
NonStop COBOL developers can now use VS Code with full syntax highlighting for all question mark directives, improving code readability and development experience when working with Tandem NonStop systems.