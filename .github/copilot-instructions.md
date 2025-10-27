# Copilot Instructions for VS Code COBOL Extension

## Overview
This is a comprehensive VS Code extension for COBOL development (`bitlang.cobol`) supporting multiple COBOL dialects including Rocket COBOL (formerly Micro Focus), ACUCOBOL, RMCOBOL, COBOL-IT, and ILECOBOL. The extension provides syntax highlighting, IntelliSense, source navigation, linting, and integration with COBOL compilers.

## Architecture & Core Components

### Multi-Target Build System
- **Main Extension**: `src/extension.ts` → `dist/extension.js` (Node.js target for desktop VS Code)
- **Web Extension**: `src/web/extension.ts` → `dist/web/extension-web.js` (WebWorker target for vscode.dev)
- **Scanner Worker**: `src/cobscanner.ts` → `dist/cobscanner.js` (standalone COBOL source scanner)
- Build via webpack with different targets for each component

### Source Format Detection & Handling
- **Fixed Format**: Traditional COBOL with margins (columns 7-11 for code)
- **Free Format**: Modern COBOL without column restrictions  
- **Variable Format**: No right margin restrictions
- **Terminal Format**: ACU COBOL/RM style
- Detection logic in `src/sourceformat.ts` with configurable patterns in `coboleditor.fileformat`

### Multi-Dialect Language Support
Each dialect has separate language IDs and configurations:
- `COBOL` (default, Rocket/Micro Focus)
- `ACUCOBOL` (ACU COBOL-GT)
- `RMCOBOL` (RM COBOL)
- `ILECOBOL` (IBM i COBOL)
- `COBOLIT` (COBOL-IT)

File associations and settings are dialect-specific via `package.json` contributions.

### Source Scanning & Symbol Management
- **Primary Scanner**: `src/cobolsourcescanner.ts` - Parses COBOL source and extracts symbols
- **VS Code Integration**: `src/vscobolscanner.ts` - Wraps scanner for VS Code features
- **Caching Layer**: `src/globalcachehelper.ts` + `src/cobolworkspacecache.ts` - Performance optimization
- **Symbol Events**: `src/cobolsymboltableeventhelper.ts` - Symbol change notifications

### Provider Architecture
Language features implemented as VS Code providers in `src/vs*provider.ts`:
- **Completion**: `vscobolprovider.ts` + `vskeywordprovider.ts` + `vssnippetprovider.ts`
- **Definition**: `vssourcedefinitionprovider.ts`
- **References**: `vsreferenceprovider.ts`
- **Hover**: `vshoverprovider.ts`
- **Symbol**: `vssymbolprovider.ts`
- **Semantic Tokens**: `vssemanticprovider.ts`
- **CodeLens**: `vsppcodelens.ts`

## Configuration System

### Settings Hierarchy
1. **Global Settings**: `ExtensionDefaults.defaultEditorConfig` ("`coboleditor`")
2. **Resource-Specific**: Per-workspace/folder via `VSCOBOLConfiguration.get_resource_settings()`
3. **Document-Based**: Language-specific overrides in `package.json` `configurationDefaults`

### Critical Settings
- `coboleditor.copybookdirs`: Copybook search paths (supports environment variables)
- `coboleditor.fileformat`: File pattern → source format mapping
- `coboleditor.valid_cobol_language_ids`: Supported dialect list
- `coboleditor.enable_source_scanner`: Master toggle for parsing features

## Extension Conflict Management

### Rocket COBOL Co-existence
- Detects official Rocket COBOL extension (`RocketSoftware.rocket-cobol`)
- Auto-switches language IDs between `COBOL` (this extension) and `cobol` (Rocket)
- Commands: `cobolplugin.change_lang_to_bitlang_cobol` / `cobolplugin.change_lang_to_mfcobol`
- LSP control via `coboleditor.enable_rocket_cobol_lsp_when_active`

### Extension Validation
`src/vscommon_commands.ts` `checkForExtensionConflicts()`:
- Scans for conflicting extensions with COBOL support
- Shows detailed conflict resolution in output channel
- Disables features when fatal conflicts detected

## Development Workflows

### Building & Testing
```bash
npm run compile        # TypeScript compilation
npm run webpack       # Production webpack build  
npm run watch         # Development file watching
npm run test          # Run test suite
```

### Debug Configuration (`.vscode/launch.json`)
- **Launch Extension**: Full extension in new VS Code window
- **Extension Tests**: Automated test execution
- **Web Extension**: Browser-based debugging for vscode.dev

### Problem Matchers
Extensive compiler integration via `package.json` `problemMatchers`:
- `$mfcobol-errformat3` / `$mfcobol-errformat2`: Rocket COBOL
- `$acucobol-ccbl`: ACU COBOL  
- `$cobolit-cobc`: COBOL-IT
- Stack multiple matchers for comprehensive error capture

## Key Patterns & Conventions

### File Organization
- `src/vs*.ts`: VS Code-specific integration layer
- `src/cobol*.ts`: Core COBOL parsing and analysis
- `src/keywords/`: Language-specific keyword definitions
- `syntaxes/`: TextMate grammars for syntax highlighting
- `schemas/`: JSON schemas for configuration validation

### Configuration Loading
Always use `VSCOBOLConfiguration.get_resource_settings(document, VSExternalFeatures)` for document-specific settings rather than global workspace config.

### Source Handler Pattern
Abstract file access via `ISourceHandler` interface:
- `VSCodeSourceHandler`: Live VS Code documents
- `FileSourceHandler`: File system access
- Enables consistent parsing across different contexts

### Caching Strategy
- **In-Memory**: Recent source scans in `InMemoryCache_SourceScanner`
- **Workspace**: Symbol tables persisted via `COBOLWorkspaceSymbolCacheHelper`
- **Global**: Cross-workspace symbols in `InMemoryGlobalSymbolCache`
- Cache invalidation on file changes via workspace watchers

### Error Handling
- Extensive validation with fallbacks for malformed COBOL
- Configurable line/time limits to prevent UI freezing
- Graceful degradation when features unavailable in untrusted workspaces

## Integration Points

### External Dependencies
- TextMate grammars for syntax highlighting
- Problem matchers for compiler integration
- File system watchers for cache invalidation
- VS Code language services for core IDE features

### Copybook Resolution
Multi-step search process:
1. Configured `copybookdirs` (supports env vars like `$COBCPY`)
2. Workspace folder traversal
3. Relative path resolution from source file
4. Extension matching via `copybookexts` setting

When adding new COBOL dialect support, ensure all provider files handle the new language ID and update `valid_cobol_language_ids` setting.