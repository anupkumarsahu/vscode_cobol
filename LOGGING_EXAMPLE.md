# COBOL Extension Enhanced Logging Configuration with Precedence

## Overview
The VS Code COBOL extension supports configurable logging levels with precedence-based filtering and enhanced caller information including filename and line numbers. The logging system follows a precedence hierarchy where each level includes all higher priority levels.

## Configuration

Add the following to your VS Code settings (user or workspace settings):

```json
{
    "coboleditor.logging_level": ["info", "warning", "error", "fatal"]
}
```

## Logging Precedence System

**Precedence**: TRACE < DEBUG < INFO < WARNING < ERROR < FATAL

Each level includes all higher priority levels:
- **trace (5)**: Most verbose - includes all other levels
- **debug (10)**: Development debugging information + info, warning, error, fatal
- **info (20)**: General information + warning, error, fatal  
- **warning (30)**: Warning messages + error, fatal
- **error (40)**: Error messages + fatal
- **fatal (50)**: Only critical/fatal messages

## Enhanced Logging Format

All log messages now include caller information in the format `[filename:line]`:

```
TRACE: [extension.ts:134] Entering function processDocument()
DEBUG: [vscobolscanner.ts:234] Processing COBOL file: program.cbl  
INFO: [extension.ts:145] Extension activation completed successfully
WARNING: [extension.ts:267] Copybook not found: mylib.cpy
ERROR: [cobolsourcescanner.ts:456] Failed to parse COBOL syntax
FATAL: [cobolsourcescanner.ts:892] Fatal parsing error: Invalid syntax
```

## Default Configuration

By default, the extension logs `info`, `warning`, `error`, and `fatal` messages. Trace and debug messages are disabled by default to reduce noise.

## Usage in Code

For developers working on the extension:

```typescript
import { VSLogger } from "./vslogger";
import { ICOBOLSettings } from "./iconfiguration";

// Get settings from configuration
const settings: ICOBOLSettings = VSCOBOLConfiguration.get_resource_settings(document, VSExternalFeatures);

// Use the new logging methods - filename and line number are automatically captured
VSLogger.logTrace(settings, "Entering function: %s", functionName);
VSLogger.logDebug(settings, "Processing %d files", fileCount);
VSLogger.logInfo(settings, "Extension loaded successfully");
VSLogger.logWarning(settings, "Copybook not found: %s", copybookName);
VSLogger.logError(settings, "Parse error in line %d", lineNumber);
VSLogger.logFatal(settings, "Fatal error occurred: %s", errorMessage);
```

## Filtering Examples

To see only high-priority messages (error and fatal):
```json
{
    "coboleditor.logging_level": ["error"]
}
```

To see warnings and above (warning, error, fatal):
```json
{
    "coboleditor.logging_level": ["warning"]
}
```

To see all messages including trace (most verbose):
```json
{
    "coboleditor.logging_level": ["trace"]
}
```

To disable all configurable logging:
```json
{
    "coboleditor.logging_level": []
}
```

## Precedence Examples

If you configure `["info"]`, you'll see:
- ✅ INFO messages
- ✅ WARNING messages (higher priority)
- ✅ ERROR messages (higher priority)
- ✅ FATAL messages (highest priority)
- ❌ DEBUG messages (lower priority)
- ❌ TRACE messages (lowest priority)

If you configure `["warning"]`, you'll see:
- ❌ TRACE, DEBUG, INFO messages (lower priority)
- ✅ WARNING messages
- ✅ ERROR messages (higher priority)
- ✅ FATAL messages (highest priority)

## Legacy Logging

The existing `VSLogger.logMessage()` and `VSLogger.logWarningMessage()` methods continue to work and are not affected by the logging level filter. These always display regardless of the `logging_level` setting.