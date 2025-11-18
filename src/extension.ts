"use strict";

// Core Node.js and VS Code imports
import * as path from "path";
import * as vscode from "vscode";
import os from "os";

// VS Code API imports for extension functionality
import { commands, workspace, StatusBarItem, StatusBarAlignment, ExtensionContext, languages, TextDocument, Position, CancellationToken, ProviderResult, Definition, window, extensions, ViewColumn, ConfigurationChangeEvent } from "vscode";

// COBOL-specific functionality imports
import * as opencopybook from "./opencopybook";

// Language service providers for IntelliSense and code navigation
import { KeywordAutocompleteCompletionItemProvider } from "./vskeywordprovider";
import { CobolSymbolInformationProvider, JCLDocumentSymbolProvider, MFDirectivesSymbolProvider } from "./vssymbolprovider";

// Core COBOL source scanning and configuration
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";

// Language feature providers
import { CobolReferenceProvider } from "./vsreferenceprovider";
import { CobolLinterProvider, CobolLinterActionFixer } from "./cobollinter";
import { VSSourceTreeViewHandler } from "./vssourceviewtree";
import { CobolSourceCompletionItemProvider } from "./vscobolprovider";

// Utility and helper classes
import { VSCOBOLUtils } from "./vscobolutils";
import { ICOBOLSettings } from "./iconfiguration";

// Third-party library for reading properties files (MFU configuration)
// eslint-disable-next-line @typescript-eslint/no-var-requires
const propertiesReader = require("properties-reader");

// Additional COBOL extension functionality
import { VSWorkspaceFolders } from "./vscobolfolders";
import { COBOLSourceDefinition } from "./vssourcedefinitionprovider";
import { VSExternalFeatures } from "./vsexternalfeatures";
import { VSCobScanner } from "./vscobscanner";
import { BldScriptTaskProvider } from "./bldTaskProvider";
// import { COBOLCaseFormatter } from "./caseformatter"; // Currently disabled
import { COBOLCallTargetProvider } from "./vscobolcalltargetprovider";
import { COBOLWorkspaceSymbolCacheHelper } from "./cobolworkspacecache";
import { SourceOrFolderTreeItem } from "./sourceItem";

// Advanced language features
import { VSSemanticProvider } from "./vssemanticprovider";
import { VSPPCodeLens } from "./vsppcodelens";
import { InMemoryGlobalSymbolCache } from "./globalcachehelper";

// File and utility management
import { VSCOBOLFileUtils } from "./vsfileutils";
import { COBOLOutputChannel, VSLogger } from "./vslogger";
import { VSExtensionUtils, TextLanguage } from "./vsextutis";

// Editor decorations and UI enhancements
import { vsMarginHandler } from "./vsmargindecorations";
import { commentUtils } from "./commenter";
import { colourCommentHandler } from "./vscolourcomments";
import { SnippetCompletionItemProvider } from "./vssnippetprovider";

// Extension configuration and constants
import { ExtensionDefaults } from "./extensionDefaults";

// Additional language service providers
import { VSCobolRenameProvider } from "./vsrenameprovider";
import { activateCommonCommands, checkForExtensionConflicts, install_call_hierarchy, isMicroFocusCOBOL_LSPActive, toggleMicroFocusLSP } from "./vscommon_commands";
import { VSHelpAndFeedViewHandler } from "./feedbacktree";
import { VSCustomIntelliseRules } from "./vscustomrules";
import { VSHoverProvider } from "./vshoverprovider";
import { COBOLTypeFormatter } from "./vsformatter";
import { TabUtils } from "./tabstopper";
import { VSTerminal } from "./vsterminals";

// Commented out: Future markdown/syntax highlighting integration
// import type MarkdownIt from 'markdown-it';
// import hijs from 'highlight.js/lib/core';

// try {
//     // hijs.registerLanguage('COBOL', hljsCOBOL);
//     hijs.registerLanguage('shell', require('highlight.js/lib/languages/shell'));
//     hijs.registerLanguage('COBOL', require('highlightjs-cobol'));
// } catch {
//     //
// }

// Global extension state and UI elements
export const progressStatusBarItem: StatusBarItem = window.createStatusBarItem(StatusBarAlignment.Left);

// Extension lifecycle and state management variables
let bldscriptTaskProvider: vscode.Disposable | undefined;  // Build script task provider for workspace tasks
let shown_enable_semantic_token_provider = false;           // Flag to show semantic token notification only once
let activeEditor: vscode.TextEditor;                        // Currently active text editor
let unitTestTerminal: vscode.Terminal | undefined = undefined; // Terminal for running MFU unit tests
const terminalName = "UnitTest";                            // Name for the unit test terminal
let updateDecorationsOnTextEditorEnabled = false;           // Flag to enable/disable decoration updates
let sharedContext: ExtensionContext;                        // Shared extension context for command handlers

/**
 * Opens the appropriate changelog file based on the current extension version.
 * 
 * This function checks if a version-specific changelog exists and displays it
 * to the user when the extension is updated. It handles both full version changes
 * and minor version updates (e.g., 1.2.3 -> 1.2.4 shows 1.2 changelog).
 * 
 * @param currentContext - VS Code extension context for state management
 */
function openChangeLog(currentContext: ExtensionContext): void {
    const thisExtension = extensions.getExtension(ExtensionDefaults.thisExtensionName);
    if (thisExtension !== undefined) {
        const extPath = `${thisExtension.extensionPath}`;
        const version = `${thisExtension.packageJSON.version}`;
        
        // Check if this version's changelog has already been shown
        const glastVersion = currentContext.globalState.get(`${ExtensionDefaults.thisExtensionName}.version`);
        if (glastVersion !== version) {
            // Look for version-specific changelog file
            const verFile = path.join(extPath, `CHANGELOG_${version}.md`);
            if (VSExternalFeatures.isFile(verFile)) {
                const readmeUri = vscode.Uri.file(verFile);
                commands.executeCommand("markdown.showPreview", readmeUri, ViewColumn.One, { locked: true });
                currentContext.globalState.update(`${ExtensionDefaults.thisExtensionName}.version`, version);
            }
        }
        
        // Handle minor version changelog (e.g., show 1.2 changelog for 1.2.x versions)
        const lastDot = version.lastIndexOf(".");
        if (lastDot !== -1) {
            const lastSVersion = version.substring(0, lastDot);
            const glastsVersion = currentContext.globalState.get(`${ExtensionDefaults.thisExtensionName}.sversion`);

            if (glastsVersion !== lastSVersion) {
                const verFile = path.join(extPath, `CHANGELOG_${lastSVersion}.md`);
                if (VSExternalFeatures.isFile(verFile)) {
                    const readmeUri = vscode.Uri.file(verFile);
                    commands.executeCommand("markdown.showPreview", readmeUri, ViewColumn.One, { locked: true });
                    currentContext.globalState.update(`${ExtensionDefaults.thisExtensionName}.sversion`, lastSVersion);
                }
            }
        }
    }
}

/**
 * Sets up the COBOL output channel with environment and extension information.
 * 
 * This function initializes the logging channel and displays comprehensive
 * diagnostic information about the VS Code environment, extension versions,
 * and configuration settings. It also demonstrates the new logging system
 * with different log levels.
 * 
 * @param hide - Whether to hide the output channel initially
 * @param settings - COBOL extension settings
 * @param quiet - Whether to suppress channel visibility operations
 */
async function setupLogChannel(hide: boolean, settings: ICOBOLSettings, quiet: boolean) {
    // Configure output channel visibility
    if (!quiet) {
        if (hide) {
            COBOLOutputChannel.hide();
        } else {
            VSLogger.logChannelSetPreserveFocus(true);
        }
    }
    COBOLOutputChannel.clear();

    // Get extension information for diagnostics
    const thisExtension = extensions.getExtension(ExtensionDefaults.thisExtensionName);

    if (thisExtension !== undefined) {
        const githubCopilotExtension = extensions.getExtension("GitHub.copilot");
        const mfExt = extensions.getExtension(ExtensionDefaults.rocketCOBOLExtension);

        // Display environment information
        if (vscode.env.uriScheme !== "vscode") {
            VSLogger.logMessage("----------------------------------------------------------------------");
            VSLogger.logMessage(`Warning: you are using a untested environment : ${vscode.env.uriScheme}`);
            VSLogger.logMessage("----------------------------------------------------------------------");
            VSLogger.logMessage(`Version                                     : ${vscode.version}`);
        } else {
            VSLogger.logMessage(`VSCode version                              : ${vscode.version}`);
        }
        
        // Log system information
        VSLogger.logMessage(` Platform                                   : ${os.platform}`);
        VSLogger.logMessage(` Architecture                               : ${os.arch}`);
        VSLogger.logMessage(`  URI Scheme                                : ${vscode.env.uriScheme}`)
        VSLogger.logMessage(`  App Name                                  : ${vscode.env.appName}`);
        VSLogger.logMessage(`  App Host                                  : ${vscode.env.appHost}`);
        VSLogger.logMessage(`  App Root                                  : ${vscode.env.appRoot}`);
        
        // Log extension information
        VSLogger.logMessage("Extension Information:");
        VSLogger.logMessage(` Extension path                             : ${thisExtension.extensionPath}`);
        VSLogger.logMessage(` Version                                    : ${thisExtension.packageJSON.version}`);

        // Only log detailed configuration in trusted workspaces for security
        if (workspace.isTrusted) {
            VSLogger.logMessage(` UNC paths disabled                         : ${settings.disable_unc_copybooks_directories}`);
            VSLogger.logMessage(` Parse copybook for references              : ${settings.parse_copybooks_for_references}`);
            VSLogger.logMessage(` Editor maxTokenizationLineLength           : ${settings.editor_maxTokenizationLineLength}`);
            VSLogger.logMessage(` Semantic token provider enabled            : ${settings.enable_semantic_token_provider}`);
            VSLogger.logMessage(` Logging levels enabled                     : ${settings.logging_level.join(", ")}`);
            
            // Demonstrate the new precedence-based logging system
            VSLogger.logInfo(settings, "Extension activation completed successfully");
            VSLogger.logDebug(settings, "Settings loaded: %d copybook directories configured", settings.copybookdirs.length);
            VSLogger.logWarning(settings, "This is a warning message example");
            VSLogger.logFatal(settings, "This is a fatal message example");
            // Log VS Code editor settings that affect COBOL editing
            try {
                const editor_semanticHighlighting_enabled = workspace.getConfiguration("editor.semanticHighlighting").get<number>("enabled");
                VSLogger.logMessage(` editor.semanticHighlighting.enabled        : ${editor_semanticHighlighting_enabled}`);
            } catch {
                // Ignore configuration read errors
            }

            try {
                const editor_semanticHighlighting_enabled = workspace.getConfiguration("editor.semanticHighlighting",
                    { languageId: ExtensionDefaults.defaultCOBOLLanguage }).get<number>("enabled");
                VSLogger.logMessage(` [COBOL]editor.semanticHighlighting.enabled : ${editor_semanticHighlighting_enabled}`);
            } catch {
                // Ignore configuration read errors
            }

            try {
                const workbench_theme = workspace.getConfiguration("workbench").get<string>("colorTheme");
                VSLogger.logMessage(` workbench color theme                      : ${workbench_theme}`);
            } catch {
                // Ignore configuration read errors
            }

            // Log active workspace file if available
            if (vscode.workspace.workspaceFile !== undefined) {
                VSLogger.logMessage(` Active workspacefile                       : ${vscode.workspace.workspaceFile}`);
            }
        }

        VSLogger.logMessage(` Is Workspace Trusted                       : ${workspace.isTrusted}`);

        // Log information about related extensions
        if (mfExt !== undefined || githubCopilotExtension !== undefined) {
            VSLogger.logMessage("Other Extension Information:");
            if (githubCopilotExtension !== undefined) {
                VSLogger.logMessage(` GitHub Copilot                             : ${githubCopilotExtension.packageJSON.version}`);
            }
            if (mfExt !== undefined) {
                VSLogger.logMessage(` Rocket COBOL                               : ${mfExt.packageJSON.version}`);
                if (window.activeTextEditor) {
                    VSLogger.logMessage(`  microFocusCOBOL.languageServerAutostart   = ${isMicroFocusCOBOL_LSPActive(window.activeTextEditor.document)}`);
                }
            }
        }
    }
}


/**
 * Activates desktop-specific functionality for the COBOL extension.
 * 
 * This function registers commands that are only available in the desktop
 * version of VS Code (not in web/browser environments). It includes commands
 * for cache management, code extraction, and file operations.
 * 
 * @param context - VS Code extension context for registering disposables
 * @param settings - COBOL extension settings
 */
function activateDesktop(context: ExtensionContext, settings: ICOBOLSettings): void {

    // Register command to clear the global metadata cache
    context.subscriptions.push(commands.registerCommand("cobolplugin.clearGlobalCache", function () {
        VSLogger.logDebug(settings, "Clear global cache command invoked");
        window.showQuickPick(["Yes", "No"], { placeHolder: "Are you sure you want to clear the metadata?" }).then(function (data) {
            VSLogger.logDebug(settings, "User selected option for cache clearing: %s", data);
            if (data === "Yes") {
                VSCOBOLUtils.clearGlobalCache(settings);
                VSLogger.logInfo(settings, "Metadata cache cleared");
            } else {
                VSLogger.logDebug(settings, "Cache clearing cancelled by user");
            }
        });
    }));

    // Register command to extract selected COBOL code to a copybook
    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.extractSelectionToCopybook", async ()  => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                await VSCOBOLFileUtils.extractSelectionToCopybook(vscode.window.activeTextEditor, VSExternalFeatures);
            }
        }
    }));

    // Register command to extract selected code to a new paragraph
    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.extractSelectionToParagraph", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                VSCOBOLUtils.extractSelectionTo(vscode.window.activeTextEditor, true);
            }
        }
    }));

    // Register command to extract selected code to a new section
    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.extractSelectionToSection", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                VSCOBOLUtils.extractSelectionTo(vscode.window.activeTextEditor, false);
            }
        }
    }));

    // Register context menu command for running files from explorer
    // Handles both COBOL programs (.cbl, etc.) and MFU unit test files (.mfu)
    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.explorerRun", function (fileUri) {

        const fsPath = fileUri.fsPath
        VSLogger.logDebug(settings, "Explorer run command invoked for file: %s", fsPath);
        
        // Special handling for MFU (Micro Focus Unit test) files
        if (fsPath.endsWith(".mfu")) {
            VSLogger.logDebug(settings, "Processing MFU file: %s", fsPath);
            
            // Create or reuse unit test terminal
            if (unitTestTerminal === undefined) {
                unitTestTerminal = vscode.window.createTerminal(terminalName);
                VSLogger.logDebug(settings, "Created new unit test terminal: %s", terminalName);
            }

            unitTestTerminal.show(true);
            const enableAnsiColor = VSCOBOLUtils.getMFUnitAnsiColorConfig();
            VSLogger.logDebug(settings, "ANSI color enabled for MFU: %s", enableAnsiColor);

            // Read MFU configuration properties
            const properties = propertiesReader(fileUri.fsPath);
            const prefRunner = properties.get("global.preferred-runner");
            VSLogger.logDebug(settings, "Preferred runner from properties: %s", prefRunner);
            
            // Build and execute MFU command
            const command = prefRunner + " -show-progress " +
                (enableAnsiColor ? " -dc:ansi " : " ") +
                fileUri.fsPath;
            VSLogger.logDebug(settings, "Executing MFU command: %s", command);
            
            unitTestTerminal.sendText(command);

            return;
        }

        // Handle regular COBOL program execution
        VSLogger.logDebug(settings, "Running or debugging COBOL file: %s", fsPath);
        VSCOBOLUtils.runOrDebug(fsPath, false);
    }));

    // Register context menu command for debugging files from explorer
    // Similar to explorerRun but launches in debug mode
    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.explorerDebug", function (fileUri) {

        const fsPath = fileUri.fsPath
        VSLogger.logDebug(settings, "Explorer debug command invoked for file: %s", fsPath);
        
        // Special handling for MFU files in debug mode
        if (fsPath.endsWith(".mfu")) {
            VSLogger.logDebug(settings, "Processing MFU file for debugging: %s", fsPath);
            
            // Create or reuse unit test terminal
            if (unitTestTerminal === undefined) {
                unitTestTerminal = vscode.window.createTerminal(terminalName);
                VSLogger.logDebug(settings, "Created new unit test terminal for debug: %s", terminalName);
            }

            unitTestTerminal.show(true);
            const enableAnsiColor = VSCOBOLUtils.getMFUnitAnsiColorConfig();
            VSLogger.logDebug(settings, "ANSI color enabled for MFU debug: %s", enableAnsiColor);

            // Read MFU configuration properties
            const properties = propertiesReader(fileUri.fsPath);
            const prefRunner = properties.get("global.preferred-runner");
            VSLogger.logDebug(settings, "Preferred runner from properties (debug): %s", prefRunner);
            
            // Build and execute MFU debug command
            const command = prefRunner + " -show-progress " +
                (enableAnsiColor ? " -dc:ansi " : " ") +
                fileUri.fsPath;
            VSLogger.logDebug(settings, "Executing MFU debug command: %s", command);
            
            unitTestTerminal.sendText(command);

            return;
        }

        // Handle regular COBOL program debugging
        VSLogger.logDebug(settings, "Running or debugging COBOL file in debug mode: %s", fsPath);
        VSCOBOLUtils.runOrDebug(fsPath, true);
    }));

    // Register command for processing all workspace files (startup version)
    context.subscriptions.push(commands.registerCommand("cobolplugin.processAllFilesInWorkspaceOnStartup", async () => {
        await VSCobScanner.processAllFilesInWorkspaceOutOfProcess(VSExternalFeatures, settings, false, false, -1);
    }));

    // Register commands for source tree view actions
    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.runCommand", function (si: SourceOrFolderTreeItem) {
        if (si !== undefined) {
            VSSourceTreeViewHandler.actionSourceViewItemFunction(si, false);
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.runDebugCommand", function (si: SourceOrFolderTreeItem) {
        if (si !== undefined) {
            VSSourceTreeViewHandler.actionSourceViewItemFunction(si, true);
        }
    }));

}

/**
 * Handles configuration changes with proper scoping.
 * 
 * This function responds to configuration changes and updates the extension
 * accordingly. It handles both global and scoped configuration changes,
 * refreshing caches, updating providers, and notifying users as needed.
 * 
 * @param event - VS Code configuration change event
 * @param scope - Optional configuration scope (workspace, folder, etc.)
 */
async function handleScopedChange(event:ConfigurationChangeEvent, scope?: vscode.ConfigurationScope) {
    // Check which configuration settings have changed
    const updated = event.affectsConfiguration(ExtensionDefaults.defaultEditorConfig, scope);
    const outline_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig, scope}.outline`, scope);
    const md_syms = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.metadata_symbols`, scope);
    const md_eps = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.metadata_entrypoints`, scope);
    const md_types = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.metadata_types`, scope);
    const md_metadata_files = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.metadata_files`, scope);
    const md_metadata_knowncopybooks = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.metadata_knowncopybooks`, scope);
    const md_copybookdirs = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.copybookdirs`, scope);
    const enable_semantic_token_provider = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.enable_semantic_token_provider`, scope);
    const maintain_metadata_recursive_search = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.maintain_metadata_recursive_search`, scope);
    const enable_comments_tags_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.enable_comments_tags`, scope);
    const comments_tags_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.comments_tags`, scope);
    const intellisense_style_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.intellisense_style`, scope);
    const enable_columns_tags_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.enable_columns_tags`, scope);
    const columns_tags_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.columns_tags`, scope);
    const margin_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.margin`, scope);
    const margin_identification_area_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.margin_identification_area`, scope);
    const intellisense_add_space_keywords_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.intellisense_add_space_keywords`, scope);
    const custom_intellisense_rules_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.custom_intellisense_rules`, scope);
    const tabstops_anchors_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.tabstops_anchors`, scope);
    const enable_program_information_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.enable_program_information`, scope);

    // Handle general configuration updates
    if (updated) {
        VSCOBOLConfiguration.reinitWorkspaceSettingsScoped(VSExternalFeatures);
        const settings = VSCOBOLConfiguration.get_workspace_settings();
        
        // Only clear cache and reinitialize if metadata settings haven't changed
        if (!md_syms && !md_eps && !md_types && !md_metadata_files && !md_metadata_knowncopybooks && !enable_semantic_token_provider) {
            VSCOBOLSourceScanner.clearCOBOLCache();
            setupLogChannel(true, settings, true);
            VSCOBOLUtils.setupFilePaths(settings);
            async () => {
                await VSCOBOLUtils.setupUrlPaths(settings);
                await VSSourceTreeViewHandler.setupSourceViewTree(settings, true);
            }
        }

        // Handle specific metadata setting changes
        if (md_syms) {
            COBOLWorkspaceSymbolCacheHelper.loadGlobalCacheFromArray(settings, settings.metadata_symbols, true);
        }

        if (md_eps) {
            COBOLWorkspaceSymbolCacheHelper.loadGlobalEntryCacheFromArray(settings, settings.metadata_entrypoints, true);
        }

        if (md_types) {
            COBOLWorkspaceSymbolCacheHelper.loadGlobalTypesCacheFromArray(settings, settings.metadata_types, true);
        }

        if (md_metadata_files) {
            COBOLWorkspaceSymbolCacheHelper.loadFileCacheFromArray(settings, VSExternalFeatures, settings.metadata_files, true);
        }

        // Show notification for semantic token provider changes (only once)
        if (enable_semantic_token_provider && !shown_enable_semantic_token_provider) {
            shown_enable_semantic_token_provider = true;
            vscode.window.showInformationMessage(`The configuration setting '${ExtensionDefaults.defaultEditorConfig}.enable_semantic_token_provider' has changed but you may not see the affects until you have either close/reload your documents or restarted this session`);
        }

        if (md_metadata_knowncopybooks) {
            COBOLWorkspaceSymbolCacheHelper.loadGlobalKnownCopybooksFromArray(settings, settings.metadata_knowncopybooks, true);
        }

        // Handle copybook directory changes (requires full refresh)
        if (md_copybookdirs) {
            VSCOBOLSourceScanner.clearCOBOLCache();
            setupLogChannel(true, settings, true);
            VSCOBOLUtils.setupFilePaths(settings);
            await VSCOBOLUtils.setupUrlPaths(settings);

            VSCOBOLUtils.populateDefaultCallableSymbolsSync(settings, true);
            VSCOBOLUtils.populateDefaultCopyBooksSync(settings, true);
        }

        if (maintain_metadata_recursive_search) {
            VSCOBOLUtils.populateDefaultCallableSymbolsSync(settings, true);
            VSCOBOLUtils.populateDefaultCopyBooksSync(settings, true);
        }

        // Show notification for outline changes
        if (outline_changed) {
            vscode.window.showInformationMessage(`The configuration setting '${ExtensionDefaults.defaultEditorConfig}.outline' has changed but you may not see the affects until you have either reloaded your window or restarted this session`);
        }

        // Handle IntelliSense and UI-related setting changes
        if (custom_intellisense_rules_changed) {
            VSCustomIntelliseRules.Default.reFreshConfiguration(settings);
        }

        if (enable_comments_tags_changed || comments_tags_changed) {
            colourCommentHandler.setupTags();
        }

        if (enable_columns_tags_changed || columns_tags_changed || margin_changed || margin_identification_area_changed) {
            vsMarginHandler.setupTags();
            // Refresh decorations for all visible editors
            for (const editor of vscode.window.visibleTextEditors) {
                if (VSExtensionUtils.isSupportedLanguage(editor.document) !== TextLanguage.Unknown) {
                    await vsMarginHandler.updateDecorations(editor);
                }
            }
        }

        // Update IntelliSense completion providers
        if (intellisense_style_changed) {
            SnippetCompletionItemProvider.Default.reInitCallMap(settings);
        }

        if (intellisense_add_space_keywords_changed) {
            KeywordAutocompleteCompletionItemProvider.Default4COBOL.reFreshConfiguration(settings);
        }

        if (tabstops_anchors_changed) {
            TabUtils.clearTabstopCache();
        }

        // Handle call hierarchy changes
        if (enable_program_information_changed && settings.enable_program_information) {
            install_call_hierarchy(settings, sharedContext)
        }
        
        // Always check for extension conflicts after configuration changes
        checkForExtensionConflicts(settings, sharedContext);
    }
}
/**
 * Activates the COBOL extension and initializes the full desktop feature set.
 *
 * This async entry point wires up:
 * - Configuration bootstrap and reinitialization on workspace / settings changes.
 * - Logging channel creation (initial and subsequent extension change refreshes).
 * - Metadata cache priming (symbols, entry points, types, files, known copybooks).
 * - Registration of all language feature providers (definitions, references, rename, hover,
 *   completion, document symbols, code lenses, semantic tokens, formatting, code actions, etc.).
 * - Command registrations for comment handling, metadata scanning, copybook utilities,
 *   ignore directives, and alignment features.
 * - Background scanners / linter initialization and dynamic decoration refresh logic tied to
 *   editor/selection/document change events (debounced for performance).
 * - Status bar items, task providers, terminal profile (Linux only), and context keys used by
 *   when-clauses in package contributions.
 * - Default symbol seeding for workspaces lacking metadata, and optional auto metadata
 *   processing on startup.
 * - Conditional Micro Focus LSP integration toggling based on visible documents.
 *
 * The activation process is intentionally ordered to:
 * 1. Establish settings and logging.
 * 2. Detect and abort on extension conflicts.
 * 3. Prepare caches before language feature providers rely on them.
 * 4. Register core commands early (so subsequent async operations can invoke them).
 * 5. Defer expensive decoration + linter work via a throttled trigger mechanism.
 *
 * Event Subscriptions Established:
 * - Extension changes (to refresh logging).
 * - Configuration changes (to handle scoped setting updates).
 * - Workspace folder changes (to recompute paths & settings).
 * - Document open/close (to manage caches and initiate symbol seeding).
 * - Visible text editor changes, active editor changes, selections, and document edits
 *   (to keep decorations and diagnostics current).
 *
 * Performance Considerations:
 * - Debounced decoration updates reduce redundant passes during rapid typing.
 * - Workspace metadata scan prompts for thread scaling when large symbol sets are detected.
 * - Conditional feature registration (eg: outline providers) honors user settings flags.
 *
 * Error Handling:
 * - Throws an Error if a blocking extension conflict is detected prior to full activation.
 * - Metadata processing on startup is wrapped defensively to avoid failing activation.
 *
 * Side Effects:
 * - Mutates global/shared context references.
 * - Populates and updates multiple in-memory caches.
 * - Sets VS Code context keys affecting command/menu availability.
 * - Potentially launches out-of-process scanners for metadata.
 *
 * @param context VS Code extension context used to register disposables and manage lifecycle.
 * @throws {Error} When an extension conflict prevents safe activation.
 * @returns A promise that resolves once all asynchronous activation tasks complete.
 *
 * @remarks
 * Call this exactly once from the extension's exported activate function. All disposables
 * are pushed onto the provided context to ensure proper teardown in deactivate().
 *
 * @see deactivate (companion lifecycle method)
 * @see VSCOBOLConfiguration for workspace/resource configuration logic.
 * @see VSCobScanner for asynchronous metadata scanning routines.
 * @see VSSemanticProvider for semantic token provisioning.
 */
export async function activate(context: ExtensionContext) {
    // Store the extension context globally for use in command handlers
    sharedContext = context;
    
    // Initialize workspace settings and configuration
    const settings: ICOBOLSettings = VSCOBOLConfiguration.reinitWorkspaceSettings(VSExternalFeatures);

    // Set up logging channel and display environment information
    await setupLogChannel(true, settings, true);
    
    // Initialize file system paths and URL configurations
    VSCOBOLUtils.setupFilePaths(settings);
    await VSCOBOLUtils.setupUrlPaths(settings);

    // Check for conflicting extensions and abort activation if conflicts found
    if (checkForExtensionConflicts(settings, context)) {
        throw new Error("Unable to activate extension due to conflicts");
    }

    VSLogger.logDebug(settings, "Activating desktop environment");
    
    // Register desktop-specific commands and functionality
    activateDesktop(context, settings);
    
    // Register common commands shared between desktop and web versions
    activateCommonCommands(context);

    // Monitor extension changes and refresh logging when extensions are installed/removed
    context.subscriptions.push(vscode.extensions.onDidChange(() => {
        setupLogChannel(true, settings, false);
        VSLogger.logMessage("extensions changed");
    }));

    // Set up configuration change monitoring with scoped handling
    const onDidChangeConfiguration = workspace.onDidChangeConfiguration(async (event: ConfigurationChangeEvent) => {
        await handleScopedChange(event,undefined);
    });
    context.subscriptions.push(onDidChangeConfiguration);

    // Initialize linting and diagnostic services
    const collection = languages.createDiagnosticCollection("cobolDiag");
    const linter = new CobolLinterProvider(collection);
    const cobolfixer = new CobolLinterActionFixer();

    // Load cached metadata from workspace settings (symbols, entry points, types, etc.)
    // These caches improve performance by avoiding repeated file parsing
    COBOLWorkspaceSymbolCacheHelper.loadGlobalCacheFromArray(settings, settings.metadata_symbols, false);
    COBOLWorkspaceSymbolCacheHelper.loadGlobalEntryCacheFromArray(settings, settings.metadata_entrypoints, false);
    COBOLWorkspaceSymbolCacheHelper.loadGlobalTypesCacheFromArray(settings, settings.metadata_types, false);
    COBOLWorkspaceSymbolCacheHelper.loadFileCacheFromArray(settings, VSExternalFeatures, settings.metadata_files, false);
    COBOLWorkspaceSymbolCacheHelper.loadGlobalKnownCopybooksFromArray(settings, settings.metadata_knowncopybooks, false);

    // Register code action commands used by the linter for quick fixes
    // These commands are invoked from code action suggestions in the editor
    context.subscriptions.push(commands.registerCommand("cobolplugin.insertIgnoreCommentLine", function (docUri: vscode.Uri, offset: number, code: string) {
        cobolfixer.insertIgnoreCommentLine(docUri, offset, code);
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.portCodeCommandLine", function (docUri: vscode.Uri, lineNumber: number, code: string) {
        cobolfixer.portCodeCommandLine(docUri, lineNumber, code);
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.findCopyBookDirectory", function (docUri: vscode.Uri, linenum: number, code: string) {
        cobolfixer.findCopyBookDirectory(settings, docUri, linenum, code);
    }));

    // Register comment line command with COBOL-specific behavior
    // Handles both custom line commenting and falls back to VS Code default
    context.subscriptions.push(commands.registerCommand("cobolplugin.commentline", function () {
        if (window.activeTextEditor !== undefined) {
            const langid = window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                if (settings.line_comment) {
                    // Use COBOL-specific comment formatting
                    commentUtils.processCommentLine(settings);
                } else {
                    // Fall back to VS Code's default comment behavior
                    commands.executeCommand("editor.action.commentLine");
                }
            }
            return;
        }

        // No active editor, use default VS Code commenting
        commands.executeCommand("editor.action.commentLine");
    }));

    // Register workspace metadata processing command with threading support
    // This command scans all COBOL files in the workspace to build symbol cache
    context.subscriptions.push(commands.registerCommand("cobolplugin.processAllFilesInWorkspace", async () => {
        // Get appropriate settings (document-specific or workspace-wide)
        let settings: ICOBOLSettings;
        if (window.activeTextEditor !== undefined) {
            settings = VSCOBOLConfiguration.get_resource_settings(window.activeTextEditor.document, VSExternalFeatures);
        } else {
            settings = VSCOBOLConfiguration.get_workspace_settings();
        }
        
        // For small workspaces, process without asking about threading
        if (InMemoryGlobalSymbolCache.defaultCallableSymbols.size < 500) {
            VSCobScanner.processAllFilesInWorkspaceOutOfProcess(VSExternalFeatures, settings, true, false, -1);
            return;
        }

        // For large workspaces, offer multi-threading option for better performance
        window.showQuickPick(["Yes", "No"], { placeHolder: "Your workspace is large, do you want to extra threads for your metadata scan?" }).then(function (data) {
            if (data === "Yes") {
                // Calculate optimal thread count based on CPU cores
                const cpuCount = os.cpus().length;
                const defCpuCount = cpuCount >= 4 ? Math.trunc(cpuCount / 2) : cpuCount;
                
                // Prompt user for custom thread count with validation
                vscode.window.showInputBox({
                    prompt: "How many threads do you want to use?",
                    value: "" + defCpuCount,
                    validateInput: (threadString: string): string | undefined => {
                        const threadCount: number = Number.parseInt(threadString, 10);

                        if (threadCount < 2 || threadCount > (defCpuCount * 3)) {
                            return `Thread count must be between 2 and ${defCpuCount * 3}`;
                        } else {
                            return undefined;
                        }
                    }
                }).then(value => {
                    // Exit early if user cancels input
                    if (value === undefined) {
                        return;
                    }

                    const threadCount: number = Number.parseInt(value, 10);
                    VSCobScanner.processAllFilesInWorkspaceOutOfProcess(VSExternalFeatures,settings, true, true, threadCount);
                });

            } else {
                // User declined threading, process with single thread
                VSCobScanner.processAllFilesInWorkspaceOutOfProcess(VSExternalFeatures,settings, true, false, -1);
            }
        });
    }));


    // Monitor workspace folder changes and reinitialize settings
    context.subscriptions.push(workspace.onDidChangeWorkspaceFolders(async () => {
        const settings: ICOBOLSettings = VSCOBOLConfiguration.reinitWorkspaceSettings(VSExternalFeatures);

        await setupLogChannel(false, settings, true);
        VSCOBOLUtils.setupFilePaths(settings);
        await VSCOBOLUtils.setupUrlPaths(settings);
    }));

    // Enable Micro Focus migration tasks context (controlled by package.json when clauses)
    vscode.commands.executeCommand("setContext", `${ExtensionDefaults.defaultEditorConfig}.enable_migrate2mf_tasks`, true);

    // Monitor document open events for language detection and metadata seeding
    context.subscriptions.push(workspace.onDidOpenTextDocument(async (doc: vscode.TextDocument) => {
        // Convert plain text files to COBOL language if they match COBOL patterns
        VSExtensionUtils.flip_plaintext(doc);

        // Update decorations for supported COBOL documents
        if (VSExtensionUtils.isSupportedLanguage(doc)) {
            if (window.activeTextEditor) {
                activeEditor = window.activeTextEditor;
                await triggerUpdateDecorations();
            }
        }

        // Seed metadata cache if workspace lacks symbol information
        // This helps provide IntelliSense for workspaces without pre-built metadata
        const ws = VSWorkspaceFolders.get(settings);
        if (ws !== undefined) {
            await vscode.commands.executeCommand<vscode.SymbolInformation[]>("vscode.executeDocumentSymbolProvider", doc.uri);
            await VSCOBOLUtils.populateDefaultCallableSymbols(settings, false);
        }
    }));

    // Monitor visible text editor changes for decoration updates
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    context.subscriptions.push(vscode.window.onDidChangeVisibleTextEditors(async (viewEditors) => {
        if (updateDecorationsOnTextEditorEnabled) {
            for (const textEditor of viewEditors) {
                await updateDecorationsOnTextEditor(textEditor);
            }
        }
    }));

    // Commented out: Alternative approach for visible range changes
    // Can be useful for performance optimization in large files
    // const onDidChangeTextEditorVisibleRanges = vscode.window.onDidChangeTextEditorVisibleRanges(async (tevr) => {
    //     activeEditor = tevr.textEditor;
    //     await triggerUpdateDecorations();
    // });
    // context.subscriptions.push(onDidChangeTextEditorVisibleRanges);

    // Monitor document close events for cache cleanup
    context.subscriptions.push(workspace.onDidCloseTextDocument(async (doc: vscode.TextDocument) => {
        if (VSExtensionUtils.isSupportedLanguage(doc)) {
            // Remove cached parsing results to free memory
            const config = VSCOBOLConfiguration.get_resource_settings(doc, VSExternalFeatures);
            VSCOBOLSourceScanner.removeCachedObject(doc, config);
            VSCOBOLConfiguration.clearResourceCache(doc);
        }
    }));

    // Process already opened documents for language detection
    // This handles documents that were open before the extension activated
    for (let docid = 0; docid < workspace.textDocuments.length; docid++) {
        VSExtensionUtils.flip_plaintext(workspace.textDocuments[docid]);
    }

    // Initialize UI components and tree views
    await VSSourceTreeViewHandler.setupSourceViewTree(settings, false);
    VSHelpAndFeedViewHandler.setupSourceViewTree(settings, false);
    
    // Register COBOL-specific formatting provider
    context.subscriptions.push(COBOLTypeFormatter.register(settings));

    // Register multiple definition providers for comprehensive "Go to Definition" support
    // Each provider handles different aspects of COBOL navigation
    
    // Copybook definition provider - handles COPY statements
    context.subscriptions.push(languages.registerDefinitionProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), {
        provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {
            const ccbp = new opencopybook.COBOLCopyBookProvider(VSExternalFeatures);
            return ccbp.provideDefinition(doc, pos, ct);
        }
    }));

    // Source definition provider - handles program-id, section, paragraph navigation
    context.subscriptions.push(languages.registerDefinitionProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), {
        provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {
            const csd = new COBOLSourceDefinition();
            return csd.provideDefinition(doc, pos, ct);
        }
    }));

    // Call target definition provider - handles CALL statement navigation
    context.subscriptions.push(languages.registerDefinitionProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), {
        provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {
            const csdp = new COBOLCallTargetProvider(VSExternalFeatures);
            return csdp.provideDefinition(doc, pos, ct);
        }
    }));

    // Register language service providers for IntelliSense and navigation
    context.subscriptions.push(languages.registerReferenceProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), new CobolReferenceProvider()));
    context.subscriptions.push(languages.registerCodeActionsProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), cobolfixer));

    // Register completion item providers for IntelliSense
    // JCL (Job Control Language) keyword completion
    context.subscriptions.push(languages.registerCompletionItemProvider(VSExtensionUtils.getAllJCLSelectors(settings), new KeywordAutocompleteCompletionItemProvider(false, settings)));
    
    // COBOL keyword completion
    context.subscriptions.push(languages.registerCompletionItemProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), new KeywordAutocompleteCompletionItemProvider(true, settings)));

    // COBOL snippet completion for common code patterns
    context.subscriptions.push(languages.registerCompletionItemProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), SnippetCompletionItemProvider.Default.reInitCallMap(settings)));

    // Register document symbol providers for outline view (conditional on user settings)
    if (settings.outline) {
        // JCL document outline provider
        const jclDocumentSymbolProvider = new JCLDocumentSymbolProvider();
        context.subscriptions.push(languages.registerDocumentSymbolProvider(VSExtensionUtils.getAllJCLSelectors(settings), jclDocumentSymbolProvider));

        // COBOL document outline provider (shows programs, sections, paragraphs, etc.)
        const symbolInformationProvider = new CobolSymbolInformationProvider();
        context.subscriptions.push(languages.registerDocumentSymbolProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), symbolInformationProvider));

        // Micro Focus directives outline provider
        // TODO: Consider adding .DIR keywords for enhanced directive support
        const mfDirectivesProvider = new MFDirectivesSymbolProvider();
        context.subscriptions.push(languages.registerDocumentSymbolProvider(VSExtensionUtils.getAllMFProvidersSelectors(settings), mfDirectivesProvider));
    }

    // COBOL source completion provider for variables, fields, and copybook members
    context.subscriptions.push(languages.registerCompletionItemProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), new CobolSourceCompletionItemProvider(VSExternalFeatures)));

    // Commented out: Alternative comment completion provider
    // Can be enabled for specialized comment-based IntelliSense
    // const cobolCommentProvider = new CobolCommentProvider(VSCOBOLConfiguration.get());
    // const cobolCommentProviderDisposible = languages.registerCompletionItemProvider(allCobolSelectors, cobolCommentProvider);
    // context.subscriptions.push(cobolCommentProviderDisposible);

    // Register hover provider for showing information on hover
    context.subscriptions.push(languages.registerHoverProvider(VSExtensionUtils.getAllCobolSelectors(settings, false), {
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        provideHover(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken): ProviderResult<vscode.Hover> {
            return VSHoverProvider.provideHover(settings, document, position);
        }
    }));
    
    // Register rename provider for symbol renaming across files
    context.subscriptions.push(languages.registerRenameProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), new VSCobolRenameProvider()));

    // Define decoration update functions for performance and modularity
    
    /**
     * Updates decorations for a specific text editor.
     * Handles margin decorations, linting, and comment highlighting.
     * @param editor - The text editor to update decorations for
     */
    const updateDecorationsOnTextEditor = async (editor: vscode.TextEditor) => {
        await vsMarginHandler.updateDecorations(editor);
        if (editor && editor.document) {
            await linter.updateLinter(editor.document);
        }
        await colourCommentHandler.updateDecorations(editor);
    };

    /**
     * Updates decorations for the currently active editor.
     * Used as the callback for debounced decoration updates.
     */
    const activeEditorupdateDecorations = async () => {
        await updateDecorationsOnTextEditor(activeEditor);
    };

    // Debouncing mechanism to prevent excessive decoration updates during rapid changes
    let timeout: NodeJS.Timeout | undefined = undefined;

    /**
     * Triggers a debounced decoration update.
     * Cancels previous pending updates and schedules a new one after 200ms.
     * This improves performance during rapid typing or selection changes.
     */
    const triggerUpdateDecorations = async () => {
        if (timeout) {
            clearTimeout(timeout);
        }
        timeout = setTimeout(activeEditorupdateDecorations, 200);
    }

    // Register editor event handlers for real-time updates
    
    // Monitor active text editor changes
    window.onDidChangeActiveTextEditor(async (editor) => {
        if (!editor) {
            return;
        }
        activeEditor = editor;
        triggerUpdateDecorations();
    }, null, context.subscriptions);

    // Monitor text editor selection changes (cursor movement, text selection)
    window.onDidChangeTextEditorSelection(async (event) => {
        if (!event.textEditor) {
            return;
        }
        triggerUpdateDecorations();
    }, null, context.subscriptions);

    // Monitor document content changes (typing, editing)
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    workspace.onDidChangeTextDocument(async (event) => {
        if (!window.activeTextEditor) {
            return;
        }

        // Only update decorations if the changed document is the active one
        if (event.document === activeEditor.document) {
            activeEditor = window.activeTextEditor;
            triggerUpdateDecorations();
        }
    }, null, context.subscriptions);

    // Initialize decorations for the currently active editor (if any)
    if (window.activeTextEditor !== undefined) {
        activeEditor = window.activeTextEditor;
        triggerUpdateDecorations();
    }

    // Configure status bar and progress indicators
    progressStatusBarItem.command = "cobolplugin.showCOBOLChannel";
    progressStatusBarItem.hide();
    context.subscriptions.push(progressStatusBarItem);

    // Register build script task provider (only in trusted workspaces for security)
    if (workspace.isTrusted) {
        bldscriptTaskProvider = vscode.tasks.registerTaskProvider(BldScriptTaskProvider.BldScriptType, new BldScriptTaskProvider());
        context.subscriptions.push(bldscriptTaskProvider);
    }

    // Register semantic token provider for syntax highlighting enhancement
    const provider = VSSemanticProvider.provider();
    vscode.languages.registerDocumentSemanticTokensProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), provider, VSSemanticProvider.getLegend());

    // Register code lens provider for inline actionable information
    const codelensProvider = new VSPPCodeLens();
    languages.registerCodeLensProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), codelensProvider);

    // Initialize storage alignment context for COBOL data structures
    vscode.commands.executeCommand("setContext", "cobolplugin.enableStorageAlign", true);

    // Monitor text selection changes for storage alignment features
    // This enables/disables storage alignment commands based on cursor position
    window.onDidChangeTextEditorSelection((e: vscode.TextEditorSelectionChangeEvent) => {
        if (!VSExtensionUtils.isSupportedLanguage(e.textEditor.document)) {
            return;
        }

        // Check if any selection contains storage items (COBOL data definitions)
        for (const sel of e.selections) {
            for (let startLine = sel.start.line; startLine <= sel.end.line; startLine++) {
                const textSelection = e.textEditor.document.lineAt(startLine).text;
                const line = textSelection.trimEnd();
                const sipos = VSCOBOLUtils.getStorageItemPosition(line);
                if (sipos !== -1) {
                    // Found storage item, enable alignment commands
                    vscode.commands.executeCommand("setContext", "cobolplugin.enableStorageAlign", true);
                    return;
                }
            }
        }

        // No storage items found in selection, disable alignment commands
        vscode.commands.executeCommand("setContext", "cobolplugin.enableStorageAlign", false);
    })

    // Commented out: Future signature help provider implementation
    // This would provide parameter hints for COBOL statements and procedures
    // vscode.languages.registerSignatureHelpProvider(VSExtensionUtils.getAllCobolSelectors(settings), new class implements vscode.SignatureHelpProvider {
    //     provideSignatureHelp(
    //         // eslint-disable-next-line @typescript-eslint/no-unused-vars
    //         document: vscode.TextDocument,
    //         // eslint-disable-next-line @typescript-eslint/no-unused-vars
    //         position: vscode.Position,
    //         // eslint-disable-next-line @typescript-eslint/no-unused-vars
    //         token: vscode.CancellationToken,
    //         // eslint-disable-next-line @typescript-eslint/no-unused-vars
    //         context: vscode.SignatureHelpContext
    //     ): vscode.ProviderResult<vscode.SignatureHelp> {
    //
    //         // console.log(context.activeSignatureHelp);
    //
    //         // Return fake signature help result
    //         const sigHelp = new vscode.SignatureHelp();
    //         sigHelp.activeParameter = 0;
    //         sigHelp.activeSignature = 0;
    //         sigHelp.signatures = [
    //             new vscode.SignatureInformation("1", "Paramter 1"),
    //             new vscode.SignatureInformation("2")
    //         ];
    //         return sigHelp;
    //     }
    // }, {
    //     triggerCharacters: ["by"],
    //     retriggerCharacters: [","]
    // });

    // Process visible editors for Micro Focus LSP integration
    let toggleDone = false;
    for (const vte of vscode.window.visibleTextEditors) {
        // Update decorations for all visible editors
        await updateDecorationsOnTextEditor(vte);

        // Enable Micro Focus LSP if a document with 'cobol' language ID is detected
        if (!toggleDone && vte.document.languageId === ExtensionDefaults.microFocusCOBOLLanguageId) {
            const mfExt = extensions.getExtension(ExtensionDefaults.rocketCOBOLExtension);
            if (mfExt) {
                await toggleMicroFocusLSP(settings, vte.document, true);
            }
            toggleDone = true;
        }
    }

    // Register terminal profile provider (Linux only)
    // Provides custom terminal configurations for COBOL development
    if (process.platform === 'linux') {
        vscode.window.registerTerminalProfileProvider('bitlang.terminals', new VSTerminal(context));
    }

    // Register additional reference provider (duplicate registration for redundancy)
    // Note: This appears to be a duplicate registration - consider consolidating
    context.subscriptions.push(languages.registerReferenceProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), new CobolReferenceProvider()));

    // Optional startup metadata processing
    // Processes workspace files for metadata cache if enabled in settings
    if (settings.process_metadata_cache_on_start) {
        try {
            if (settings.maintain_metadata_cache) {
                commands.executeCommand("cobolplugin.processAllFilesInWorkspaceOnStartup");
            }
        } catch {
            // Defensive error handling to prevent activation failure
            // If metadata processing fails, extension should still activate
        }
    }

    // Display changelog for new versions
    openChangeLog(context);

    // Enable decoration updates now that all providers are registered
    // This flag prevents premature decoration updates during activation
    updateDecorationsOnTextEditorEnabled = true;

    // Commented out: Future markdown integration for enhanced documentation
    // Would enable syntax highlighting in markdown documents containing COBOL
    //     return {
    //         extendMarkdownIt(md: MarkdownIt) {
    //             return md.use(require('markdown-it-highlightjs/core'), {hijs});
    //         }
    //     };
}

/**
 * Performs asynchronous cleanup tasks during extension deactivation.
 * 
 * This function saves the current workspace metadata cache to ensure
 * that symbol information is persisted across VS Code sessions.
 * 
 * @returns Promise that resolves when cleanup is complete
 */
export async function deactivateAsync(): Promise<void> {
    VSCOBOLUtils.saveGlobalCacheToWorkspace(VSCOBOLConfiguration.get_workspace_settings());
}

/**
 * Extension deactivation entry point.
 * 
 * Called by VS Code when the extension is being deactivated.
 * Performs cleanup of disposable resources and saves state.
 * 
 * @returns Promise that resolves when deactivation is complete
 */
export async function deactivate(): Promise<void> {
    // Clean up build script task provider if it was registered
    if (bldscriptTaskProvider) {
        bldscriptTaskProvider.dispose();
    }
    
    // Perform additional async cleanup tasks
    await deactivateAsync();
}
