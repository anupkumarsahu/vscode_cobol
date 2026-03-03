"use strict";

// Core Node.js and VS Code imports
import * as path from "path";
import * as vscode from "vscode";
import os from "os";

// VS Code API imports for extension functionality
import { commands, workspace, StatusBarItem, StatusBarAlignment, ExtensionContext, languages, window, extensions, ViewColumn, ConfigurationChangeEvent } from "vscode";

// Language service providers for IntelliSense and code navigation

import { cobolLinterProvider, cobolLinterActionFixer } from "./providers/language/cobolLinter";
import { VSSourceTreeViewHandler } from "./features/tree/sourceViewTree";

// Utility and helper classes
import { VSCOBOLUtils } from "./utils/cobolUtils";
import { ICOBOLSettings } from "./config/IConfiguration";

// Additional COBOL extension functionality
import { VSExternalFeatures } from "./features/runtime/externalFeatures";
import { BldScriptTaskProvider } from "./extension/buildTaskProvider";
// import { COBOLcaseFormatter } from "./caseFormatter"; // Currently disabled
import { COBOLWorkspaceSymbolCacheHelper } from "./features/workspace/cobolworkspacecache";

// File and utility management
import { COBOLOutputChannel, VSLogger } from "./utils/logger";
import { VSExtensionUtils } from "./utils/extensionUtils";

// Editor decorations and UI enhancements
import { CommentUtils } from "./features/editor/commentCommands";

// Extension configuration and constants
import { ExtensionDefaults } from "./config/extensionDefaults";

// Additional language service providers
import { activateCommonCommands, checkForExtensionConflicts } from "./extension/commands/commonCommands";
import { VSHelpAndFeedViewHandler } from "./features/tree/feedbackTree";
import { VSTerminal } from "./features/runtime/terminalProfiles";
import { ConfigurationService } from "./config/configurationService";
import { FeatureFlags } from "./config/featureFlags";
import { LazyLanguageFeatures } from "./features/lazyLanguageFeatures";
import { COBOL_LANGUAGE_ID } from "./language/languageConstants";
import { registerDesktopCommands } from "./extension/desktopCommands";
import { handleScopedConfigurationChange } from "./extension/configurationChangeHandler";
import { registerLanguageProviders } from "./extension/providerRegistry";
import { EditorLifecycleManager } from "./extension/editorLifecycle";
import { registerWorkspaceMetadataCommands } from "./extension/workspaceMetadataCommands";

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
/** Shared status item used to display scanner/progress updates in the status bar. */
export const progressStatusBarItem: StatusBarItem = window.createStatusBarItem(StatusBarAlignment.Left);

// Extension lifecycle and state management variables
let bldscriptTaskProvider: vscode.Disposable | undefined;  // Build script task provider for workspace tasks
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
                    { languageId: COBOL_LANGUAGE_ID }).get<number>("enabled");
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
        if (githubCopilotExtension !== undefined) {
            VSLogger.logMessage("Other Extension Information:");
            VSLogger.logMessage(` GitHub Copilot                             : ${githubCopilotExtension.packageJSON.version}`);
        }
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
 * @see VScobolWorkspaceScanner for asynchronous metadata scanning routines.
 * @see VSSemanticProvider for semantic token provisioning.
 */
export async function activate(context: ExtensionContext) {
    // Store the extension context globally for use in command handlers
    sharedContext = context;
    
    // Initialize workspace settings and configuration
    const settings: ICOBOLSettings = ConfigurationService.reinitializeWorkspace();

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
    registerDesktopCommands(context, settings);
    
    // Register common commands shared between desktop and web versions
    activateCommonCommands(context);

    // Monitor extension changes and refresh logging when extensions are installed/removed
    context.subscriptions.push(vscode.extensions.onDidChange(() => {
        setupLogChannel(true, settings, false);
        VSLogger.logMessage("extensions changed");
    }));

    // Set up configuration change monitoring with scoped handling
    const onDidChangeConfiguration = workspace.onDidChangeConfiguration(async (event: ConfigurationChangeEvent) => {
        await handleScopedConfigurationChange(event, undefined, sharedContext, setupLogChannel);
    });
    context.subscriptions.push(onDidChangeConfiguration);

    // Initialize linting and diagnostic services
    const collection = languages.createDiagnosticCollection("cobolDiag");
    const linter = new cobolLinterProvider(collection);
    const cobolfixer = new cobolLinterActionFixer();

    // Load cached metadata from workspace settings (symbols, entry points, types, etc.)
    // These caches improve performance by avoiding repeated file parsing
    COBOLWorkspaceSymbolCacheHelper.loadGlobalCacheFromArray(settings, settings.metadata_symbols, false);
    COBOLWorkspaceSymbolCacheHelper.loadGlobalEntryCacheFromArray(settings, settings.metadata_entrypoints, false);
    COBOLWorkspaceSymbolCacheHelper.loadGlobalTypesCacheFromArray(settings, settings.metadata_types, false);
    COBOLWorkspaceSymbolCacheHelper.loadFileCacheFromArray(settings, VSExternalFeatures, settings.metadata_files, false);
    COBOLWorkspaceSymbolCacheHelper.loadGlobalKnownCopybooksFromArray(settings, settings.metadata_knowncopybooks, false);

    // Register code action commands used by the linter for quick fixes.
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
                if (FeatureFlags.useCobolLineComment(settings)) {
                    // Use COBOL-specific comment formatting
                    CommentUtils.processCommentLine(settings);
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

    registerWorkspaceMetadataCommands(context);


    // Monitor workspace folder changes and reinitialize settings
    context.subscriptions.push(workspace.onDidChangeWorkspaceFolders(async () => {
        const settings: ICOBOLSettings = ConfigurationService.reinitializeWorkspace();

        await setupLogChannel(false, settings, true);
        VSCOBOLUtils.setupFilePaths(settings);
        await VSCOBOLUtils.setupUrlPaths(settings);
    }));

    // Initialize UI components and tree views
    await VSSourceTreeViewHandler.setupSourceViewTree(settings, false);
    VSHelpAndFeedViewHandler.setupSourceViewTree(settings, false);
    
    registerLanguageProviders(context, settings, cobolfixer);

    const editorLifecycleManager = new EditorLifecycleManager(context, linter, settings);
    editorLifecycleManager.normalizeAlreadyOpenDocuments();
    editorLifecycleManager.registerEventHandlers();

    await editorLifecycleManager.initializeActiveEditor();
    await editorLifecycleManager.refreshVisibleEditors();
    editorLifecycleManager.enableDecorationUpdates();

    // Configure status bar and progress indicators
    progressStatusBarItem.command = "cobolplugin.showCOBOLChannel";
    progressStatusBarItem.hide();
    context.subscriptions.push(progressStatusBarItem);

    // Register build script task provider (only in trusted workspaces for security)
    if (FeatureFlags.enableBuildTaskProvider()) {
        bldscriptTaskProvider = vscode.tasks.registerTaskProvider(BldScriptTaskProvider.BldScriptType, new BldScriptTaskProvider());
        context.subscriptions.push(bldscriptTaskProvider);
    }

    // Lazy-load heavyweight language features to reduce startup cost
    const lazyLanguageFeatures = new LazyLanguageFeatures(context);
    lazyLanguageFeatures.bindDocumentActivation(settings);

    editorLifecycleManager.registerStorageAlignmentContext();

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

    // Register terminal profile provider (Linux only)
    // Provides custom terminal configurations for COBOL development
    if (process.platform === 'linux') {
        vscode.window.registerTerminalProfileProvider('bitlang.terminals', new VSTerminal(context));
    }

    // Optional startup metadata processing.
    // Processes workspace files for metadata cache if enabled in settings
    if (FeatureFlags.startMetadataCacheProcessing(settings)) {
        try {
            commands.executeCommand("cobolplugin.processAllFilesInWorkspaceOnStartup");
        } catch {
            // Defensive error handling to prevent activation failure
            // If metadata processing fails, extension should still activate
        }
    }

    // Display changelog for new versions
    openChangeLog(context);

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
    VSCOBOLUtils.saveGlobalCacheToWorkspace(ConfigurationService.getWorkspace());
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
