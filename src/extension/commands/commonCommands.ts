import * as vscode from "vscode";
import { ICOBOLSettings, intellisenseStyle } from "../../config/IConfiguration";
import { VSExtensionUtils } from "../../utils/extensionUtils";
import { VSExternalFeatures } from "../../features/runtime/externalFeatures";
import { cobolProgramCommandsCommands } from "./cobolProgramCommands";
import { TabUtils } from "../../features/editor/tabStopUtils";
import { VSLogger } from "../../utils/logger";
import { AlignStyle, VSCOBOLUtils, FoldAction } from "../../utils/cobolUtils";
import { commands, ExtensionContext } from "vscode";
import { VSPPCodeLens } from "../../providers/language/preprocessorCodeLensProvider";
import { ExtensionDefaults } from "../../config/extensionDefaults";
import { COBOLSourceScanner } from "../../features/workspace/cobolsourcescanner";
import path from "path";
import fs from "fs";
import { VSWorkspaceFolders } from "../../features/workspace/workspaceFolders";
import { VSDiagCommands } from "./diagnosticCommands";
import { CopyBookDragDropProvider } from "./copybookDragDropProvider";
import { VSCOBOLConfiguration } from "../../config/workspaceConfiguration";
import { COBOLHierarchyProvider } from "../../providers/navigation/callHierarchyProvider";
import { newFile_dot_callgraph, view_dot_callgraph } from "./dotMarkdownCommands";
import { VSMakeDep } from "./makeDependencyCommand";

/**
 * Creates a new untitled COBOL file in the current workspace (or process cwd when no workspace exists).
 *
 * Flow:
 * 1. Prompt for a valid program name.
 * 2. Validate name using COBOL literal rules.
 * 3. Prevent accidental overwrite when a matching .cbl file already exists.
 * 4. Open as an untitled document so users can review/modify before first save.
 * 5. Force language id to the requested COBOL dialect.
 */
async function emptyFile(title: string, doclang: string, config: ICOBOLSettings) {
    let fpath = "";
    let fdir = "";

    const ws = VSWorkspaceFolders.get(config);
    if (ws) {
        fdir = ws[0].uri.fsPath;
    } else {
        fdir = process.cwd();
    }

    vscode.window.showInputBox({
        title: title,
        prompt: `In directory : ${fdir}`,
        value: "untitled",
        validateInput: (text: string): string | undefined => {
            if (!text || !COBOLSourceScanner.isValidLiteral(text)) {
                return "Invalid program name";
            }

            fpath = path.join(fdir, text + ".cbl");

            if (fs.existsSync(fpath)) {
                return `File already exists (${fpath})`;
            }

            return undefined;
        }
    }
    ).then(async function (data) {
        const ws = VSWorkspaceFolders.get(config);
        if (ws) {
            fpath = path.join(ws[0].uri.fsPath, data + ".cbl");
        } else {
            fpath = path.join(process.cwd(), data + ".cbl");
        }
        const furl = vscode.Uri.file(fpath).with({ scheme: "untitled" });
        await vscode.workspace.openTextDocument(furl).then(async document => {
            const editor = await vscode.window.showTextDocument(document);
            if (editor !== undefined) {
                await vscode.languages.setTextDocumentLanguage(document, doclang);
            }
        });
    });
}

/**
 * Extensions that are intentionally ignored during conflict detection.
 *
 * Notes:
 * - Prefixes (eg. "vscode.") are treated as trusted namespaces.
 * - This extension namespace ("bitlang.") is ignored to avoid self-reporting.
 */
const blessed_extensions: string[] = [
    "HCLTechnologies.hclappscancodesweep",          // code scanner
    "bitlang.",                                     // mine
    "vscode.",                                      // vscode internal extensions
    "ms-vscode.",                                   //
    "ms-python.",                                   //
    "ms-vscode-remote.",
    "redhat."                                       // redhat
];

/**
 * Curated list of known problematic extensions.
 *
 * Tuple schema:
 * [humanReason, extensionIdOrNamespace, isFatalEditorConflict]
 */
const known_problem_extensions: [string, string, boolean][] = [
    ["bitlang.cobol already provides autocomplete and highlight for COBOL source code", "BroadcomMFD.cobol-language-support", true],
    ["A control flow extension that is not compatible with this dialect of COBOL", "BroadcomMFD.ccf", true],             // control flow extension
    ["COBOL debugger for different dialect of COBOL", "COBOLworx.cbl-gdb", true],
    ["Inline completion provider causes problems with this extension", "bloop.bloop-write", false],
    ["Language provider of COBOL that is not supported with extension", "heirloomcomputinginc", true]
];


// eslint-disable-next-line @typescript-eslint/no-explicit-any
/**
 * Builds a human-readable conflict report section for one extension.
 *
 * The message includes actionable uninstall guidance plus optional metadata
 * (description/version/repository/bug channels) when supplied by the extension.
 */
function getExtensionInformation(grab_info_for_ext: vscode.Extension<any>, reasons: string[]): string {
    let dupExtensionMessage = "";

    if (grab_info_for_ext.packageJSON === undefined) {
        return dupExtensionMessage
    }

    if (grab_info_for_ext.packageJSON !== undefined && grab_info_for_ext.packageJSON.publisher === "bitlang") {
        return dupExtensionMessage;
    }

    if (grab_info_for_ext.packageJSON.id !== undefined) {
        dupExtensionMessage += `\nThe extension ${grab_info_for_ext.packageJSON.name} from ${grab_info_for_ext.packageJSON.publisher} has conflicting functionality\n`;
        dupExtensionMessage += " Solution      : Disable or uninstall this extension, eg: use command:\n";
        dupExtensionMessage += `                 code --uninstall-extension ${grab_info_for_ext.packageJSON.id}\n`;
    }

    if (reasons.length !== 0) {
        let rcount = 1;
        const reasonMessage = reasons.length === 1 ? "Reason " : "Reasons";
        for (const reason of reasons) {
            if (rcount === 1) {
                dupExtensionMessage += ` ${reasonMessage}       : ${reason}\n`;
            } else {
                dupExtensionMessage += `               : ${reason}\n`;
            }
            rcount++;
        }
    }

    if (grab_info_for_ext.packageJSON.id !== undefined) {
        dupExtensionMessage += ` Id            : ${grab_info_for_ext.packageJSON.id}\n`;

        if (grab_info_for_ext.packageJSON.description !== undefined) {
            dupExtensionMessage += ` Description   : ${grab_info_for_ext.packageJSON.description}\n`;
        }
        if (grab_info_for_ext.packageJSON.version !== undefined) {
            dupExtensionMessage += ` Version       : ${grab_info_for_ext.packageJSON.version}\n`;
        }
        if (grab_info_for_ext.packageJSON.repository !== undefined && grab_info_for_ext.packageJSON.repository.url !== undefined) {
            dupExtensionMessage += ` Repository    : ${grab_info_for_ext.packageJSON.repository.url}\n`;
        }
        if (grab_info_for_ext.packageJSON.bugs !== undefined && grab_info_for_ext.packageJSON.bugs.url !== undefined) {
            dupExtensionMessage += ` Bug Reporting : ${grab_info_for_ext.packageJSON.bugs.url}\n`;
        }
        if (grab_info_for_ext.packageJSON.bugs !== undefined && grab_info_for_ext.packageJSON.bugs.email !== undefined) {
            dupExtensionMessage += ` Bug Email     : ${grab_info_for_ext.packageJSON.bugs.email}\n`;
        }
        if (dupExtensionMessage.length !== 0) {
            dupExtensionMessage += "\n";
        }
    }

    return dupExtensionMessage;
}


/**
 * Scans all installed extensions for known or inferred COBOL conflicts.
 *
 * Returns tuple:
 * [detailedConflictMessage, conflictingDebuggerFound, fatalEditorConflict]
 */
function checkExtensions(): [string, boolean, boolean] {
    let dupExtensionMessage = "";
    let conflictingDebuggerFound = false;
    let fatalEditorConflict = false;

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    for (const ext of vscode.extensions.all) {
        const reason = [];
        let ignore_blessed = false;
        if (ext !== undefined && ext.packageJSON !== undefined) {
            if (ext.packageJSON.id !== undefined) {
                const idLower = `${ext.packageJSON.id}`.toLowerCase();
                if (ext.packageJSON.id === ExtensionDefaults.thisExtensionName) {
                    continue;
                }

                // Ignore trusted namespaces and explicit allow-list entries.
                for (const blessed_extension of blessed_extensions) {
                    if (blessed_extensions.indexOf(".") !== -1) {
                        if (blessed_extension.toLowerCase() === idLower) {
                            ignore_blessed = true;
                        }
                    } else {
                        if (idLower.startsWith(blessed_extension.toLowerCase())) {
                            ignore_blessed = true;
                        }
                    }
                }

                if (ignore_blessed) {
                    continue;
                }

                // Apply exact/prefix matching against known problem extensions.
                for (const [type_of_extension, known_problem_extension, editor_confict] of known_problem_extensions) {
                    if (known_problem_extension.indexOf(".") !== -1) {
                        // exact match
                        if (known_problem_extension.toLowerCase() === idLower) {
                            reason.push(`contributes '${type_of_extension}'`);
                            if (type_of_extension.includes("debugger")) {
                                conflictingDebuggerFound = true;
                            }
                            fatalEditorConflict = editor_confict;
                        }
                    } else {
                        if (idLower.startsWith(known_problem_extension.toLowerCase()) ||
                            idLower.endsWith(known_problem_extension.toLowerCase())) {
                            reason.push(`contributes '${type_of_extension}'`);
                            if (type_of_extension.includes("debugger")) {
                                conflictingDebuggerFound = true;
                            }
                            fatalEditorConflict = editor_confict;
                        }
                    }
                }
            }

            let extMarkedAsDebugger = false;
            // Check category tags first, then inspect debugger/breakpoint contributions.

            if (ext.packageJSON.categories !== undefined) {
                const categoriesBody = ext.packageJSON.categories;
                if (categoriesBody !== undefined && categoriesBody instanceof Object) {
                    for (const key in categoriesBody) {
                        try {
                            const element = categoriesBody[key];
                            if (element !== undefined) {
                                const l = `${element}`.toUpperCase();
                                if (l === "DEBUGGERS") {
                                    extMarkedAsDebugger = true;
                                }
                            }
                        } catch {
                            // just incase
                        }
                    }
                }
            }

            if (ext.packageJSON.contributes !== undefined) {
                const grammarsBody = ext.packageJSON.contributes.grammars;
                const languagesBody = ext.packageJSON.contributes.languages;

                // Detect other extensions registering COBOL grammar ownership.
                if (grammarsBody !== undefined && grammarsBody instanceof Object) {
                    for (const key in grammarsBody) {
                        try {
                            const element = grammarsBody[key];
                            if (element !== undefined && element.language !== undefined) {
                                const l = `${element.language}`.toUpperCase();
                                if (l === ExtensionDefaults.defaultCOBOLLanguage) {
                                    reason.push("contributes conflicting grammar (COBOL)");
                                    fatalEditorConflict = true;
                                }
                            }
                        } catch {
                            // just incase
                        }
                    }
                }

                // Detect duplicate COBOL language id contributions.
                if (languagesBody !== undefined && languagesBody instanceof Object) {
                    for (const key in languagesBody) {
                        const languageElement = languagesBody[key];
                        try {

                            if (languageElement !== undefined && languageElement.id !== undefined) {
                                const l = `${languageElement.id}`.toUpperCase();
                                if (l === ExtensionDefaults.defaultCOBOLLanguage) {
                                    reason.push("contributes language id (COBOL)");
                                    fatalEditorConflict = true;
                                }
                            }
                        }
                        catch {
                            // just incase
                        }
                    }
                }

                if (extMarkedAsDebugger) {
                    const debuggerBody = ext.packageJSON.contributes.debuggers;
                    const breakpointsBody = ext.packageJSON.contributes.breakpoints;
                    // A debugger targeting our COBOL language id is treated as a hard conflict.
                    if (debuggerBody !== undefined && debuggerBody instanceof Object) {
                        for (const key in debuggerBody) {
                            try {
                                const debuggerElement = debuggerBody[key];
                                if (debuggerElement !== undefined) {
                                    const debuggerLanguages = debuggerElement.languages;
                                    if (debuggerLanguages !== undefined && debuggerLanguages instanceof Object) {
                                        for (const keyLanguage of debuggerLanguages) {
                                            if (keyLanguage === ExtensionDefaults.defaultCOBOLLanguage) {
                                                reason.push(`extension includes a debugger for a different COBOL vendor -> ${debuggerElement.label} of debugger type ${debuggerElement.type}`);
                                                conflictingDebuggerFound = true;
                                                fatalEditorConflict = true;
                                            }
                                        }
                                    }
                                }
                            }
                            catch {
                                // just incase
                            }
                        }
                    }

                    if (breakpointsBody !== undefined && breakpointsBody instanceof Object) {
                        try {
                            for (const bpLangKey of breakpointsBody) {
                                if (bpLangKey !== undefined && bpLangKey.language !== undefined) {
                                    const bpLang = `${bpLangKey.language}`;
                                    if (bpLang === ExtensionDefaults.defaultCOBOLLanguage) {
                                        reason.push("extension includes debug breakpoint support for a different COBOL vendor");
                                        conflictingDebuggerFound = true;
                                        fatalEditorConflict = true;
                                    }
                                }
                            }
                        }
                        catch {
                            // just incase
                        }
                    }
                }
            }

            if (reason.length !== 0) {
                dupExtensionMessage += getExtensionInformation(ext, reason);
            }
        }
    }

    return [dupExtensionMessage, conflictingDebuggerFound, fatalEditorConflict];
}

/**
 * Performs conflict checks and applies guard rails when incompatibilities are found.
 *
 * Behavior:
 * - Logs rich diagnostics to the COBOL output channel.
 * - Optionally downgrades open/new COBOL documents to plaintext when editor conflicts are fatal.
 * - Presents a modal notification to make the conflict explicit.
 * - Throws when a conflicting COBOL debugger is detected, forcing this extension inactive.
 */
export function checkForExtensionConflicts(settings: ICOBOLSettings, context: ExtensionContext): boolean {

    const checkResults = checkExtensions()
    const checkForExtensionConflictsMessage = checkResults[0];
    const conflictingDebuggerFound = checkResults[1];
    const fatalEditorConflict = checkResults[2];

    // display the message
    if (checkForExtensionConflictsMessage.length !== 0) {
        VSLogger.logMessage(checkForExtensionConflictsMessage);

        if (fatalEditorConflict) {
            // Convert already-open COBOL documents to plaintext to avoid dueling providers.
            for (const veditor of vscode.window.visibleTextEditors) {
                const doc = veditor.document;
                if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, doc.languageId)) {
                    VSLogger.logMessage(`Document ${doc.fileName} changed to plaintext to avoid errors, as the COBOL extension is inactive`);
                    vscode.languages.setTextDocumentLanguage(doc, "plaintext");
                }
            }

            // Keep future opens safe until the conflicting extension is removed.
            const onDidOpenTextDocumentHandler = vscode.workspace.onDidOpenTextDocument(async (doc: vscode.TextDocument) => {
                if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, doc.languageId)) {
                    VSLogger.logMessage(`Document ${doc.fileName} changed to plaintext to avoid errors, as the COBOL extension is inactive`);
                    vscode.languages.setTextDocumentLanguage(doc, "plaintext");
                }
            });

            context.subscriptions.push(onDidOpenTextDocumentHandler);
        }

        vscode.window.showInformationMessage(
            `${ExtensionDefaults.thisExtensionName} Extension has located duplicate or conflicting functionality`,
            { modal: true })
            // eslint-disable-next-line @typescript-eslint/no-unused-vars
            .then(function (data) {
                VSLogger.logChannelSetPreserveFocus(false);
            });


        if (conflictingDebuggerFound) {
            const msg = "This Extension is now inactive until conflict is resolved";
            VSLogger.logMessage(`\n${msg}\nRestart 'vscode' once the conflict is resolved or you can disabled the ${ExtensionDefaults.thisExtensionName} extension`);
            throw new Error(msg);
        }

        return false;
    }

    return false;
}


export function activateCommonCommands(context: vscode.ExtensionContext) {
    // Core language-id normalization command.
    context.subscriptions.push(commands.registerCommand("cobolplugin.change_lang_to_cobol", async function () {
        const act = vscode.window.activeTextEditor;
        if (act === null || act === undefined) {
            return;
        }

        // ensure all documents with the same id are change to the current ext id
        await VSCOBOLUtils.changeDocumentId(act.document.languageId, ExtensionDefaults.defaultCOBOLLanguage);

        const settings = VSCOBOLConfiguration.get_resource_settings(act.document, VSExternalFeatures);
        VSCOBOLUtils.enforceFileExtensions(settings, act, VSExternalFeatures, true, ExtensionDefaults.defaultCOBOLLanguage);
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.move2pd", function () {
        cobolProgramCommandsCommands.move2pd();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.move2dd", function () {
        cobolProgramCommandsCommands.move2dd();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.move2ws", function () {
        cobolProgramCommandsCommands.move2ws();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.move2anyforward", function () {
        cobolProgramCommandsCommands.move2anyforward();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.move2anybackwards", function () {
        cobolProgramCommandsCommands.move2anybackwards();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.tab", async function () {
        await TabUtils.processTabKey(true);
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.revtab", async function () {
        await TabUtils.processTabKey(false);
    }));

    // Text transformation commands (comments/format/case) are guarded by language-id checks.
    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.removeAllComments", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            const settings = VSCOBOLConfiguration.get_resource_settings(vscode.window.activeTextEditor.document, VSExternalFeatures);
            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                VSCOBOLUtils.RemoveComments(vscode.window.activeTextEditor);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.removeIdentificationArea", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;
            const settings = VSCOBOLConfiguration.get_resource_settings(vscode.window.activeTextEditor.document, VSExternalFeatures);

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                VSCOBOLUtils.RemoveIdentificationArea(vscode.window.activeTextEditor);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.removeColumnNumbers", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;
            const settings = VSCOBOLConfiguration.get_resource_settings(vscode.window.activeTextEditor.document, VSExternalFeatures);

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                VSCOBOLUtils.removeColumnNumbers(vscode.window.activeTextEditor);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.makeKeywordsLowercase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;
            const settings = VSCOBOLConfiguration.get_resource_settings(vscode.window.activeTextEditor.document, VSExternalFeatures);

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                VSCOBOLUtils.foldToken(VSExternalFeatures, settings, vscode.window.activeTextEditor, FoldAction.Keywords, langid, intellisenseStyle.LowerCase);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.makeKeywordsUppercase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;
            const settings = VSCOBOLConfiguration.get_resource_settings(vscode.window.activeTextEditor.document, VSExternalFeatures);

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                VSCOBOLUtils.foldToken(VSExternalFeatures, settings, vscode.window.activeTextEditor, FoldAction.Keywords, langid, intellisenseStyle.UpperCase);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.makeKeywordsCamelCase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;
            const settings = VSCOBOLConfiguration.get_resource_settings(vscode.window.activeTextEditor.document, VSExternalFeatures);

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                VSCOBOLUtils.foldToken(VSExternalFeatures, settings, vscode.window.activeTextEditor, FoldAction.Keywords, langid, intellisenseStyle.CamelCase);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.makeFieldsLowercase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;
            const settings = VSCOBOLConfiguration.get_resource_settings(vscode.window.activeTextEditor.document, VSExternalFeatures);

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                VSCOBOLUtils.foldToken(VSExternalFeatures, settings, vscode.window.activeTextEditor, FoldAction.ConstantsOrVariables, langid, intellisenseStyle.LowerCase);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.makeFieldsUppercase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;
            const settings = VSCOBOLConfiguration.get_resource_settings(vscode.window.activeTextEditor.document, VSExternalFeatures);

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                VSCOBOLUtils.foldToken(VSExternalFeatures, settings, vscode.window.activeTextEditor, FoldAction.ConstantsOrVariables, langid, intellisenseStyle.UpperCase);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.makeFieldsCamelCase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;
            const settings = VSCOBOLConfiguration.get_resource_settings(vscode.window.activeTextEditor.document, VSExternalFeatures);

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                VSCOBOLUtils.foldToken(VSExternalFeatures, settings, vscode.window.activeTextEditor, FoldAction.ConstantsOrVariables, langid, intellisenseStyle.CamelCase);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.makePerformTargetsLowerCase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;
            const settings = VSCOBOLConfiguration.get_resource_settings(vscode.window.activeTextEditor.document, VSExternalFeatures);

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                VSCOBOLUtils.foldToken(VSExternalFeatures, settings, vscode.window.activeTextEditor, FoldAction.PerformTargets, langid, intellisenseStyle.LowerCase);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.makePerformTargetsUpperCase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;
            const settings = VSCOBOLConfiguration.get_resource_settings(vscode.window.activeTextEditor.document, VSExternalFeatures);

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                VSCOBOLUtils.foldToken(VSExternalFeatures, settings, vscode.window.activeTextEditor, FoldAction.PerformTargets, langid, intellisenseStyle.UpperCase);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.makePerformTargetsCamelCase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;
            const settings = VSCOBOLConfiguration.get_resource_settings(vscode.window.activeTextEditor.document, VSExternalFeatures);

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                VSCOBOLUtils.foldToken(VSExternalFeatures, settings, vscode.window.activeTextEditor, FoldAction.PerformTargets, langid, intellisenseStyle.CamelCase);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.showCOBOLChannel", () => {
        VSLogger.logChannelSetPreserveFocus(true);
    }));

    // Column resequencing supports configurable start and increment values.
    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.resequenceColumnNumbers", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;
            const settings = VSCOBOLConfiguration.get_resource_settings(vscode.window.activeTextEditor.document, VSExternalFeatures);

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {

                vscode.window.showInputBox({
                    prompt: "Enter start line number and increment",
                    validateInput: (text: string): string | undefined => {
                        if (!text || text.indexOf(" ") === -1) {
                            return "You must enter two spaced delimited numbers (start increment)";
                        } else {
                            return undefined;
                        }
                    }
                }).then(value => {
                    // leave early
                    if (value === undefined) {
                        return;
                    }
                    const values: string[] = value.split(" ");
                    const startValue: number = Number.parseInt(values[0], 10);
                    const incrementValue: number = Number.parseInt(values[1], 10);
                    if (startValue >= 0 && incrementValue >= 1) {
                        VSCOBOLUtils.resequenceColumnNumbers(vscode.window.activeTextEditor, startValue, incrementValue);
                    } else {
                        vscode.window.showErrorMessage("Sorry invalid re-sequence given");
                    }
                });
            }
        }
    }));

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    context.subscriptions.push(commands.registerCommand("cobolplugin.ppcodelenaction", (args: string) => {
        VSPPCodeLens.actionCodeLens(args);
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.indentToCursor", () => {
        VSCOBOLUtils.indentToCursor();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.leftAdjustLine", () => {
        VSCOBOLUtils.leftAdjustLine();
    }));

    context.subscriptions.push(vscode.commands.registerTextEditorCommand("cobolplugin.transposeSelection", (textEditor, edit) => {
        VSCOBOLUtils.transposeSelection(textEditor, edit);
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.alignStorageFirst", () => {
        VSCOBOLUtils.alignStorage(AlignStyle.First);
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.alignStorageLeft", () => {
        VSCOBOLUtils.alignStorage(AlignStyle.Left);
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.alignStorageCenter", () => {
        VSCOBOLUtils.alignStorage(AlignStyle.Center);
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.alignStorageRight", () => {
        VSCOBOLUtils.alignStorage(AlignStyle.Right);
    }));
    vscode.commands.executeCommand("setContext", "cobolplugin.enableStorageAlign", true);

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.padTo72", () => {
        VSCOBOLUtils.padTo72();
    }));

    // Allows users to align VS Code file associations with a selected COBOL dialect.
    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.enforceFileExtensions", () => {
        if (vscode.window.activeTextEditor) {
            const dialects = ["COBOL"];
            const settings = VSCOBOLConfiguration.get_resource_settings(vscode.window.activeTextEditor.document, VSExternalFeatures);


            vscode.window.showQuickPick(dialects, { placeHolder: "Which Dialect do you prefer?" }).then(function (dialect) {
                if (vscode.window.activeTextEditor && dialect) {
                    VSCOBOLUtils.enforceFileExtensions(settings, vscode.window.activeTextEditor, VSExternalFeatures, true, dialect);
                }
            });

        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.selectionToCOBOLHEX", () => {
        VSCOBOLUtils.selectionToHEX(true);
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.selectionToHEX", () => {
        VSCOBOLUtils.selectionToHEX(false);
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.selectionHEXToASCII", () => {
        VSCOBOLUtils.selectionHEXToASCII();
    }));


    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.selectionToCOBOLNXHEX", () => {
        VSCOBOLUtils.selectionToNXHEX(true);
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.selectionToNXHEX", () => {
        VSCOBOLUtils.selectionToNXHEX(false);
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.newFile_BlankFile", async function () {
        if (vscode.window.activeTextEditor === undefined) {
            return;
        }
        const settings = VSCOBOLConfiguration.get_resource_settings(vscode.window.activeTextEditor.document, VSExternalFeatures);

        emptyFile("Empty COBOL file", "COBOL", settings);
    }));

    // Snapshot workspace settings once for commands that do not depend on active editor context.
    const _settings = VSCOBOLConfiguration.get_workspace_settings();

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.dumpAllSymbols", async function () {
        await VSDiagCommands.DumpAllSymbols(_settings);
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.makeDependencyFile", async function () {
        await VSMakeDep.MakeDependency(_settings);
    }));

    const langIds = _settings.valid_cobol_language_ids;

    // Register status items and drag-drop handlers per enabled COBOL dialect.
    for (const langid of langIds) {
        context.subscriptions.push(getLangStatusItem("Output Window", "cobolplugin.showCOBOLChannel", "Show", _settings, langid + "_1", langid));
        context.subscriptions.push(vscode.languages.registerDocumentDropEditProvider(VSExtensionUtils.getAllCobolSelector(langid), new CopyBookDragDropProvider()));
    }

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.dot_callgraph", async function () {
        if (vscode.window.activeTextEditor === undefined) {
            return;
        }
        const settings = VSCOBOLConfiguration.get_resource_settings(vscode.window.activeTextEditor.document, VSExternalFeatures);
        await newFile_dot_callgraph(settings);
    }));


    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.view_dot_callgraph", async function () {
        if (vscode.window.activeTextEditor === undefined) {
            return;
        }
        const settings = VSCOBOLConfiguration.get_resource_settings(vscode.window.activeTextEditor.document, VSExternalFeatures);
        await view_dot_callgraph(context,settings);
    }));

    // Call hierarchy registration is optional and feature-flag controlled.
    if (_settings.enable_program_information) {
        install_call_hierarchy(_settings, context)
    }
}


let installed_call_hierarchy:boolean = false;

/**
 * Installs call hierarchy providers once per extension lifecycle.
 *
 * This function is idempotent and safe to call repeatedly.
 */
export function install_call_hierarchy(_settings:ICOBOLSettings,  context: ExtensionContext) {
    // already installed
    if (installed_call_hierarchy) {
        return;
    }

    const langIds = _settings.valid_cobol_language_ids;
    for (const langid of langIds) {
        context.subscriptions.push(vscode.languages.registerCallHierarchyProvider(VSExtensionUtils.getAllCobolSelector(langid), new COBOLHierarchyProvider()));
    }
    installed_call_hierarchy=true;
}

/**
 * Creates a language-scoped status item that routes users to a command.
 */
function getLangStatusItem(text: string, command: string, title: string, settings: ICOBOLSettings, id: string, langid: string): vscode.LanguageStatusItem {
    const langStatusItem = vscode.languages.createLanguageStatusItem(id, VSExtensionUtils.getAllCobolSelector(langid));
    langStatusItem.text = text;
    langStatusItem.command = {
        command: command,
        title: title
    };
    return langStatusItem;
}

