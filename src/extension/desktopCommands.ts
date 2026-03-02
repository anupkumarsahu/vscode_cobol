import * as vscode from "vscode";
import { commands, ExtensionContext, window } from "vscode";
import { ICOBOLSettings } from "../config/IConfiguration";
import { VSLogger } from "../utils/logger";
import { VSCOBOLUtils } from "../utils/cobolUtils";
import { VSExtensionUtils } from "../utils/extensionUtils";
import { VSCOBOLFileUtils } from "../features/workspace/workspaceFileUtils";
import { VSExternalFeatures } from "../features/runtime/externalFeatures";
import { VScobolWorkspaceScanner } from "../features/workspace/cobolScannerController";
import { SourceOrFolderTreeItem } from "../features/tree/sourceItem";
import { VSSourceTreeViewHandler } from "../features/tree/sourceViewTree";

export function registerDesktopCommands(context: ExtensionContext, settings: ICOBOLSettings): void {
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

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.extractSelectionToCopybook", async () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                await VSCOBOLFileUtils.extractSelectionToCopybook(vscode.window.activeTextEditor, VSExternalFeatures);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.extractSelectionToParagraph", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                VSCOBOLUtils.extractSelectionTo(vscode.window.activeTextEditor, true);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.extractSelectionToSection", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                VSCOBOLUtils.extractSelectionTo(vscode.window.activeTextEditor, false);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.explorerRun", function (fileUri) {
        const fsPath = fileUri.fsPath;
        VSLogger.logDebug(settings, "Explorer run command invoked for file: %s", fsPath);
        VSLogger.logDebug(settings, "Running or debugging COBOL file: %s", fsPath);
        VSCOBOLUtils.runOrDebug(fsPath, false);
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.explorerDebug", function (fileUri) {
        const fsPath = fileUri.fsPath;
        VSLogger.logDebug(settings, "Explorer debug command invoked for file: %s", fsPath);
        VSLogger.logDebug(settings, "Running or debugging COBOL file in debug mode: %s", fsPath);
        VSCOBOLUtils.runOrDebug(fsPath, true);
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.processAllFilesInWorkspaceOnStartup", async () => {
        await VScobolWorkspaceScanner.processAllFilesInWorkspaceOutOfProcess(VSExternalFeatures, settings, false, false, -1);
    }));

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
