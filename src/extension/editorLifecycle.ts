import * as vscode from "vscode";
import { ExtensionContext, window, workspace } from "vscode";
import { cobolLinterProvider } from "../providers/language/cobolLinter";
import { ICOBOLSettings } from "../config/IConfiguration";
import { VSCOBOLSourceScanner } from "../features/workspace/workspaceSymbolScanner";
import { ConfigurationService } from "../config/configurationService";
import { VSExtensionUtils } from "../utils/extensionUtils";
import { vsMarginHandler } from "../features/editor/marginDecorations";
import { colourCommentHandler } from "../features/editor/colorComments";
import { VSWorkspaceFolders } from "../features/workspace/workspaceFolders";
import { VSCOBOLUtils } from "../utils/cobolUtils";

/**
 * Coordinates editor events, scanner cache updates, decorations, and lint refresh.
 */
export class EditorLifecycleManager {
    private activeEditor: vscode.TextEditor | undefined;
    private updateDecorationsOnTextEditorEnabled = false;
    private timeout: NodeJS.Timeout | undefined;

    constructor(
        private readonly context: ExtensionContext,
        private readonly linter: cobolLinterProvider,
        private readonly settings: ICOBOLSettings
    ) {
    }

    public setActiveEditor(editor: vscode.TextEditor | undefined): void {
        this.activeEditor = editor;
    }

    public enableDecorationUpdates(): void {
        this.updateDecorationsOnTextEditorEnabled = true;
    }

    public registerEventHandlers(): void {
        this.context.subscriptions.push(workspace.onDidOpenTextDocument(async (doc: vscode.TextDocument) => {
            VSExtensionUtils.flip_plaintext(doc);

            if (VSExtensionUtils.isSupportedLanguage(doc) && window.activeTextEditor) {
                this.activeEditor = window.activeTextEditor;
                await this.triggerUpdateDecorations();
            }

            const workspaceFolder = VSWorkspaceFolders.get(this.settings);
            if (workspaceFolder !== undefined) {
                await vscode.commands.executeCommand<vscode.SymbolInformation[]>("vscode.executeDocumentSymbolProvider", doc.uri);
                await VSCOBOLUtils.populateDefaultCallableSymbols(this.settings, false);
            }
        }));

        this.context.subscriptions.push(window.onDidChangeActiveTextEditor(async (editor) => {
            if (!editor) {
                return;
            }
            this.activeEditor = editor;
            await this.triggerUpdateDecorations();
        }));

        this.context.subscriptions.push(window.onDidChangeTextEditorSelection(async (event) => {
            if (!event.textEditor) {
                return;
            }
            await this.triggerUpdateDecorations();
        }));

        this.context.subscriptions.push(workspace.onDidChangeTextDocument(async (event) => {
            if (!window.activeTextEditor || !this.activeEditor) {
                return;
            }

            if (event.document === this.activeEditor.document) {
                this.activeEditor = window.activeTextEditor;
                await this.triggerUpdateDecorations();
            }
        }));

        this.context.subscriptions.push(window.onDidChangeVisibleTextEditors(async (viewEditors) => {
            if (!this.updateDecorationsOnTextEditorEnabled) {
                return;
            }

            for (const textEditor of viewEditors) {
                await this.updateDecorationsOnTextEditor(textEditor);
            }
        }));

        this.context.subscriptions.push(workspace.onDidCloseTextDocument(async (doc: vscode.TextDocument) => {
            if (!VSExtensionUtils.isSupportedLanguage(doc)) {
                return;
            }

            const config = ConfigurationService.getResourceSettings(doc);
            VSCOBOLSourceScanner.removeCachedObject(doc, config);
            ConfigurationService.clearResourceCache(doc);
        }));
    }

    public normalizeAlreadyOpenDocuments(): void {
        for (const document of workspace.textDocuments) {
            VSExtensionUtils.flip_plaintext(document);
        }
    }

    public registerStorageAlignmentContext(): void {
        vscode.commands.executeCommand("setContext", "cobolplugin.enableStorageAlign", true);

        window.onDidChangeTextEditorSelection((event: vscode.TextEditorSelectionChangeEvent) => {
            if (!VSExtensionUtils.isSupportedLanguage(event.textEditor.document)) {
                return;
            }

            for (const selection of event.selections) {
                for (let startLine = selection.start.line; startLine <= selection.end.line; startLine++) {
                    const textSelection = event.textEditor.document.lineAt(startLine).text;
                    const line = textSelection.trimEnd();
                    const storageItemPosition = VSCOBOLUtils.getStorageItemPosition(line);
                    if (storageItemPosition !== -1) {
                        vscode.commands.executeCommand("setContext", "cobolplugin.enableStorageAlign", true);
                        return;
                    }
                }
            }

            vscode.commands.executeCommand("setContext", "cobolplugin.enableStorageAlign", false);
        }, null, this.context.subscriptions);
    }

    public async initializeActiveEditor(): Promise<void> {
        if (window.activeTextEditor !== undefined) {
            this.activeEditor = window.activeTextEditor;
            await this.triggerUpdateDecorations();
        }
    }

    public async refreshVisibleEditors(): Promise<void> {
        for (const editor of window.visibleTextEditors) {
            await this.updateDecorationsOnTextEditor(editor);
        }
    }

    public async triggerUpdateDecorations(): Promise<void> {
        if (!this.activeEditor) {
            return;
        }

        if (this.timeout) {
            clearTimeout(this.timeout);
        }

        this.timeout = setTimeout(async () => {
            if (this.activeEditor) {
                await this.updateDecorationsOnTextEditor(this.activeEditor);
            }
        }, 200);
    }

    private async updateDecorationsOnTextEditor(editor: vscode.TextEditor): Promise<void> {
        await vsMarginHandler.updateDecorations(editor);
        if (editor.document) {
            await this.linter.updateLinter(editor.document);
        }
        await colourCommentHandler.updateDecorations(editor);
    }
}
