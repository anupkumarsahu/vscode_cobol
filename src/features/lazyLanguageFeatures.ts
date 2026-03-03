import * as vscode from "vscode";
import { ICOBOLSettings } from "../config/IConfiguration";
import { VSExtensionUtils } from "../utils/extensionUtils";
import { VSPPCodeLens } from "../providers/language/preprocessorCodeLensProvider";
import { VSSemanticProvider } from "../providers/language/semanticTokensProvider";

/**
 * Lazily registers semantic tokens and code lens providers after COBOL activation.
 */
export class LazyLanguageFeatures {
    private semanticRegistered = false;
    private codeLensRegistered = false;

    constructor(private readonly context: vscode.ExtensionContext) {
    }

    /**
     * Registers language providers once for the current extension session.
     */
    public registerIfNeeded(settings: ICOBOLSettings): void {
        if (!this.semanticRegistered) {
            const provider = VSSemanticProvider.provider();
            this.context.subscriptions.push(
                vscode.languages.registerDocumentSemanticTokensProvider(
                    VSExtensionUtils.getAllCobolSelectors(settings, true),
                    provider,
                    VSSemanticProvider.getLegend()
                )
            );
            this.semanticRegistered = true;
        }

        if (!this.codeLensRegistered) {
            const codelensProvider = new VSPPCodeLens();
            this.context.subscriptions.push(
                vscode.languages.registerCodeLensProvider(
                    VSExtensionUtils.getAllCobolSelectors(settings, true),
                    codelensProvider
                )
            );
            this.codeLensRegistered = true;
        }
    }

    /**
     * Hooks editor/document events and registers providers when a COBOL file is opened.
     */
    public bindDocumentActivation(settings: ICOBOLSettings): void {
        const maybeRegister = (document: vscode.TextDocument | undefined) => {
            if (!document) {
                return;
            }

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, document.languageId)) {
                this.registerIfNeeded(settings);
            }
        };

        maybeRegister(vscode.window.activeTextEditor?.document);

        this.context.subscriptions.push(vscode.workspace.onDidOpenTextDocument((document) => {
            maybeRegister(document);
        }));

        this.context.subscriptions.push(vscode.window.onDidChangeActiveTextEditor((editor) => {
            maybeRegister(editor?.document);
        }));
    }
}
