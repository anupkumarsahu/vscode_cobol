import * as vscode from "vscode";
import { ExtensionContext, languages, Position, TextDocument, CancellationToken, ProviderResult, Definition } from "vscode";
import * as openCopybookProvider from "../features/workspace/openCopybookProvider";
import { ICOBOLSettings } from "../config/IConfiguration";
import { VSExternalFeatures } from "../features/runtime/externalFeatures";
import { COBOLTypeFormatter, VSHoverProvider } from "../providers/language";
import { VSExtensionUtils } from "../utils/extensionUtils";
import { COBOLSourceDefinition, COBOLCallTargetProvider, COBOLReferenceProvider, VSCOBOLRenameProvider } from "../providers/navigation";
import { cobolLinterActionFixer } from "../providers/language/cobolLinter";
import { KeywordAutocompleteCompletionItemProvider, SnippetCompletionItemProvider, COBOLSymbolInformationProvider, COBOLSourceCompletionItemProvider } from "../providers/intellisense";

export function registerLanguageProviders(
    context: ExtensionContext,
    settings: ICOBOLSettings,
    cobolFixer: cobolLinterActionFixer
): void {
    context.subscriptions.push(COBOLTypeFormatter.register(settings));

    context.subscriptions.push(languages.registerDefinitionProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), {
        provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {
            const ccbp = new openCopybookProvider.COBOLCopyBookProvider(VSExternalFeatures);
            return ccbp.provideDefinition(doc, pos, ct);
        }
    }));

    context.subscriptions.push(languages.registerDefinitionProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), {
        provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {
            const csd = new COBOLSourceDefinition();
            return csd.provideDefinition(doc, pos, ct);
        }
    }));

    context.subscriptions.push(languages.registerDefinitionProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), {
        provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {
            const csdp = new COBOLCallTargetProvider(VSExternalFeatures);
            return csdp.provideDefinition(doc, pos, ct);
        }
    }));

    context.subscriptions.push(languages.registerReferenceProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), new COBOLReferenceProvider()));
    context.subscriptions.push(languages.registerCodeActionsProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), cobolFixer));

    context.subscriptions.push(languages.registerCompletionItemProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), new KeywordAutocompleteCompletionItemProvider(true, settings)));
    context.subscriptions.push(languages.registerCompletionItemProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), SnippetCompletionItemProvider.Default.reInitCallMap(settings)));

    if (settings.outline) {
        const symbolInformationProvider = new COBOLSymbolInformationProvider();
        context.subscriptions.push(languages.registerDocumentSymbolProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), symbolInformationProvider));
    }

    context.subscriptions.push(languages.registerCompletionItemProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), new COBOLSourceCompletionItemProvider(VSExternalFeatures)));

    context.subscriptions.push(languages.registerHoverProvider(VSExtensionUtils.getAllCobolSelectors(settings, false), {
        provideHover(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken): ProviderResult<vscode.Hover> {
            return VSHoverProvider.provideHover(settings, document, position);
        }
    }));

    context.subscriptions.push(languages.registerRenameProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), new VSCOBOLRenameProvider()));
}
