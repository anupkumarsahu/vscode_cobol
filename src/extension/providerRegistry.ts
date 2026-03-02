import * as vscode from "vscode";
import { ExtensionContext, languages, Position, TextDocument, CancellationToken, ProviderResult, Definition } from "vscode";
import * as opencopybook from "../features/workspace/opencopybook";
import { ICOBOLSettings } from "../config/iconfiguration";
import { VSExternalFeatures } from "../features/runtime/vsexternalfeatures";
import { COBOLTypeFormatter, VSHoverProvider } from "../providers/language";
import { VSExtensionUtils } from "../utils/vsextutis";
import { COBOLSourceDefinition, COBOLCallTargetProvider, CobolReferenceProvider, VSCobolRenameProvider } from "../providers/navigation";
import { CobolLinterActionFixer } from "../providers/language/cobollinter";
import { KeywordAutocompleteCompletionItemProvider, SnippetCompletionItemProvider, CobolSymbolInformationProvider, CobolSourceCompletionItemProvider } from "../providers/intellisense";

export function registerLanguageProviders(
    context: ExtensionContext,
    settings: ICOBOLSettings,
    cobolFixer: CobolLinterActionFixer
): void {
    context.subscriptions.push(COBOLTypeFormatter.register(settings));

    context.subscriptions.push(languages.registerDefinitionProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), {
        provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {
            const ccbp = new opencopybook.COBOLCopyBookProvider(VSExternalFeatures);
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

    context.subscriptions.push(languages.registerReferenceProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), new CobolReferenceProvider()));
    context.subscriptions.push(languages.registerCodeActionsProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), cobolFixer));

    context.subscriptions.push(languages.registerCompletionItemProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), new KeywordAutocompleteCompletionItemProvider(true, settings)));
    context.subscriptions.push(languages.registerCompletionItemProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), SnippetCompletionItemProvider.Default.reInitCallMap(settings)));

    if (settings.outline) {
        const symbolInformationProvider = new CobolSymbolInformationProvider();
        context.subscriptions.push(languages.registerDocumentSymbolProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), symbolInformationProvider));
    }

    context.subscriptions.push(languages.registerCompletionItemProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), new CobolSourceCompletionItemProvider(VSExternalFeatures)));

    context.subscriptions.push(languages.registerHoverProvider(VSExtensionUtils.getAllCobolSelectors(settings, false), {
        provideHover(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken): ProviderResult<vscode.Hover> {
            return VSHoverProvider.provideHover(settings, document, position);
        }
    }));

    context.subscriptions.push(languages.registerRenameProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), new VSCobolRenameProvider()));
}
