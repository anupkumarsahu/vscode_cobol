import * as vscode from "vscode";
import { ConfigurationChangeEvent } from "vscode";
import { ICOBOLSettings } from "../config/IConfiguration";
import { ExtensionDefaults } from "../config/extensionDefaults";
import { ConfigurationService } from "../config/configurationService";
import { VSCOBOLSourceScanner } from "../features/workspace/workspaceSymbolScanner";
import { VSCOBOLUtils } from "../utils/cobolUtils";
import { COBOLWorkspaceSymbolCacheHelper } from "../features/workspace/cobolworkspacecache";
import { VSExternalFeatures } from "../features/runtime/externalFeatures";
import { VSCustomIntellisenseRules } from "../providers/intellisense/customRules";
import { colourCommentHandler } from "../features/editor/colorComments";
import { vsMarginHandler } from "../features/editor/marginDecorations";
import { VSExtensionUtils, TextLanguage } from "../utils/extensionUtils";
import { SnippetCompletionItemProvider } from "../providers/intellisense/snippetCompletionProvider";
import { KeywordAutocompleteCompletionItemProvider } from "../providers/intellisense/keywordCompletionProvider";
import { TabUtils } from "../features/editor/tabStopUtils";
import { checkForExtensionConflicts, install_call_hierarchy } from "../extension/commands/commonCommands";

let shownEnableSemanticTokenProvider = false;

export type SetupLogChannelFn = (hide: boolean, settings: ICOBOLSettings, quiet: boolean) => Promise<void> | void;

/**
 * Applies settings-change side effects for a given configuration scope.
 */
export async function handleScopedConfigurationChange(
    event: ConfigurationChangeEvent,
    scope: vscode.ConfigurationScope | undefined,
    sharedContext: vscode.ExtensionContext,
    setupLogChannel: SetupLogChannelFn
): Promise<void> {
    const updated = event.affectsConfiguration(ExtensionDefaults.defaultEditorConfig, scope);
    const outlineChanged = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig, scope}.outline`, scope);
    const mdSymbols = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.metadata_symbols`, scope);
    const mdEntryPoints = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.metadata_entrypoints`, scope);
    const mdTypes = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.metadata_types`, scope);
    const mdMetadataFiles = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.metadata_files`, scope);
    const mdKnownCopybooks = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.metadata_knowncopybooks`, scope);
    const mdCopybookdirs = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.copybookdirs`, scope);
    const enableSemanticTokenProvider = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.enable_semantic_token_provider`, scope);
    const maintainMetadataRecursiveSearch = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.maintain_metadata_recursive_search`, scope);
    const enableCommentsTagsChanged = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.enable_comments_tags`, scope);
    const commentsTagsChanged = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.comments_tags`, scope);
    const intellisenseStyleChanged = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.intellisense_style`, scope);
    const enableColumnsTagsChanged = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.enable_columns_tags`, scope);
    const columnsTagsChanged = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.columns_tags`, scope);
    const marginChanged = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.margin`, scope);
    const marginIdentificationAreaChanged = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.margin_identification_area`, scope);
    const intellisenseAddSpaceKeywordsChanged = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.intellisense_add_space_keywords`, scope);
    const customIntellisenseRulesChanged = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.custom_intellisense_rules`, scope);
    const tabstopsAnchorsChanged = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.tabstops_anchors`, scope);
    const enableProgramInformationChanged = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.enable_program_information`, scope);

    if (!updated) {
        return;
    }

    ConfigurationService.reinitializeWorkspaceScoped();
    const settings = ConfigurationService.getWorkspace();

    if (!mdSymbols && !mdEntryPoints && !mdTypes && !mdMetadataFiles && !mdKnownCopybooks && !enableSemanticTokenProvider) {
        VSCOBOLSourceScanner.clearCOBOLCache();
        setupLogChannel(true, settings, true);
        VSCOBOLUtils.setupFilePaths(settings);
        async () => {
            await VSCOBOLUtils.setupUrlPaths(settings);
            await vscode.commands.executeCommand("setContext", "cobolplugin.sourceTreeRefreshRequested", true);
        };
    }

    if (mdSymbols) {
        COBOLWorkspaceSymbolCacheHelper.loadGlobalCacheFromArray(settings, settings.metadata_symbols, true);
    }

    if (mdEntryPoints) {
        COBOLWorkspaceSymbolCacheHelper.loadGlobalEntryCacheFromArray(settings, settings.metadata_entrypoints, true);
    }

    if (mdTypes) {
        COBOLWorkspaceSymbolCacheHelper.loadGlobalTypesCacheFromArray(settings, settings.metadata_types, true);
    }

    if (mdMetadataFiles) {
        COBOLWorkspaceSymbolCacheHelper.loadFileCacheFromArray(settings, VSExternalFeatures, settings.metadata_files, true);
    }

    if (enableSemanticTokenProvider && !shownEnableSemanticTokenProvider) {
        shownEnableSemanticTokenProvider = true;
        vscode.window.showInformationMessage(`The configuration setting '${ExtensionDefaults.defaultEditorConfig}.enable_semantic_token_provider' has changed but you may not see the affects until you have either close/reload your documents or restarted this session`);
    }

    if (mdKnownCopybooks) {
        COBOLWorkspaceSymbolCacheHelper.loadGlobalKnownCopybooksFromArray(settings, settings.metadata_knowncopybooks, true);
    }

    if (mdCopybookdirs) {
        VSCOBOLSourceScanner.clearCOBOLCache();
        setupLogChannel(true, settings, true);
        VSCOBOLUtils.setupFilePaths(settings);
        await VSCOBOLUtils.setupUrlPaths(settings);

        VSCOBOLUtils.populateDefaultCallableSymbolsSync(settings, true);
        VSCOBOLUtils.populateDefaultCopyBooksSync(settings, true);
    }

    if (maintainMetadataRecursiveSearch) {
        VSCOBOLUtils.populateDefaultCallableSymbolsSync(settings, true);
        VSCOBOLUtils.populateDefaultCopyBooksSync(settings, true);
    }

    if (outlineChanged) {
        vscode.window.showInformationMessage(`The configuration setting '${ExtensionDefaults.defaultEditorConfig}.outline' has changed but you may not see the affects until you have either reloaded your window or restarted this session`);
    }

    if (customIntellisenseRulesChanged) {
        VSCustomIntellisenseRules.Default.reFreshConfiguration(settings);
    }

    if (enableCommentsTagsChanged || commentsTagsChanged) {
        colourCommentHandler.setupTags();
    }

    if (enableColumnsTagsChanged || columnsTagsChanged || marginChanged || marginIdentificationAreaChanged) {
        vsMarginHandler.setupTags();
        for (const editor of vscode.window.visibleTextEditors) {
            if (VSExtensionUtils.isSupportedLanguage(editor.document) !== TextLanguage.Unknown) {
                await vsMarginHandler.updateDecorations(editor);
            }
        }
    }

    if (intellisenseStyleChanged) {
        SnippetCompletionItemProvider.Default.reInitCallMap(settings);
    }

    if (intellisenseAddSpaceKeywordsChanged) {
        KeywordAutocompleteCompletionItemProvider.Default4COBOL.reFreshConfiguration(settings);
    }

    if (tabstopsAnchorsChanged) {
        TabUtils.clearTabstopCache();
    }

    if (enableProgramInformationChanged && settings.enable_program_information) {
        install_call_hierarchy(settings, sharedContext);
    }

    checkForExtensionConflicts(settings, sharedContext);
}
