import { CancellationToken, FormattingOptions, OnTypeFormattingEditProvider, Position, ProviderResult, TextDocument, TextEdit, languages } from "vscode";
import { ICOBOLSettings } from "../../config/IConfiguration";
import { VSExtensionUtils } from "../../utils/extensionUtils";
import { COBOLcaseFormatter } from "./caseFormatter";
import { COBOLDocumentationCommentHandler } from "../../features/editor/documentationCommentHandler";

/**
 * Composes on-type formatting from case normalization and documentation-comment continuation.
 */
export class COBOLTypeFormatter implements OnTypeFormattingEditProvider {
    caseFormatter: COBOLcaseFormatter;
    docFormatter: COBOLDocumentationCommentHandler;

    constructor() {
        this.caseFormatter = new COBOLcaseFormatter();
        this.docFormatter = new COBOLDocumentationCommentHandler();
    }

    /**
     * Merges edits from all formatter delegates while isolating delegate failures.
     */
    provideOnTypeFormattingEdits(document: TextDocument, position: Position, ch: string, options: FormattingOptions, token: CancellationToken): ProviderResult<TextEdit[]> {

        const ed: TextEdit[] =  [];

        try {
            // Keep formatting resilient: if one formatter fails, continue with others.
            const tmp_ed = this.caseFormatter.provideOnTypeFormattingEdits(document, position, ch, options, token);
            if (tmp_ed !== undefined && tmp_ed.length > 0) {
                ed.push(...tmp_ed);
            }
        }
        catch
        {
            // ignored
        }

        try {
            // Keep formatting resilient: if one formatter fails, continue with others.
            const tmp_ed = this.docFormatter.provideOnTypeFormattingEdits(document, position, ch, options, token);
            if (tmp_ed !== undefined && tmp_ed.length > 0) {
                ed.push(...tmp_ed);
            }
        }
        catch
        {
            // ignored
        }

        return ed;
    }
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    /**
     * Registers this provider for all configured COBOL language selectors.
     */
    static register(settings: ICOBOLSettings): any {
        const langPlusSchemas = VSExtensionUtils.getAllCobolSelectors(settings, false);

        return languages.registerOnTypeFormattingEditProvider(langPlusSchemas, new COBOLTypeFormatter(), "\n");
    }
}
