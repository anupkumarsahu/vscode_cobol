import { ICOBOLSettings } from "../config/IConfiguration";
import * as vscode from "vscode";

export enum TextLanguage {
    Unknown = 0,
    COBOL = 1
}

/**
 * Utility helpers for language/scheme checks and document selector creation.
 */
export class VSExtensionUtils {

    public static isSupportedLanguage(document: vscode.TextDocument): TextLanguage {

        switch (document.languageId.toLowerCase()) {
            case "cobol":
                return TextLanguage.COBOL;
        }

        /* not a supported language? */
        return TextLanguage.Unknown;
    }

    private static readonly knownSchemes: string[] = [
        "file",
        "ftp",
        "git",
        "member",
        "sftp",
        "ssh",
        "streamfile",
        "untitled",
        "vscode-vfs",
        "zip"
    ];

    public static isKnownScheme(scheme: string): boolean {
        for (const kscheme of VSExtensionUtils.knownSchemes) {
            if (scheme === kscheme) {
                return true;
            }
        }
        return false;
    }

    public static getAllCobolSelectors(config: ICOBOLSettings, for_intellisense: boolean): vscode.DocumentSelector {
        const ret = [];

        for (const langid of for_intellisense ? config.valid_cobol_language_ids_for_intellisense : config.valid_cobol_language_ids) {
            for (const kscheme of VSExtensionUtils.knownSchemes) {
                ret.push(
                    { scheme: kscheme, language: langid },
                )
            }
        }

        return ret;
    }

    public static getAllCobolSelector(langid: string): vscode.DocumentSelector {
        const ret = [];

        for (const kscheme of VSExtensionUtils.knownSchemes) {
            ret.push(
                { scheme: kscheme, language: langid },
            )
        }

        return ret;
    }

    public static isKnownCOBOLLanguageId(config: ICOBOLSettings, possibleLangid: string): boolean {
        for (const langid of config.valid_cobol_language_ids) {
            if (possibleLangid === langid) {
                return true;
            }
        }

        return false;
    }

    public static flip_plaintext(doc: vscode.TextDocument): void {
        if (doc === undefined) {
            return;
        }

        const maxLines = Math.min(doc.lineCount, 200);

        // Upgrade plain text documents to COBOL when strong ANSI/COBOL markers are present.
        if (doc.languageId === "plaintext") {
            for (let lcount = 0; lcount < maxLines; lcount++) {
                const line = doc.lineAt(lcount).text;

                if (/^\s*\?ANSI\b/i.test(line)) {
                    vscode.languages.setTextDocumentLanguage(doc, "COBOL");
                    return;
                }

                if (/^\s*(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION\./i.test(line)) {
                    vscode.languages.setTextDocumentLanguage(doc, "COBOL");
                    return;
                }

                if (/^\s*\d{6}\s+(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION\./i.test(line)) {
                    vscode.languages.setTextDocumentLanguage(doc, "COBOL");
                    return;
                }
            }
        }

        // Check if document is COBOL_TANDEM with ?ANSI directive - should be COBOL instead
        if (doc.languageId === "COBOL_TANDEM") {
            for (let lcount = 0; lcount < maxLines; lcount++) {
                const qline = doc.lineAt(lcount).text;
                // ?ANSI indicates ANSI standard COBOL format, not Tandem-specific
                if (qline.match(/^\s*\?ANSI\b/)) {
                    vscode.languages.setTextDocumentLanguage(doc, "COBOL");
                    return;
                }
            }
        }
    }


}
