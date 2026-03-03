import * as vscode from "vscode";
import { VSLogger } from "../../utils/logger";
import { VSWorkspaceFolders } from "../../features/workspace/workspaceFolders";
import { ICOBOLSettings } from "../../config/IConfiguration";
import path from "path";

/**
 * Diagnostic commands that help inspect symbol-provider output.
 *
 * Current capability:
 * - Dump document symbols into a generated markdown document for easy review.
 */
export class VSDiagCommands {

    /**
     * Recursively writes one symbol and all of its children to a markdown buffer.
     *
     * Includes symbol kind, name/detail metadata, ranges, and extracted source text.
     */
    private static dumpSymbol(sb: Array<string>, depth: string, symbol: vscode.DocumentSymbol) {
        const symbolKind = vscode.SymbolKind[symbol.kind];
        const detail = symbol.detail !== undefined && symbol.detail.length !== 0 ? ` "{symbol.detail}` : "";
        const tag = symbol.tags !== undefined ? ` (Tag=${symbol.tags.join(',')})` : "";

        const rrinfo = symbol.range.isSingleLine ?
            `${symbol.range.start.line}:${symbol.range.start.character}-${symbol.range.end.character}` :
            `${symbol.range.start.line}:${symbol.range.start.character} -> ${symbol.range.end.line}:${symbol.range.end.character}`

        const srinfo = symbol.selectionRange.isSingleLine ?
            `${symbol.selectionRange.start.line}:${symbol.selectionRange.start.character}-${symbol.selectionRange.end.character}` :
            `${symbol.selectionRange.start.line}:${symbol.selectionRange.start.character} -> ${symbol.selectionRange.end.line}:${symbol.selectionRange.end.character}`

        // Show both range and selection range when they differ to aid provider debugging.
        const rinfo = symbol.range.isSingleLine === false ? (rrinfo.localeCompare(srinfo) === 0 ? rrinfo : `${rrinfo} / ${srinfo}`) : rrinfo;

        sb.push(`${depth}  ${symbolKind} : "${symbol.name}" ${detail}${tag} @ ${rinfo}`);
        const activeEditor = vscode.window.activeTextEditor;
        const text = activeEditor?.document.getText(symbol.range);
        sb.push(`\`\`\`${activeEditor?.document.languageId}\n${text}\n\`\`\`\n\n`)
        for (const childSymbol of symbol.children) {
            VSDiagCommands.dumpSymbol(sb, depth + "#", childSymbol);
        }
    }

    /**
     * Executes the document-symbol provider for the active editor and emits
     * a markdown report in an untitled `dump.md` document.
     */
    public static async DumpAllSymbols(config: ICOBOLSettings) {
        if (vscode.window.activeTextEditor) {
            const sb = new Array<string>();
            const activeUrl = vscode.window.activeTextEditor.document.uri;
            // Ask VS Code for symbols exactly as contributed by active providers.
            const symbolsArray = await vscode.commands.executeCommand<[]>("vscode.executeDocumentSymbolProvider", activeUrl, "*");

            sb.push("Document Symbols found:")
            const symbols = symbolsArray as vscode.DocumentSymbol[];
            for (const symbol of symbols) {
                try {
                    VSDiagCommands.dumpSymbol(sb, "#", symbol);
                } catch (e) {
                    // Preserve diagnostics generation even when one symbol fails.
                    VSLogger.logException(`Symbol: ${symbol.name}`, e as Error);
                }
            }
            let fpath = "";
            let data="dump";
            // Default output location is workspace root; fallback to process cwd.
            const ws = VSWorkspaceFolders.get(config);
            if (ws) {
                fpath = path.join(ws[0].uri.fsPath, data + ".md");
            } else {
                fpath = path.join(process.cwd(), data + ".md");
            }
            const furl = vscode.Uri.file(fpath).with({ scheme: "untitled" });
            await vscode.workspace.openTextDocument(furl).then(async document => {
                const editor = await vscode.window.showTextDocument(document);
                if (editor !== undefined) {
                    // Use markdown so symbol hierarchy and code fences render cleanly.
                    await vscode.languages.setTextDocumentLanguage(document, "markdown");
                    editor.edit(edit => {
                        edit.insert(new vscode.Position(0, 0), sb.join("\n"))

                    });
                }
            });
            VSLogger.logMessage(sb.join("\n"));
        }
    }
}