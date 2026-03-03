import * as vscode from "vscode";
import { VSLogger } from "../../utils/logger";
import { VSWorkspaceFolders } from "../../features/workspace/workspaceFolders";
import { ICOBOLSettings } from "../../config/IConfiguration";
import path from "path";
import { VSCOBOLSourceScanner } from "../../features/workspace/workspaceSymbolScanner";
import { MakeDep } from "./makeDependencies";

/**
 * VS Code command bridge for dependency-file generation.
 *
 * This class extends the pure generator (`MakeDep`) and adds editor/workspace
 * integration: collecting symbols from scanner cache, creating an untitled
 * makefile buffer, and logging the generated output.
 */
export class VSMakeDep extends MakeDep {
    /**
     * Creates a Makefile dependency document for the active COBOL source.
     *
     * Workflow:
     * 1. Resolve the active editor's cached scanner object.
     * 2. Gather resolved and unresolved copybook references.
     * 3. Generate dependency text lines via `CreateDependencyFile`.
     * 4. Open an untitled `.d` file in makefile language mode.
     * 5. Insert generated content and log the final text to the COBOL channel.
     */
    public static async MakeDependency(config: ICOBOLSettings) {
        // Command is a no-op when no active editor exists.
        if (vscode.window.activeTextEditor) {
            // Reuse scanner cache to avoid reparsing on-demand for this command.
            const current = VSCOBOLSourceScanner.getCachedObject(vscode.window.activeTextEditor.document, config);
            if (current === undefined) {
                return;
            }

            const sn = current.sourceHandler.getShortWorkspaceFilename();
            const sn_with_no_ext = path.basename(sn, path.extname(sn));

            // Flatten resolved copybook map keys into a simple string array.
            const copyBookNames = new Array<string>();
            for (const [key,] of current.copyBooksUsed) {
                copyBookNames.push(key);
            }

            // Collect unresolved copybooks for diagnostic comments in output.
            const processUnUsedCopyBooks = new Array<string>();
            for (const [key,] of current.copyBooksUnresolved) {
                processUnUsedCopyBooks.push(key);
            }

            // Generate dependency lines (empty when feature is disabled by config).
            const sb = VSMakeDep.CreateDependencyFile(config, sn, copyBookNames, processUnUsedCopyBooks, vscode.window.activeTextEditor.document.fileName);
            if (sb.length === 0) {
                return;
            }
            
            // Pick workspace root when available, otherwise fallback to process cwd.
            let fpath = "";
            const ws = VSWorkspaceFolders.get(config);
            if (ws) {
                fpath = path.join(ws[0].uri.fsPath, config.makefile_dependency_prefix +sn_with_no_ext + ".d");
            } else {
                fpath = path.join(process.cwd(), config.makefile_dependency_prefix + sn_with_no_ext + ".d");
            }

            // Open as untitled so users can inspect/save to desired location manually.
            const furl = vscode.Uri.file(fpath).with({ scheme: "untitled" });
            await vscode.workspace.openTextDocument(furl).then(async document => {
                const editor = await vscode.window.showTextDocument(document);
                if (editor !== undefined) {
                    // Mark as makefile for syntax highlighting and formatting support.
                    await vscode.languages.setTextDocumentLanguage(document, "makefile");
                    editor.edit(edit => {
                        edit.insert(new vscode.Position(0, 0), sb.join("\n"))
                    });
                }
            });

            // Mirror generated content to output channel for quick troubleshooting.
            VSLogger.logMessage(sb.join("\n"));
        }
    }
}
