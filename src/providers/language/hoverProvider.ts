import { ProviderResult } from "vscode";
import vscode from "vscode";
import { ICOBOLSettings, hoverApi } from "../../config/IConfiguration";
import { CallTarget, KnownAPIs } from "../../keywords/cobolCallTargets";
import { VSCOBOLUtils } from "../../utils/cobolUtils";
import { COBOLToken, COBOLVariable, SQLDeclare, COBOLCopybookToken } from "../../features/workspace/cobolsourcescanner";
import { VSCOBOLSourceScanner } from "../../features/workspace/workspaceSymbolScanner";
import { cobolSourceScannerInterfaces } from "../../features/workspace/ICobolSourceScannerInterfaces";
import * as path from "path";
import * as fs from "fs";
// import { ESourceFormat } from "../features/runtime/IExternalFeatures";

const nhexRegEx = new RegExp("[nN][xX][\"'][0-9A-Fa-f]*[\"']");
const hexRegEx = new RegExp("[xX][\"'][0-9A-Fa-f]*[\"']");
const wordRegEx = new RegExp("[#0-9a-zA-Z][a-zA-Z0-9-_$]*");
const variableRegEx = new RegExp("[$#0-9a-zA-Z_][a-zA-Z0-9-_]*");

/**
 * Provides context-aware hover content for COBOL symbols, APIs, literals, and COPY artifacts.
 */
export class VSHoverProvider {

    /**
     * Builds hover text for COPY ... IN/OF library names by probing nearby files.
     */
    private static getLibraryHover(document: vscode.TextDocument, libraryName: string, settings: ICOBOLSettings): vscode.Hover | undefined {
        const sourceUri = vscode.Uri.parse(document.uri.toString());
        const sourceDir = path.dirname(sourceUri.fsPath);
        
        // Try to find the library file
        const extensions = settings.copybookexts || [".cpy", ".cbl", ".cob"];
        
        // Try with configured extensions
        for (const ext of extensions) {
            const filePath = path.join(sourceDir, libraryName + ext);
            if (fs.existsSync(filePath)) {
                return new vscode.Hover(`**Library File**: ${libraryName}${ext}\n\n**Path**: ${filePath}\n\n*Click to open library definition*`);
            }
        }
        
        // Try without extension
        const filePathNoExt = path.join(sourceDir, libraryName);
        if (fs.existsSync(filePathNoExt)) {
            return new vscode.Hover(`**Library File**: ${libraryName}\n\n**Path**: ${filePathNoExt}\n\n*Click to open library definition*`);
        }
        
        // Try lowercase variations
        for (const ext of extensions) {
            const filePath = path.join(sourceDir, libraryName.toLowerCase() + ext);
            if (fs.existsSync(filePath)) {
                return new vscode.Hover(`**Library File**: ${libraryName.toLowerCase()}${ext}\n\n**Path**: ${filePath}\n\n*Click to open library definition*`);
            }
        }
        
        const filePathNoExtLower = path.join(sourceDir, libraryName.toLowerCase());
        if (fs.existsSync(filePathNoExtLower)) {
            return new vscode.Hover(`**Library File**: ${libraryName.toLowerCase()}\n\n**Path**: ${filePathNoExtLower}\n\n*Click to open library definition*`);
        }
        
        // File not found
        return new vscode.Hover(`**Library**: ${libraryName}\n\n⚠️ Library file not found in source directory`);
    }

    /**
     * Builds hover text for a resolved copybook reference, including path and statement context.
     */
    private static getCopybookHover(document: vscode.TextDocument, copybookName: string, copybookToken: COBOLCopybookToken, settings: ICOBOLSettings): vscode.Hover | undefined {
        let hoverText = `**Copybook**: ${copybookName}\n\n`;
        
        const token = copybookToken.token;
        if (!token) {
            return new vscode.Hover(`**Copybook**: ${copybookName}\n\n⚠️ Token not available`);
        }
        
        // Get the COPY statement line
        const line = token.sourceHandler.getLine(token.startLine, false);
        if (line) {
            hoverText += `**Statement**:\n\`\`\`cobol\n${line.trim()}\n\`\`\`\n\n`;
        }
        
        // Check if copybook file exists in the workspace
        const copybookPath = token.extraInformation1;
        if (copybookPath && copybookPath.length > 0) {
            hoverText += `**File**: ${copybookPath}\n\n`;
            
            try {
                if (fs.existsSync(copybookPath)) {
                    const stats = fs.statSync(copybookPath);
                    hoverText += `**Size**: ${stats.size} bytes\n\n`;
                    hoverText += `*Click to navigate to copybook*`;
                } else {
                    hoverText += `⚠️ File not found`;
                }
            } catch {
                hoverText += `⚠️ Unable to access file`;
            }
        } else {
            hoverText += `⚠️ Copybook file not resolved\n\n`;
            hoverText += `Check \`coboleditor.copybookdirs\` setting`;
        }
        
        return new vscode.Hover(hoverText);
    }

    /**
     * Wraps comment and code snippets into markdown blocks for hover rendering.
     */
    private static wrapCommentAndCode(comment: string, code: string): string {
        let cleanCode = code;
        if (code.endsWith("\n")) {
            cleanCode = code.slice(0, -1)
        }

        if (comment.trim().length === 0) {
            // return just the code
            return `\`\`\`COBOL
${cleanCode}
\`\`\`
`;
        }

        // return both the comment and the code
        return `\`\`\`text
${comment}
\`\`\`

\`\`\`COBOL
${cleanCode}
\`\`\`

`;
    }

    /**
     * Main hover resolver for COBOL documents.
     */
    public static provideHover(settings: ICOBOLSettings, document: vscode.TextDocument, position: vscode.Position): ProviderResult<vscode.Hover> {
        // Check if hovering over a COPY statement with IN/OF clause
        const line = document.lineAt(position.line).text;
        if (/.*copy\s+.*\s+(in|of)\s+.*/i.test(line)) {
            const wordRange = document.getWordRangeAtPosition(position, variableRegEx);
            const word = document.getText(wordRange);
            
            if (word && word.length > 0) {
                const sf: cobolSourceScannerInterfaces | undefined = VSCOBOLSourceScanner.getCachedObject(document, settings);
                if (sf !== undefined) {
                    // Check if hovering over a library name
                    const libraryTokens = sf.copyBookLibraries.get(word.toLowerCase());
                    if (libraryTokens && libraryTokens.length > 0) {
                        return VSHoverProvider.getLibraryHover(document, word, settings);
                    }
                    
                    // Check if hovering over a copybook name
                    const copybookTokens = sf.copyBooksUsed.get(word.toLowerCase());
                    if (copybookTokens && copybookTokens.length > 0) {
                        return VSHoverProvider.getCopybookHover(document, word, copybookTokens[0], settings);
                    }
                }
            }
        }

        if (settings.hover_show_known_api !== hoverApi.Off) {
            // Known API hover can optionally include extended examples depending on mode.
            const txt = document.getText(document.getWordRangeAtPosition(position, wordRegEx));
            const txtTarget: CallTarget | undefined = KnownAPIs.getCallTarget(document.languageId, txt);
            if (txtTarget !== undefined) {
                let example = txtTarget.example.length > 0 ? `\n\n---\n\n~~~\n${txtTarget.example.join("\r\n")}\n~~~\n` : "";
                if (settings.hover_show_known_api === hoverApi.Short) {
                    example = "";
                }
                return new vscode.Hover(`**${txtTarget.api}** - ${txtTarget.description}\n\n[\u2192 ${txtTarget.apiGroup}](${txtTarget.url})${example}`);
            }
        }

        if (settings.hover_show_encoded_literals) {
            const nxtxt = document.getText(document.getWordRangeAtPosition(position, nhexRegEx));
            if (nxtxt.toLowerCase().startsWith("nx\"") || nxtxt.toLowerCase().startsWith("nx'")) {
                const ascii = VSCOBOLUtils.nxhex2a(nxtxt);
                if (ascii.length !== 0) {
                    return new vscode.Hover(`UTF16=${ascii}`);
                }
                return undefined;
            }
            const txt = document.getText(document.getWordRangeAtPosition(position, hexRegEx));
            if (txt.toLowerCase().startsWith("x\"") || txt.toLowerCase().startsWith("x'")) {
                const ascii = VSCOBOLUtils.hex2a(txt);
                if (ascii.length !== 0) {
                    return new vscode.Hover(`ASCII=${ascii}`);
                }
            }
        }

        const wordRange = document.getWordRangeAtPosition(position, variableRegEx);
        const word = document.getText(wordRange);
        if (word === undefined || word.length === 0) {
            return undefined;
        }
        const sf: cobolSourceScannerInterfaces | undefined = VSCOBOLSourceScanner.getCachedObject(document, settings);
        if (sf === undefined) {
            return undefined;
        }


        let inProcedureDivision = false;
        if (sf.sourceReferences.state.procedureDivision?.startLine !== undefined && position.line > sf.sourceReferences.state.procedureDivision?.startLine) {
            inProcedureDivision = true;
        }

        if (settings.hover_show_variable_definition) {
            // only show when in the procedure division
            if (sf.sourceReferences.state.procedureDivision?.startLine !== undefined && position.line < sf.sourceReferences.state.procedureDivision?.startLine) {
                return undefined;
            }

            const tokenLower: string = word.toLowerCase();
            const variables: COBOLVariable[] | undefined = sf.constantsOrVariables.get(tokenLower);
            if (variables !== undefined && variables.length !== 0) {
                let hoverMessage = "";
                for (const variable of variables) {
                    if (variable.token !== undefined) {
                        const token = variable.token;
                        let line = token.sourceHandler.getLine(token.startLine, false);
                        if (line === undefined) {
                            continue;
                        }

                        let newHoverMessage = line.trimEnd();
                        if (variables.length === 1) {
                            newHoverMessage = newHoverMessage.trimStart();
                        }
                        let commentLine = token.sourceHandler.getCommentAtLine(token.startLine);
                        if (hoverMessage.length !== 0) {
                            hoverMessage += "\n\n----\n\n";
                        }
                        hoverMessage += VSHoverProvider.wrapCommentAndCode(commentLine, newHoverMessage);
                    }
                }

                if (hoverMessage.length !== 0) {
                    let md = new vscode.MarkdownString(hoverMessage);
                    return new vscode.Hover(md);
                }
            }
        }

        const showSection = true;
        if (showSection && inProcedureDivision) {
            const tokenLower: string = word.toLowerCase();
            const token: COBOLToken | undefined = sf.sections.get(tokenLower);
            if (token !== undefined && token.startLine !== position.line) {
                let line = token.sourceHandler.getLine(token.startLine, false);
                if (line !== undefined) {
                    let newHoverMessage = line.trimEnd();
                    let sectionsCommentLine = token.sourceHandler.getCommentAtLine(token.startLine);
                    let sectionsHoverMessage = VSHoverProvider.wrapCommentAndCode(sectionsCommentLine, newHoverMessage);

                    if (sectionsHoverMessage.length !== 0) {
                        let md = new vscode.MarkdownString(sectionsHoverMessage);
                        return new vscode.Hover(md);
                    }
                }
            }
        }

        if (showSection && inProcedureDivision) {
            const tokenLower: string = word.toLowerCase();
            const token: COBOLToken | undefined = sf.paragraphs.get(tokenLower);
            if (token !== undefined && token.startLine !== position.line) {
                let line = token.sourceHandler.getLine(token.startLine, false);
                if (line !== undefined) {
                    let newHoverMessage = line.trimEnd();
                    let paragraphCommentLine = token.sourceHandler.getCommentAtLine(token.startLine);
                    let paragraphHoverMessage = VSHoverProvider.wrapCommentAndCode(paragraphCommentLine, newHoverMessage);

                    if (paragraphHoverMessage.length !== 0) {
                        let md = new vscode.MarkdownString(paragraphHoverMessage);
                        return new vscode.Hover(md);
                    }
                }
            }
        }

        if (settings.enable_exec_sql_cursors) {
            const tokenLower: string = word.toLowerCase();
            const sqlToken: SQLDeclare | undefined = sf.execSQLDeclare.get(tokenLower);
            if (sqlToken !== undefined) {
                const token = sqlToken.token;
                if (token !== undefined && token.startLine !== position.line) {
                    let sc = token.rangeStartLine === token.rangeEndLine ? token.rangeStartColumn : 0;
                    let lines = token.sourceHandler.getText(token.rangeStartLine, sc, token.rangeEndLine, token.rangeEndColumn);
                    if (lines !== undefined) {
                        let newHoverMessage = lines.trimEnd();
                        let sqldeclareComment = token.sourceHandler.getCommentAtLine(token.startLine);
                        let sqldeclareHoverMarkdown = VSHoverProvider.wrapCommentAndCode(sqldeclareComment, newHoverMessage);

                        if (sqldeclareHoverMarkdown.length !== 0) {
                            let md = new vscode.MarkdownString(sqldeclareHoverMarkdown);
                            return new vscode.Hover(md);
                        }
                    }
                }
            }
        }
        return undefined;
    }


}
