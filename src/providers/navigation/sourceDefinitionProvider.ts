import * as vscode from "vscode";
import * as path from "path";
import * as fs from "fs";
import { COBOLTokenStyle, COBOLToken, COBOLVariable } from "../../features/workspace/cobolsourcescanner";
import { VSCOBOLSourceScanner } from "../../features/workspace/workspaceSymbolScanner";
import { VSCOBOLConfiguration } from "../../config/workspaceConfiguration";
import { ICOBOLSettings } from "../../config/IConfiguration";
import { VSLogger } from "../../utils/logger";
import { getCOBOLKeywordDictionary } from "../../keywords/cobolKeywords";
import { VSCOBOLSourceScannerTools } from "../../features/workspace/sourceScannerUtils";
import { VSExternalFeatures } from "../../features/runtime/externalFeatures";
import { cobolSourceScannerInterfaces } from "../../features/workspace/ICobolSourceScannerInterfaces";

/**
 * Resolves go-to-definition for COBOL targets, variables, classes/methods, cursors, and libraries.
 */
export class COBOLSourceDefinition implements vscode.DefinitionProvider {

    readonly sectionRegEx = new RegExp("[$0-9a-zA-Z][a-zA-Z0-9-_]*");
    readonly variableRegEx = new RegExp("[$#0-9a-zA-Z_][a-zA-Z0-9-_]*");
    readonly classRegEx = new RegExp("[$0-9a-zA-Z][a-zA-Z0-9-_]*");
    readonly methodRegEx = new RegExp("[$0-9a-zA-Z][a-zA-Z0-9-_]*");

    /**
     * Main definition resolver entry point.
     */
    public provideDefinition(document: vscode.TextDocument,
        position: vscode.Position,
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        token: vscode.CancellationToken): vscode.ProviderResult<vscode.Definition> {
        const locations: vscode.Location[] = [];

        let loc;
        const config = VSCOBOLConfiguration.get_resource_settings(document, VSExternalFeatures);

        const theline = document.lineAt(position.line).text;
        
        // Check for copybook library names first (in COPY statements with IN/OF).
        if (theline.match(/.*copy\s+.*\s+(in|of)\s+.*/i)) {
            const qcp: cobolSourceScannerInterfaces | undefined = VSCOBOLSourceScanner.getCachedObject(document,config);
            if (qcp !== undefined) {
                const libLoc = this.getCopyBookLibrary(document, qcp, position);
                if (libLoc !== undefined) {
                    locations.push(libLoc);
                    return locations;
                }
            }
        }
        
        if (theline.match(/.*(perform|thru|go\s*to|until|varying).*$/i)) {
            const qcp: cobolSourceScannerInterfaces | undefined = VSCOBOLSourceScanner.getCachedObject(document,config);
            if (qcp === undefined) {
                return locations;
            }

            loc = this.getSectionOrParaLocation(document, qcp, position);
            if (loc) {
                locations.push(loc);
                return locations;
            }
        }

        if (theline.match(/.*(new\s*|type).*$/i)) {
            const qcp: cobolSourceScannerInterfaces | undefined = VSCOBOLSourceScanner.getCachedObject(document,config);
            if (qcp === undefined) {
                return locations;
            }

            loc = this.getClassTarget(document, qcp, position);
            if (loc !== undefined) {
                locations.push(loc);
                return locations;
            }
        }

        if (theline.match(/.*(invoke\s*|::)(.*$)/i)) {
            const qcp: cobolSourceScannerInterfaces | undefined = VSCOBOLSourceScanner.getCachedObject(document, config);
            if (qcp === undefined) {
                return locations;
            }
            loc = this.getMethodTarget(document, qcp, position);
            if (loc !== undefined) {
                locations.push(loc);
                return locations;
            }
        }

        /* is it a known variable? */
        if (this.getVariableInCurrentDocument(locations, document, position, config)) {
            return locations;
        }

        /* is it a sql cursor? */
        const qcp: cobolSourceScannerInterfaces | undefined = VSCOBOLSourceScanner.getCachedObject(document,config);
        if (qcp !== undefined) {
            if (VSCOBOLSourceScannerTools.isPositionInEXEC(qcp, position)) {
                const loc = this.getSQLCursor(document, qcp, position);
                if (loc !== undefined) {
                    locations.push(loc);
                }
                return locations;
            }
        }

        return locations;
    }

    /**
     * Attempts to resolve COPY library identifiers to files near the active source.
     */
    private getCopyBookLibrary(document: vscode.TextDocument, sf: cobolSourceScannerInterfaces, position: vscode.Position): vscode.Location | undefined {
        const wordRange = document.getWordRangeAtPosition(position, this.sectionRegEx);
        const word = wordRange ? document.getText(wordRange) : "";
        if (word === "") {
            return undefined;
        }

        const config = VSCOBOLConfiguration.get_resource_settings(document, VSExternalFeatures);
        
        try {
            // Resolve against source directory first to match common COPY ... IN/OF workflows.
            const sourceUri = vscode.Uri.parse(sf.sourceHandler.getUriAsString());
            const sourcePath = sourceUri.fsPath;
            const sourceDir = path.dirname(sourcePath);
            
            VSLogger.logMessage(`Looking for library file: ${word} in directory: ${sourceDir}`, config);
            
            // Try to find the library file in the same directory as the source
            let fileName = "";
            
            // Try with configured extensions
            for (const ext of config.copybookexts) {
                const possiblePath = path.join(sourceDir, word + "." + ext);
                VSLogger.logMessage(`  Checking: ${possiblePath}`, config);
                if (fs.existsSync(possiblePath)) {
                    fileName = possiblePath;
                    VSLogger.logMessage(`  Found: ${fileName}`, config);
                    break;
                }
            }
            
            // Try without extension
            if (fileName.length === 0) {
                const possiblePath = path.join(sourceDir, word);
                VSLogger.logMessage(`  Checking (no ext): ${possiblePath}`, config);
                if (fs.existsSync(possiblePath)) {
                    fileName = possiblePath;
                    VSLogger.logMessage(`  Found: ${fileName}`, config);
                }
            }
            
            // Try lowercase
            if (fileName.length === 0) {
                for (const ext of config.copybookexts) {
                    const possiblePath = path.join(sourceDir, word.toLowerCase() + "." + ext);
                    VSLogger.logMessage(`  Checking (lowercase): ${possiblePath}`, config);
                    if (fs.existsSync(possiblePath)) {
                        fileName = possiblePath;
                        VSLogger.logMessage(`  Found: ${fileName}`, config);
                        break;
                    }
                }
            }
            
            // Try lowercase without extension
            if (fileName.length === 0) {
                const possiblePath = path.join(sourceDir, word.toLowerCase());
                VSLogger.logMessage(`  Checking (lowercase, no ext): ${possiblePath}`, config);
                if (fs.existsSync(possiblePath)) {
                    fileName = possiblePath;
                    VSLogger.logMessage(`  Found: ${fileName}`, config);
                }
            }
            
            if (fileName.length > 0) {
                // Open the library file
                const uri = vscode.Uri.file(fileName);
                return new vscode.Location(uri, new vscode.Position(0, 0));
            } else {
                VSLogger.logMessage(`  Library file not found for: ${word}`, config);
            }
        }
        catch (e) {
            VSLogger.logMessage(`Error in getCopyBookLibrary: ${(e as Error).message}`, config);
        }

        return undefined;
    }

    /**
     * Resolves section or paragraph definition at cursor.
     */
    private getSectionOrParaLocation(document: vscode.TextDocument, sf: cobolSourceScannerInterfaces, position: vscode.Position): vscode.Location | undefined {
        const wordRange = document.getWordRangeAtPosition(position, this.sectionRegEx);
        const word = wordRange ? document.getText(wordRange) : "";
        if (word === "") {
            return undefined;
        }

        const wordLower = word.toLowerCase();

        try {

            if (sf.sections.has(wordLower)) {
                const token = sf.sections.get(wordLower);
                if (token !== undefined) {
                    const spos = new vscode.Position(token.rangeStartLine, token.rangeStartColumn);
                    const rpos = new vscode.Position(token.rangeEndLine, token.rangeEndColumn);
                    const trange = new vscode.Range(spos, rpos);
                    const uri = vscode.Uri.parse(token.filenameAsURI);
                    return new vscode.Location(uri, trange);
                }
            }

        }
        catch (e) {
            VSLogger.logMessage((e as Error).message);
        }

        try {
            if (sf.paragraphs.has(wordLower)) {
                const token = sf.paragraphs.get(wordLower);
                if (token !== undefined) {
                    const spos = new vscode.Position(token.rangeStartLine, token.rangeStartColumn);
                    const rpos = new vscode.Position(token.rangeEndLine, token.rangeEndColumn);
                    const trange = new vscode.Range(spos, rpos);
                    const uri = vscode.Uri.parse(token.filenameAsURI);
                    return new vscode.Location(uri, trange);
                }
            }
        }
        catch (e) {
            VSLogger.logMessage((e as Error).message);
        }
        return undefined;
    }



    /**
     * Resolves EXEC SQL cursor declaration location at cursor.
     */
    private getSQLCursor(document: vscode.TextDocument, sf: cobolSourceScannerInterfaces, position: vscode.Position): vscode.Location | undefined {
        const wordRange = document.getWordRangeAtPosition(position, this.sectionRegEx);
        const word = wordRange ? document.getText(wordRange) : "";
        if (word === "") {
            return undefined;
        }

        try {
            const wordLower = word.toLowerCase();
            if (sf.execSQLDeclare.has(wordLower)) {
                const sd = sf.execSQLDeclare.get(wordLower);
                if (sd !== undefined) {
                    const token = sd.token;
                    const spos = new vscode.Position(token.rangeStartLine, token.rangeStartColumn);
                    const rpos = new vscode.Position(token.rangeEndLine, token.rangeEndColumn);
                    const trange = new vscode.Range(spos, rpos);
                    const uri = vscode.Uri.parse(token.filenameAsURI);
                    return new vscode.Location(uri, trange);
                }
            }

        }
        catch (e) {
            VSLogger.logMessage((e as Error).message);
        }

        return undefined;
    }

    /**
     * Adds in-document variable definitions to `locations` when symbol matches.
     */
    private getVariableInCurrentDocument(locations: vscode.Location[], document: vscode.TextDocument, position: vscode.Position, settings: ICOBOLSettings): boolean {
        const wordRange = document.getWordRangeAtPosition(position, this.variableRegEx);
        const word = wordRange ? document.getText(wordRange) : "";
        if (word === "") {
            return false;
        }

        const tokenLower: string = word.toLowerCase();
        if (getCOBOLKeywordDictionary(document.languageId).has(tokenLower)) {
            return false;
        }

        const sf: cobolSourceScannerInterfaces | undefined = VSCOBOLSourceScanner.getCachedObject(document,settings);
        if (sf === undefined) {
            return false;
        }

        const variables: COBOLVariable[] | undefined = sf.constantsOrVariables.get(tokenLower);
        if (variables === undefined || variables.length === 0) {
            return false;
        }

        for (let i = 0; i < variables.length; i++) {
            const token: COBOLToken = variables[i].token;
            if (token.tokenNameLower === "filler") {
                continue;
            }

            const spos = new vscode.Position(token.rangeStartLine, token.rangeStartColumn);
            const rpos = new vscode.Position(token.rangeEndLine, token.rangeEndColumn);
            const trange = new vscode.Range(spos, rpos);
            const uri = vscode.Uri.parse(token.filenameAsURI);
            switch (token.tokenType) {
                case COBOLTokenStyle.Union:
                    {
                        locations.push(new vscode.Location(uri, trange));
                        break;
                    }
                case COBOLTokenStyle.Constant:
                    {
                        locations.push(new vscode.Location(uri, trange));
                        break;
                    }
                case COBOLTokenStyle.ConditionName:
                    {
                        locations.push(new vscode.Location(uri, trange));
                        break;
                    }
                case COBOLTokenStyle.Variable:
                    {
                        locations.push(new vscode.Location(uri, trange));
                        break;
                    }
            }
        }

        if (locations.length === 0) {
            return false;
        }
        return true;
    }

    /**
     * Generic token-map target resolver used by class/method lookup.
     */
    private getGenericTarget(queryRegEx: RegExp, tokenMap: Map<string, COBOLToken>, document: vscode.TextDocument, position: vscode.Position): vscode.Location | undefined {
        const wordRange = document.getWordRangeAtPosition(position, queryRegEx);
        const word = wordRange ? document.getText(wordRange) : "";
        if (word === "") {
            return undefined;
        }

        const workLower = word.toLowerCase();
        if (tokenMap.has(workLower)) {
            const token: COBOLToken | undefined = tokenMap.get(workLower);
            if (token !== undefined) {
                const spos = new vscode.Position(token.rangeStartLine, token.rangeStartColumn);
                const rpos = new vscode.Position(token.rangeEndLine, token.rangeEndColumn);
                const trange = new vscode.Range(spos, rpos);
                const uri = vscode.Uri.parse(token.filenameAsURI);
                return new vscode.Location(uri, trange);
            }
        }
        return undefined;
    }

    private getClassTarget(document: vscode.TextDocument, sf: cobolSourceScannerInterfaces, position: vscode.Position): vscode.Location | undefined {
        return this.getGenericTarget(this.classRegEx, sf.classes, document, position);
    }

    private getMethodTarget(document: vscode.TextDocument, sf: cobolSourceScannerInterfaces, position: vscode.Position): vscode.Location | undefined {
        return this.getGenericTarget(this.methodRegEx, sf.methods, document, position);
    }

}

