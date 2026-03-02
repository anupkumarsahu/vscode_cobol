import * as vscode from "vscode";
import { COBOLTokenStyle } from "../../features/workspace/cobolsourcescanner";
import { VSCOBOLConfiguration } from "../../config/workspaceConfiguration";
import { VSLogger } from "../../utils/logger";
import { outlineFlag } from "../../config/IConfiguration";
import { VSCOBOLSourceScanner } from "../../features/workspace/workspaceSymbolScanner";
import { VSExternalFeatures } from "../../features/runtime/externalFeatures";

export class COBOLSymbolInformationProvider implements vscode.DocumentSymbolProvider {

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public async provideDocumentSymbols(document: vscode.TextDocument, canceltoken: vscode.CancellationToken): Promise<vscode.SymbolInformation[]> {
        const symbols: vscode.SymbolInformation[] = [];
        const config = VSCOBOLConfiguration.get_resource_settings(document, VSExternalFeatures);

        const outlineLevel = config.outline;

        if (outlineLevel === outlineFlag.Off) {
            return symbols;
        }

        const sf = VSCOBOLSourceScanner.getCachedObject(document,config);

        if (sf === undefined) {
            return symbols;
        }

        const ownerUri = document.uri;

        let includePara = true;
        let includeVars = true;
        let includeSections = true;

        if (outlineLevel === outlineFlag.Partial) {
            includePara = false;
        }

        if (outlineLevel === outlineFlag.Skeleton) {
            includeVars = false;
            includeSections = false;
            includePara = false;
        }

        for (const token of sf.tokensInOrder) {
            try {
                if (token.ignoreInOutlineView === false) {
                    const srange = new vscode.Range(new vscode.Position(token.rangeStartLine, token.rangeStartColumn),
                        new vscode.Position(token.rangeEndLine, token.rangeEndColumn));

                    const lrange = new vscode.Location(ownerUri, srange);

                    const container = token.parentToken !== undefined ? token.parentToken.description : "";
                    switch (token.tokenType) {
                        case COBOLTokenStyle.ClassId:
                        case COBOLTokenStyle.ProgramId:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Class, container, lrange));
                            break;
                        case COBOLTokenStyle.CopyBook:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.File, container, lrange));
                            break;
                        case COBOLTokenStyle.CopyBookInOrOf:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.File, container, lrange));
                            break;
                        case COBOLTokenStyle.Declaratives:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Method, container, lrange));
                            break;
                        case COBOLTokenStyle.Division:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Method, container, lrange));
                            break;
                        case COBOLTokenStyle.Paragraph:
                            if (includePara === false) {
                                break;
                            }
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Method, container, lrange));
                            break;
                        case COBOLTokenStyle.Section:
                            if (includeSections === false) {
                                break;
                            }
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Method, container, lrange));
                            break;
                        case COBOLTokenStyle.Exec:
                        case COBOLTokenStyle.EntryPoint:
                        case COBOLTokenStyle.FunctionId:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Function, container, lrange));
                            break;
                        case COBOLTokenStyle.EnumId:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Enum, container, lrange));
                            break;
                        case COBOLTokenStyle.InterfaceId:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Interface, container, lrange));
                            break;
                        case COBOLTokenStyle.ValueTypeId:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Struct, container, lrange));
                            break;
                        case COBOLTokenStyle.IgnoreLS:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Null, container, lrange));
                            break;
                        case COBOLTokenStyle.Variable:
                            if (includeVars === false) {
                                break;
                            }

                            // drop fillers
                            if (token.tokenNameLower === "filler") {
                                continue;
                            }

                            if (token.extraInformation1 === "fd" || token.extraInformation1 === "sd"
                                || token.extraInformation1 === "rd" || token.extraInformation1 === "select") {
                                symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.File, container, lrange));
                            }
                            else {
                                if (token.extraInformation1.indexOf("-GROUP") !== -1) {
                                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Struct, container, lrange));
                                } else if (token.extraInformation1.indexOf("88") !== -1) {
                                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.EnumMember, container, lrange));
                                } else if (token.extraInformation1.indexOf("-OCCURS") !== -1) {
                                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Array, container, lrange));
                                } else {
                                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Field, container, lrange));
                                }
                            }
                            break;
                        case COBOLTokenStyle.ConditionName:
                            if (includeVars === false) {
                                break;
                            }
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.TypeParameter, container, lrange));
                            break;
                        case COBOLTokenStyle.Union:
                            if (includeVars === false) {
                                break;
                            }
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Struct, container, lrange));
                            break;
                        case COBOLTokenStyle.Constant:
                            if (includeVars === false) {
                                break;
                            }
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Constant, container, lrange));
                            break;
                        case COBOLTokenStyle.MethodId:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Method, container, lrange));
                            break;
                        case COBOLTokenStyle.Property:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Property, container, lrange));
                            break;

                        case COBOLTokenStyle.Constructor:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Constructor, container, lrange));
                            break;

                        case COBOLTokenStyle.Region:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.File, container, lrange));
                            break;
                    }
                }
            }
            catch (e) {
                VSLogger.logException("Failed " + e + " on " + JSON.stringify(token), e as Error);
            }
        }
        return symbols;
    }
}

export { COBOLSymbolInformationProvider as CobolSymbolInformationProvider };