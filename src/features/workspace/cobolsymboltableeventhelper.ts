import { InMemoryGlobalCacheHelper } from "./globalcachehelper";
import { COBOLToken, COBOLTokenStyle } from "./cobolsourcescanner";
import { ICOBOLSettings } from "../../config/IConfiguration";
import { COBOLSymbol, COBOLSymbolTable } from "./cobolglobalcache";
import { COBOLWorkspaceSymbolCacheHelper } from "./cobolworkspacecache";
import { cobolWorkspaceScanner_ADDFILE, cobolWorkspaceScanner_KNOWNCOPYBOOK, cobolWorkspaceScanner_SENDCLASS, cobolWorkspaceScanner_SENDENUM, cobolWorkspaceScanner_SENDEP, cobolWorkspaceScanner_SENDINTERFACE, cobolWorkspaceScanner_SENDPRGID } from "./cobolWorkspaceScannerData";
import { cobolSourceScannerInterfaces, cobolSourceScannerInterfacesEventer, cobolSourceScannerInterfacesEvents } from "./ICobolSourceScannerInterfaces";


export class COBOLSymbolTableEventHelper implements cobolSourceScannerInterfacesEvents {
    private st: COBOLSymbolTable | undefined;
    private parse_copybooks_for_references: boolean;
    private sender: cobolSourceScannerInterfacesEventer;

    public constructor(config: ICOBOLSettings, sender: cobolSourceScannerInterfacesEventer) {
        this.sender = sender;
        this.parse_copybooks_for_references = config.parse_copybooks_for_references;
    }

    public start(qp: cobolSourceScannerInterfaces): void {
        this.st = new COBOLSymbolTable();
        this.st.fileName = qp.filename;
        this.st.lastModifiedTime = qp.lastModifiedTime;

        if (this.st?.fileName !== undefined && this.st.lastModifiedTime !== undefined) {
            InMemoryGlobalCacheHelper.addFilename(this.st?.fileName, qp.workspaceFile);

            if (this.sender !== undefined) {
                this.sender.sendMessage(`${cobolWorkspaceScanner_ADDFILE},${this.st?.lastModifiedTime},${this.st?.fileName}`);
            }
        }

        COBOLWorkspaceSymbolCacheHelper.removeAllPrograms(this.st?.fileName);
        COBOLWorkspaceSymbolCacheHelper.removeAllProgramEntryPoints(this.st?.fileName);
        COBOLWorkspaceSymbolCacheHelper.removeAllTypes(this.st?.fileName);
        InMemoryGlobalCacheHelper.addFilename(this.st?.fileName, qp.workspaceFile);
    }

    public processToken(token: COBOLToken): void {
        // hidden token should not be placed in the symbol table, as they from a different file
        if (token.ignoreInOutlineView) {
            return;
        }

        if (this.st === undefined) {
            return;
        }

        if (this.parse_copybooks_for_references === false) {
            switch (token.tokenType) {
                case COBOLTokenStyle.Union:
                    this.st.variableSymbols.set(token.tokenNameLower, new COBOLSymbol(token.tokenName, token.startLine));
                    break;
                case COBOLTokenStyle.Constant:
                    this.st.variableSymbols.set(token.tokenNameLower, new COBOLSymbol(token.tokenName, token.startLine));
                    break;
                case COBOLTokenStyle.ConditionName:
                    this.st.variableSymbols.set(token.tokenNameLower, new COBOLSymbol(token.tokenName, token.startLine));
                    break;
                case COBOLTokenStyle.Variable:
                    this.st.variableSymbols.set(token.tokenNameLower, new COBOLSymbol(token.tokenName, token.startLine));
                    break;
                case COBOLTokenStyle.Paragraph:
                    this.st.labelSymbols.set(token.tokenNameLower, new COBOLSymbol(token.tokenName, token.startLine));
                    break;
                case COBOLTokenStyle.Section:
                    this.st.labelSymbols.set(token.tokenNameLower, new COBOLSymbol(token.tokenName, token.startLine));
                    break;
            }
        }

        switch (token.tokenType) {
            case COBOLTokenStyle.CopyBook:
                if (this.sender) {
                    this.sender.sendMessage(`${cobolWorkspaceScanner_KNOWNCOPYBOOK},${token.tokenName},${this.st.fileName},${token.extraInformation1}`);
                }
                break;
            case COBOLTokenStyle.CopyBookInOrOf:
                if (this.sender) {
                    this.sender.sendMessage(`${cobolWorkspaceScanner_KNOWNCOPYBOOK},${token.tokenName},${this.st.fileName},${token.extraInformation1}`);
                }
                break;
            case COBOLTokenStyle.ImplicitProgramId:
                COBOLWorkspaceSymbolCacheHelper.addCalableSymbol(this.st.fileName, token.tokenNameLower, token.startLine);
                if (this.sender) {
                    this.sender.sendMessage(`${cobolWorkspaceScanner_SENDPRGID},${token.tokenName},${token.startLine},${this.st.fileName}`);
                }
                break;
            case COBOLTokenStyle.ProgramId:
                if (this.sender) {
                    this.sender.sendMessage(`${cobolWorkspaceScanner_SENDPRGID},${token.tokenName},${token.startLine},${this.st.fileName}`);
                }
                break;
            case COBOLTokenStyle.EntryPoint:
                if (this.sender) {
                    this.sender.sendMessage(`${cobolWorkspaceScanner_SENDEP},${token.tokenName},${token.startLine},${this.st.fileName}`);
                }
                break;
            case COBOLTokenStyle.InterfaceId:
                if (this.sender) {
                    this.sender.sendMessage(`${cobolWorkspaceScanner_SENDINTERFACE},${token.tokenName},${token.startLine},${this.st.fileName}`);
                }
                break;
            case COBOLTokenStyle.EnumId:
                if (this.sender) {
                    this.sender.sendMessage(`${cobolWorkspaceScanner_SENDENUM},${token.tokenName},${token.startLine},${this.st.fileName}`);
                }
                break;
            case COBOLTokenStyle.ClassId:
                if (this.sender) {
                    this.sender.sendMessage(`${cobolWorkspaceScanner_SENDCLASS},${token.tokenName},${token.startLine},${this.st.fileName}`);
                }
                break;
            case COBOLTokenStyle.MethodId:
                // GlobalCachesHelper.addMethodSymbol(this.st.fileName, token.tokenName, token.startLine);
                break;
        }
    }

    processRawMessage(messageId: string, message: string): void {
        if (this.sender) {
            this.sender.sendMessage(`${messageId},${message}`);
        }
    }

    public finish(): void {
        //
    }
}
