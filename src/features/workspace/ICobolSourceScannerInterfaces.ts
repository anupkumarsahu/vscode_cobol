import { sourceHandlerInterfaces, sourceHandlerInterfacesLite } from "./ISourceHandlerInterfaces";
import { COBOLFileAndColumnSymbol, COBOLFileSymbol, COBOLWorkspaceFile } from "./cobolglobalcache";
import { ICOBOLSettings } from "../../config/IConfiguration";
import { ESourceFormat, IExternalFeatures } from "../runtime/IExternalFeatures";
import { PortResult } from "./directivesConverter";
import { CallTargetInformation, COBOLCopybookToken, COBOLToken, COBOLVariable, SharedSourceReferences, SQLDeclare } from "./cobolsourcescanner";

/**
 * Sends scanner messages to a host process/thread.
 */
export interface cobolSourceScannerInterfacesEventer {
    sendMessage(message: string): void;
}

/**
 * Receives scanner lifecycle callbacks during tokenization.
 */
export interface cobolSourceScannerInterfacesEvents {
    start(qp: cobolSourceScannerInterfaces): void;
    processToken(token: COBOLToken): void;
    processRawMessage(messageId:string, message: string): void;
    finish(): void;
}

/**
 * Public shape of a parsed COBOL source document and its indexed symbols.
 */
export interface cobolSourceScannerInterfaces{
    id: string;
    readonly sourceHandler: sourceHandlerInterfaces;
    filename: string;
    lastModifiedTime: BigInt;
    tokensInOrder: COBOLToken[];
    execTokensInOrder: COBOLToken[];
    readonly execSQLDeclare: Map<string, SQLDeclare>;
    readonly sections: Map<string, COBOLToken>;
    readonly paragraphs: Map<string, COBOLToken>;
    readonly constantsOrVariables: Map<string, COBOLVariable[]>;
    readonly callTargets:  Map<string, CallTargetInformation>;
    readonly functionTargets:  Map<string, CallTargetInformation>;
    readonly classes: Map<string, COBOLToken>;
    readonly methods: Map<string, COBOLToken>;
    readonly copyBooksUsed: Map<string, COBOLCopybookToken[]>;
    readonly copyBooksUnresolved: Map<string, COBOLToken[]>;
    readonly copyBookLibraries: Map<string, COBOLToken[]>;
    readonly diagMissingFileWarnings: Map<string, COBOLFileSymbol>;
    readonly portWarnings: PortResult[];
    readonly generalWarnings: COBOLFileSymbol[];
    readonly commentReferences: COBOLFileAndColumnSymbol[];
    readonly parse4References: boolean;
    readonly sourceReferences: SharedSourceReferences;
    readonly sourceFileId: number;
    cache4PerformTargets: any;
    cache4ConstantsOrVars: any;
    ImplicitProgramId: string;
    ProgramId: string;
    sourceFormat: ESourceFormat;
    sourceIsCopybook: boolean;
    workspaceFile: COBOLWorkspaceFile;
    readonly parse_copybooks_for_references: boolean;
    readonly scan_comments_for_hints: boolean;
    readonly isFromScanCommentsForReferences: boolean;
    readonly scan_comment_for_ls_control: boolean;
    readonly copybookNestedInSection: boolean;
    readonly configHandler: ICOBOLSettings;
    parseHint_OnOpenFiles: string[];
    parseHint_WorkingStorageFiles: string[];
    parseHint_LocalStorageFiles: string[];
    parseHint_ScreenSectionFiles: string[];
    scanAborted: boolean;
    parseExecStatement(currentExecStype: string, token: COBOLToken, lines: string): void;
    parseSQLDeclareForReferences(fileid: number, _refExecSQLDeclareName: string, refExecToken: COBOLToken, lines: string, sqldeclare: SQLDeclare): void;
    processComment(configHandler: ICOBOLSettings, sourceHandler: sourceHandlerInterfacesLite, commentLine: string, sourceFilename: string, sourceLineNumber: number, startPos: number, format: ESourceFormat): void;
    findNearestSectionOrParagraph(line: number): COBOLToken|undefined;
    eventHandler: cobolSourceScannerInterfacesEvents;
    externalFeatures: IExternalFeatures;
}

