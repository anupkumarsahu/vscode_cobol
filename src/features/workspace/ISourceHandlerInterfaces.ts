/* eslint-disable @typescript-eslint/ban-types */
import { ESourceFormat } from "../runtime/IExternalFeatures";
import { ICOBOLSettings } from "../../config/IConfiguration";

export interface ICommentCallback {
    processComment(config: ICOBOLSettings, sourceHandler: sourceHandlerInterfacesLite, commentLine: string, sourceFilename: string, sourceLineNumber:number, startPos: number, format: ESourceFormat) : void;
}

export class CommentRange {
    public startLine: number;
    public startColumn: number;
    public length: number;
    public commentStyle: string;

    constructor(startLine: number, startColumn: number, length: number, commentStyle: string) {
        this.startLine = startLine;
        this.startColumn = startColumn;
        this.length = length;
        this.commentStyle = commentStyle;
    }
}

export interface sourceHandlerInterfacesLite {
    getLineCount(): number;
    getLanguageId():string;
    getFilename(): string;
    getLineTabExpanded(lineNumber: number):string|undefined;
    getNotedComments(): CommentRange[];
    getCommentAtLine(lineNumber: number):string;
}

export interface sourceHandlerInterfaces {
    getUriAsString(): string;
    getLineCount(): number;
    getCommentCount(): number;
    resetCommentCount():void;
    getLine(lineNumber: number, raw: boolean): string|undefined;
    getLineTabExpanded(lineNumber: number):string|undefined;
    setUpdatedLine(lineNumber: number, line:string) : void;
    getUpdatedLine(linenumber: number) : string|undefined;
    setDumpAreaA(flag: boolean): void;
    setDumpAreaBOnwards(flag: boolean): void;
    getFilename(): string;
    addCommentCallback(commentCallback: ICommentCallback):void;
    getDocumentVersionId(): BigInt;
    getIsSourceInWorkSpace(): boolean;
    getShortWorkspaceFilename(): string;
    getLanguageId():string;
    setSourceFormat(format: ESourceFormat):void;
    getNotedComments(): CommentRange[];
    getCommentAtLine(lineNumber: number):string;
    getText(startLine: number, startColumn:number, endLine: number, endColumn:number): string;
}

