/* eslint-disable @typescript-eslint/no-explicit-any */
import fs from "fs";

/**
 * Serializable payload used to pass scanner configuration and workspace state.
 */
export class ScanData {
    public sendOnCount = 0;
    public sendOnFileCount = 0;
    public sendPercent=0;
    public scannerBinDir = "";
    public directoriesScanned = 0;
    public maxDirectoryDepth = 0;
    public fileCount = 0;
    public parse_copybooks_for_references = false;
    public cache_metadata_verbose_messages = false;
    public Files: string[] = [];
    public showStats = true;
    public md_symbols: string[] = [];
    public md_entrypoints: string[] = [];
    public md_types: string[] = [];
    public md_metadata_files: string[] = [];
    public md_metadata_knowncopybooks: string[] = [];
    public workspaceFolders: string[] = [];
    public md_file_search_directory: string[] = [];
    public md_copybookdirs: string[] = [];
    public md_copybookexts: string[] = []
    public md_makefile_dependancy_fullpath = false;
}

/**
 * Runtime counters and timing data produced by scanner execution.
 */
export class ScanStats {
    directoriesScanned = 0;
    directoryDepth = 0;
    maxDirectoryDepth = 0;
    filesScanned = 0;
    copyBookExts = 0;
    fileCount = 0;
    filesUptodate = 0;
    programsDefined = 0;
    entryPointsDefined = 0;
    start = 0;
    endTime = 0;
    showMessage = false;
}

export const cobolWorkspaceScanner_STATUS = "@@STATUS";
export const cobolWorkspaceScanner_SENDPRGID = "@@SEND.PRGID";
export const cobolWorkspaceScanner_SENDEP = "@@SEND.EP";
export const cobolWorkspaceScanner_SENDINTERFACE = "@@SEND.INTID";
export const cobolWorkspaceScanner_SENDENUM = "@@SEND.ENUMID";
export const cobolWorkspaceScanner_SENDCLASS = "@@SEND.CLASSID";
export const cobolWorkspaceScanner_ADDFILE = "@@SEND.FILES";
export const cobolWorkspaceScanner_KNOWNCOPYBOOK = "@@SEND.KNOWNCOPYBOOK";
export const cobolWorkspaceScanner_START_OF_FILE = "@@SEND.START_OF_FILE";
export const cobolWorkspaceScanner_END_OF_FILE = "@@SEND.END_OF_FILE";


function replacer(this: any, key: any, value: any): any {

    if (typeof value === "bigint") {
        return value.toString();
    }

    return value;
}

function reviver(key: any, value: any): any {
    if (typeof value === "bigint" && value !== null) {
        return BigInt(value);
    }

    return value;
}

/**
 * Utility methods for serializing/deserializing scanner transfer payloads.
 */
export class ScanDataHelper {

    public static load(fn: string) : ScanData {
        // const fn = path.join(cacheDirectory, ScanDataHelper.scanFilename);
        const str: string = fs.readFileSync(fn).toString();
        return JSON.parse(str) as ScanData;
    }

    public static parseScanData(str: string) : ScanData {
        return JSON.parse(str, reviver) as ScanData;
    }

    public static getScanData(st: ScanData) : string {
        return  JSON.stringify(st,replacer);
    }

    public static setupPercent(scanData: ScanData, numberOfFiles: number, percentIncrement: number):void {
        scanData.sendOnFileCount = numberOfFiles;
        scanData.sendPercent = percentIncrement;
        scanData.sendOnCount = Math.round(scanData.sendOnFileCount * (scanData.sendPercent / 100));
    }
}
