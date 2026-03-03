// eslint-disable-next-line @typescript-eslint/no-var-requires
import * as path from "path";

import { COBOLGlobalSymbolTable, COBOLWorkspaceFile } from "./cobolglobalcache";

/**
 * Global singleton containing workspace-wide symbol metadata.
 */
export const InMemoryGlobalSymbolCache = new COBOLGlobalSymbolTable();

/**
 * Helper methods for updating shared in-memory cache collections.
 */
export class InMemoryGlobalCacheHelper {

    public static getFilenameWithoutPath(fullPath: string): string {
        const lastSlash = fullPath.lastIndexOf(path.sep);
        if (lastSlash === -1) {
            return fullPath;
        }
        return fullPath.substring(1 + lastSlash);
    }

    public static addFilename(filename: string, wsf: COBOLWorkspaceFile): void {
        if (InMemoryGlobalSymbolCache.sourceFilenameModified.has(filename)) {
            InMemoryGlobalSymbolCache.sourceFilenameModified.delete(filename);
            InMemoryGlobalSymbolCache.sourceFilenameModified.set(filename, wsf);
        } else {
            InMemoryGlobalSymbolCache.sourceFilenameModified.set(filename, wsf);
        }
        InMemoryGlobalSymbolCache.isDirty = true;
    }
}
