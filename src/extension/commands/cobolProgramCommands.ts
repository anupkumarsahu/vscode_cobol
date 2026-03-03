"use strict";

import { Range, Selection, TextEditorRevealType, window } from "vscode";

/**
 * Lightweight navigation commands for jumping between common COBOL structural points.
 *
 * These commands intentionally use regex scanning instead of scanner metadata so they
 * can run quickly and independently of background parsing state.
 */
export class cobolProgramCommandsCommands {
    /**
     * Moves cursor to `PROCEDURE DIVISION`.
     */
    public static move2pd(): void {
        const line = cobolProgramCommandsCommands.findProcedureDivision();
        if (line > 0) {
            cobolProgramCommandsCommands.goToLine(line);
        } else {
            window.setStatusBarMessage("ERROR: 'PROCEDURE DIVISION' not found.", 4000);
        }
    }

    /**
     * Moves cursor to `DATA DIVISION` (fallback: `WORKING-STORAGE SECTION`).
     */
    public static move2dd(): void {
        let line = cobolProgramCommandsCommands.findDataDivision();
        if (line > 0) {
            cobolProgramCommandsCommands.goToLine(line);
            return;
        }

        line = cobolProgramCommandsCommands.findWorkingStorageSection();
        if (line > 0) {
            cobolProgramCommandsCommands.goToLine(line);
            return;
        }

        window.setStatusBarMessage("ERROR: 'DATA DIVISION' or 'WORKING-STORAGE SECTION' not found.", 4000);
    }

    /**
     * Moves cursor to `WORKING-STORAGE SECTION`.
     */
    public static move2ws(): void {
        const line = cobolProgramCommandsCommands.findWorkingStorageSection();
        if (line > 0) {
            cobolProgramCommandsCommands.goToLine(line);
            return;
        }

        window.setStatusBarMessage("ERROR: 'WORKING-STORAGE SECTION' not found.", 4000);
    }

    /**
     * Moves forward to next structural marker (division/section/entry/etc).
     */
    public static move2anyforward(): void {
        const line = cobolProgramCommandsCommands.findAnyNext(1);
        if (line > 0) {
            cobolProgramCommandsCommands.goToLine(line);
        }
    }

    /**
     * Moves backward to previous structural marker (division/section/entry/etc).
     */
    public static move2anybackwards(): void {
        const line = cobolProgramCommandsCommands.findAnyNext(-1);
        if (line > 0) {
            cobolProgramCommandsCommands.goToLine(line);
        }
    }

    /**
     * Finds first line matching a single regex from top of active document.
     */
    private static findMatch(mat: RegExp) {
        if (window.activeTextEditor) {
            const doc = window.activeTextEditor.document;
            for (let line = 0; line <= doc.lineCount; line++) {
                // COBOL columns can extend broadly; scan up to 132 for compatibility.
                const range = new Range(line, 0, line, 132);
                const txt = doc.getText(range);
                if (txt.match(mat)) {
                    return line;
                }
            }
        }
        return 0;
    }

    /**
     * Finds the next/previous line that matches any regex in `mats`.
     *
     * @param counter Direction step (`1` forward, `-1` backward).
     */
    private static findAnyMatch(mats: RegExp[], counter: number) {
        if (window.activeTextEditor) {
            const doc = window.activeTextEditor.document;
            const editor = window.activeTextEditor;
            const endValue = counter === 1 ? doc.lineCount : 0;
            let line = editor.selection.active.line + counter;
            for (; line !== endValue; line += counter) {
                const range = new Range(line, 0, line, 132);
                const txt = doc.getText(range);
                const matsLen = mats.length;
                // Check all structural patterns at each candidate line.
                for (let matpos = 0; matpos < matsLen; matpos++) {
                    const mat = mats[matpos];
                    if (txt.match(mat)) {
                        return line;
                    }
                }
            }
        }
        return 0;
    }

    /**
     * Builds and searches the generic structural navigation pattern set.
     */
    private static findAnyNext(counter: number) {
        const mats = [
            /.*\s*division/i,
            /entry\s*"/i,
            /.*\s*section\./i,
            /eject/i,
            /program-id\./i,
            /class-id[.|\s]/i,
            /method-id[.|\s]/i];
        return cobolProgramCommandsCommands.findAnyMatch(mats, counter);
    }

    private static findProcedureDivision() {
        return cobolProgramCommandsCommands.findMatch(/procedure\s*division/i);
    }

    private static findDataDivision() {
        return cobolProgramCommandsCommands.findMatch(/data\s*division/i);
    }

    private static findWorkingStorageSection() {
        return cobolProgramCommandsCommands.findMatch(/working-storage\s*section/i);
    }


    /**
     * Positions cursor at the requested line and reveals it in the editor.
     */
    private static goToLine(line: number) {
        if (window.activeTextEditor) {
            let reviewType = TextEditorRevealType.InCenter;
            // Avoid aggressive recentering if user is already on target line.
            if (line === window.activeTextEditor.selection.active.line) {
                reviewType = TextEditorRevealType.InCenterIfOutsideViewport;
            }
            const newSe = new Selection(line, 0, line, 0);
            window.activeTextEditor.selection = newSe;
            window.activeTextEditor.revealRange(newSe, reviewType);
        }
    }

}
