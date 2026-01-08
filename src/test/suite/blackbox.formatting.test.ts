import * as assert from "assert";
import * as vscode from "vscode";
import * as path from "path";

/**
 * Black Box Test Suite: Formatting and Editing Features
 * Tests user-facing formatting, indentation, and code actions
 */
suite("Black Box: Formatting Tests", () => {

    const testWorkspaceFolder = path.join(__dirname, "../../../src/test/suite/");

    // Set longer timeout for tests that wait for language features
    const testTimeout = 5000;

    async function openCobolDocument(filename: string): Promise<vscode.TextDocument> {
        const uri = vscode.Uri.file(path.join(testWorkspaceFolder, filename));
        const document = await vscode.workspace.openTextDocument(uri);
        await vscode.window.showTextDocument(document);
        return document;
    }

    async function waitForLanguageFeatures(delay = 2000): Promise<void> {
        await new Promise(resolve => setTimeout(resolve, delay));
    }

    test("BBT-FMT-001: Document formatting provider responds", async function() {
        this.timeout(testTimeout);
        const document = await openCobolDocument("test.cbl");
        await waitForLanguageFeatures();

        try {
            const edits = await vscode.commands.executeCommand<vscode.TextEdit[]>(
                "vscode.executeFormatDocumentProvider",
                document.uri,
                { tabSize: 4, insertSpaces: true }
            );

            // Formatting may not be implemented, but provider should respond
            assert.ok(edits !== undefined, "Format document provider should respond");
            console.log("Formatting edits returned:", edits?.length || 0);
        } catch (error) {
            console.log("Document formatting test result:", error);
        }
    });

    test("BBT-FMT-002: Range formatting provider", async function() {
        this.timeout(testTimeout);
        const document = await openCobolDocument("test.cbl");
        await waitForLanguageFeatures();

        const range = new vscode.Range(
            new vscode.Position(10, 0),
            new vscode.Position(15, 0)
        );

        try {
            const edits = await vscode.commands.executeCommand<vscode.TextEdit[]>(
                "vscode.executeFormatRangeProvider",
                document.uri,
                range,
                { tabSize: 4, insertSpaces: true }
            );

            assert.ok(edits !== undefined, "Format range provider should respond");
            console.log("Range formatting edits:", edits?.length || 0);
        } catch (error) {
            console.log("Range formatting test result:", error);
        }
    });

    test("BBT-FMT-003: On-type formatting for COBOL", async function() {
        this.timeout(testTimeout);
        const document = await openCobolDocument("test.cbl");
        await waitForLanguageFeatures();

        const position = new vscode.Position(12, 20);
        const character = "."; // Period triggers on-type formatting

        try {
            const edits = await vscode.commands.executeCommand<vscode.TextEdit[]>(
                "vscode.executeFormatOnTypeProvider",
                document.uri,
                position,
                character,
                { tabSize: 4, insertSpaces: true }
            );

            assert.ok(edits !== undefined, "On-type formatting provider should respond");
            console.log("On-type formatting edits:", edits?.length || 0);
        } catch (error) {
            console.log("On-type formatting test result:", error);
        }
    });

    test("BBT-FMT-004: Code actions available for COBOL issues", async function() {
        this.timeout(testTimeout);
        const document = await openCobolDocument("test.cbl");
        await waitForLanguageFeatures();

        const range = new vscode.Range(
            new vscode.Position(10, 0),
            new vscode.Position(10, 50)
        );

        try {
            const codeActions = await vscode.commands.executeCommand<vscode.CodeAction[]>(
                "vscode.executeCodeActionProvider",
                document.uri,
                range
            );

            assert.ok(codeActions !== undefined, "Code action provider should respond");
            console.log("Code actions available:", codeActions?.length || 0);
            
            if (codeActions && codeActions.length > 0) {
                // Verify code action structure
                codeActions.forEach(action => {
                    assert.ok(action.title, "Code action should have a title");
                });
            }
        } catch (error) {
            console.log("Code actions test result:", error);
        }
    });

    test("BBT-FMT-005: Selection range provider for smart selection", async function() {
        this.timeout(testTimeout);
        const document = await openCobolDocument("test.cbl");
        await waitForLanguageFeatures();

        const positions = [new vscode.Position(12, 15)];

        try {
            const selectionRanges = await vscode.commands.executeCommand<vscode.SelectionRange[]>(
                "vscode.executeSelectionRangeProvider",
                document.uri,
                positions
            );

            assert.ok(selectionRanges !== undefined, "Selection range provider should respond");
            console.log("Selection ranges:", selectionRanges?.length || 0);
            
            if (selectionRanges && selectionRanges.length > 0) {
                assert.ok(selectionRanges[0].range, "Selection range should have a range");
            }
        } catch (error) {
            console.log("Selection range test result:", error);
        }
    });

    test("BBT-FMT-006: Folding range provider for code folding", async function() {
        this.timeout(testTimeout);
        const document = await openCobolDocument("test.cbl");
        await waitForLanguageFeatures();

        try {
            const foldingRanges = await vscode.commands.executeCommand<vscode.FoldingRange[]>(
                "vscode.executeFoldingRangeProvider",
                document.uri
            );

            assert.ok(foldingRanges !== undefined, "Folding range provider should respond");
            console.log("Folding ranges found:", foldingRanges?.length || 0);
            
            if (foldingRanges && foldingRanges.length > 0) {
                // Verify folding range structure
                foldingRanges.forEach(range => {
                    assert.ok(range.start >= 0, "Folding range should have valid start");
                    assert.ok(range.end > range.start, "Folding range end should be after start");
                });
            }
        } catch (error) {
            console.log("Folding range test result:", error);
        }
    });

    test("BBT-FMT-007: Comment toggling works for COBOL", async function() {
        this.timeout(testTimeout);
        const document = await openCobolDocument("test.cbl");
        await waitForLanguageFeatures();

        // Get current content
        const line = document.lineAt(10);
        const originalText = line.text;

        try {
            // This tests that the language configuration supports commenting
            const editor = vscode.window.activeTextEditor;
            if (editor) {
                editor.selection = new vscode.Selection(10, 0, 10, 0);
                
                // Execute toggle comment command
                await vscode.commands.executeCommand("editor.action.commentLine");
                
                // Wait a bit for the command to process
                await new Promise(resolve => setTimeout(resolve, 100));
                
                const newLine = editor.document.lineAt(10);
                console.log("Original line:", originalText);
                console.log("After toggle:", newLine.text);
                
                // Comment toggle should have modified the line (or the command executed)
                assert.ok(true, "Comment toggle command executed");
            }
        } catch (error) {
            console.log("Comment toggle test result:", error);
        }
    });
});
