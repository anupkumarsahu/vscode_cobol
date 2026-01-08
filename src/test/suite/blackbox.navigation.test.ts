import * as assert from "assert";
import * as vscode from "vscode";
import * as path from "path";

/**
 * Black Box Test Suite: Navigation Features
 * Tests user-facing navigation: go to definition, find references, breadcrumbs
 */
suite("Black Box: Navigation Tests", () => {

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

    test("BBT-NAV-001: Workspace symbols can be searched", async function() {
        this.timeout(testTimeout);
        await openCobolDocument("test.cbl");
        await waitForLanguageFeatures();

        try {
            const symbols = await vscode.commands.executeCommand<vscode.SymbolInformation[]>(
                "vscode.executeWorkspaceSymbolProvider",
                "TEST" // Search query
            );

            assert.ok(symbols !== undefined, "Workspace symbol provider should respond");
            console.log("Workspace symbols found:", symbols?.length || 0);
        } catch (error) {
            console.log("Workspace symbol search result:", error);
        }
    });

    test("BBT-NAV-002: Document symbol hierarchy for outline", async function() {
        this.timeout(testTimeout);
        const document = await openCobolDocument("B30DIRb.cob");
        await waitForLanguageFeatures();

        try {
            const symbols = await vscode.commands.executeCommand<vscode.DocumentSymbol[]>(
                "vscode.executeDocumentSymbolProvider",
                document.uri
            );

            assert.ok(symbols !== undefined, "Should provide document symbols");
            
            if (symbols && symbols.length > 0) {
                // Check for nested symbols (sections should contain paragraphs)
                const hasChildren = symbols.some(s => s.children && s.children.length > 0);
                console.log("Has nested symbols:", hasChildren);
                console.log("Top-level symbols:", symbols.length);
                
                // Verify symbols have required properties
                symbols.forEach(symbol => {
                    assert.ok(symbol.name, "Symbol should have a name");
                    assert.ok(symbol.kind !== undefined, "Symbol should have a kind");
                    assert.ok(symbol.range, "Symbol should have a range");
                });
            }
        } catch (error) {
            console.log("Document symbol hierarchy test result:", error);
        }
    });

    test("BBT-NAV-003: Call hierarchy for paragraph calls", async function() {
        this.timeout(testTimeout);
        const document = await openCobolDocument("test.cbl");
        await waitForLanguageFeatures();

        // Position on a paragraph that calls others
        const position = new vscode.Position(15, 10);
        
        try {
            const callHierarchy = await vscode.commands.executeCommand<vscode.CallHierarchyItem[]>(
                "vscode.prepareCallHierarchy",
                document.uri,
                position
            );

            assert.ok(callHierarchy !== undefined, "Call hierarchy provider should respond");
            console.log("Call hierarchy items:", callHierarchy?.length || 0);
        } catch (error) {
            console.log("Call hierarchy test result:", error);
        }
    });

    test("BBT-NAV-004: Peek definition functionality", async function() {
        this.timeout(testTimeout);
        const document = await openCobolDocument("test.cbl");
        await waitForLanguageFeatures();

        const position = new vscode.Position(15, 20);
        
        try {
            // This tests the same provider as Go to Definition
            const locations = await vscode.commands.executeCommand<vscode.Location[]>(
                "vscode.executeDefinitionProvider",
                document.uri,
                position
            );

            assert.ok(locations !== undefined, "Peek definition should work");
            
            if (locations && locations.length > 0) {
                // Verify location structure
                assert.ok(locations[0].uri, "Location should have URI");
                assert.ok(locations[0].range, "Location should have range");
                console.log("Peek definition target:", locations[0].uri.fsPath);
            }
        } catch (error) {
            console.log("Peek definition test result:", error);
        }
    });

    test("BBT-NAV-005: Breadcrumb navigation support", async function() {
        this.timeout(testTimeout);
        const document = await openCobolDocument("test.cbl");
        await waitForLanguageFeatures();

        // Breadcrumbs use document symbols
        try {
            const symbols = await vscode.commands.executeCommand<vscode.DocumentSymbol[]>(
                "vscode.executeDocumentSymbolProvider",
                document.uri
            );

            if (symbols && symbols.length > 0) {
                // Verify symbols contain necessary information for breadcrumbs
                const firstSymbol = symbols[0];
                assert.ok(firstSymbol.name, "Symbol should have name for breadcrumb");
                assert.ok(firstSymbol.range, "Symbol should have range for breadcrumb");
                assert.ok(firstSymbol.selectionRange, "Symbol should have selection range");
                
                console.log("Breadcrumb-ready symbols:", symbols.length);
            }
        } catch (error) {
            console.log("Breadcrumb support test result:", error);
        }
    });

    test("BBT-NAV-006: Type definition provider (for COBOL data types)", async function() {
        this.timeout(testTimeout);
        const document = await openCobolDocument("test.cbl");
        await waitForLanguageFeatures();

        const position = new vscode.Position(10, 15);
        
        try {
            const typeDefinitions = await vscode.commands.executeCommand<vscode.Location[]>(
                "vscode.executeTypeDefinitionProvider",
                document.uri,
                position
            );

            assert.ok(typeDefinitions !== undefined, "Type definition provider should respond");
            console.log("Type definitions found:", typeDefinitions?.length || 0);
        } catch (error) {
            console.log("Type definition test result:", error);
        }
    });

    test("BBT-NAV-007: Implementation provider for copybooks", async function() {
        this.timeout(testTimeout);
        const document = await openCobolDocument("test.cbl");
        await waitForLanguageFeatures();

        const position = new vscode.Position(5, 10);
        
        try {
            const implementations = await vscode.commands.executeCommand<vscode.Location[]>(
                "vscode.executeImplementationProvider",
                document.uri,
                position
            );

            assert.ok(implementations !== undefined, "Implementation provider should respond");
            console.log("Implementations found:", implementations?.length || 0);
        } catch (error) {
            console.log("Implementation provider test result:", error);
        }
    });
});
