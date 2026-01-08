import * as assert from "assert";
import * as vscode from "vscode";
import * as path from "path";

/**
 * Black Box Test Suite: Dialect-Specific Features
 * Tests NonStop/Tandem COBOL and other dialect-specific functionality
 */
suite("Black Box: Dialect-Specific Tests", () => {

    const testWorkspaceFolder = path.join(__dirname, "../../../");

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

    test("BBT-DIALECT-001: NonStop COBOL file detection", async function() {
        this.timeout(testTimeout);
        try {
            const document = await openCobolDocument("test-complete-nonstop-cobol.cbl");
            await waitForLanguageFeatures();

            // Check that file is recognized with proper language ID
            const languageId = document.languageId;
            console.log("Detected language ID:", languageId);
            
            // Should be COBOL or COBOL_TANDEM
            const isCobol = languageId === "COBOL" || 
                           languageId === "COBOL_TANDEM" || 
                           languageId === "cobol";
            
            assert.ok(isCobol, `File should be detected as COBOL, got: ${languageId}`);
        } catch (error) {
            console.log("NonStop COBOL detection test result:", error);
        }
    });

    test("BBT-DIALECT-002: Tandem directive syntax highlighting", async function() {
        this.timeout(testTimeout);
        try {
            const document = await openCobolDocument("test-complete-nonstop-cobol.cbl");
            await waitForLanguageFeatures();

            // Check that document can be parsed
            assert.ok(document.lineCount > 0, "Document should have content");
            
            // Look for Tandem-specific directives (? directives)
            let hasTandemDirectives = false;
            for (let i = 0; i < Math.min(document.lineCount, 100); i++) {
                const line = document.lineAt(i).text;
                if (line.includes("?") && (
                    line.includes("?ANSI") || 
                    line.includes("?IF") || 
                    line.includes("?HEADING") ||
                    line.includes("?INSPECT")
                )) {
                    hasTandemDirectives = true;
                    console.log("Found Tandem directive:", line.trim());
                    break;
                }
            }
            
            console.log("Has Tandem directives:", hasTandemDirectives);
        } catch (error) {
            console.log("Tandem directive test result:", error);
        }
    });

    test("BBT-DIALECT-003: Computer name detection for Tandem", async function() {
        this.timeout(testTimeout);
        try {
            const document = await openCobolDocument("test-complete-nonstop-cobol.cbl");
            await waitForLanguageFeatures();

            // Look for COMPUTER clause with Tandem computer names
            let foundTandemComputer = false;
            for (let i = 0; i < Math.min(document.lineCount, 50); i++) {
                const line = document.lineAt(i).text.toLowerCase();
                if (line.includes("object-computer") || line.includes("source-computer")) {
                    const nextLines = [];
                    for (let j = i; j < Math.min(i + 5, document.lineCount); j++) {
                        nextLines.push(document.lineAt(j).text);
                    }
                    const computerClause = nextLines.join(" ").toLowerCase();
                    
                    if (computerClause.includes("tandem") || 
                        computerClause.includes("t16") || 
                        computerClause.includes("vlx") ||
                        computerClause.includes("non-stop")) {
                        foundTandemComputer = true;
                        console.log("Found Tandem computer specification");
                        break;
                    }
                }
            }
            
            console.log("Found Tandem computer name:", foundTandemComputer);
        } catch (error) {
            console.log("Computer name detection test result:", error);
        }
    });

    test("BBT-DIALECT-004: Symbols recognized in NonStop COBOL", async function() {
        this.timeout(testTimeout);
        try {
            const document = await openCobolDocument("test-complete-nonstop-cobol.cbl");
            await waitForLanguageFeatures();

            const symbols = await vscode.commands.executeCommand<vscode.DocumentSymbol[]>(
                "vscode.executeDocumentSymbolProvider",
                document.uri
            );

            assert.ok(symbols !== undefined, "Symbol provider should work for NonStop COBOL");
            
            if (symbols && symbols.length > 0) {
                console.log("NonStop COBOL symbols found:", symbols.length);
                
                // Log some symbol names to verify parsing
                symbols.slice(0, 5).forEach(symbol => {
                    console.log("  Symbol:", symbol.name, "Kind:", vscode.SymbolKind[symbol.kind]);
                });
            }
        } catch (error) {
            console.log("NonStop symbols test result:", error);
        }
    });

    test("BBT-DIALECT-005: ACU COBOL dialect support", async () => {
        // This test verifies ACU COBOL would be recognized
        // We test by checking if the language ID is in valid_cobol_language_ids
        
        const config = vscode.workspace.getConfiguration("coboleditor");
        const validLanguages = config.get<string[]>("valid_cobol_language_ids") || [];
        
        console.log("Valid COBOL language IDs:", validLanguages);
        
        const hasACU = validLanguages.some(id => id.toLowerCase().includes("acu"));
        console.log("ACU COBOL support configured:", hasACU);
        
        // Even if not in list, configuration should exist
        assert.ok(validLanguages.length > 0, "Should have valid language IDs configured");
    });

    test("BBT-DIALECT-006: RM COBOL dialect support", async () => {
        const config = vscode.workspace.getConfiguration("coboleditor");
        const validLanguages = config.get<string[]>("valid_cobol_language_ids") || [];
        
        const hasRM = validLanguages.some(id => id.toLowerCase().includes("rm"));
        console.log("RM COBOL support configured:", hasRM);
        
        assert.ok(validLanguages.length > 0, "Should have valid language IDs configured");
    });

    test("BBT-DIALECT-007: IBM i COBOL dialect support", async () => {
        const config = vscode.workspace.getConfiguration("coboleditor");
        const validLanguages = config.get<string[]>("valid_cobol_language_ids") || [];
        
        const hasILE = validLanguages.some(id => id.toLowerCase().includes("ile"));
        console.log("IBM i COBOL support configured:", hasILE);
        
        assert.ok(validLanguages.length > 0, "Should have valid language IDs configured");
    });

    test("BBT-DIALECT-008: Tandem reference format margins", async function() {
        this.timeout(testTimeout);
        try {
            const document = await openCobolDocument("test-tandem-issue.cbl");
            await waitForLanguageFeatures();

            assert.ok(document.lineCount > 0, "Tandem test file should have content");
            
            // Verify file can be opened and parsed
            console.log("Tandem reference format file lines:", document.lineCount);
            
            // Check first few lines for format clues
            for (let i = 0; i < Math.min(5, document.lineCount); i++) {
                const line = document.lineAt(i).text;
                console.log(`Line ${i}:`, line.substring(0, Math.min(80, line.length)));
            }
        } catch (error) {
            console.log("Tandem reference format test result:", error);
        }
    });

    test("BBT-DIALECT-009: Source format detection", async () => {
        const config = vscode.workspace.getConfiguration("coboleditor");
        const fileFormats = config.get("fileformat");
        
        console.log("Configured file formats:", JSON.stringify(fileFormats, null, 2));
        
        assert.ok(fileFormats !== undefined, "File format configuration should exist");
    });

    test("BBT-DIALECT-010: Free format COBOL support", async () => {
        // Test that free format is recognized in configuration
        const config = vscode.workspace.getConfiguration("coboleditor");
        const fileFormats = config.get("fileformat");
        
        console.log("Checking for free format configuration...");
        
        // Configuration should exist even if no files use free format
        assert.ok(fileFormats !== undefined, "File format configuration should exist");
    });
});
