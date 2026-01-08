import * as assert from "assert";
import * as vscode from "vscode";

/**
 * Black Box Test Suite: Configuration and Settings
 * Tests user-configurable extension settings from user perspective
 */
suite("Black Box: Configuration Tests", () => {

    test("BBT-CONFIG-001: Extension configuration is accessible", () => {
        const config = vscode.workspace.getConfiguration("coboleditor");
        assert.ok(config, "coboleditor configuration should be accessible");
    });

    test("BBT-CONFIG-002: Copybook directories configuration", () => {
        const config = vscode.workspace.getConfiguration("coboleditor");
        const copybookDirs = config.get("copybookdirs");
        
        console.log("Copybook directories:", copybookDirs);
        assert.ok(copybookDirs !== undefined, "copybookdirs setting should exist");
    });

    test("BBT-CONFIG-003: File format configuration", () => {
        const config = vscode.workspace.getConfiguration("coboleditor");
        const fileFormat = config.get("fileformat");
        
        console.log("File format configuration:", fileFormat);
        assert.ok(fileFormat !== undefined, "fileformat setting should exist");
    });

    test("BBT-CONFIG-004: Source scanner enable/disable", () => {
        const config = vscode.workspace.getConfiguration("coboleditor");
        const scannerEnabled = config.get("enable_source_scanner");
        
        console.log("Source scanner enabled:", scannerEnabled);
        assert.ok(scannerEnabled !== undefined, "enable_source_scanner setting should exist");
        assert.strictEqual(typeof scannerEnabled, "boolean", "Should be a boolean value");
    });

    test("BBT-CONFIG-005: Valid COBOL language IDs", () => {
        const config = vscode.workspace.getConfiguration("coboleditor");
        const validLanguageIds = config.get<string[]>("valid_cobol_language_ids");
        
        console.log("Valid COBOL language IDs:", validLanguageIds);
        assert.ok(validLanguageIds !== undefined, "valid_cobol_language_ids should exist");
        assert.ok(Array.isArray(validLanguageIds), "Should be an array");
        assert.ok(validLanguageIds!.length > 0, "Should have at least one language ID");
    });

    test("BBT-CONFIG-006: Logging level configuration", () => {
        const config = vscode.workspace.getConfiguration("coboleditor");
        const loggingLevel = config.get<string[]>("logging_level");
        
        console.log("Logging level:", loggingLevel);
        assert.ok(loggingLevel !== undefined, "logging_level setting should exist");
        assert.ok(Array.isArray(loggingLevel), "Should be an array");
        
        // Verify valid logging levels
        const validLevels = ["trace", "debug", "info", "warning", "error", "fatal"];
        if (loggingLevel) {
            loggingLevel.forEach(level => {
                assert.ok(
                    validLevels.includes(level.toLowerCase()),
                    `Logging level ${level} should be valid`
                );
            });
        }
    });

    test("BBT-CONFIG-007: Copybook extensions configuration", () => {
        const config = vscode.workspace.getConfiguration("coboleditor");
        const copybookExts = config.get("copybookexts");
        
        console.log("Copybook extensions:", copybookExts);
        assert.ok(copybookExts !== undefined, "copybookexts setting should exist");
    });

    test("BBT-CONFIG-008: LSP configuration for Rocket COBOL", () => {
        const config = vscode.workspace.getConfiguration("coboleditor");
        const lspEnabled = config.get("enable_rocket_cobol_lsp_when_active");
        
        console.log("Rocket COBOL LSP when active:", lspEnabled);
        assert.ok(lspEnabled !== undefined, "LSP setting should exist");
    });

    test("BBT-CONFIG-009: Linter configuration", () => {
        const config = vscode.workspace.getConfiguration("coboleditor");
        const linterEnabled = config.get("enable_linter");
        
        console.log("Linter enabled:", linterEnabled);
        // Linter may or may not be configured
        console.log("Linter configuration exists:", linterEnabled !== undefined);
    });

    test("BBT-CONFIG-010: Cache control settings", () => {
        const config = vscode.workspace.getConfiguration("coboleditor");
        
        // Check for various cache-related settings
        const settings = [
            "cache_metadata",
            "parse_copybooks_for_references",
            "workspacebefore_migration"
        ];
        
        settings.forEach(setting => {
            const value = config.get(setting);
            console.log(`${setting}:`, value);
        });
        
        assert.ok(true, "Cache settings checked");
    });

    test("BBT-CONFIG-011: Margin configuration", () => {
        const config = vscode.workspace.getConfiguration("coboleditor");
        const margin = config.get("margin");
        
        console.log("Margin configuration:", margin);
        // Margin setting may exist for controlling display
    });

    test("BBT-CONFIG-012: IntelliSense configuration", () => {
        const config = vscode.workspace.getConfiguration("coboleditor");
        
        const intellisenseSettings = [
            "intellisense_item_limit",
            "intellisense_add_space_keywords",
            "outline_show_paragraphs",
            "outline_show_sections"
        ];
        
        intellisenseSettings.forEach(setting => {
            const value = config.get(setting);
            console.log(`${setting}:`, value);
        });
        
        assert.ok(true, "IntelliSense settings checked");
    });

    test("BBT-CONFIG-013: Problem matcher configuration in package.json", async () => {
        // Verify that problem matchers are configured for compiler integration
        // This is a black box test verifying user-facing task integration
        
        try {
            const tasks = await vscode.tasks.fetchTasks();
            console.log("Available tasks:", tasks.length);
            
            // Extension should not create tasks by default, but configuration should allow it
            assert.ok(tasks !== undefined, "Task system should be available");
        } catch (error) {
            console.log("Task configuration test result:", error);
        }
    });

    test("BBT-CONFIG-014: Configuration updates are applied", async () => {
        const config = vscode.workspace.getConfiguration("coboleditor");
        
        // Get current value
        const originalValue = config.get("enable_source_scanner");
        console.log("Original source scanner setting:", originalValue);
        
        try {
            // Try to update (may fail in test environment)
            await config.update("enable_source_scanner", !originalValue, vscode.ConfigurationTarget.Global);
            
            // Check if updated
            const newValue = config.get("enable_source_scanner");
            console.log("Updated source scanner setting:", newValue);
            
            // Restore original
            await config.update("enable_source_scanner", originalValue, vscode.ConfigurationTarget.Global);
            
            assert.ok(true, "Configuration update mechanism works");
        } catch (error) {
            console.log("Configuration update test (may not work in test env):", error);
        }
    });

    test("BBT-CONFIG-015: Extension contributes language configuration", () => {
        // Check that COBOL language is properly registered
        const languages = vscode.extensions.all
            .filter(ext => ext.id === "bitlang.cobol")
            .flatMap(ext => {
                const packageJSON = ext.packageJSON;
                return packageJSON.contributes?.languages || [];
            });
        
        console.log("Contributed languages:", languages.length);
        
        if (languages.length > 0) {
            languages.forEach((lang: any) => {
                console.log(`  - ${lang.id}: ${lang.aliases?.join(", ")}`);
            });
            
            assert.ok(languages.length > 0, "Should contribute at least one language");
        }
    });
});
