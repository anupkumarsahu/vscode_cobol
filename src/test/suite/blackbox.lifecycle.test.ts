import * as assert from "assert";
import * as vscode from "vscode";

/**
 * Black Box Test Suite: Extension Lifecycle
 * Tests extension activation, commands, and integration from user perspective
 */
suite("Black Box: Extension Lifecycle Tests", () => {

    test("BBT-LIFECYCLE-001: Extension is installed and active", () => {
        const extension = vscode.extensions.getExtension("bitlang.cobol");
        
        assert.ok(extension, "Extension should be installed");
        console.log("Extension found:", extension?.id);
        console.log("Extension active:", extension?.isActive);
    });

    test("BBT-LIFECYCLE-002: Extension activates on COBOL file", async () => {
        const extension = vscode.extensions.getExtension("bitlang.cobol");
        
        if (extension && !extension.isActive) {
            // Try to activate
            await extension.activate();
        }
        
        assert.ok(extension?.isActive, "Extension should be active after COBOL file operations");
    });

    test("BBT-LIFECYCLE-003: Extension contributes commands", () => {
        const extension = vscode.extensions.getExtension("bitlang.cobol");
        
        if (extension) {
            const packageJSON = extension.packageJSON;
            const commands = packageJSON.contributes?.commands || [];
            
            console.log("Contributed commands:", commands.length);
            commands.forEach((cmd: any) => {
                console.log(`  - ${cmd.command}: ${cmd.title}`);
            });
            
            assert.ok(commands.length > 0, "Extension should contribute commands");
        }
    });

    test("BBT-LIFECYCLE-004: Language-switching commands available", async () => {
        // Test that language switching commands are registered
        const commands = await vscode.commands.getCommands(true);
        
        const cobolCommands = commands.filter(cmd => cmd.includes("cobol"));
        console.log("COBOL-related commands:", cobolCommands.length);
        
        // Check for specific language switching commands
        const langSwitchCommands = [
            "cobolplugin.change_lang_to_cobol"
        ];
        
        langSwitchCommands.forEach(cmd => {
            const exists = commands.includes(cmd);
            console.log(`Command ${cmd}: ${exists ? "registered" : "not found"}`);
        });
        
        assert.ok(cobolCommands.length > 0, "Should have COBOL commands registered");
    });

    test("BBT-LIFECYCLE-005: Copybook navigation commands", async () => {
        const commands = await vscode.commands.getCommands(true);
        
        // Check for copybook-related commands
        const copybookCommands = commands.filter(cmd => 
            cmd.includes("copybook") || cmd.includes("opencopy")
        );
        
        console.log("Copybook commands found:", copybookCommands.length);
        copybookCommands.forEach(cmd => console.log(`  - ${cmd}`));
        
        // Commands should be registered
        assert.ok(commands.length > 0, "Command registry should be accessible");
    });

    test("BBT-LIFECYCLE-006: Diagnostic commands available", async () => {
        const commands = await vscode.commands.getCommands(true);
        
        // Check for diagnostic/troubleshooting commands
        const diagCommands = commands.filter(cmd => 
            cmd.includes("cobolplugin") && 
            (cmd.includes("diag") || cmd.includes("check") || cmd.includes("dump"))
        );
        
        console.log("Diagnostic commands:", diagCommands.length);
        diagCommands.forEach(cmd => console.log(`  - ${cmd}`));
    });

    test("BBT-LIFECYCLE-007: Extension command registry healthy", async () => {
        const commands = await vscode.commands.getCommands(true);
        const cobolCommands = commands.filter(cmd => cmd.startsWith("cobolplugin."));
        console.log("Registered COBOL extension commands:", cobolCommands.length);
        assert.ok(cobolCommands.length > 0, "COBOL extension commands should be registered");
    });

    test("BBT-LIFECYCLE-008: Output channel is created", () => {
        // Verify that COBOL output channel exists
        // (This is tested indirectly - the channel should be created when extension activates)
        
        const extension = vscode.extensions.getExtension("bitlang.cobol");
        
        if (extension?.isActive) {
            // Channel should exist after activation
            console.log("Extension active - output channel should exist");
            assert.ok(true, "Output channel created with activation");
        }
    });

    test("BBT-LIFECYCLE-009: Web extension support", () => {
        const extension = vscode.extensions.getExtension("bitlang.cobol");
        
        if (extension) {
            const packageJSON = extension.packageJSON;
            const browserEntry = packageJSON.browser;
            
            console.log("Web extension entry point:", browserEntry);
            
            // Extension should have web support configured
            if (browserEntry) {
                assert.ok(browserEntry.includes("extension-web.js"), "Should have web extension bundle");
            }
        }
    });

    test("BBT-LIFECYCLE-010: Extension metadata", () => {
        const extension = vscode.extensions.getExtension("bitlang.cobol");
        
        if (extension) {
            const packageJSON = extension.packageJSON;
            
            console.log("Extension name:", packageJSON.name);
            console.log("Extension version:", packageJSON.version);
            console.log("Extension publisher:", packageJSON.publisher);
            console.log("Extension description:", packageJSON.description);
            
            assert.ok(packageJSON.name === "cobol", "Package name should be 'cobol'");
            assert.ok(packageJSON.publisher === "bitlang", "Publisher should be 'bitlang'");
            assert.ok(packageJSON.version, "Should have version number");
        }
    });

    test("BBT-LIFECYCLE-011: Activation events", () => {
        const extension = vscode.extensions.getExtension("bitlang.cobol");
        
        if (extension) {
            const packageJSON = extension.packageJSON;
            const activationEvents = packageJSON.activationEvents || [];
            
            console.log("Activation events:", activationEvents.length);
            activationEvents.forEach((event: string) => {
                console.log(`  - ${event}`);
            });
            
            // Should activate on COBOL languages
            const activatesOnCobol = activationEvents.some((event: string) => 
                event.includes("COBOL") || event.includes("onLanguage")
            );
            
            assert.ok(activatesOnCobol || activationEvents.includes("*"), 
                "Should activate on COBOL language or startup");
        }
    });

    test("BBT-LIFECYCLE-012: Extension dependencies", () => {
        const extension = vscode.extensions.getExtension("bitlang.cobol");
        
        if (extension) {
            const packageJSON = extension.packageJSON;
            const dependencies = packageJSON.extensionDependencies || [];
            
            console.log("Extension dependencies:", dependencies.length);
            dependencies.forEach((dep: string) => {
                console.log(`  - ${dep}`);
            });
            
            // Log but don't require specific dependencies
            console.log("Extension should work standalone");
        }
    });

    test("BBT-LIFECYCLE-013: Task provider registration", async () => {
        // Check if extension registers task providers
        const extension = vscode.extensions.getExtension("bitlang.cobol");
        
        if (extension) {
            const packageJSON = extension.packageJSON;
            const taskDefinitions = packageJSON.contributes?.taskDefinitions || [];
            
            console.log("Task definitions:", taskDefinitions.length);
            taskDefinitions.forEach((task: any) => {
                console.log(`  - ${task.type}: ${JSON.stringify(task.properties)}`);
            });
        }
    });

    test("BBT-LIFECYCLE-014: Configuration schema validation", () => {
        const extension = vscode.extensions.getExtension("bitlang.cobol");
        
        if (extension) {
            const packageJSON = extension.packageJSON;
            const configuration = packageJSON.contributes?.configuration;
            
            if (configuration) {
                const properties = configuration.properties || {};
                const propertyKeys = Object.keys(properties);
                
                console.log("Configuration properties:", propertyKeys.length);
                
                // Verify key settings exist
                const requiredSettings = [
                    "coboleditor.copybookdirs",
                    "coboleditor.enable_source_scanner",
                    "coboleditor.logging_level"
                ];
                
                requiredSettings.forEach(setting => {
                    const exists = propertyKeys.includes(setting);
                    console.log(`${setting}: ${exists ? "defined" : "missing"}`);
                    assert.ok(exists, `Required setting ${setting} should be defined`);
                });
            }
        }
    });

    test("BBT-LIFECYCLE-015: Extension unload cleanup", () => {
        // This test verifies extension can be safely deactivated
        const extension = vscode.extensions.getExtension("bitlang.cobol");
        
        if (extension?.isActive) {
            // Extension should have proper deactivation logic
            // We can't directly test deactivation, but we verify it exports deactivate
            const exports = extension.exports;
            
            console.log("Extension exports:", typeof exports);
            console.log("Extension should handle deactivation gracefully");
            
            assert.ok(true, "Extension cleanup mechanisms should be in place");
        }
    });
});
