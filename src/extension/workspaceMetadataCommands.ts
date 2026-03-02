import * as vscode from "vscode";
import os from "os";
import { commands, window } from "vscode";
import { ICOBOLSettings } from "../config/IConfiguration";
import { ConfigurationService } from "../config/configurationService";
import { InMemoryGlobalSymbolCache } from "../features/workspace/globalcachehelper";
import { VScobolWorkspaceScanner } from "../features/workspace/cobolScannerController";
import { VSExternalFeatures } from "../features/runtime/externalFeatures";

export function registerWorkspaceMetadataCommands(context: vscode.ExtensionContext): void {
    context.subscriptions.push(commands.registerCommand("cobolplugin.processAllFilesInWorkspace", async () => {
        let settings: ICOBOLSettings;
        if (window.activeTextEditor !== undefined) {
            settings = ConfigurationService.getResourceSettings(window.activeTextEditor.document);
        } else {
            settings = ConfigurationService.getWorkspace();
        }

        if (InMemoryGlobalSymbolCache.defaultCallableSymbols.size < 500) {
            VScobolWorkspaceScanner.processAllFilesInWorkspaceOutOfProcess(VSExternalFeatures, settings, true, false, -1);
            return;
        }

        window.showQuickPick(["Yes", "No"], { placeHolder: "Your workspace is large, do you want to extra threads for your metadata scan?" }).then(function (data) {
            if (data === "Yes") {
                const cpuCount = os.cpus().length;
                const defCpuCount = cpuCount >= 4 ? Math.trunc(cpuCount / 2) : cpuCount;

                vscode.window.showInputBox({
                    prompt: "How many threads do you want to use?",
                    value: "" + defCpuCount,
                    validateInput: (threadString: string): string | undefined => {
                        const threadCount: number = Number.parseInt(threadString, 10);

                        if (threadCount < 2 || threadCount > (defCpuCount * 3)) {
                            return `Thread count must be between 2 and ${defCpuCount * 3}`;
                        } else {
                            return undefined;
                        }
                    }
                }).then(value => {
                    if (value === undefined) {
                        return;
                    }

                    const threadCount: number = Number.parseInt(value, 10);
                    VScobolWorkspaceScanner.processAllFilesInWorkspaceOutOfProcess(VSExternalFeatures, settings, true, true, threadCount);
                });
            } else {
                VScobolWorkspaceScanner.processAllFilesInWorkspaceOutOfProcess(VSExternalFeatures, settings, true, false, -1);
            }
        });
    }));
}
