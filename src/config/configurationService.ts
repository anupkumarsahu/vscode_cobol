import * as vscode from "vscode";
import { ICOBOLSettings } from "./iconfiguration";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { VSExternalFeatures } from "../features/runtime/vsexternalfeatures";

export class ConfigurationService {
    public static reinitializeWorkspace(): ICOBOLSettings {
        return VSCOBOLConfiguration.reinitWorkspaceSettings(VSExternalFeatures);
    }

    public static reinitializeWorkspaceScoped(): void {
        VSCOBOLConfiguration.reinitWorkspaceSettingsScoped(VSExternalFeatures);
    }

    public static getWorkspace(): ICOBOLSettings {
        return VSCOBOLConfiguration.get_workspace_settings();
    }

    public static getResourceSettings(document: vscode.TextDocument): ICOBOLSettings {
        return VSCOBOLConfiguration.get_resource_settings(document, VSExternalFeatures);
    }

    public static clearResourceCache(document: vscode.TextDocument): void {
        VSCOBOLConfiguration.clearResourceCache(document);
    }
}
