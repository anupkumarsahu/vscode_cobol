import { workspace } from "vscode";
import { ICOBOLSettings } from "./IConfiguration";

/**
 * Groups feature toggles derived from user settings and workspace trust.
 */
export class FeatureFlags {
    public static useCobolLineComment(settings: ICOBOLSettings): boolean {
        return settings.line_comment;
    }

    public static enableBuildTaskProvider(): boolean {
        return workspace.isTrusted;
    }

    public static startMetadataCacheProcessing(settings: ICOBOLSettings): boolean {
        return settings.process_metadata_cache_on_start && settings.maintain_metadata_cache;
    }
}
