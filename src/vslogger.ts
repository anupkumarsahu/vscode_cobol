import { OutputChannel, window } from "vscode";
import util from "util";
import { ICOBOLSettings } from "./iconfiguration";

export const COBOLOutputChannel: OutputChannel = window.createOutputChannel("COBOL");

export enum LogLevel {
    Trace = "trace",
    Debug = "debug", 
    Info = "info",
    Warning = "warning",
    Error = "error",
    Fatal = "fatal"
}

export enum LogLevelPriority {
    Trace = 5,
    Debug = 10,
    Info = 20,
    Warning = 30,
    Error = 40,
    Fatal = 50
}

export class VSLogger {
    public static readonly logTimeThreshold = 500;

    // Helper function to get caller information from stack trace
    private static getCallerInfo(): string {
        const originalPrepareStackTrace = Error.prepareStackTrace;
        Error.prepareStackTrace = (_, stack) => stack;
        const stack = new Error().stack as unknown as NodeJS.CallSite[];
        Error.prepareStackTrace = originalPrepareStackTrace;

        // Skip the first few frames to get to the actual caller
        // [0] = getCallerInfo, [1] = logging method, [2] = actual caller
        const caller = stack[3];
        if (caller) {
            const fileName = caller.getFileName();
            const lineNumber = caller.getLineNumber();
            if (fileName && lineNumber) {
                // Extract just the filename from the full path
                const shortFileName = fileName.split(/[/\\]/).pop() || fileName;
                return `[${shortFileName}:${lineNumber}]`;
            }
        }
        return "[unknown]";
    }

    // Enhanced logging function with caller info
    private static logWithLevel(level: string, settings: ICOBOLSettings | null, message: string, ...parameters: any[]): void {
        const callerInfo = VSLogger.getCallerInfo();
        const formattedMessage = (parameters && parameters.length > 0) 
            ? util.format(message, parameters) 
            : message;
        COBOLOutputChannel.appendLine(`${level}: ${callerInfo} ${formattedMessage}`);
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public static logTimedMessage(timeTaken: number, message: string, ...parameters: any[]): boolean {
        const fixedTimeTaken = " (" + timeTaken.toFixed(2) + "ms)";

        if (timeTaken < VSLogger.logTimeThreshold) {
            return false;
        }

        const callerInfo = VSLogger.getCallerInfo();
        if ((parameters !== undefined || parameters !== null) && parameters.length !== 0) {
            const m: string = util.format(message, parameters);
            COBOLOutputChannel.appendLine(`${callerInfo} ${m.padEnd(60)}${fixedTimeTaken}`);
        } else {
            COBOLOutputChannel.appendLine(`${callerInfo} ${message.padEnd(60)}${fixedTimeTaken}`);
        }

        return true;
    }

    public static logChannelSetPreserveFocus(preserveFocus: boolean): void {
        COBOLOutputChannel.show(preserveFocus);
    }


    public static logChannelHide(): void {
        COBOLOutputChannel.hide();
    }


    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public static logWarningMessage(message: string, ...parameters: any[]): void {
        const callerInfo = VSLogger.getCallerInfo();
        const trimmedLeftCount = message.length - message.trimStart().length;
        const spacesToLeft = " ".repeat(trimmedLeftCount);

        // TODO: Could this be colorized?
        if ((parameters !== undefined || parameters !== null) && parameters.length !== 0) {
            COBOLOutputChannel.appendLine(`${spacesToLeft}WARNING: ${callerInfo} ${util.format(message, parameters)}`);
        } else {
            COBOLOutputChannel.appendLine(`${spacesToLeft}WARNING: ${callerInfo} ${message}`);
        }
    }


    public static logException(message: string, ex: Error): void {
        const callerInfo = VSLogger.getCallerInfo();
        COBOLOutputChannel.appendLine(`${callerInfo} ${ex.name}: ${message}`);
        if (ex !== undefined && ex.stack !== undefined) {
            COBOLOutputChannel.appendLine(`${callerInfo} ${ex.stack}`);
        }
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public static logMessage(message: string, ...parameters: any[]): void {
        const callerInfo = VSLogger.getCallerInfo();
        if ((parameters !== undefined || parameters !== null) && parameters.length !== 0) {
            COBOLOutputChannel.appendLine(`${callerInfo} ${util.format(message, parameters)}`);
        } else {
            COBOLOutputChannel.appendLine(`${callerInfo} ${message}`);
        }
    }

    // Enhanced logging methods with precedence-based level filtering and caller info
    // Precedence: TRACE < DEBUG < INFO < WARN < ERROR < FATAL
    // Each level includes all higher priority levels
    private static isLogLevelEnabled(level: LogLevel, settings: ICOBOLSettings): boolean {
        const targetPriority = LogLevelPriority[level.charAt(0).toUpperCase() + level.slice(1) as keyof typeof LogLevelPriority];
        
        // Check if any configured level has equal or lower priority (should be logged)
        return settings.logging_level.some(configuredLevel => {
            const configuredPriority = LogLevelPriority[configuredLevel.charAt(0).toUpperCase() + configuredLevel.slice(1) as keyof typeof LogLevelPriority];
            return configuredPriority <= targetPriority;
        });
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public static logTrace(settings: ICOBOLSettings, message: string, ...parameters: any[]): void {
        if (!VSLogger.isLogLevelEnabled(LogLevel.Trace, settings)) {
            return;
        }
        VSLogger.logWithLevel("TRACE", settings, message, ...parameters);
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public static logDebug(settings: ICOBOLSettings, message: string, ...parameters: any[]): void {
        if (!VSLogger.isLogLevelEnabled(LogLevel.Debug, settings)) {
            return;
        }
        VSLogger.logWithLevel("DEBUG", settings, message, ...parameters);
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public static logInfo(settings: ICOBOLSettings, message: string, ...parameters: any[]): void {
        if (!VSLogger.isLogLevelEnabled(LogLevel.Info, settings)) {
            return;
        }
        VSLogger.logWithLevel("INFO", settings, message, ...parameters);
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public static logWarning(settings: ICOBOLSettings, message: string, ...parameters: any[]): void {
        if (!VSLogger.isLogLevelEnabled(LogLevel.Warning, settings)) {
            return;
        }
        VSLogger.logWithLevel("WARNING", settings, message, ...parameters);
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public static logError(settings: ICOBOLSettings, message: string, ...parameters: any[]): void {
        if (!VSLogger.isLogLevelEnabled(LogLevel.Error, settings)) {
            return;
        }
        VSLogger.logWithLevel("ERROR", settings, message, ...parameters);
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public static logFatal(settings: ICOBOLSettings, message: string, ...parameters: any[]): void {
        if (!VSLogger.isLogLevelEnabled(LogLevel.Fatal, settings)) {
            return;
        }
        VSLogger.logWithLevel("FATAL", settings, message, ...parameters);
    }
}






