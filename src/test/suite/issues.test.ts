import * as assert from "assert";

import * as vscode from "vscode";
import { COBOLOutputChannel, VSLogger } from "../../utils/logger";
import { ICOBOLSettings } from "../../config/IConfiguration";

suite("Issues Raised Test Suite", () => {
	vscode.window.showInformationMessage("Start all tests.");

	test("Logger formats variadic arguments correctly", () => {
		const output: string[] = [];
		const channel = COBOLOutputChannel as unknown as { appendLine: (value: string) => void };
		const originalAppendLine = channel.appendLine;

		const settings = {
			logging_level: ["trace"]
		} as ICOBOLSettings;

		channel.appendLine = (value: string) => {
			output.push(value);
		};

		try {
			VSLogger.logInfo(settings, "value=%s count=%d", "abc", 7);
		} finally {
			channel.appendLine = originalAppendLine;
		}

		assert.ok(output.length > 0, "Expected at least one log entry");
		assert.ok(output[0].includes("value=abc count=7"), "Expected formatted arguments to be expanded");
		assert.ok(!output[0].includes("[ 'abc', 7 ]"), "Expected no array-style formatting in output");
	});
});
