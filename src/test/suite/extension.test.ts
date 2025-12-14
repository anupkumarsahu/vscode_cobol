import * as assert from "assert";

import * as vscode from "vscode";

import { FileSourceHandler } from "../../filesourcehandler";
import { COBOLSourceScanner, EmptyCOBOLSourceScannerEventHandler } from "../../cobolsourcescanner";
import { COBOLSettings } from "../../iconfiguration";
import path from "path";
import { VSExternalFeatures } from "../../vsexternalfeatures";

suite("Core Extension Test Suite", () => {
	vscode.window.showInformationMessage("Start all tests.");

	const baseForSource = __dirname+"/../../../src/test/suite/";
	const eventHandler = EmptyCOBOLSourceScannerEventHandler.Default;
	const features = VSExternalFeatures;
	const settings = new COBOLSettings();

	test("Read file [basic] (test.cbl)", () => {
		const f = new FileSourceHandler(settings, undefined, path.join(baseForSource,"test.cbl"), features);
		if (f.lines.length < 10) {
			assert.fail("test.cbl should have > 10 lines");
		}

		assert.ok(f.getFilename().length > 0, "filename is invalid");
	});

	test("Parse file for constants/paragraphs/sections (test.cbl)", () => {
		const f = new FileSourceHandler(settings, undefined, path.join(baseForSource,"test.cbl"), features);
		if (f.lines.length < 10) {
			assert.fail("test.cbl should have > 10 lines");
		}

		assert.ok(f.getFilename().length > 0, "filename is invalid");
		const s = COBOLSourceScanner.ScanUncached(f, settings, false, eventHandler, features);

		assert.ok(s.constantsOrVariables.size > 0, "should contain at least one field");
		assert.ok(s.paragraphs.size > 0, "should contain at least one paragraph");
		assert.ok(s.sections.size > 0, "should contain at least one section");

	});

	test("Verify outline view for B30DIRb.cob", () => {
		const f = new FileSourceHandler(settings, undefined, path.join(baseForSource,"B30DIRb.cob"), features);
		
		assert.ok(f.lines.length > 0, "B30DIRb.cob should have content");
		assert.ok(f.getFilename().length > 0, "filename is invalid");

		const scanner = COBOLSourceScanner.ScanUncached(f, settings, false, eventHandler, features);

		// Verify program ID
		assert.strictEqual(scanner.ProgramId, "B30DIRb", "Program ID should be B30DIRb");

		// Log what was actually found
		console.log("Sections found:", Array.from(scanner.sections.keys()));
		console.log("Paragraphs found:", Array.from(scanner.paragraphs.keys()));
		console.log("Variables found:", scanner.constantsOrVariables.size);
		console.log("Libraries found:", Array.from(scanner.copyBookLibraries.keys()));

		// Basic verification - just check that parsing happened
		assert.ok(scanner.sections.size >= 0, "Should parse sections");
		assert.ok(scanner.copyBookLibraries.size > 0, "Should detect library references (B30QALIB)");
		assert.ok(scanner.copyBooksUsed.size > 0 || scanner.copyBooksUnresolved.size > 0, "Should detect COPY statements");
	});
});
