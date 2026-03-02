import * as assert from "assert";
import * as path from "path";
import * as fs from "fs";
import { FileSourceHandler } from "../../features/workspace/filesourcehandler";
import { COBOLSourceScanner, EmptyCOBOLSourceScannerEventHandler } from "../../features/workspace/cobolsourcescanner";
import { COBOLSettings } from "../../config/iconfiguration";
import { VSExternalFeatures } from "../../features/runtime/vsexternalfeatures";
import { COBOLTokenStyle } from "../../features/workspace/cobolsourcescanner";

suite("Library Navigation Test Suite", () => {
	const baseForSource = path.join(__dirname, "../../../src/test/suite/");
	const eventHandler = EmptyCOBOLSourceScannerEventHandler.Default;
	const features = VSExternalFeatures;
	const settings = new COBOLSettings();

	// Helper to create temp file for testing
	function createTestFile(filename: string, content: string): string {
		const filepath = path.join(baseForSource, filename);
		fs.writeFileSync(filepath, content, "utf8");
		return filepath;
	}

	// Helper to cleanup test files
	function deleteTestFile(filepath: string): void {
		if (fs.existsSync(filepath)) {
			fs.unlinkSync(filepath);
		}
	}

	test("Parse COPY statement with library name (IN syntax)", () => {
		const testCode = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY COPYLIB-STANDARD-DATA IN B30QALIB.
       PROCEDURE DIVISION.
           STOP RUN.
`;
		const testFile = createTestFile("test-copy-in.cbl", testCode);
		const handler = new FileSourceHandler(settings, undefined, testFile, features);
		const scanner = COBOLSourceScanner.ScanUncached(handler, settings, false, eventHandler, features);

		// Verify library tokens were created
		assert.ok(scanner.copyBookLibraries.size > 0, "Should create library tokens");
		
		const libraryTokens = scanner.copyBookLibraries.get("b30qalib");  // lowercase key
		assert.ok(libraryTokens !== undefined, "Should have tokens for B30QALIB");
		assert.ok(libraryTokens!.length > 0, "Should have at least one library token");
		
		const token = libraryTokens![0];
		assert.strictEqual(token.tokenType, COBOLTokenStyle.CopyBookLib, "Token should be CopyBookLib type");
		assert.strictEqual(token.tokenName, "B30QALIB", "Token name should be B30QALIB");

		deleteTestFile(testFile);
	});

	test("Parse COPY statement with library name (OF syntax)", () => {
		const testCode = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY COPYLIB-DATA OF SYSLIB.
       PROCEDURE DIVISION.
           STOP RUN.
`;
		const testFile = createTestFile("test-copy-of.cbl", testCode);
		const handler = new FileSourceHandler(settings, undefined, testFile, features);
		const scanner = COBOLSourceScanner.ScanUncached(handler, settings, false, eventHandler, features);

		// Verify library tokens were created with OF syntax
		assert.ok(scanner.copyBookLibraries.size > 0, "Should create library tokens with OF syntax");
		
		const libraryTokens = scanner.copyBookLibraries.get("syslib");  // lowercase key
		assert.ok(libraryTokens !== undefined, "Should have tokens for SYSLIB");

		deleteTestFile(testFile);
	});

	test("Parse multiple COPY statements with same library", () => {
		const testCode = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY COPYLIB1 IN B30QALIB.
       COPY COPYLIB2 IN B30QALIB.
       COPY COPYLIB3 IN B30QALIB.
       PROCEDURE DIVISION.
           STOP RUN.
`;
		const testFile = createTestFile("test-copy-multiple.cbl", testCode);
		const handler = new FileSourceHandler(settings, undefined, testFile, features);
		const scanner = COBOLSourceScanner.ScanUncached(handler, settings, false, eventHandler, features);

		// Verify multiple tokens for same library
		const libraryTokens = scanner.copyBookLibraries.get("b30qalib");  // lowercase key
		assert.ok(libraryTokens !== undefined, "Should have tokens for B30QALIB");
		assert.strictEqual(libraryTokens!.length, 3, "Should have three library tokens for B30QALIB");

		deleteTestFile(testFile);
	});

	test("Parse COPY statement without library (no IN/OF)", () => {
		const testCode = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY COPYLIB-DATA.
       PROCEDURE DIVISION.
           STOP RUN.
`;
		const testFile = createTestFile("test-copy-no-lib.cbl", testCode);
		const handler = new FileSourceHandler(settings, undefined, testFile, features);
		const scanner = COBOLSourceScanner.ScanUncached(handler, settings, false, eventHandler, features);

		// Verify no library tokens without IN/OF
		assert.strictEqual(scanner.copyBookLibraries.size, 0, "Should not create library tokens without IN/OF");

		deleteTestFile(testFile);
	});

	test("Verify library token positions are valid", () => {
		const testCode = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY COPYLIB-STANDARD-DATA IN B30QALIB.
       PROCEDURE DIVISION.
           STOP RUN.
`;
		const testFile = createTestFile("test-token-position.cbl", testCode);
		const handler = new FileSourceHandler(settings, undefined, testFile, features);
		const scanner = COBOLSourceScanner.ScanUncached(handler, settings, false, eventHandler, features);

		const libraryTokens = scanner.copyBookLibraries.get("b30qalib");  // lowercase key
		assert.ok(libraryTokens !== undefined, "Should have tokens for B30QALIB");
		
		const token = libraryTokens![0];
		
		// Verify token has valid position information
		assert.ok(token.startLine >= 0, "Token should have valid start line");
		assert.ok(token.startColumn >= 0, "Token should have valid start column");
		assert.ok(token.endLine >= token.startLine, "End line should be >= start line");
		assert.ok(token.endColumn > 0, "Token should have valid end column");

		deleteTestFile(testFile);
	});
});
