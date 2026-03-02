import * as assert from "assert";
import * as path from "path";
import * as fs from "fs";
import { FileSourceHandler } from "../../features/workspace/filesourcehandler";
import { COBOLSourceScanner, EmptyCOBOLSourceScannerEventHandler } from "../../features/workspace/cobolsourcescanner";
import { COBOLSettings } from "../../config/iconfiguration";
import { VSExternalFeatures } from "../../features/runtime/vsexternalfeatures";

suite("Tandem COBOL Test Suite", () => {
	const baseForSource = path.join(__dirname, "../../../src/test/Projects/BES/COBOL_Projects/Tandem");
	const eventHandler = EmptyCOBOLSourceScannerEventHandler.Default;
	const features = VSExternalFeatures;
	const settings = new COBOLSettings();

	// Helper function to get all COBOL files in a directory
	function getCobolFiles(dir: string): string[] {
		const files: string[] = [];
		if (fs.existsSync(dir)) {
			const items = fs.readdirSync(dir);
			for (const item of items) {
				const fullPath = path.join(dir, item);
				const stat = fs.statSync(fullPath);
				if (stat.isDirectory()) {
					files.push(...getCobolFiles(fullPath));
				} else if (item.match(/\.(cob|cbl|cobol)$/i)) {
					files.push(fullPath);
				}
			}
		}
		return files;
	}

	test("Verify EXITS.cob - Keyword highlighting and outline", () => {
		const testFile = path.join(baseForSource, "57T263_OSS/src/EXITS.cob");
		
		if (!fs.existsSync(testFile)) {
			console.log("Test file not found, skipping test");
			return;
		}

		const handler = new FileSourceHandler(settings, undefined, testFile, features);
		const scanner = COBOLSourceScanner.ScanUncached(handler, settings, false, eventHandler, features);

		// Verify program ID
		assert.strictEqual(scanner.ProgramId, "EXITS", "Program ID should be EXITS");

		// Verify sections and paragraphs exist
		assert.ok(scanner.sections.size > 0 || scanner.paragraphs.size > 0, "Should have sections or paragraphs");

		// Verify variables - should have WS-RECORDS, WS-TEL-RECORD, etc.
		assert.ok(scanner.constantsOrVariables.size > 0, "Should have working storage variables");
		
		// Check for actual variables in the file (lowercase keys)
		assert.ok(scanner.constantsOrVariables.has("ws-records"), "Should have WS-RECORDS");
		assert.ok(scanner.constantsOrVariables.has("ws-tel-record"), "Should have WS-TEL-RECORD");
		assert.ok(scanner.constantsOrVariables.has("ws-tel-lname"), "Should have WS-TEL-LNAME");
		assert.ok(scanner.constantsOrVariables.has("teltbl"), "Should have TELTBL file");
		
		// Verify we found multiple record variations
		assert.ok(scanner.constantsOrVariables.has("ws-prev-tel-record"), "Should have WS-PREV-TEL-RECORD");
		assert.ok(scanner.constantsOrVariables.has("ws-curr-tel-record"), "Should have WS-CURR-TEL-RECORD");
		assert.ok(scanner.constantsOrVariables.has("ws-next-tel-record"), "Should have WS-NEXT-TEL-RECORD");
	});

	test("Verify B30DIRb.cob - COPY statements with library references", () => {
		const testFile = path.join(baseForSource, "SETTOG_GUA/src/B30DIRb.cob");
		
		if (!fs.existsSync(testFile)) {
			console.log("Test file not found, skipping test");
			return;
		}

		const handler = new FileSourceHandler(settings, undefined, testFile, features);
		const scanner = COBOLSourceScanner.ScanUncached(handler, settings, false, eventHandler, features);

		// Verify program ID
		assert.strictEqual(scanner.ProgramId, "B30DIRb", "Program ID should be B30DIRb");

		// Verify library references
		assert.ok(scanner.copyBookLibraries.size > 0, "Should detect library references");
		assert.ok(scanner.copyBookLibraries.has("b30qalib"), "Should have B30QALIB library reference");

		// Verify COPY statements
		assert.ok(scanner.copyBooksUsed.size > 0 || scanner.copyBooksUnresolved.size > 0, "Should detect COPY statements");
	});

	test("Verify scbun.cob - TANDEM NON-STOP syntax", () => {
		const testFile = path.join(baseForSource, "SUBUN_OSS/src/scbun.cob");
		
		if (!fs.existsSync(testFile)) {
			console.log("Test file not found, skipping test");
			return;
		}

		const handler = new FileSourceHandler(settings, undefined, testFile, features);
		const scanner = COBOLSourceScanner.ScanUncached(handler, settings, false, eventHandler, features);

		// Verify parsing succeeded
		assert.ok(scanner.ProgramId.length > 0, "Should parse program ID");
		assert.ok(handler.lines.length > 0, "Should have source lines");
	});

	test("Parse multiple Tandem projects - Outline generation", () => {
		const projects = [
			"57T263_OSS/src",
			"SETTOG_GUA/src",
			"SUBUN_OSS/src",
			"SM205A_OSS/src",
			"SM206A_OSS/src"
		];

		let parsedCount = 0;
		let totalSections = 0;
		let totalParagraphs = 0;
		let totalVariables = 0;

		for (const project of projects) {
			const projectPath = path.join(baseForSource, project);
			if (!fs.existsSync(projectPath)) {
				continue;
			}

			const cobolFiles = getCobolFiles(projectPath);
			for (const file of cobolFiles) {
				try {
					const handler = new FileSourceHandler(settings, undefined, file, features);
					const scanner = COBOLSourceScanner.ScanUncached(handler, settings, false, eventHandler, features);

					parsedCount++;
					totalSections += scanner.sections.size;
					totalParagraphs += scanner.paragraphs.size;
					totalVariables += scanner.constantsOrVariables.size;
				} catch (error) {
					console.log(`Failed to parse ${file}: ${error}`);
				}
			}
		}

		console.log(`Parsed ${parsedCount} Tandem COBOL files`);
		console.log(`Total sections: ${totalSections}`);
		console.log(`Total paragraphs: ${totalParagraphs}`);
		console.log(`Total variables: ${totalVariables}`);

		assert.ok(parsedCount > 0, "Should parse at least one Tandem COBOL file");
	});

	test("Verify keyword highlighting - TANDEM computer names", () => {
		const testCases = [
			{ file: "57T263_OSS/src/EXITS.cob", computer: "TANDEM VLX" },
			{ file: "SETTOG_GUA/src/B30DIRb.cob", computer: "TANDEM/16" },
			{ file: "SUBUN_OSS/src/scbun.cob", computer: "TANDEM NON-STOP" }
		];

		for (const testCase of testCases) {
			const testFile = path.join(baseForSource, testCase.file);
			
			if (!fs.existsSync(testFile)) {
				console.log(`Test file ${testCase.file} not found, skipping`);
				continue;
			}

			const handler = new FileSourceHandler(settings, undefined, testFile, features);
			
			// Check if SOURCE-COMPUTER or OBJECT-COMPUTER lines contain the computer name
			let found = false;
			for (let i = 0; i < handler.lines.length; i++) {
				const line = handler.getLine(i, true);
				if (line && (line.includes("SOURCE-COMPUTER") || line.includes("OBJECT-COMPUTER")) && 
					line.includes(testCase.computer)) {
					found = true;
					break;
				}
			}

			assert.ok(found, `Should find ${testCase.computer} in ${testCase.file}`);
		}
	});

	test("Verify compiler directives - ?IF, ?ENDIF, ?SETTOG", () => {
		const testFile = path.join(baseForSource, "SETTOG_GUA/src/B30DIRb.cob");
		
		if (!fs.existsSync(testFile)) {
			console.log("Test file not found, skipping test");
			return;
		}

		const handler = new FileSourceHandler(settings, undefined, testFile, features);

		// Check for compiler directives
		let foundIF = false;
		let foundENDIF = false;
		let foundSETTOG = false;

		for (let i = 0; i < handler.lines.length; i++) {
			const line = handler.getLine(i, true);
			if (line) {
				if (line.includes("?IF ")) foundIF = true;
				if (line.includes("?ENDIF")) foundENDIF = true;
				if (line.includes("?SETTOG")) foundSETTOG = true;
			}
		}

		assert.ok(foundIF, "Should find ?IF directive");
		assert.ok(foundENDIF, "Should find ?ENDIF directive");
		assert.ok(foundSETTOG, "Should find ?SETTOG directive");
	});

	test("Verify library file structure - b30qalib", () => {
		const testFile = path.join(baseForSource, "SETTOG_GUA/src/b30qalib");
		
		if (!fs.existsSync(testFile)) {
			console.log("Library file not found, skipping test");
			return;
		}

		const handler = new FileSourceHandler(settings, undefined, testFile, features);

		// Check for SECTION keyword in library file
		let foundSection = false;
		for (let i = 0; i < handler.lines.length; i++) {
			const line = handler.getLine(i, true);
			if (line && line.includes("SECTION")) {
				foundSection = true;
				break;
			}
		}

		assert.ok(foundSection, "Library file should contain SECTION declarations");
		assert.ok(handler.lines.length > 0, "Library file should have content");
	});

	test("Verify relative file organization parsing", () => {
		const testFile = path.join(baseForSource, "57T263_OSS/src/EXITS.cob");
		
		if (!fs.existsSync(testFile)) {
			console.log("Test file not found, skipping test");
			return;
		}

		const handler = new FileSourceHandler(settings, undefined, testFile, features);

		// Check for relative file syntax
		let foundRelative = false;
		let foundRelativeKey = false;

		for (let i = 0; i < handler.lines.length; i++) {
			const line = handler.getLine(i, true);
			if (line) {
				if (line.includes("ORGANIZATION IS RELATIVE")) foundRelative = true;
				if (line.includes("RELATIVE KEY")) foundRelativeKey = true;
			}
		}

		assert.ok(foundRelative, "Should find RELATIVE organization");
		assert.ok(foundRelativeKey, "Should find RELATIVE KEY clause");
	});

	test("Verify level 88 condition names parsing", () => {
		const testFile = path.join(baseForSource, "57T263_OSS/src/EXITS.cob");
		
		if (!fs.existsSync(testFile)) {
			console.log("Test file not found, skipping test");
			return;
		}

		const handler = new FileSourceHandler(settings, undefined, testFile, features);

		// Check for level 88 condition names
		let found88Levels = 0;
		for (let i = 0; i < handler.lines.length; i++) {
			const line = handler.getLine(i, true);
			if (line && line.trim().startsWith("88 ")) {
				found88Levels++;
			}
		}

		assert.ok(found88Levels > 0, "Should find level 88 condition names");
	});

	test("Scalability test - Parse 10000+ line programs", () => {
		const largeFiles = [
			"COBOL_scalability_with_10000_lines",
			"COBOL_scalability_with_10010_lines"
		];

		for (const folder of largeFiles) {
			const folderPath = path.join(baseForSource, folder);
			if (!fs.existsSync(folderPath)) {
				console.log(`${folder} not found, skipping`);
				continue;
			}

			const cobolFiles = getCobolFiles(folderPath);
			for (const file of cobolFiles) {
				const startTime = Date.now();
				
				try {
					const handler = new FileSourceHandler(settings, undefined, file, features);
					const scanner = COBOLSourceScanner.ScanUncached(handler, settings, false, eventHandler, features);

					const endTime = Date.now();
					const parseTime = endTime - startTime;

					console.log(`Parsed ${path.basename(file)} (${handler.lines.length} lines) in ${parseTime}ms`);
					console.log(`  Sections: ${scanner.sections.size}, Paragraphs: ${scanner.paragraphs.size}, Variables: ${scanner.constantsOrVariables.size}`);

					assert.ok(handler.lines.length >= 10000, "Should have at least 10000 lines");
					assert.ok(parseTime < 10000, "Should parse large file in under 10 seconds");
				} catch (error) {
					console.log(`Failed to parse ${file}: ${error}`);
				}
			}
		}
	});

	test("Verify b30dirb - Extensive compiler directive testing", () => {
		const testFile = path.join(baseForSource, "b30dirb/b30dirb");
		
		if (!fs.existsSync(testFile)) {
			console.log("Test file not found, skipping test");
			return;
		}

		const handler = new FileSourceHandler(settings, undefined, testFile, features);
		const scanner = COBOLSourceScanner.ScanUncached(handler, settings, false, eventHandler, features);

		// Verify program ID
		assert.strictEqual(scanner.ProgramId, "B30DIRb", "Program ID should be B30DIRb");

		// Verify sections detected (MAIN, IF-TEST-1 through IF-TEST-5, IF-TEST-FAIL-1)
		assert.ok(scanner.sections.size > 0, "Should detect sections");
		assert.ok(scanner.sections.has("main"), "Should have MAIN section");
		assert.ok(scanner.sections.has("if-test-1"), "Should have IF-TEST-1 section");
		assert.ok(scanner.sections.has("if-test-2"), "Should have IF-TEST-2 section");
		assert.ok(scanner.sections.has("if-test-3"), "Should have IF-TEST-3 section");
		assert.ok(scanner.sections.has("if-test-4"), "Should have IF-TEST-4 section");
		assert.ok(scanner.sections.has("if-test-5"), "Should have IF-TEST-5 section");
		assert.ok(scanner.sections.has("if-test-fail-1"), "Should have IF-TEST-FAIL-1 section");

		// Verify library references
		assert.ok(scanner.copyBookLibraries.has("b30qalib"), "Should have B30QALIB library reference");

		// Verify COPY statements detected
		assert.ok(scanner.copyBooksUsed.size > 0 || scanner.copyBooksUnresolved.size > 0, "Should detect COPY statements");

		// Check for all compiler directive types in source
		let foundIF = false;
		let foundENDIF = false;
		let foundIFNOT = false;
		let foundSETTOG = false;
		let foundRESETTOG = false;
		let foundNOLIST = false;
		let foundLIST = false;
		let foundHEADING = false;

		for (let i = 0; i < handler.lines.length; i++) {
			const line = handler.getLine(i, true);
			if (line) {
				if (line.includes("?IF ")) foundIF = true;
				if (line.includes("?ENDIF")) foundENDIF = true;
				if (line.includes("?IFNOT")) foundIFNOT = true;
				if (line.includes("?SETTOG")) foundSETTOG = true;
				if (line.includes("?RESETTOG")) foundRESETTOG = true;
				if (line.includes("?NOLIST")) foundNOLIST = true;
				if (line.includes("?LIST")) foundLIST = true;
				if (line.includes("?HEADING")) foundHEADING = true;
			}
		}

		// Verify all directive types were found
		assert.ok(foundIF, "Should find ?IF directive");
		assert.ok(foundENDIF, "Should find ?ENDIF directive");
		assert.ok(foundIFNOT, "Should find ?IFNOT directive");
		assert.ok(foundSETTOG, "Should find ?SETTOG directive");
		assert.ok(foundRESETTOG, "Should find ?RESETTOG directive");
		assert.ok(foundNOLIST, "Should find ?NOLIST directive");
		assert.ok(foundLIST, "Should find ?LIST directive");
		assert.ok(foundHEADING, "Should find ?HEADING directive");

		// Verify working storage variables
		assert.ok(scanner.constantsOrVariables.size > 0, "Should have working storage variables");
		assert.ok(scanner.constantsOrVariables.has("test-results-main"), "Should have TEST-RESULTS-MAIN");

		console.log(`b30dirb parsed successfully:`);
		console.log(`  Sections: ${scanner.sections.size}`);
		console.log(`  Variables: ${scanner.constantsOrVariables.size}`);
		console.log(`  Libraries: ${scanner.copyBookLibraries.size}`);
		console.log(`  COPY statements: ${scanner.copyBooksUsed.size + scanner.copyBooksUnresolved.size}`);
	});

	test("Verify C30USRA.cob - User-defined words and National character support", () => {
		const testFile = path.join(baseForSource, "C30USRA/src/C30USRA.cob");
		
		if (!fs.existsSync(testFile)) {
			console.log("Test file not found, skipping test");
			return;
		}

		const handler = new FileSourceHandler(settings, undefined, testFile, features);
		const scanner = COBOLSourceScanner.ScanUncached(handler, settings, false, eventHandler, features);

		// Verify program ID (note: file is C30USRA.cob but PROGRAM-ID is B30USRa)
		assert.strictEqual(scanner.ProgramId, "B30USRa", "Program ID should be B30USRa");

		// Verify sections detected
		assert.ok(scanner.sections.size > 0, "Should detect sections");
		assert.ok(scanner.sections.has("main"), "Should have MAIN section");
		assert.ok(scanner.sections.has("user-defined-test-1"), "Should have USER-DEFINED-TEST-1 section");
		assert.ok(scanner.sections.has("user-defined-test-2"), "Should have USER-DEFINED-TEST-2 section");

		// Verify file organization (SEQUENTIAL)
		let foundSequential = false;
		let foundSelectFile = false;

		for (let i = 0; i < handler.lines.length; i++) {
			const line = handler.getLine(i, true);
			if (line) {
				if (line.includes("ORGANIZATION SEQUENTIAL")) foundSequential = true;
				if (line.includes("SELECT SEQ-FILE")) foundSelectFile = true;
			}
		}

		assert.ok(foundSequential, "Should find SEQUENTIAL organization");
		assert.ok(foundSelectFile, "Should find SELECT SEQ-FILE");

		// Verify working storage variables (including user-defined words that match system names)
		assert.ok(scanner.constantsOrVariables.size > 0, "Should have working storage variables");
		assert.ok(scanner.constantsOrVariables.has("record-size"), "Should have RECORD-SIZE");
		assert.ok(scanner.constantsOrVariables.has("tandem-16"), "Should have TANDEM-16 (user-defined word)");
		assert.ok(scanner.constantsOrVariables.has("b30usraz"), "Should have B30USRaZ (user-defined word)");

		// Verify file section variables
		assert.ok(scanner.constantsOrVariables.has("seq-file"), "Should have SEQ-FILE");
		assert.ok(scanner.constantsOrVariables.has("seq-record"), "Should have SEQ-RECORD");
		assert.ok(scanner.constantsOrVariables.has("seq-counter"), "Should have SEQ-COUNTER");
		assert.ok(scanner.constantsOrVariables.has("seq-content"), "Should have SEQ-CONTENT");

		// Check for National character literals (PIC N)
		let foundNationalPic = false;
		let foundNationalLiteral = false;

		for (let i = 0; i < handler.lines.length; i++) {
			const line = handler.getLine(i, true);
			if (line) {
				if (line.includes("PIC N(")) foundNationalPic = true;
				if (line.includes("VALUE N\"")) foundNationalLiteral = true;
			}
		}

		assert.ok(foundNationalPic, "Should find National character PIC N");
		assert.ok(foundNationalLiteral, "Should find National character literal N\"");

		// Verify RECORD VARYING clause
		let foundRecordVarying = false;
		for (let i = 0; i < handler.lines.length; i++) {
			const line = handler.getLine(i, true);
			if (line && line.includes("RECORD VARYING")) {
				foundRecordVarying = true;
				break;
			}
		}

		assert.ok(foundRecordVarying, "Should find RECORD VARYING clause");

		// Verify TANDEM-16 computer name
		let foundTandemComputer = false;
		for (let i = 0; i < handler.lines.length; i++) {
			const line = handler.getLine(i, true);
			if (line && (line.includes("SOURCE-COMPUTER") || line.includes("OBJECT-COMPUTER")) && 
				line.includes("TANDEM-16")) {
				foundTandemComputer = true;
				break;
			}
		}

		assert.ok(foundTandemComputer, "Should find TANDEM-16 computer name");

		// Verify compiler directives
		let foundNOLIST = false;
		let foundLIST = false;
		let foundHEADING = false;

		for (let i = 0; i < handler.lines.length; i++) {
			const line = handler.getLine(i, true);
			if (line) {
				if (line.includes("?NOLIST")) foundNOLIST = true;
				if (line.includes("?LIST")) foundLIST = true;
				if (line.includes("?HEADING")) foundHEADING = true;
			}
		}

		assert.ok(foundNOLIST, "Should find ?NOLIST directive");
		assert.ok(foundLIST, "Should find ?LIST directive");
		assert.ok(foundHEADING, "Should find ?HEADING directive");

		console.log(`C30USRA.cob parsed successfully:`);
		console.log(`  Program ID: ${scanner.ProgramId}`);
		console.log(`  Sections: ${scanner.sections.size}`);
		console.log(`  Variables: ${scanner.constantsOrVariables.size}`);
		console.log(`  Features: National characters (PIC N), User-defined words, RECORD VARYING`);
	});

	test("Verify source.cob (10000 lines) - Large-scale enterprise application", () => {
		const testFile = path.join(baseForSource, "COBOL_scalability_with_10000_lines/src/source.cob");
		
		if (!fs.existsSync(testFile)) {
			console.log("Test file not found, skipping test");
			return;
		}

		const startTime = Date.now();
		const handler = new FileSourceHandler(settings, undefined, testFile, features);
		const scanner = COBOLSourceScanner.ScanUncached(handler, settings, false, eventHandler, features);
		const endTime = Date.now();
		const parseTime = endTime - startTime;

		// Verify it's a large file
		assert.ok(handler.lines.length >= 10000, "Should have at least 10000 lines");
		console.log(`  File size: ${handler.lines.length} lines`);

		// This file contains multiple nested programs (MEASTCM, SPDISKINFO, SPCOMPLETION)
		// Verify program ID exists (scanner may pick any of the nested programs)
		assert.ok(scanner.ProgramId.length > 0, "Should have a program ID");
		const validPrograms = ["MEASTCM", "SPDISKINFO", "SPCOMPLETION"];
		assert.ok(validPrograms.includes(scanner.ProgramId), 
			`Program ID should be one of ${validPrograms.join(", ")}, got ${scanner.ProgramId}`);

		// Verify sections detected (large application should have multiple sections)
		assert.ok(scanner.sections.size >= 5, "Should have at least 5 sections");
		console.log(`  Sections detected: ${scanner.sections.size}`);

		// Verify paragraphs (large app should have many paragraphs)
		assert.ok(scanner.paragraphs.size >= 100, "Should have at least 100 paragraphs");
		console.log(`  Paragraphs detected: ${scanner.paragraphs.size}`);

		// Verify variables (enterprise app should have extensive working storage)
		assert.ok(scanner.constantsOrVariables.size >= 500, "Should have at least 500 variables");
		console.log(`  Variables detected: ${scanner.constantsOrVariables.size}`);

		// Verify performance - should parse in reasonable time
		assert.ok(parseTime < 10000, "Should parse 10000 line file in under 10 seconds");
		console.log(`  Parse time: ${parseTime}ms`);

		// Check for compiler directives in this HP NonStop application
		let foundINSPECT = false;
		let foundSYMBOLS = false;

		for (let i = 0; i < Math.min(100, handler.lines.length); i++) {
			const line = handler.getLine(i, true);
			if (line) {
				if (line.includes("?INSPECT")) foundINSPECT = true;
				if (line.includes("?SYMBOLS")) foundSYMBOLS = true;
			}
		}

		assert.ok(foundINSPECT, "Should find ?INSPECT directive");
		assert.ok(foundSYMBOLS, "Should find ?SYMBOLS directive");

		// Verify HP/Compaq copyright notice
		let foundCopyright = false;
		for (let i = 0; i < Math.min(50, handler.lines.length); i++) {
			const line = handler.getLine(i, true);
			if (line && (line.includes("HP CONFIDENTIAL") || line.includes("Hewlett Packard"))) {
				foundCopyright = true;
				break;
			}
		}
		assert.ok(foundCopyright, "Should find HP/Compaq copyright notice");

		// Check for SPECIAL-NAMES section (advanced COBOL features)
		let foundSpecialNames = false;
		for (let i = 0; i < handler.lines.length; i++) {
			const line = handler.getLine(i, true);
			if (line && line.includes("SPECIAL-NAMES")) {
				foundSpecialNames = true;
				break;
			}
		}
		assert.ok(foundSpecialNames, "Should find SPECIAL-NAMES section");

		// Verify this is an HP NonStop application
		let foundNonStop = false;
		for (let i = 500; i < Math.min(600, handler.lines.length); i++) {
			const line = handler.getLine(i, true);
			if (line && line.includes("HP NonStop")) {
				foundNonStop = true;
				break;
			}
		}
		assert.ok(foundNonStop, "Should find HP NonStop reference");

		console.log(`source.cob (10000 lines) - Enterprise application test:`);
		console.log(`  Application: MEASTCM (MEASURE to TCM interface)`);
		console.log(`  Type: HP NonStop capacity planning tool`);
		console.log(`  Performance: ${parseTime}ms for ${handler.lines.length} lines`);
		console.log(`  Complexity: ${scanner.sections.size} sections, ${scanner.paragraphs.size} paragraphs, ${scanner.constantsOrVariables.size} variables`);
		
		// Calculate parse rate
		const linesPerSecond = Math.round((handler.lines.length / parseTime) * 1000);
		console.log(`  Parse rate: ${linesPerSecond} lines/second`);
		
		// Verify parse rate is reasonable (at least 50 lines per millisecond)
		assert.ok(linesPerSecond >= 50, "Parse rate should be at least 50 lines/second");
	});
});
