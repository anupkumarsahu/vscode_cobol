import { CompletionItemProvider, TextDocument, Position, CancellationToken, CompletionItem, CompletionContext, ProviderResult, CompletionList, CompletionItemKind, Range } from "vscode";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { ICOBOLSettings, intellisenseStyle } from "./iconfiguration";
import { COBOLToken, COBOLVariable, SourceScannerUtils } from "./cobolsourcescanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import TrieSearch from "trie-search";
import { VSLogger } from "./vslogger";
import { InMemoryGlobalSymbolCache } from "./globalcachehelper";
import { IExternalFeatures } from "./externalfeatures";
import { VSCustomIntelliseRules } from "./vscustomrules";
import { VSExternalFeatures } from "./vsexternalfeatures";
import { ICOBOLSourceScanner } from "./icobolsourcescanner";

export class CobolSourceCompletionItemProvider implements CompletionItemProvider {
    private features: IExternalFeatures;

    public constructor(features: IExternalFeatures) {
        this.features = features;
    }

    private newCompletionItem(token:COBOLToken, kind:CompletionItemKind):CompletionItem {
        const ci = new CompletionItem(token.tokenName, kind);
        ci.detail = token.description;
        return ci;
    }

    private getPerformTargets(iconfig: ICOBOLSettings, document: TextDocument): TrieSearch {
        const sf: ICOBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document, iconfig);

        if (sf !== undefined) {
            if (sf.cache4PerformTargets === undefined) {
                sf.cache4PerformTargets = new TrieSearch("tokenName");
                const words = sf.cache4PerformTargets;

                // eslint-disable-next-line @typescript-eslint/no-unused-vars
                for (const [, token] of sf.sections) {
                    if (token.inProcedureDivision) {
                        words.add(token);
                    }
                }
                // eslint-disable-next-line @typescript-eslint/no-unused-vars
                for (const [, token] of sf.paragraphs) {
                    if (token.inProcedureDivision) {
                        words.add(token);
                    }
                }
                return words;
            }
            else {
                return sf.cache4PerformTargets;
            }
        }

        return new TrieSearch("tokenName");
    }

    private getALlPerformTargets(document: TextDocument, settings: ICOBOLSettings): CompletionItem[] {
        const sf = VSCOBOLSourceScanner.getCachedObject(document, settings);
        const items: CompletionItem[] = [];

        if (sf !== undefined) {
            // eslint-disable-next-line @typescript-eslint/no-unused-vars
            for (const [, token] of sf.sections) {
                if (token.inProcedureDivision) {
                    items.push(this.newCompletionItem(token, CompletionItemKind.Method));
                }
            }
            // eslint-disable-next-line @typescript-eslint/no-unused-vars
            for (const [, token] of sf.paragraphs) {
                if (token.inProcedureDivision) {
                    items.push(this.newCompletionItem(token, CompletionItemKind.Method));
                }
            }
        }

        return items;
    }

    private getConstantsOrVariables(document: TextDocument, settings: ICOBOLSettings, filterOnGroups: boolean): TrieSearch {
        const sf = VSCOBOLSourceScanner.getCachedObject(document, settings);

        if (sf !== undefined) {
            if (sf.cache4ConstantsOrVars === undefined) {
                sf.cache4PerformTargets = new TrieSearch("tokenName");
                const words: TrieSearch = sf.cache4PerformTargets;

                for (const key of sf.constantsOrVariables.keys()) {
                    const variables: COBOLVariable[] | undefined = sf.constantsOrVariables.get(key);
                    if (variables !== undefined) {
                        for (const variable of variables) {
                            const token = variable.token;
                            if (token.tokenNameLower === "filler") {
                                continue;
                            }
                            if (filterOnGroups && token.extraInformation1.indexOf("GROUP") === -1) {
                                continue
                            }
                            words.add(token);
                        }
                    }
                }
                return words;
            } else {
                return sf.cache4ConstantsOrVars;
            }
        }

        return new TrieSearch("tokenName");
    }


    private getItemsFromList(iconfig: ICOBOLSettings, tsearch: TrieSearch, wordToComplete: string, kind: CompletionItemKind): CompletionItem[] {
        const limit: number = iconfig.intellisense_item_limit;

        const words: COBOLToken[] = tsearch.get(wordToComplete);
        const numberOfWordsInResults = words.length;

        const workMap = new Map<string, string>();

        const items: CompletionItem[] = [];
        for (let c = 0; c < numberOfWordsInResults; c++) {

            //if the text is uppercase, the present the items as uppercase
            const key: COBOLToken = words[c];
            const retKeys = [];

            if (workMap.has(key.tokenNameLower)) {
                continue;
            }
            workMap.set(key.tokenNameLower, key.tokenNameLower);

            const orgKey = key.tokenName;
            const iStyle = VSCustomIntelliseRules.Default.findCustomIStyle(iconfig,key.tokenNameLower, iconfig.intellisense_style);
            switch (iStyle) {
                case intellisenseStyle.Unchanged:
                    retKeys.push((key.tokenName));
                    break;
                case intellisenseStyle.LowerCase:
                    if (key.tokenName !== key.tokenNameLower) {
                        retKeys.push(key.tokenNameLower);
                    }
                    break;
                case intellisenseStyle.UpperCase:
                    if (key.tokenName !== key.tokenName.toUpperCase()) {
                        retKeys.push(key.tokenName.toUpperCase());
                    }
                    break;
                case intellisenseStyle.CamelCase:
                    retKeys.push(SourceScannerUtils.camelize(key.tokenName));
                    break;
            }
            
            const uniqueRetKeys = retKeys.filter(function (elem, index, self) {
                return index === self.indexOf(elem);
            });

            for (const uniqueRetKey of uniqueRetKeys) {
                const ci = new CompletionItem(uniqueRetKey, kind);
                ci.detail = orgKey;
                items.push(ci);
            }

            if (items.length >= limit) {
                return items;
            }
        }

        // logMessage("Search for [" + wordToComplete + "] gives " + items.length + " words");
        return items;

    }

    private getAllCopyBook(includeQuoted: boolean): CompletionItem[] {
        const items: CompletionItem[] = [];
        const mapOfCopybooks = new Map<string, string>();

        for (const [encodedKey,] of InMemoryGlobalSymbolCache.knownCopybooks) {
            const copybook = encodedKey.split(",")[0];
            if (mapOfCopybooks.has(copybook) === false) {
                mapOfCopybooks.set(copybook, copybook);
            }
        }

        // include default copybooks
        for (const [copybook,] of InMemoryGlobalSymbolCache.defaultCopybooks) {
            if (mapOfCopybooks.has(copybook) === false) {
                mapOfCopybooks.set(copybook, copybook);
            }
        }
        
        for (const [copybook] of mapOfCopybooks) {
            items.push(new CompletionItem(copybook, CompletionItemKind.File));
            if (includeQuoted) {
                items.push(new CompletionItem(`"${copybook}"`, CompletionItemKind.File));
                items.push(new CompletionItem(`'${copybook}'`, CompletionItemKind.File));
            }
        }
        return items;
    }

    private getAllTypes(workToComplete: string): CompletionItem[] {
        const startsWith = workToComplete.length !== 0;
        const items: CompletionItem[] = [];
        const itemMap = new Map<string, CompletionItem>();

        for (const [type] of InMemoryGlobalSymbolCache.types) {
            if (itemMap.has(type) === false) {
                continue;
            }

            if (startsWith) {
                if (type.startsWith(workToComplete)) {
                    items.push(new CompletionItem(type, CompletionItemKind.Class));
                }
            } else {
                items.push(new CompletionItem(type, CompletionItemKind.Class));
            }
        }

        for (const [type] of InMemoryGlobalSymbolCache.interfaces) {
            if (itemMap.has(type) === false) {
                continue;
            }

            if (startsWith) {
                if (type.startsWith(workToComplete)) {
                    items.push(new CompletionItem(type, CompletionItemKind.Interface));
                }
            } else {
                items.push(new CompletionItem(type, CompletionItemKind.Interface));
            }

        }

        for (const [type] of InMemoryGlobalSymbolCache.enums) {
            if (itemMap.has(type) === false) {
                continue;
            }
            if (startsWith) {
                if (type.startsWith(workToComplete)) {
                    items.push(new CompletionItem(type, CompletionItemKind.Enum));
                }
            } else {
                items.push(new CompletionItem(type, CompletionItemKind.Enum));
            }
        }

        return items;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public provideCompletionItems(document: TextDocument, position: Position, token: CancellationToken, context: CompletionContext): ProviderResult<CompletionItem[] | CompletionList> {
        let items: CompletionItem[] = [];

        const iconfig: ICOBOLSettings = VSCOBOLConfiguration.get_resource_settings(document, VSExternalFeatures);

        if (iconfig.enable_data_provider === false) {
            return new CompletionList(items, false);
        }

        const startTime = this.features.performance_now();
        let wordToComplete = "";
        let wordBefore = "";
        let wordBeforeLower = "";
        let currentLine: string = document.lineAt(position.line).text.trimStart();
        let listComplete = false;

        const range = document.getWordRangeAtPosition(position);
        if (range) {
            wordToComplete = document.getText(new Range(range.start, position)); 
            wordBefore = document.getText(new Range(new Position(range.start.line, 0), new Position(position.line, position.character - wordToComplete.length))).trimEnd();
            const lastSpace = wordBefore.lastIndexOf(" ");
            if (lastSpace !== -1) {
                const lineOrg = wordBefore;
                wordBefore = wordBefore.substring(1 + lastSpace);
                wordBeforeLower = wordBefore.toLowerCase();
                if (wordBeforeLower === "to") {
                    if (lineOrg.toLowerCase().indexOf("go") !== -1) {
                        wordBefore = wordBeforeLower = "goto";
                    }
                }
            }
        } else {
            let lastSpace = currentLine.lastIndexOf(" ");
            if (lastSpace === -1) {
                wordBefore = currentLine;
            } else {
                if (currentLine.endsWith(" ")) {
                    currentLine = currentLine.trimEnd();
                    lastSpace = currentLine.lastIndexOf(" ");
                    if (lastSpace === -1) {
                        wordBefore = currentLine;
                    } else {
                        wordBefore = currentLine.substring(1 + lastSpace);
                    }
                } else {
                    const lastSpaceLine = currentLine.substring(0, lastSpace);
                    let prevLastSpace = lastSpaceLine.lastIndexOf(" ");
                    if (prevLastSpace === -1) {
                        prevLastSpace = 0;
                    }
                    const l = lastSpace - prevLastSpace;
                    wordBefore = currentLine.substr(prevLastSpace, l).trim();
                    wordToComplete = currentLine.substring(1 + lastSpace).trim();
                }
            }
            wordBeforeLower = wordBefore.toLowerCase();
        }

        if (wordBefore.length !== 0) {
            if ((wordBeforeLower.startsWith("'") && wordBeforeLower.endsWith("'")) ||
                (wordBeforeLower.startsWith("\"") && wordBeforeLower.endsWith("\""))
            ) {
                if (currentLine.toLowerCase().indexOf("call") !== -1) {
                    items = this.getCallTargets();
                    listComplete = true;
                }
                if (currentLine.toLowerCase().indexOf("cancel") !== -1) {
                    items = this.getCallTargets();
                    listComplete = true;
                }
                if (currentLine.toLowerCase().indexOf("copy") !== -1) {
                    items = this.getAllCopyBook(false);
                    listComplete = true;
                }
            }

            if (items.length === 0) {
                switch (wordBeforeLower) {
                    case "copy":
                        items = this.getAllCopyBook(true);
                        listComplete = true;
                        break;
                    case "type": {
                        items = this.getAllTypes(wordToComplete);
                        listComplete = true;
                        break;
                    }
                    case "thru":
                    case "through":
                    case "perform":
                    case "goto":
                        {
                            if (wordToComplete.length === 0) {
                                items = this.getALlPerformTargets(document, iconfig);
                                listComplete = true;
                            } else {
                                const words = this.getPerformTargets(iconfig, document);
                                items = this.getItemsFromList(iconfig, words, wordToComplete, CompletionItemKind.Method);
                                listComplete = false;
                            }
                            break;
                        }

                    case "move":
                        {
                            const words = this.getConstantsOrVariables(document, iconfig, false);

                            // TODO:
                            //
                            // if (this.iconfig.intellisense_include_uppercase &&
                            //     words.indexOf("SPACE") === -1) {
                            //     words.push("SPACE");
                            //     words.push("SPACES");
                            //     words.push("LOW-VALUES");
                            //     words.push("HIGH-VALUES");
                            // }

                            // if (words.hasWord("space") === false) {
                            //     words.addWord("space");
                            //     words.addWord("spaces");
                            //     words.addWord("low-values");
                            //     words.addWord("high-values");
                            // }

                            items = this.getItemsFromList(iconfig, words, wordToComplete, CompletionItemKind.Variable);
                            listComplete = false;
                            break;
                        }


                    case "add":
                    case "ascending":
                    case "compute":
                    case "corr":
                    case "corresponding":
                    case "descending":
                    case "divide":
                    case "from":
                    case "giving":
                    case "if":
                    case "initialize":
                    case "inspect":
                    case "into":
                    case "key":
                    case "multiply":
                    case "named":
                    case "pointer-32":
                    case "pointer":
                    case "search":
                    case "set":
                    case "string":
                    case "subtract":
                    case "to":
                    case "unstring":
                    case "varying":
                    case "with":
                    case "display":
                    case "value":
                    case "values":
                    case "content":
                    case "reference":
                        {
                            if (wordToComplete.length === 0) {
                                items = this.getAllConstantsOrVariables(document, iconfig, false);
                                listComplete = true;
                            } else {
                                const words = this.getConstantsOrVariables(document, iconfig, false);
                                items = this.getItemsFromList(iconfig, words, wordToComplete, CompletionItemKind.Variable);
                                listComplete = false;
                            }
                        }
                        break;
                    case "of":
                        {
                            if (wordToComplete.length === 0) {
                                items = this.getAllConstantsOrVariables(document, iconfig, true);
                                listComplete = true;
                            } else {
                                const words = this.getConstantsOrVariables(document, iconfig, true);
                                items = this.getItemsFromList(iconfig, words, wordToComplete, CompletionItemKind.Variable);
                                listComplete = false;
                            }
                        }
                        break;
                }
            }
        }

        const totalTimeInMS = this.features.performance_now() - startTime;
        const timeTaken = totalTimeInMS.toFixed(2);
        if (totalTimeInMS > VSLogger.logTimeThreshold) {
            VSLogger.logMessage(" - CobolSourceCompletionItemProvider took " + timeTaken + " ms");
        }

        if (items.length === 0 && wordBefore !== "$" && wordBefore !== ">>") {
            if (iconfig.suggest_variables_when_context_is_unknown) {
                items = this.getAllConstantsOrVariables(document, iconfig, false);
                listComplete = false;
            }
        }
        return Promise.resolve(new CompletionList(items, listComplete));
    }

    private getAllConstantsOrVariables(document: TextDocument, iconfig: ICOBOLSettings, filterOnGroups: boolean): CompletionItem[] {
        const sf = VSCOBOLSourceScanner.getCachedObject(document, iconfig);
        const wordMap = new Map<string, CompletionItem>();
        if (sf !== undefined) {

            for (const key of sf.constantsOrVariables.keys()) {
                const variables: COBOLVariable[] | undefined = sf.constantsOrVariables.get(key);
                if (variables !== undefined) {
                    for (const variable of variables) {
                        const token = variable.token;
                        if (token.tokenNameLower === "filler") {
                            continue;
                        }
                        if (filterOnGroups && token.extraInformation1.indexOf("GROUP") === -1) {
                            continue
                        }
                        
                        if (wordMap.has(token.tokenName) === false) {
                            wordMap.set(token.tokenName,this.newCompletionItem(token, CompletionItemKind.Variable));
                        }
                    }
                }
            }
        }

        // convert to []
        const words: CompletionItem[] = [];
        for(const [,b] of wordMap) {
            words.push(b);
        }
        return words;
    }

    private getCallTargets(): CompletionItem[] {
        const targets: CompletionItem[] = [];

        for (const [i] of InMemoryGlobalSymbolCache.callableSymbols.entries()) {
            targets.push(new CompletionItem(`${i}`, CompletionItemKind.Function));
        }

        for (const [i] of InMemoryGlobalSymbolCache.entryPoints.entries()) {
            targets.push(new CompletionItem(`${i}`, CompletionItemKind.Function));
        }

        return targets;
    }
}
