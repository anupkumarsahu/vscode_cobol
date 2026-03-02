// import { VSExternalFeatures } from "../features/runtime/vsexternalfeatures";

export interface IKnownApis {
	url: string;
	name: string;
	apis: Map<string, string[]>;
	examples: Map<string, string[]>;
	snippets: Map<string, string[]>;
}

export class CallTarget {
	public api: string;
	public url: string;
	public apiGroup: string;
	public description: string[];
	public example: string[];
	public snippet: string[];

	constructor(_name: string, _url: string, _api: string, _description: string[], _example: string[], _snippet: string[]) {
		this.api = _api;
		this.apiGroup = _name;
		this.url = _url;
		this.description = _description;
		// if (this.description.length === 0) {
		// 	VSExternalFeatures.logMessage(`INFO: Missing description for ${_api}`);
		// }
		this.example = _example;
		this.snippet = _snippet;
	}
}

const emptyMap = new Map<string, CallTarget>();
const callTargets_cobol = new Map<string, CallTarget>();

export class KnownAPIs {
	// /* inline decl */
	public static getCallTarget(language: string, api: string): CallTarget | undefined {
		switch (language) {
			case "COBOL": return callTargets_cobol.get(api);
		}
		return undefined;
	}

	public static getCallTargetMap(language: string): Map<string, CallTarget> {
		switch (language) {
			case "COBOL": return callTargets_cobol;
		}
		return emptyMap;
	}
}
