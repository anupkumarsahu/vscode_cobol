/** Primary language id used by the extension. */
export const COBOL_LANGUAGE_ID = "COBOL";
/** Language id used for NonStop/Tandem COBOL files. */
export const TANDEM_LANGUAGE_ID = "COBOL_TANDEM";

/** Supported COBOL language ids recognized by provider registration logic. */
export const SUPPORTED_COBOL_LANGUAGE_IDS: readonly string[] = [
    COBOL_LANGUAGE_ID,
    TANDEM_LANGUAGE_ID
];
