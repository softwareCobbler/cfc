import * as fs from "fs";
import * as path from "path";

const completionMarkerPattern = /\|<<<</g;
export function loadCompletionAtTest(absPath: string) {
    let sourceText = fs.readFileSync(absPath).toString();
    let i = 0;
    let match : RegExpExecArray | null = null;
    let matchIndex : number | null = null;

    while (match = completionMarkerPattern.exec(sourceText)) {
        if (i > 0) {
            throw "expected only one completions marker";
        }
        matchIndex = match.index;
        sourceText = sourceText.slice(0, matchIndex) + sourceText.slice(matchIndex + match[0].length);
    }

    if (matchIndex === null) {
        throw "expected a completions marker";
    }

    return {
        index: matchIndex,
        sourceText: sourceText,
    }
}