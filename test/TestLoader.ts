import * as fs from "fs";
import * as path from "path";

const completionMarkerPattern = /\|<<<</g;

export function loadMultiFileTest(absPath: string) {
    const splitOn = /\/\/\/\/.*?\r?\n/gy;
    const namePattern = /@file="(.*)"/;
    const fullLine = /(?!\/\/\/\/).*?\r?\n/gy;
    
    const sourceText = fs.readFileSync(absPath).toString();
    let fileBoundary : RegExpExecArray | null;

    const result : {[absPath: string]: string} = {};
    while (fileBoundary = splitOn.exec(sourceText)) {
        const nameMatch = namePattern.exec(fileBoundary[0]);
        if (!nameMatch?.[1]) throw("bad name boundary?");

        const sourceBuilder : string[] = [];

        let lineMatch : RegExpExecArray | null;
        fullLine.lastIndex = splitOn.lastIndex;
        while (lineMatch = fullLine.exec(sourceText)) {
            sourceBuilder.push(lineMatch[0]);
        }

        const source = sourceBuilder.join("");
        result[nameMatch[1]] = source;
        splitOn.lastIndex = source.length + splitOn.lastIndex; // fullLine.lastIndex;
    }

    return result;
}

export function loadCompletionAtTest(absPath: string) {
    let sourceText = fs.readFileSync(absPath).toString();
    return loadCompletionAtTestFromSource(sourceText);
}

// trigger character?
export function loadCompletionAtTestFromSource(sourceText: string) {
    let i = 0;
    let match : RegExpExecArray | null = null;
    let matchIndex : number | null = null;

    while (match = completionMarkerPattern.exec(sourceText)) {
        if (i > 0) {
            throw "expected only one completions marker";
        }
        matchIndex = match.index;
        sourceText = sourceText.slice(0, matchIndex) + sourceText.slice(matchIndex + match[0].length); // "x.|<<<<y" becomes "x.y"
    }

    if (matchIndex === null) {
        throw "expected a completions marker";
    }

    return {
        index: matchIndex,
        sourceText: sourceText,
    }
}