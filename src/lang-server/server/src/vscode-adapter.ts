import { Diagnostic as vsDiagnostic, DiagnosticSeverity as vsDiagnosticSeverity } from "vscode-languageserver/node";
import { Position as vsPosition, Range as vsRange } from "vscode-languageserver-types"
import { Diagnostic, DiagnosticKind } from "../../../compiler/node";
import { SourceRange } from "../../../compiler/scanner"
import { AnnotatedCharGetter, ClientAdapter } from "../../../services/clientAdapter";

export interface VsClientAdapter extends ClientAdapter {
    diagnostic: (getAnnotatedChar: AnnotatedCharGetter, diagnostic: Diagnostic) => vsDiagnostic
}

function cfPosToVsLocation(getAnnotatedChar: AnnotatedCharGetter, pos: number) : vsPosition {
    const annotatedChar = getAnnotatedChar(pos);
    return {
        line: annotatedChar.line,
        character: annotatedChar.col
    }
}

function cfRangeToVsRange(getAnnotatedChar: AnnotatedCharGetter, sourceRange: SourceRange) : vsRange {
	const from = getAnnotatedChar(sourceRange.fromInclusive);
	const to = getAnnotatedChar(sourceRange.toExclusive);
	return {
		start: {line: from.line, character: from.col},
		end: {line: to.line, character: to.col}
	}
}

function diagnostic(getAnnotatedChar: AnnotatedCharGetter, diagnostic: Diagnostic) : vsDiagnostic {
    return {
        severity: diagnostic.kind === DiagnosticKind.error ? vsDiagnosticSeverity.Error : vsDiagnosticSeverity.Warning,
        range: {
            start: cfPosToVsLocation(getAnnotatedChar, diagnostic.fromInclusive),
            end: cfPosToVsLocation(getAnnotatedChar, diagnostic.toExclusive)
        },
        message: diagnostic.msg,
        source: "cfls"
    }
}

const VsClientAdapters : Readonly<VsClientAdapter> = Object.freeze({
    diagnostic
});

export default VsClientAdapters;