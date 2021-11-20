import { CompletionItemKind as vsCompletionItemKind, CompletionItem as vsCompletionItem, Diagnostic as vsDiagnostic, DiagnosticSeverity as vsDiagnosticSeverity } from "vscode-languageserver/node";
import { Position as vsPosition, Range as vsRange, Location as vsLocation } from "vscode-languageserver-types"
import { Diagnostic, DiagnosticKind } from "../../../compiler/node";
import { CompletionItemKind, CompletionItem } from "../../../services/completions";
import { SourceRange } from "../../../compiler/scanner"
import { AnnotatedCharGetter, asClientAdapter } from "../../../services/clientAdapter";
import { AbsPath, exhaustiveCaseGuard } from "../../../compiler/utils";
import { URI } from "vscode-uri";

function cfPositionToVsPosition(getAnnotatedChar: AnnotatedCharGetter, pos: number) : vsPosition {
    const annotatedChar = getAnnotatedChar(pos);
    return {
        line: annotatedChar.line,
        character: annotatedChar.col
    }
}

const nilRangeAsAnnotatedCharLike = {line: 0, col: 0};

function cfRangeToVsRange(getAnnotatedChar: AnnotatedCharGetter, sourceRange: SourceRange) : vsRange {
	const from = sourceRange.isNil() ? nilRangeAsAnnotatedCharLike : getAnnotatedChar(sourceRange.fromInclusive);
	const to = sourceRange.isNil() ? nilRangeAsAnnotatedCharLike : getAnnotatedChar(sourceRange.toExclusive);
	return {
		start: {line: from.line, character: from.col},
		end: {line: to.line, character: to.col}
	}
}

function diagnostic(getAnnotatedChar: AnnotatedCharGetter, diagnostic: Diagnostic) : vsDiagnostic {
    return {
        severity: diagnostic.kind === DiagnosticKind.error ? vsDiagnosticSeverity.Error : vsDiagnosticSeverity.Warning,
        range: {
            start: cfPositionToVsPosition(getAnnotatedChar, diagnostic.fromInclusive),
            end: cfPositionToVsPosition(getAnnotatedChar, diagnostic.toExclusive)
        },
        message: diagnostic.msg,
        source: "cfls"
    }
}

function mapCflsCompletionItemKindToVsCodeCompletionItemKind(kind: CompletionItemKind) : vsCompletionItemKind {
	switch (kind) {
		case CompletionItemKind.function: return vsCompletionItemKind.Function;
		case CompletionItemKind.structMember: return vsCompletionItemKind.Field;
		case CompletionItemKind.tagName: return vsCompletionItemKind.Property;
		case CompletionItemKind.variable: return vsCompletionItemKind.Variable;
		case CompletionItemKind.stringLiteral: return vsCompletionItemKind.Constant; // not value
		default: exhaustiveCaseGuard(kind);
	}
}

function mapCflsCompletionToVsCodeCompletion(getAnnotatedChar: AnnotatedCharGetter, completion: CompletionItem) : vsCompletionItem {
	const result : Partial<vsCompletionItem> = {};
	result.label = completion.label;
	result.kind = mapCflsCompletionItemKindToVsCodeCompletionItemKind(completion.kind);
	if (completion.detail) result.detail = completion.detail;
	if (completion.insertText) result.insertText = completion.insertText;
	if (completion.sortText) result.sortText = completion.sortText;
	if (completion.textEdit) {
		result.textEdit = {
			insert: cfRangeToVsRange(getAnnotatedChar, completion.textEdit.range),
			newText: completion.textEdit.newText,
			replace: cfRangeToVsRange(getAnnotatedChar, completion.textEdit.replace),
		}
	}
	return result as vsCompletionItem;
}

function sourceLocation(getAnnotatedChar: AnnotatedCharGetter, fsPath: AbsPath, sourceRange: SourceRange) : vsLocation {
	return {
		uri: URI.file(fsPath).toString(),
		range: cfRangeToVsRange(getAnnotatedChar, sourceRange)
	}
}

const VsClientAdapter = asClientAdapter(Object.freeze({
    diagnostic,
    completionItem: mapCflsCompletionToVsCodeCompletion,
	sourceLocation
}));

export default VsClientAdapter;