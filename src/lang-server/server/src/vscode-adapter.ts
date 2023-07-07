import { CompletionItemKind as vsCompletionItemKind, CompletionItem as vsCompletionItem, Diagnostic as vsDiagnostic, DiagnosticSeverity as vsDiagnosticSeverity } from "vscode-languageserver/node";
import { Position as vsPosition, Range as vsRange, Location as vsLocation } from "vscode-languageserver-types"
import { Diagnostic, DiagnosticKind } from "../../../compiler/node";
import { CompletionItemKind, CompletionItem } from "../../../services/completions";
import { SourceRange } from "../../../compiler/scanner"
import { ClientAdapter, PosMapper } from "../../../services/clientAdapter";
import { AbsPath, exhaustiveCaseGuard } from "../../../compiler/utils";
import { URI } from "vscode-uri";

interface LineCol { line: number, col: number }

function cfPositionToVsPosition(posMapper: PosMapper<LineCol>, pos: number) : vsPosition {
    const annotatedChar = posMapper(pos);
    return {
        line: annotatedChar.line,
        character: annotatedChar.col
    }
}

const nilRangeAsAnnotatedCharLike = {line: 0, col: 0};

function cfRangeToVsRange(posMapper: PosMapper<LineCol>, sourceRange: SourceRange) : vsRange {
	const from = sourceRange.isNil() ? nilRangeAsAnnotatedCharLike : posMapper(sourceRange.fromInclusive);
	const to = sourceRange.isNil() ? nilRangeAsAnnotatedCharLike : posMapper(sourceRange.toExclusive);
	return {
		start: {line: from.line, character: from.col},
		end: {line: to.line, character: to.col}
	}
}

function diagnostic(posMapper: PosMapper<LineCol>, diagnostic: Diagnostic) : vsDiagnostic {
    return {
        severity: diagnostic.kind === DiagnosticKind.error ? vsDiagnosticSeverity.Error : vsDiagnosticSeverity.Warning,
        range: {
            start: cfPositionToVsPosition(posMapper, diagnostic.fromInclusive),
            end: cfPositionToVsPosition(posMapper, diagnostic.toExclusive)
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

function mapCflsCompletionToVsCodeCompletion(posMapper: PosMapper<LineCol>, completion: CompletionItem) : vsCompletionItem {
	const result : Partial<vsCompletionItem> = {};
	result.label = completion.label;
	result.kind = mapCflsCompletionItemKindToVsCodeCompletionItemKind(completion.kind);
	if (completion.detail) result.detail = completion.detail;
	if (completion.insertText) result.insertText = completion.insertText;
	if (completion.sortText) result.sortText = completion.sortText;
	if (completion.textEdit) {
		result.textEdit = {
			insert: cfRangeToVsRange(posMapper, completion.textEdit.range),
			newText: completion.textEdit.newText,
			replace: cfRangeToVsRange(posMapper, completion.textEdit.replace),
		}
	}
	return result as vsCompletionItem;
}

function sourceLocation(posMapper: PosMapper<LineCol>, fsPath: AbsPath, sourceRange: SourceRange) : vsLocation {
	return {
		uri: URI.file(fsPath).toString(),
		range: cfRangeToVsRange(posMapper, sourceRange)
	}
}

export const adapter = {
    diagnostic,
    completionItem: mapCflsCompletionToVsCodeCompletion,
	sourceLocation
} as const satisfies ClientAdapter<LineCol>