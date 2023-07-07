import { CompletionItemKind as vsCompletionItemKind, CompletionItem as vsCompletionItem, Diagnostic as vsDiagnostic, DiagnosticSeverity as vsDiagnosticSeverity } from "vscode-languageserver/node";
import { Position as vsPosition, Range as vsRange, Location as vsLocation } from "vscode-languageserver-types"
import { Diagnostic, DiagnosticKind, SourceFile } from "../../../compiler/node";
import { CompletionItemKind, CompletionItem } from "../../../services/completions";
import { SourceRange } from "../../../compiler/scanner"
import { ClientAdapter, PosMapper } from "../../../services/clientAdapter";
import { AbsPath, exhaustiveCaseGuard } from "../../../compiler/utils";
import { URI } from "vscode-uri";

import {
	TextDocument
} from 'vscode-languageserver-textdocument';

function cfPositionToVsPosition(doc: TextDocument) : (pos: number) => vsPosition {
	return (pos: number) => doc.positionAt(pos);
}

const nilRangeAsAnnotatedCharLike : vsPosition = {line: 0, character: 0}

function cfRangeToVsRange(posMapper: PosMapper<vsPosition>, sourceRange: SourceRange) : vsRange {
	const start = sourceRange.isNil() ? nilRangeAsAnnotatedCharLike : posMapper(sourceRange.fromInclusive);
	const end = sourceRange.isNil() ? nilRangeAsAnnotatedCharLike : posMapper(sourceRange.toExclusive);
	return {
		start,
		end
	}
}

function diagnostic(posMapper: PosMapper<vsPosition>, diagnostic: Diagnostic) : vsDiagnostic {
    return {
        severity: diagnostic.kind === DiagnosticKind.error ? vsDiagnosticSeverity.Error : vsDiagnosticSeverity.Warning,
        range: {
            start: posMapper(diagnostic.fromInclusive),
            end: posMapper(diagnostic.toExclusive)
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

function mapCflsCompletionToVsCodeCompletion(posMapper: PosMapper<vsPosition>, completion: CompletionItem) : vsCompletionItem {
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

function sourceLocation(posMapper: PosMapper<vsPosition>, sourceLocation: {sourceFile: SourceFile, range: SourceRange}) : vsLocation {
	return {
		uri: URI.file(sourceLocation.sourceFile.absPath).toString(),
		range: cfRangeToVsRange(posMapper, sourceLocation.range)
	}
}

export const adapter = {
	cfPositionToVsPosition,
    diagnostic,
    completionItem: mapCflsCompletionToVsCodeCompletion,
	sourceLocation
}
