import { EngineVersions } from "../compiler/engines";
import { Parser } from "../compiler/parser";
import { setDebug as setNodeFactoryDebug, SourceFile, Diagnostic, NodeKind, Node, NodeFlags } from "../compiler/node";
import { visit } from "../compiler/utils"

export function roundTrip(sourceFile: SourceFile) : {ok: true, text: string} | {ok: false, diagnostics: Diagnostic[]} {
    setNodeFactoryDebug(true);
    const parser = Parser({
        cancellationToken: {
            cancellationRequested: () => false,
            throwIfCancellationRequested: () => void 0
        },
        checkReturnTypes: false,
        debug: true,
        engineVersion: EngineVersions["lucee.5"],
        genericFunctionInference: false,
        parseTypes: false,
        wireboxConfigFileCanonicalAbsPath: null,
        withWireboxResolution: false,
    });

    parser.setSourceFile(sourceFile);
    parser.parse();

    if (sourceFile.diagnostics.length !== 0) {
        return {ok: false, diagnostics: [...sourceFile.diagnostics]};
    }

    const sourceText = sourceFile.scanner.getSourceText();
    const result : string[] = [];

    const visitor = (node: Node | null | undefined) => {
        if (!node) return;
        else if (node.kind === NodeKind.terminal) {
            result.push(sourceText.slice(node.rangeWithTrivia.fromInclusive, node.rangeWithTrivia.toExclusive));
        }
        else if (node.kind === NodeKind.textSpan || node.kind === NodeKind.comment) {
            result.push(sourceText.slice(node.range.fromInclusive, node.range.toExclusive));
        }
        else if (node.kind === NodeKind.tagAttribute && node.flags & NodeFlags.docBlock) {
            // tag-attributes that originate from docblocks will have been written out as part of their containing comment/docblock
            return;
        }
        else {
            visit(node, visitor);
        }
    }
    visit(sourceFile, visitor);

    return {ok: true, text: result.join("")};
}