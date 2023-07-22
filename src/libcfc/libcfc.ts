import { Parser } from "../compiler/parser"
import { SourceFile, type Node } from "../compiler/node"
import { EngineVersions } from "../compiler/engines";
import { CfFileType } from "../compiler/scanner";

export function parse(kind: string, src: string) {
    if (kind !== "cfm" && kind !== "cfc") {
        throw Error(`parse() called with kind '${kind}', expected either 'cfm' or 'cfc'`);
    }

    const file = SourceFile("", kind === "cfm" ? CfFileType.cfm : CfFileType.cfc, src);

    const dummyConfig = {
        debug: false,
        parseTypes: false,
        engineVersion: EngineVersions["lucee.5"],
        withWireboxResolution: false,
        cfConfigProjectRelativePath: null,
        genericFunctionInference: false,
        checkReturnTypes: false,
        checkFlowTypes: false,
        cancellationToken: {
            cancellationRequested: () => false,
            throwIfCancellationRequested: () => {}
        },
    };
    
    Parser(dummyConfig).parse(file);
    
    return {
        hasErrors: file.diagnostics.length > 0,
        errors: file.diagnostics,
        sourceFile: file
    }
}
