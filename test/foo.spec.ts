import * as assert from "assert";
import * as fs from "fs";
import * as path from "path";
import {Scanner, Parser, CfFileType} from "../out/compiler";

const parser = Parser().setDebug(true);
const cfFilePattern = /\.cfml?$|\.cfc$/i;

type AbsPath = string;
function recursiveGetCfFiles(absPathBase: AbsPath) : AbsPath[] {
    const entries = fs.readdirSync(absPathBase, {withFileTypes: true});
    const result : AbsPath[] = [];
    for (const entry of entries) {
        if (entry.isDirectory()) {
            result.push(...recursiveGetCfFiles(path.resolve(absPathBase, entry.name)));
        }
        else if (cfFilePattern.test(entry.name)) {
            result.push(path.resolve(absPathBase, entry.name));
        }
    }

    return result;
}

function cfmOrCfc(fname: string) : CfFileType {
    return /\.cfc$/.test(fname)
        ? CfFileType.cfc
        : CfFileType.cfm
}

describe("MX-Unit smoke test", () => {
    it("Should parse all CFMs/CFCs within the MX-Unit subrepo without errors", () => {
        const files = recursiveGetCfFiles(path.resolve(__dirname, "./mxunit"));
        for (const file of files) {
            parser.setScanner(Scanner(fs.readFileSync(file).toString())).parse(cfmOrCfc(file));
            const diagnostics = parser.getDiagnostics();
            assert.strictEqual(diagnostics.length, 0, `${file} parsed with 0 diagnostics`);
        }
    });
});