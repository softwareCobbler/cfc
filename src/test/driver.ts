import * as fs from "fs";
import { FileSystem, Project } from "../compiler/project";
import { recursiveGetFiles } from "../compiler/utils";

const root = "C:\\Users\\anon\\dev\\cf-ts-compiler\\mxunit";

const project = Project([root], FileSystem(), {debug: true, parseTypes: false})

const files = recursiveGetFiles(root, /\.cfc$/i)

for (const file of files) {
    console.log(file);
    project.addFile(file);
}

console.log(files.length + " files");