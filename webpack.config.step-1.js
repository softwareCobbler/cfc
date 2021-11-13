const path = require('path');
const TsconfigPathsPlugin = require('tsconfig-paths-webpack-plugin');

module.exports = [{
    mode: "development", // "production" | "development" | "none"
    target: "node",
    entry: "./src/lang-server/server/src/vscode-adapter.ts",
    output: {
        path: path.resolve("./cflsp-vscode/out/"), // string (default)
        filename: "vscode-adapter.js", // string (default)
        library: {
            type: "commonjs2",
            export: "default"
        },
        uniqueName: "vscode-adapter", // (defaults to package.json "name")
    },
    resolve: {
        extensions: [".ts", ".tsx", ".js"],
        plugins: [new TsconfigPathsPlugin({ configFile: "./src/lang-server/client/tsconfig.json" })]
    },
    externals: {
        // https://github.com/microsoft/vscode/issues/102314
        // the vscode-module is created on-the-fly and must be excluded.
        // Add other modules that cannot be webpack'ed, 📖 -> https://webpack.js.org/configuration/externals/
        vscode: 'commonjs vscode',
    },
    module: {
        rules: [
            // all files with a `.ts` or `.tsx` extension will be handled by `ts-loader`
            {
                test: /\.tsx?$/,
                use: {
                    loader: "ts-loader",
                    options: {
                        projectReferences: true,
                        configFile: "tsconfig.json"
                    }
                }
            }
        ]
    },
    devtool: "source-map",
}]