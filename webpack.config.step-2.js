const path = require('path');
const CopyPlugin = require("copy-webpack-plugin");
const { DefinePlugin } = require("webpack");

const TsconfigPathsPlugin = require('tsconfig-paths-webpack-plugin');

module.exports = [{
    mode: "development", // "production" | "development" | "none"
    target: "node",
    entry: "./src/lang-server/client/src/extension.ts",
    output: {
        path: path.resolve("./cflsp-vscode/out/"), // string (default)
        filename: "cflsp.js", // string (default)
        library: {
            type: "commonjs2",
        },
        uniqueName: "cflsp", // (defaults to package.json "name")
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
},
{
    //watch: true,
    //watchOptions: {
    //    aggregateTimeout: 2500
    //},
    mode: "development", // "production" | "development" | "none"
    target: "node",
    entry: "./src/lang-server/server/src/server.ts",
    output: {
        path: path.resolve("./cflsp-vscode/out/"), // string (default)
        filename: "server.js", // string (default)
        //library: {
        //    type: "commonjs2",
        //},
        uniqueName: "cfls", // (defaults to package.json "name")
    },
    resolve: {
        extensions: [".ts", ".tsx", ".js"],
        plugins: [new TsconfigPathsPlugin({ configFile: "./src/lang-server/server/tsconfig.json" })]
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
    plugins: [
        new CopyPlugin({
            patterns: [{
                from: "./src/lang-server/server/src/runtimelib/lib.cf2018.d.cfm",
                to: "lib.cf2018.d.cfm",
            }],
        }),
    ],
},
{
    mode: "development", // "production" | "development" | "none"
    target: "node",
    entry: "./src/services/cfls.ts",
    output: {
        path: path.resolve("./cflsp-vscode/out/"), // string (default)
        filename: "cfls-service.js", // string (default)
        library: {
            type: "commonjs2",
        },
        uniqueName: "cfls-service", // (defaults to package.json "name")
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
    plugins: [
        new DefinePlugin({
            // why "JSON.stringify"? -- https://github.com/webpack/webpack/issues/8641
            "REPLACED_BY_WEBPACK_AT_BUILD.ClientAdapterModule": JSON.stringify("../../cflsp-vscode/out/vscode-adapter.js"), // n.b. this gets built in a previous step
        })
    ]
}
]