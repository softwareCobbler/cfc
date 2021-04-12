const path = require('path');

module.exports = {
  mode: "development", // "production" | "development" | "none"
  // Chosen mode tells webpack to use its built-in optimizations accordingly.
  entry: "./out/lang-server/client/extension.js", // string | object | array
  // defaults to ./src
  // Here the application starts executing
  // and webpack starts bundling
  output: {
    // options related to how webpack emits results
    path: path.resolve("./cfls-vscode/extension/"), // string (default)
    // the target directory for all output files
    // must be an absolute path (use the Node.js path module)
    filename: "cfls.js", // string (default)
    // the filename template for entry chunks
    //publicPath: "/assets/", // string
    // the url to the output directory resolved relative to the HTML page
    library: {
      type: "commonjs2",
    },
    /*
    library: { // There is also an old syntax for this available (click to show)
      type: "umd", // universal module definition
      // the type of the exported library
      name: "MyLibrary", // string | string[]
      // the name of the exported library

      
    },*/
    uniqueName: "cflsp", // (defaults to package.json "name")
    // unique name for this build to avoid conflicts with other builds in the same HTML
  },
  resolve: {
    alias: {
      compiler: path.resolve("./out/compiler"),
    },
    modules: [
      path.resolve("./src/lang-server/client/node_modules/"),
      path.resolve("./src/lang-server/server/node_modules/"),
      "node_modules"
    ]
  },
  externals: {
    vscode: 'commonjs vscode', // the vscode-module is created on-the-fly and must be excluded. Add other modules that cannot be webpack'ed, 📖 -> https://webpack.js.org/configuration/externals/
    path: "commonjs path"
  },
  devtool: "source-map",
}