const devConfigToExtend = require("./webpack.config.js");
devConfigToExtend[0].mode = "production";
devConfigToExtend[1].mode = "production";

module.exports = devConfigToExtend;