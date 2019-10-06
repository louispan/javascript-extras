const HtmlWebpackPlugin = require('html-webpack-plugin')

module.exports = {
    entry: ["./src/index.js"],
    output: {
        filename: "bundle.js",
        publicPath: "/",
        path: __dirname + "/dist",
    },
    plugins: [
        new HtmlWebpackPlugin({
            title: 'javascript-extras',
            favicon: 'assets/favicon.ico',
        })
    ],
    devServer: {
        writeToDisk: true,
    },
};
