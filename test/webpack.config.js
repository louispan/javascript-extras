const HtmlWebpackPlugin = require('html-webpack-plugin')

module.exports = {
    mode: "development",
    entry: ["./src/index.js"],
    output: {
        filename: "bundle.js",
        publicPath: "/",
        path: __dirname + "/dist"
    },
    devServer: {
        writeToDisk: true
    },
    plugins: [
        new HtmlWebpackPlugin({
            title: 'javascript-extras',
        })
    ],
    // resolve: {
    //     // use abs path to node_modules since default parent searching resolution wont work
    //     // since symlinked ghcjs output all.js is in a different directory subtree.
    //     modules: [path.resolve(__dirname, "node_modules")]
    // }
};
