var path = require("path");

module.exports = {
  entry: [ "./src/index.js" ],
  output: {
    path: path.resolve(__dirname, "build"),
    publicPath: "/",
    filename: "bundle.js"
  },
    resolve: {
      // use abs path to node_modules since default parent searching resolution wont work
      // since symlinked ghcjs output all.js is in a different directory subtree.
      modules: [path.resolve(__dirname, "node_modules")]
  }
};
