const merge = require('webpack-merge');
const common = require('./webpack.common.js');
// const ClosurePlugin = require('closure-webpack-plugin');
const path = require('path');

module.exports = merge(common, {
    mode: "production",
    // // node: {
    // //     // closure compiler needs this or we get a "Can't resolve 'fs'"
    // //     // https://stackoverflow.com/questions/52349500/angular-6-include-google-closure-library
    // //     // https://webpack.js.org/configuration/node/#other-node-core-libraries
    // //     fs: 'empty'
    // // },
    // optimization: {
    //     concatenateModules: false,
    //     minimizer: [
    //         new ClosurePlugin({
    //             mode: 'AGGRESSIVE_BUNDLE',
    //             platform: 'native'
    //         }, {
    //             // compiler flags here
    //             //
    //             // for debuging help, try these:
    //             //
    //             formatting: 'PRETTY_PRINT',
    //             debug: true,
    //             renaming: false,
    //             // non debug flags
    //             // https://bendyworks.com/blog/google-closure-no-compile-errors
    //             // compilation_level: 'ADVANCED',
    //             module_resolution: 'NODE',
    //             // jscomp_off: 'checkVars',
    //             languageOut: 'ECMASCRIPT5',
    //             externs: [path.resolve(__dirname, '.shake', 'hsMain.externs.js')]
    //         })
    //     ]
    // }
});
