var cwd = process.cwd();
var PATH = require('path');
var FS = require('fs');

var pathAppConfig = PATH.join(cwd,'/sources/app_config.js');
if (FS.existsSync(pathAppConfig)) {
  require(pathAppConfig); // load config
}

var NI;
var pathNI = PATH.join(cwd,'/sources/NI.js');
if (FS.existsSync(pathNI)) {
  NI = require(pathNI);
}
else {
  NI = require('tkjs-ni');
}

var globalNodeModulesDir = PATH.join(
  process.env.HAM_HOME,
  "toolsets/nodejs/",
  process.env.HAM_BIN_LOA,
  "node_modules");
// NI.println("... process.env.NODE_PATH: %s", process.env.NODE_PATH);

var baseDir = cwd;

//======================================================================
// Webpack config
//======================================================================
var WEBPACK;
var ExtractTextPlugin;
var WebpackDevServer;

var loaderJSX = 'jsx-loader';
var hotReloadAll = false;

function lazyLoadWebPack() {
  if (WEBPACK) {
    return;
  }
  WEBPACK = require('webpack');
  ExtractTextPlugin = require("extract-text-webpack-plugin");
  WebpackDevServer = require('webpack-dev-server');
}

//======================================================================
// Front-end
//======================================================================
var configFrontEnd = function(aIsDev) {
  return {
    resolve: {
      extensions: [ '', '.js', '.jsx' ],
      modulesDirectories: [
        "web_modules", "node_modules",
        globalNodeModulesDir,
      ]
    },

    resolveLoader: {
      modulesDirectories: [
        "web_modules", "node_modules",
        globalNodeModulesDir,
      ]
    },

    entry: {
      client: [
        PATH.resolve(baseDir, 'sources/client.js')
      ],
      common: [
        PATH.resolve(baseDir, 'sources/client/common.js')
      ]
    },

    output: {
      path: PATH.resolve(baseDir, 'bin/client_js/'),
      filename: '[name].js'
    },

    module: {
      loaders: [
        { test: /\.jsx$/,
          loader: loaderJSX,
          exclude: /(node_modules|bower_components)/,
          include: PATH.join(baseDir, 'sources')
        },
        { test: /\.css$/, // Only .css files
          loader: ExtractTextPlugin.extract("style-loader", "css-loader?keepSpecialComments=0"),
          include: PATH.join(baseDir, 'sources')
        },
        { test: /\.(eot|woff|woff2|ttf|svg)$/,
          loader: "file-loader",
          include: PATH.join(baseDir, 'sources')
        },
        // Convert url() statements for images that are 25KB or smaller to a
        // BASE64 string and include in the CSS file where it is defined.  Not
        // completly sure whether this is a good idea, but one major plus is
        // that it will reduce the number of HTTP requests.
        { test: /\.(png|jpg|gif)$/,
          loader: 'url?limit=10000',
          include: PATH.join(baseDir, 'sources')
        }
      ],
      noParse: []
    },

    plugins: [
      new WEBPACK.DefinePlugin({
        __BACKEND__: false,
        __DEV__: aIsDev,
      }),
      // Use the plugin to specify the resulting filename (and add needed
      // behavior to the compiler)
      new ExtractTextPlugin("[name].css"),
      // The common module, assumed to be used by every page in the app.
      new WEBPACK.optimize.CommonsChunkPlugin(
        "common","common.js",Infinity)
    ]
  };
};

//======================================================================
// Webpack Tasks
//======================================================================
function onBuild(done) {
  return function(err, stats) {
    if(err) {
      console.log('Error', err);
    }
    else {
      console.log("... BUILD: " + stats.toString());
    }
    if(done) {
      done();
    }
  }
}

function frontendBuild(done) {
  lazyLoadWebPack();
  var myConfig = configFrontEnd(false);
  myConfig.devtool = 'source-map';
  myConfig.plugins = myConfig.plugins.concat(
    new WEBPACK.DefinePlugin({
      "process.env": {
        // This has effect on the react lib size
        "NODE_ENV": JSON.stringify("production")
      }
    })
    , new WEBPACK.optimize.OccurenceOrderPlugin()
    , new WEBPACK.optimize.DedupePlugin()
    , new WEBPACK.optimize.UglifyJsPlugin({
      comments: /$a/, // doesnt match anything so that all comments are removed
    })
  );
  WEBPACK(myConfig).run(onBuild(done));
}
exports.frontendBuild = frontendBuild;

function frontendWatch() {
  lazyLoadWebPack();
  var myConfig = configFrontEnd(true);

  // Use 'eval', its *much* faster than source-map
  myConfig.devtool = 'eval';

  // For hot module reloading
  myConfig.entry.common.push('webpack-dev-server/client?http://localhost:'+global.bundlePort);

  // NOTE: Hot reloading fully seems to work well enought, if you change a
  //       react component it'll reload only that, if you change a .css or
  //       other resource it'll refresh the whole browser.
  if (hotReloadAll) {
    // hot reload when any client side changes
    myConfig.entry.common.push('webpack/hot/dev-server');
  }
  else {
    // hot reload react component only
    myConfig.entry.common.push('webpack/hot/only-dev-server');
  }
  myConfig.plugins.push(new WEBPACK.HotModuleReplacementPlugin());
  myConfig.plugins.push(new WEBPACK.NoErrorsPlugin());

  // Setup hot reloading of jsx
  var jsxLoader = myConfig.module.loaders[0];
  if (jsxLoader.loader != loaderJSX) {
    throw new Error("Invalid jsx-loader, can't patch-in hot reload.");
  }
  delete jsxLoader.loader;
  myConfig.output.publicPath = 'http://localhost:'+global.bundlePort+'/bin/';
  jsxLoader.loaders = ['react-hot'];

  new WebpackDevServer(WEBPACK(myConfig), {
    publicPath: '/bin/',
    hot: true,
    quiet: false,
    noInfo: true,
    stats: {
      colors: true
    },
    headers: { 'Access-Control-Allow-Origin': '*' }
  }).listen(global.bundlePort, 'localhost', function (err /*, result*/) {
    if(err) {
      console.log(err);
    }
    else {
      console.log('Webpack dev server listening at localhost:'+global.bundlePort);
    }
  });
}
exports.frontendWatch = frontendWatch;

// $ nodemon -e js,jsx --ignore "sources/*-test.js" --ignore "sources/client.js" --ignore "sources/client/*" --watch sources sources/server.js
function backendWatch() {
  var NODEMON = require('nodemon');
  NODEMON({
    verbose: true,
    script: 'sources/server.js',
    ext: 'js jsx',
    watch: ['sources'],
    ignore: ["*-test.js", "sources/client.js", "sources/client/*"],
  });
  NODEMON.on('start', function () {
    console.log('... Nodemon server.js has started');
  }).on('quit', function () {
    console.log('... Nodemon server.js has quit');
  }).on('restart', function (files) {
    console.log('... Nodemon server.js restarted due to: ', files);
  });
}
exports.backendWatch = backendWatch;

exports.build = function() {
  frontendBuild();
}

exports.dev = function() {
  frontendWatch();
  backendWatch();
}

exports.test = function(aParams) {
  var pathUNITTEST = PATH.join(cwd,'/sources/unittest.js');
  var UT;
  if (FS.existsSync(pathUNITTEST)) {
    UT = require(pathUNITTEST);
  }
  else {
    UT = require('tkjs-ni/sources/unittest');
  }
  UT.runAllTests(aParams);
}

exports.lint = function() {
  var exec = require('child_process').exec;
  exec(
    'eslint --ext .js --ext .jsx ./sources',
    function (error, stdout, stderr) {
      console.log(NI.stringTrim(stdout));
      console.log(NI.stringTrim(stderr));
      if (error !== null) {
        // console.log('exec error: ' + error);
        errorExit("Linting failed.");
      }
    });
}

function printHelp() {
  NI.println("syntax: node ./webpack.js TARGETS");
  NI.println("  %d target(s) registered:", NI.size(exports));
  NI.forEach(exports,function(v,k) {
    NI.println("  - %s", k);
  });
  NI.println("");
}

function errorExit(aMsg) {
  NI.println(aMsg);
  process.exit(1);
}

function errorHelpExit(aMsg) {
  printHelp();
  errorExit(aMsg);
}

(function(aTargets) {
  var paramObject = {};
  var targets = [];
  var numArgs = process.argv.length;
  for (var i = 2; i < numArgs; ++i) {
    var param = process.argv[i]
    if (NI.stringStartsWith(param,"--")) {
      paramObject[NI.stringAfter(param,"--")] = process.argv[i+1];
      ++i;
    }
    else {
      if (!(param in aTargets)) {
        errorHelpExit(NI.format("Target '%s' not registered.", targetName));
      }
      targets.push(param);
    }
  }

  if (NI.isEmpty(targets)) {
    errorHelpExit("No target specified.");
  }

  // NI.println("... paramObject: %K", paramObject);
  // NI.println("... targets: %K", targets);

  for (var i = 0; i < targets.length; ++i) {
    var targetName = targets[i]
    NI.println("# %s...", targetName);
    aTargets[targetName](paramObject);
  }
}(exports));
