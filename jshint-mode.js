/* HTTP interface to JSHint.

   curl --form source="<path/to/my.js" --form=filename="my.js" http://127.0.0.1:3003/jshint

  TODO:
    parse incoming source files for embedded jshint options
    support file uploads?
    speed up
*/

var http = require('http'),
    formidable = require('formidable'),
    fs = require('fs'),
    JSLINT = require('./jslint'),
    JSHINT = require('./jshint');

var hinters = {
  jshint: JSHINT.JSHINT,
  jslint: JSLINT.JSLINT
};

function getOpt(key) {
  var index = process.argv.indexOf(key);
  return index !== -1 ? process.argv[index + 1] : false;
}

function outputErrors(errors) {

  var e, i, output = [];

  function out(s) {
    output.push(s + '\n');
  }

  for (i = 0; i < errors.length; i += 1) {
    e = errors[i];
    if (e) {
      out('Lint at line ' + e.line + ' character ' + e.character + ': ' + e.reason);
      out((e.evidence || '').replace(/^\s*(\S*(\s+\S+)*)\s*$/, "$1"));
      out('');
    }
  }
  return output.join('');
}

function lintify(mode, sourcedata, filename, config) {
  var passed = hinters[mode](sourcedata, config);
  return passed ? "js: No problems found in " + filename + "\n"
    : outputErrors(hinters[mode].errors);
}

// This is copied from jshint mode, that's how they load the config file
function _removeJsComments(str) {
  str = str || '';

  // replace everything between "/* */" in a non-greedy way
  // The English version of the regex is:
  //   match '/*'
  //   then match 0 or more instances of any character (including newlines)
  //     except for instances of '*/'
  //   then match '*/'
  str = str.replace(/\/\*(?:(?!\*\/)[\s\S])*\*\//g, '');

  str = str.replace(/\/\/[^\n\r]*/g, ''); //everything after "//"
  return str;
}

function _loadAndParseConfig(filePath) {
  return filePath && fs.existsSync(filePath) ?
    JSON.parse(_removeJsComments(fs.readFileSync(filePath, "utf-8"))) : {};
}

function _getConfig(filePath) {
  if (config[filePath]) {
    return config[filePath];
  }else{
    return config[filePath] = _loadAndParseConfig(filePath);
  }
}

var port = getOpt("--port") || 3003,
    host = getOpt("--host") || "127.0.0.1",
    config = {};

http.createServer(function(req, res) {
  if (req.url === '/check' && req.method.toUpperCase() === 'POST') {
    var form = new formidable.IncomingForm();
    form.parse(req, function(err, fields, files) {
      var mode = (fields.mode && fields.mode == "jslint") ? "jslint" : "jshint";

      console.log('Applying \'' + mode + '\' to: ' + (fields.filename || 'anonymous'));

      var config = _getConfig(fields.jshintrc);

      var results = lintify(mode, fields.source, fields.filename, config);
      res.writeHead(200, {'Content-Type': 'text/plain'});
      res.end(results);
    });
    return;
  }

  res.writeHead(200, {'Content-Type': 'text/plain'});
  res.end("hello from jshint-mode");

}).listen(port, host);

console.log('Started JSHint server at http:// ' + host + ':' + port);
