/* HTTP interface to JSHint.

   curl --form source="<path/to/my.js" --form=filename="my.js" --form mode=jshint --form showCode=1 http://127.0.0.1:3003/check

    POST parameters:

      source the contents of the file (the < is a curl construct which
             inlines the contents of the file in the param).

      filename is the name of that file

      mode     The linter to use. 'jshint' or 'jslint' (default jshint).

      showCode Whether to include source code evidence with the
               errors. '0' or '1' (default '0'). Not including error
               content speeds up flymake parsing and operation.

   CLI Usage:
        jshint-mode.js  --host HOST --port PORT --lastport LASTPORT

        Start listening at address HOST. The first port between PORT
	and LASTPORT is used for listening (default 3003-3003). The
	chosen port is printed on stdout on the first line output.

	Dynamic ports are needed when the server is started by emacs
	flymake. Multiple emacs processes can use the same server
	endpoint, but the server is shut down when one window is
	closed. By allowing each flymake process to start its own
	server on a different port, cleanup of one window does not
	interfere with another's.

	Example output indicating address and port:

        'Started JSHint server at http://127.0.0.1:3003'

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

if (!fs.existsSync) {
  fs.existsSync = require('path').existsSync;
}

var hinters = {
  jshint: JSHINT.JSHINT,
  jslint: JSLINT.JSLINT
};

function getOpt(key) {
  var index = process.argv.indexOf(key);
  return index !== -1 ? process.argv[index + 1] : false;
}

function outputErrors(errors, showCode) {

  var e, i, output = [];

  function out(s) {
    output.push(s + '\n');
  }

  for (i = 0; i < errors.length; i += 1) {
    e = errors[i];
    if (e) {
      out('Lint at line ' + e.line + ' character ' + e.character + ': ' + e.reason);
      if (showCode) {
	out((e.evidence || '').replace(/^\s*(\S*(\s+\S+)*)\s*$/, "$1"));
	out('');
      }
    }
  }
  return output.join('');
}

function lintify(mode, sourcedata, filename, showCode, config) {
  var passed = hinters[mode](sourcedata, config);
  return passed ? "js: No problems found in " + filename + "\n"
    : outputErrors(hinters[mode].errors, showCode);
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

var port = parseInt(getOpt("--port"), 10) || 3003,
lastPort = parseInt(getOpt("--lastport"),10) || 3003,
    host = getOpt("--host") || "127.0.0.1",
    config = {};

var server = http.createServer(function(req, res) {
  if (req.url === '/check' && req.method.toUpperCase() === 'POST') {
    var form = new formidable.IncomingForm();
    form.parse(req, function(err, fields, files) {
      var mode = (fields.mode && fields.mode == "jslint") ? "jslint" : "jshint";
      var showCode = (fields.showCode && fields.showCode === "1") ? true : false;
      var now = new Date().getTime();
      console.log('Applying \'' + mode + '\' to: ' + (fields.filename || 'anonymous'));

      var config = _getConfig(fields.jshintrc);

      var results = lintify(mode, fields.source, fields.filename, showCode, config);
      console.log('Took ' + (new Date().getTime() - now) + 'ms to lint ' + (fields.filename || 'anonymous'));
      res.writeHead(200, {'Content-Type': 'text/plain'});
      res.end(results);
    });
    return;
  }

  res.writeHead(200, {'Content-Type': 'text/plain'});
  res.end("hello from jshint-mode");

});

server.on('listening', function () {
  console.log('Started JSHint server at http://' + host + ':' + port + '.');
});

server.on('error', function (err) {
  if (err.errno === "EADDRINUSE") {
    if (port >= lastPort) {
      console.error("Error occurred during '" + err.syscall + "':", err.code);
      process.exit(2);
    } else {
      // find the next available port
      port += 1;
      server.listen(port, host);
    }
  }
});
server.listen(port, host);
