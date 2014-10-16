'use strict';
var vm = require('vm');
try {
  console.log(vm.runInNewContext(process.argv[2]));
} catch(err) {
  console.log(err.toString());
}
