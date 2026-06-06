const fs = require('node:fs');
const { compile } = require('./compile.bc');

const compile_wrapper = (inputDir, outDir) => {
  fs.mkdirSync(outDir, { recursive: true });
  compile(inputDir, outDir);
};

module.exports = {
  compile: compile_wrapper,
};
