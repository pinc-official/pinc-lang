const { compile } = require('./compile.bc');

const compile_wrapper = (inputDir, outDir) => {
  compile(inputDir, outDir);
};

module.exports = {
  compile: compile_wrapper,
};
