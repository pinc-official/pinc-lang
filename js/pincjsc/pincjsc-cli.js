#!/usr/bin/env node

const { Command } = require('commander');
const { compile } = require('./pincjsc');

const program = new Command();
program
  .name('pincjsc')
  .description('CLI to compile pinc-lang source code to be executable in JavaScript')
  .argument('<inputDir>', 'input directory where .pi source files are located')
  .argument('<outDir>', 'output directory where .pi.js files should be stored in')
  .action((inputDir, outDir) => {
    compile(inputDir, outDir);
  })
  .parse();
