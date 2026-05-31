#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { exec } = require('child_process');

function getPackageJson(directory) {
  const packageJson = path.resolve(directory, 'package.json');

  if (fs.existsSync(packageJson)) {
    return JSON.parse(fs.readFileSync(packageJson));
  }

  const parentDir = path.dirname(directory);

  if (parentDir === directory) {
    // System root
    throw new Error("Couldn't not find your package.json file");
  }

  return resolvePackage(path.dirname(directory));
}

const package = getPackageJson(process.cwd());

const tag = `${package.name}@${package.version}`;

exec(`npm view ${tag} version`, (error, stdout, stderr) => {
  if (stderr.includes('code E404') || (!error && stdout.trim() !== package.version.trim())) {
    console.info(`${tag} does not exist. Publishing...`);

    const child = exec(`npm publish ${process.argv.slice(2).join(' ')}`);
    child.stdout.pipe(process.stdout);
    child.stderr.pipe(process.stderr);
    child.on('exit', process.exit);
  } else if (error) {
    console.error('Something went wrong while checking the current version.');
    console.log(stdout);
    console.error(stderr);
  } else {
    console.info('Already up-to-date.');
  }
});
