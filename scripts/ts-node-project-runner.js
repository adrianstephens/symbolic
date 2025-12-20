#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const cp = require('child_process');

function findNearestTsconfig(startFile) {
  let dir = path.dirname(startFile);
  while (true) {
    const candidate = path.join(dir, 'tsconfig.json');
    if (fs.existsSync(candidate)) return candidate;
    const parent = path.dirname(dir);
    if (parent === dir) return null;
    dir = parent;
  }
}

function usage() {
  console.error('Usage: node ts-node-project-runner.js <script> [args...]');
  process.exit(2);
}

if (process.argv.length < 3) usage();

const target = process.argv[2];
const targetPath = path.resolve(process.cwd(), target);
if (!fs.existsSync(targetPath)) {
  console.error('Target file not found:', targetPath);
  process.exit(2);
}

const tsconfig = findNearestTsconfig(targetPath);
const env = Object.assign({}, process.env);
if (tsconfig) env.TS_NODE_PROJECT = tsconfig;

const nodeArgs = [].concat(process.execArgv || []);
const spawnArgs = nodeArgs.concat([
  '-r', 'ts-node/register',
  '-r', 'tsconfig-paths/register',
  targetPath
]);

const child = cp.spawn(process.execPath, spawnArgs.concat(process.argv.slice(3)), {
  stdio: 'inherit',
  env,
  cwd: process.cwd(),
});

child.on('exit', (code, signal) => {
  if (signal) process.kill(process.pid, signal);
  process.exit(code);
});
