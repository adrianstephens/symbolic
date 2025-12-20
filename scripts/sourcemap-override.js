const fs = require('fs');
const path = require('path');
const sourceMapSupport = require('source-map-support');
const workspaceRoot = path.resolve(__dirname, '..', '..');
sourceMapSupport.install({
  retrieveSourceMap(source) {
    try {
      // If source is a compiled file inside a package dist, try to read its .map on disk
      // Normalize and attempt to find corresponding .map
      const p = path.resolve(source);
      const mapPath = p + '.map';
      if (fs.existsSync(mapPath)) {
        const map = fs.readFileSync(mapPath, 'utf8');
        return { url: source, map };
      }
      // Fallback: if source path contains "/dist/", try derive map from workspace maths/dist
      const idx = p.indexOf(path.join('maths', 'dist'));
      if (idx !== -1) {
        const rel = p.slice(idx + ('maths/dist').length + 1);
        const candidate = path.join(workspaceRoot, 'maths', 'dist', rel + '.map');
        if (fs.existsSync(candidate)) {
          return { url: candidate, map: fs.readFileSync(candidate, 'utf8') };
        }
      }
    } catch (e) {
      // ignore
    }
    return null;
  }
});
console.log('sourcemap-override registered');
