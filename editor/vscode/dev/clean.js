const fs = require('fs');
const glob = require('glob');

const files = glob.sync('*.vsix');

for (const file of files) {
  fs.unlinkSync(file);
}
