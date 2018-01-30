import * as fs from 'fs';
import * as childProcess from 'child_process';

export function runJuRaCode(id: string, code: string, callback: (err: Error, stdout: string, stderr: string) => void) {
  const programPath = `./programs/${id}.jura`;

  fs.writeFile(programPath, code, 'utf8', err => {
    if (err) {
      console.log('an error ocurred when writing file for user of id:', id);
    }

    childProcess.exec(`racket ../../run.rkt ${programPath}`, callback);
  });
}
