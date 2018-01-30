import * as socketIO from 'socket.io';
import * as http from 'http';
import * as fs from 'fs';

import { runJuRaCode } from './run';

export function registerSockets(http: http.Server) {
  const io = socketIO(http);

  io.on('connection', socket => {
    socket.on('run-code', code => {
      runJuRaCode(socket.id, code, (err, stdout, stderr) => {
        socket.emit('stdout', stdout);
        socket.emit('stderr', stderr);
      });
    });
  });
}
