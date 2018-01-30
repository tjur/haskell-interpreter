import * as socketIO from "socket.io-client";

export class SocketManager {
  private _io: SocketIOClient.Socket;

  constructor() {
    this._io = socketIO();
    this._registerMethods();
  }

  sendCode(code: string) {
    this._io.emit('run-code', code);
    document.getElementById('code-output').innerHTML = '';
  }

  private _registerMethods() {
    this._io.on('stdout', (msg: string) => {
      document.getElementById('code-output').innerHTML += msg;
    });

    this._io.on('stderr', (msg: string) => {
      document.getElementById('code-output').innerHTML += msg;
    });
  }
}
