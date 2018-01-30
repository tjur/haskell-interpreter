import * as http from 'http';
import * as fs from 'fs';

import app from './app';
import * as sockets from './sockets';

const port = process.env.PORT || 3000;

const httpServer = new http.Server(app);

if (!fs.existsSync('./programs/')) {
    fs.mkdirSync('./programs/');
}

sockets.registerSockets(httpServer);

httpServer.listen(port, err => {
    if (err) {
        return console.log(err);
    }

    return console.log(`server is listening on ${port}`);
});
