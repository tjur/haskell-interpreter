import * as express from 'express';

class App {
    public express: express.Express;

    constructor() {
        this.express = express();
        this._mountRoutes();
    }

    private _mountRoutes(): void {
        this.express.use('/public', express.static('public'))
        
        this.express.get('/', (req, res) => {
            res.sendFile(__dirname + '/public/index.html');
        });
    }
}

export default new App().express;
