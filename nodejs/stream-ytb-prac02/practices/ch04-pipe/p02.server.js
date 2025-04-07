const { createReadStream } = require("fs");
const {createServer} = require("http");
const {join} = require("node:path");
const {pipeline} = require("stream/promises");

const server = createServer((req, res) => {
    // createReadStream(join(process.cwd(), 'static', '1.txt')).pipe(res);
    pipeline(createReadStream(join(process.cwd(), 'static', '1.txt')).pipe(res), (err) => {
        if (err) {
            console.error(err);
        }
    })

})

server.listen(8080);