const fs = require('fs');
const { Transform } = require('stream');
const {join} = require("path");
const {pipeline} = require("stream/promises");

const upper = new Transform({
    transform(chunk, encoding, callback) {
        this.push(chunk.toString().toUpperCase());
        callback();
    }
})

// fs.createReadStream(join(process.cwd(), 'static', '1.txt'))
//     .pipe(upper)
//     .pipe(process.stdout)

// pipeline(
//     fs.createReadStream(join(process.cwd(), 'static', '1.txt')),
//     upper,
//     process.stdout,
//     (err) => {
//         if (err) {
//             console.error(err);
//         } else {
//             console.log('done')
//         }
//     }
// ).then(() => {})

const main = async () => {
 await pipeline(
     fs.createReadStream(join(process.cwd(), 'static', '1.txt')),
     async function * (source) {
         for await (let chunk of source) {
             yield chunk.toString().toUpperCase();
         }
     },
     process.stdout,
 )
}

main().then(() => {})