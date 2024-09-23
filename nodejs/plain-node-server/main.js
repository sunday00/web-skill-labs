const http = require('http')

class ConnRedis {
    async conn() {
        const config = {
            host: '',
            port: '',
            pass: '',
        }

        config.host.replace('abc', 'fds')

        const hello = 'ho'

        console.log(hello, 'hello')

        return Redis.connect(config)
    }
}

http.createServer((req, res) => {
    const getWorld = () => {
        return 'hello world'
    }

    res.write(`<h1>${getWorld()}!</h1>`)
    res.end()
}).listen(3000)
