let n = 0

setInterval(() => {
    console.log(`${process.pid} : ${n++}`)
}, 1000)