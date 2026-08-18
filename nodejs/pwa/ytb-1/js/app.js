if ('serviceWorker' in navigator) {
    navigator.serviceWorker.register('/sw.js')
        .then(reg => console.log('serviceWorker registered', reg))
        .catch(e => console.error('failed to register serviceWorker', e))
}

// function separator(strings, ...variables) {
//     console.log({strings, variables})
//
//     let result = strings.raw[0];
//     for (let i = 0; i < variables.length; i++) {
//         result += variables[i] + strings.raw[i + 1];
//     }
//
//     console.log(result);
// }
//
// const prefix = 'OH!'
// const name = 'kim'
// const age = 21
// separator`${prefix} hello, ${name}, you are ${age}, so you can pass.`

