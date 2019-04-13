const util = require('util');
const fs = require('fs');

function wasm(filepath, callback) {
    const src = fs.readFileSync(filepath);
    const arr = new Uint8Array(src);
    WebAssembly.instantiate(arr).then(callback);
}

function timeRuns(numRuns) {
    function deltaTimeNs(t) {
        const NS_PER_SEC = 1e9;
        return t[0] * NS_PER_SEC + t[1];
    }

    return res => {
        let totalTimeNs = 0.0;
        for (let i=0; i<numRuns; i++) {
            const ts = process.hrtime();
            res.instance.exports.main();
            const te = process.hrtime(ts);
            totalTimeNs += deltaTimeNs(te);
        }
        console.log(`Total time (ns): ${totalTimeNs}\nAvg time (ns): ${totalTimeNs/numRuns}`);
    };
}

function printMainOutput() {
    return res => {
        const r = res.instance.exports.main();
        console.log(`Main: ${r}`);
    };
}

function printMemContents(maxAddr) {
    return res => {
        const r = res.instance.exports.main();
        console.log(`Main: ${r}\n`);
        console.log("Memory Contents: ");
        const mem = new Uint32Array(res.instance.exports.memory.buffer);
        for (let i=0; i<maxAddr; i++) {
            console.log(mem[i]);
        }
    };
}

wasm('./out.wasm', printMemContents(5));
// wasm('./out.wasm', timeRuns(1));
