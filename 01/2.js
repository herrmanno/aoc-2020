const input = require("fs").readFileSync("input.txt").toString().split("\n").map(Number).filter(n => !isNaN(n))

loop: for (const n of input) {
    for (const m of input.slice(input.indexOf(n) + 1)) {
        const k = 2020 - n - m;
        if (input.includes(k)) {
            console.log(n * m * k)
            break loop
        }
    }
}
