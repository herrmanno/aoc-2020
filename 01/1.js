const input = require("fs").readFileSync("input.txt").toString().split("\n").map(Number).filter(n => !isNaN(n))

for (const n of input) {
    if (input.includes(2020 - n)) {
        console.log(n * (2020 - n))
        break
    }
}
