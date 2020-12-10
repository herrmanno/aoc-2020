let input = require("fs").readFileSync("test2.txt").toString().split("\n").map(Number)
input = input.sort()
input.unshift(0)
input.push(Math.max(...input) + 3)

const groups = []

let tmp = [input[0]]
for (let i = 1; i < input.length; i++) {
    let el = input[i]
    if ()
}