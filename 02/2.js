const input = require("fs").readFileSync(process.argv[2] || "input.txt").toString().split("\n")
const lines = input.map(line => {
    let [limits, ch, password] = line.split(" ")
    const [min,max] = limits.split("-").map(Number).map(n => n-1)
    ch = ch[0]
    const valid = password[min] == ch ^ password[max] == ch
    return [valid, limits, ch, password]
})

console.log(lines.filter(l => l[0]).length)
