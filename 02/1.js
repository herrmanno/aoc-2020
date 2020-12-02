const input = require("fs").readFileSync(process.argv[2] || "input.txt").toString().split("\n")
const lines = input.map(line => {
    let [limits, ch, password] = line.split(" ")
    limits = limits.split("-")
    ch = ch[0]
    const chSize = [...password].filter(c => c == ch).length
    const valid = chSize >= + limits[0] && chSize <= limits[1]
    return [valid, limits, ch, password]
})

console.log(lines.filter(l => l[0]).length)
