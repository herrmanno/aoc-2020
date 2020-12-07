const input = require("fs").readFileSync("input.txt").toString().split("\n")
const obj = input.reduce(parseRule, {})

function parseRule(obj, line) {
    if (/no other/.test(line)) return obj
    
    const re = /((?:\w+\s)+)bags contain ((?:\d+\s+(?:\w+\s)+bags?[,|.]\s?)*)/
    const [_, color, rules] = re.exec(line)
    return {
        ...obj,
        [color.trim()]: rules.split(", ").reduce((acc,rule) => {
            const [_, num, color] = /(\d+)\s+((?:\w+\s)+?)bag/.exec(rule)
            return { ...acc, [color.trim()]: num }
        }, {})
    }
}

function count(obj, key) {
    if (!(key in obj)) return 1
    if (!isNaN(obj[key])) return +obj[key]
    else return 1 + Object.keys(obj[key]).map(k => +obj[key][k] * count(obj, k)).reduce((a,b) => a+b, 0)
}

const result = count(obj, "shiny gold") - 1 /* itself */
console.log(result)
