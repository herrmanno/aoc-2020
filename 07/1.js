const input = require("fs").readFileSync("input.txt").toString().split("\n")
const obj = input.reduce(parseRule, {})
search(obj, "shiny gold")

function parseRule(obj, line) {
    if (/no other/.test(line)) {
        return obj
    }
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

function search(obj, key, what) {
    if (key == what) return true
    if (!(key in obj)) return false
    else return Object.keys(obj[key]).some(k => search(obj, k, what))
}

const result = Object.keys(obj).map(k => search(obj, k, "shiny gold"))
console.log(result.filter(Boolean).length - 1)
