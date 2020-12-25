const input = "389125467"

const range = (n,m) => new Array(m+1-n).fill(n).map((v,i) => v+i)

const subtract = (a1, a2) => a1.filter(e => !a2.includes(e))

const m = new Map()

for (let i = 0; i < 1000000; i++) {
    if (i < input.length -1) {
        m.set(+input[i], +input[i+1])
    } else if (i == input.length -1) {
        m.set(+input[i], 10)
    } else {
        m.set(i+1, i+2)
    }
}

m.set(1000000, +input[0])

let curr = +input[0]

for (let i = 0; i < 1000000; i++) {
    const a = m.get(curr)
    const b = m.get(a)
    const c = m.get(b)
    const nextCurr = m.get(c)

    const currIsMin = curr == 1 || range(1,curr-1).every(i => [a,b,c].includes(i))
    const dst = currIsMin
        ? Math.max.apply(null, subtract(range(999995,1000000), [curr,a,b,c]))
        : Math.min.apply(null, subtract(range(1,curr-1), [a,b,c]))
    const dstF = m.get(dst)

    m.set(curr, nextCurr)
    m.set(dst, a)
    m.set(c, dstF)

    curr = nextCurr
}

const a = m.get(1)
const b = m.get(a)
console.log([a,b])