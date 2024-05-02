// hints propagate through "as const"
const arr: readonly [(x: string) => void] = [(x) => {}] as const; // okay
const obj: {+f: (x: string) => void} = {f: (x) => {}} as const; // okay
