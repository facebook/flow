// @flow

function x(a: number = throw 'x') {
  ;(a: string) // error
}

x()
x(1)
