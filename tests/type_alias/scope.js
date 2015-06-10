// Mutually recursive type aliases

type Edge = {
  vs : [Vertex, Vertex]
}
type Vertex = {
  es : [Edge]
}

var ok: Vertex = {
  es: [{
    vs: [
      { es: [] },
      { es: [] }
    ]
  }]
}

var fail: Vertex = {
  es: [{}] // edge missing vs
}
