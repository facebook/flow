interface T { ['m'](): X }
interface U { [42](): X, [-1](): Y }
interface V { ['m']?(): X }
interface W { ['g']<A>(x: A): A }
interface Y { ['m'](): X, [string]: Y }
