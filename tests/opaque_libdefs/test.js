//@flow

var q: Queue = new_queue();
enqueue(q, 3);
dequeue(q);

function flowsQueueToAny(q: Queue): any { return q; }
function flowsSomethingToQueue(x: number): Queue { return x; } // Error number ~> Queue
function flowsQueueToSomething(q: Queue): number {return q; } // Error Queue ~> number

var c = init_counter();
var y = c + 1; // Fine, since Counter is a number.
counter_to_number(y); // Error: number ~> Counter
