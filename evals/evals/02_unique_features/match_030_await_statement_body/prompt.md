Write an async Flow function `runJob(job: Job): Promise<void>` that uses a `match` statement to carry out a job.

The `Job` type and the `doFetch` and `delay` helpers are provided.

- a `'fetch'` job awaits `doFetch(url)`
- a `'wait'` job awaits `delay(ms)`
- a `'noop'` job does nothing
