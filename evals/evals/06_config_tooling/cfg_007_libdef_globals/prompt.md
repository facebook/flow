`main.js` uses two globals injected by the runtime environment: a `track(event, payload)` function and an `Analytics` namespace with an `appId` string. Neither is declared, so `flow check` fails:

```
Cannot resolve name `track`. [cannot-resolve-name]
Cannot resolve name `Analytics`. [cannot-resolve-name]
```

Declare these ambient globals so the project type-checks, without changing `main.js`. Put the declarations in a new library-definition file under a `lib/` directory, and configure the project so Flow loads library definitions from `lib/`. The shapes:

- `track(event: string, payload: {userId: string}): void`
- `Analytics.appId` is a `string`
