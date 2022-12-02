//@flow

declare var c: {
    <T>(T, (T) => void): void,
    (): void,
}

c<number>(42, (x) => {})
