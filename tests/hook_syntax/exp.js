export hook useFoo(): [number] {
    return [42];
}

useFoo() as empty; // error
