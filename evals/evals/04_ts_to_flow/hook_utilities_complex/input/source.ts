import { useState, useCallback, useEffect } from "react";

function useToggle(initial: boolean = false): [boolean, () => void] {
  const [on, setOn] = useState(initial);
  const toggle = useCallback(() => setOn((v) => !v), []);
  return [on, toggle];
}

function useList<T>(
  initial: T[] = []
): [T[], (item: T) => void, () => void] {
  const [items, setItems] = useState<T[]>(initial);
  const add = useCallback((item: T) => {
    setItems((prev) => [...prev, item]);
  }, []);
  const clear = useCallback(() => {
    setItems([]);
  }, []);
  return [items, add, clear];
}

function useDebouncedValue<T>(value: T, delayMs: number): T {
  const [debounced, setDebounced] = useState(value);
  useEffect(() => {
    const id = setTimeout(() => setDebounced(value), delayMs);
    return () => clearTimeout(id);
  }, [value, delayMs]);
  return debounced;
}

export { useToggle, useList, useDebouncedValue };
