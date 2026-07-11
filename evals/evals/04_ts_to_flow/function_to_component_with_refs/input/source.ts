import React, { forwardRef, useImperativeHandle, useRef } from "react";

interface InputHandle {
  focus: () => void;
  clear: () => void;
}

interface FancyInputProps {
  placeholder: string;
  initialValue?: string;
}

const FancyInput = forwardRef<InputHandle, FancyInputProps>(
  ({ placeholder, initialValue }, ref) => {
    const inputRef = useRef<HTMLInputElement>(null);

    useImperativeHandle(ref, () => ({
      focus: () => {
        inputRef.current?.focus();
      },
      clear: () => {
        if (inputRef.current) {
          inputRef.current.value = "";
        }
      },
    }));

    return (
      <input
        ref={inputRef}
        placeholder={placeholder}
        defaultValue={initialValue}
      />
    );
  }
);

FancyInput.displayName = "FancyInput";

export default FancyInput;
