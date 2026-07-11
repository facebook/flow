import React, { ChangeEvent, MouseEvent, KeyboardEvent, CSSProperties } from "react";

interface SearchFieldProps {
  value: string;
  placeholder?: string;
  disabled?: boolean;
  containerStyle?: CSSProperties;
  onChange: (value: string) => void;
  onSubmit: (value: string) => void;
}

function SearchField({
  value,
  placeholder,
  disabled,
  containerStyle,
  onChange,
  onSubmit,
}: SearchFieldProps) {
  const handleChange = (e: ChangeEvent<HTMLInputElement>) => {
    onChange(e.currentTarget.value);
  };

  const handleKeyDown = (e: KeyboardEvent<HTMLInputElement>) => {
    if (e.key === "Enter") {
      onSubmit(e.currentTarget.value);
    }
  };

  const handleClick = (e: MouseEvent<HTMLButtonElement>) => {
    e.preventDefault();
    onSubmit(value);
  };

  return (
    <div style={containerStyle} className="search-field">
      <input
        type="search"
        value={value}
        placeholder={placeholder}
        disabled={disabled}
        onChange={handleChange}
        onKeyDown={handleKeyDown}
      />
      <button type="button" disabled={disabled} onClick={handleClick}>
        Search
      </button>
    </div>
  );
}

export default SearchField;
