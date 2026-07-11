import React from "react";

type AlertSpec =
  | { level: "info"; message: string }
  | { level: "warning"; message: string; dismissible: boolean }
  | { level: "error"; message: string; code: number };

function Alert({ spec }: { spec: AlertSpec }) {
  switch (spec.level) {
    case "info":
      return <div className="info">{spec.message}</div>;
    case "warning":
      return (
        <div className="warning">
          {spec.message}
          {spec.dismissible && <button type="button">Dismiss</button>}
        </div>
      );
    case "error":
      return (
        <div className="error">
          [{spec.code}] {spec.message}
        </div>
      );
  }
}

export default function App() {
  return (
    <div>
      <Alert spec={{ level: "info", message: "All good" }} />
      <Alert
        spec={{ level: "warning", message: "Careful", dismissible: true }}
      />
      <Alert spec={{ level: "error", message: "Boom", code: 500 }} />
    </div>
  );
}
