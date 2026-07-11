import React, { ComponentType } from "react";

function withBorder<P extends object>(Wrapped: ComponentType<P>): ComponentType<P> {
  return function Bordered(props: P) {
    return (
      <div className="bordered">
        <Wrapped {...props} />
      </div>
    );
  };
}

function withTimestamp<P extends { now: number }>(
  Wrapped: ComponentType<P>
): ComponentType<Omit<P, "now">> {
  return function Timestamped(props: Omit<P, "now">) {
    const full = { ...props, now: Date.now() } as P;
    return <Wrapped {...full} />;
  };
}

interface ClockProps {
  now: number;
  label: string;
}

function Clock({ now, label }: ClockProps) {
  return (
    <div>
      {label}: {new Date(now).toISOString()}
    </div>
  );
}

const BorderedClock = withBorder(Clock);
const AutoClock = withTimestamp(Clock);

export { withBorder, withTimestamp, Clock, BorderedClock, AutoClock };
