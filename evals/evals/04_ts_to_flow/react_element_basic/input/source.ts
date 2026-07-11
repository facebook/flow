import React, { ReactNode, ReactElement } from "react";

interface PanelProps {
  header: ReactNode;
  children: ReactNode;
  footer?: ReactNode;
}

function Panel({ header, children, footer }: PanelProps) {
  return (
    <section className="panel">
      <header>{header}</header>
      <div className="panel-body">{children}</div>
      {footer != null && <footer>{footer}</footer>}
    </section>
  );
}

function repeat(element: ReactElement, times: number): ReactElement {
  const copies: ReactElement[] = [];
  for (let i = 0; i < times; i++) {
    copies.push(element);
  }
  return <>{copies}</>;
}

export default function App() {
  return (
    <Panel header={<h1>Title</h1>} footer={<small>fin</small>}>
      {repeat(<span>hi</span>, 3)}
    </Panel>
  );
}
