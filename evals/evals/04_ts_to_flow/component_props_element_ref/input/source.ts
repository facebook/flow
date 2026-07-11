import React, { forwardRef, ReactNode } from "react";

interface CardProps {
  title: string;
  children: ReactNode;
  footer?: ReactNode;
  onDismiss?: () => void;
}

const Card = forwardRef<HTMLDivElement, CardProps>(
  ({ title, children, footer, onDismiss }, ref) => {
    return (
      <div ref={ref} className="card">
        <div className="card-header">
          <h2>{title}</h2>
          {onDismiss && (
            <button type="button" onClick={onDismiss}>
              Close
            </button>
          )}
        </div>
        <div className="card-body">{children}</div>
        {footer && <div className="card-footer">{footer}</div>}
      </div>
    );
  }
);

Card.displayName = "Card";

export default Card;
