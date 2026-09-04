//@flow

// React.ElementType was behaving differently from React$ElementType when evaluated by
// React.ElementConfig due to a mishandled type destructor case. These should be interchangeable.
declare const aliasProps: React.ElementConfig<React.ElementType>;
aliasProps as React.ElementConfig<React$ElementType>;

declare const dollarProps: React.ElementConfig<React$ElementType>;
dollarProps as React.ElementConfig<React.ElementType>;
