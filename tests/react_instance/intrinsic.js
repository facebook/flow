declare var any: any;

(any as React.ElementRef<'foo'>).yep1; // OK
(any as React.ElementRef<'foo'>).yep2; // Error
(any as React.ElementRef<'foo'>).nope; // Error
(any as React.ElementRef<'bar'>).yep1; // Error
(any as React.ElementRef<'bar'>).yep2; // OK
(any as React.ElementRef<'bar'>).nope; // Error
