import type {C} from './wrapper';
import expectFooProps from './wrapper';

declare const props: React.ComponentProps<C>;
expectFooProps({...props}); // ok, previously busted due to subst cache collision
