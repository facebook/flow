import type {C} from './wrapper';
import expectFooProps from './wrapper';

declare const props: React.ElementConfig<C>;
expectFooProps({...props}); // busted due to subst cache collision
