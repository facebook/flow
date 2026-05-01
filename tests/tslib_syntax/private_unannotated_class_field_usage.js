import ThemeCache from './private_unannotated_class_field';

const t = new ThemeCache();
t.size as number; // OK
t.cache; // ERROR: prop-missing (private not in public type)
