import Foo from './export_default_component_type';
import useBar from './export_default_hook_type';

Foo as component(bar: string) renders 'svg'; // ok
Foo as component(bar: number) renders 'svg'; // error

useBar as hook (bar: string) => void; // ok
useBar as hook (bar: number) => void; // error
