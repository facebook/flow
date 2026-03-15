/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use flow_aloc::ALoc;
    use flow_common::reason::Name;

    use crate::ty::NamedProp;
    use crate::ty::ObjKind;
    use crate::ty::ObjT;
    use crate::ty::Polarity;
    use crate::ty::Prop;
    use crate::ty::PropSource;
    use crate::ty::TupleElement;
    use crate::ty::Ty;
    use crate::ty_printer::PrinterOptions;
    use crate::ty_printer::string_of_t;

    #[test]
    fn test_object_property_get() {
        let getter = Prop::NamedProp {
            name: Name::new("foo"),
            prop: NamedProp::Get(Arc::new(Ty::Str)),
            inherited: false,
            source: PropSource::Other,
            def_locs: vec![].into(),
        };
        let obj = Ty::<ALoc>::Obj(ObjT {
            obj_kind: ObjKind::ExactObj,
            obj_def_loc: None,
            obj_props: vec![getter].into(),
        });
        let opts = PrinterOptions {
            prefer_single_quotes: false,
            size: 80,
            with_comments: false,
            exact_by_default: true,
            ts_syntax: false,
        };
        let result = string_of_t(&obj, &opts);
        assert_eq!(result, "{get foo(): string}");
    }

    #[test]
    fn test_object_property_set() {
        let setter = Prop::NamedProp {
            name: Name::new("foo"),
            prop: NamedProp::Set(Arc::new(Ty::Str)),
            inherited: false,
            source: PropSource::Other,
            def_locs: vec![].into(),
        };
        let obj = Ty::<ALoc>::Obj(ObjT {
            obj_kind: ObjKind::ExactObj,
            obj_def_loc: None,
            obj_props: vec![setter].into(),
        });
        let opts = PrinterOptions {
            prefer_single_quotes: false,
            size: 80,
            with_comments: false,
            exact_by_default: true,
            ts_syntax: false,
        };
        let result = string_of_t(&obj, &opts);
        assert_eq!(result, "{set foo(string): void}");
    }

    #[test]
    fn test_empty_inexact_tuple() {
        let tup = Ty::<ALoc>::Tup {
            elements: vec![].into(),
            inexact: true,
        };
        let opts = PrinterOptions {
            prefer_single_quotes: false,
            size: 80,
            with_comments: false,
            exact_by_default: false,
            ts_syntax: false,
        };
        let result = string_of_t(&tup, &opts);
        assert_eq!(result, "[...]");
    }

    #[test]
    fn test_inexact_tuple() {
        let elements = vec![TupleElement::TupleElement {
            name: None,
            t: Arc::new(Ty::Num),
            polarity: Polarity::Neutral,
            optional: false,
        }];
        let tup = Ty::<ALoc>::Tup {
            elements: elements.into(),
            inexact: true,
        };
        let opts = PrinterOptions {
            prefer_single_quotes: false,
            size: 80,
            with_comments: false,
            exact_by_default: false,
            ts_syntax: false,
        };
        let result = string_of_t(&tup, &opts);
        assert_eq!(result, "[number, ...]");
    }
}
