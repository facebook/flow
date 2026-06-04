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
    use flow_data_structure_wrapper::smol_str::FlowSmolStr;

    use crate::ty::Dict;
    use crate::ty::FunEffect;
    use crate::ty::FunT;
    use crate::ty::NamedProp;
    use crate::ty::ObjKind;
    use crate::ty::ObjT;
    use crate::ty::Polarity;
    use crate::ty::Prop;
    use crate::ty::PropSource;
    use crate::ty::ReturnT;
    use crate::ty::TupleElement;
    use crate::ty::Ty;
    use crate::ty::TypeParam;
    use crate::ty_printer::PrinterOptions;
    use crate::ty_printer::string_of_t;

    fn test_options() -> PrinterOptions {
        PrinterOptions {
            prefer_single_quotes: false,
            size: 80,
            with_comments: false,
        }
    }

    #[test]
    fn test_object_property_get() {
        let getter = Prop::NamedProp {
            name: Name::new("foo"),
            prop: NamedProp::Get(Arc::new(Ty::Str)),
            inherited: false,
            source: PropSource::Other,
            def_locs: vec![].into(),
        };
        let obj = Ty::<ALoc>::Obj(Box::new(ObjT {
            obj_kind: ObjKind::ExactObj,
            obj_def_loc: None,
            obj_props: vec![getter].into(),
        }));
        let opts = test_options();
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
        let obj = Ty::<ALoc>::Obj(Box::new(ObjT {
            obj_kind: ObjKind::ExactObj,
            obj_def_loc: None,
            obj_props: vec![setter].into(),
        }));
        let opts = test_options();
        let result = string_of_t(&obj, &opts);
        assert_eq!(result, "{set foo(string): void}");
    }

    #[test]
    fn test_empty_inexact_tuple() {
        let tup = Ty::<ALoc>::Tup {
            elements: vec![].into(),
            inexact: true,
        };
        let opts = test_options();
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
        let opts = test_options();
        let result = string_of_t(&tup, &opts);
        assert_eq!(result, "[number, ...]");
    }

    #[test]
    fn test_top_type() {
        let opts = test_options();
        let result = string_of_t(&Ty::<ALoc>::Top, &opts);
        assert_eq!(result, "unknown");
    }

    #[test]
    fn test_variance_keywords() {
        let mk_field = |name: &str, polarity: Polarity, t: Ty<ALoc>| Prop::NamedProp {
            name: Name::new(name),
            prop: NamedProp::Field {
                t: Arc::new(t),
                polarity,
                optional: false,
            },
            inherited: false,
            source: PropSource::Other,
            def_locs: vec![].into(),
        };
        let obj = Ty::<ALoc>::Obj(Box::new(ObjT {
            obj_kind: ObjKind::IndexedObj(Dict {
                dict_polarity: Polarity::Positive,
                dict_name: Some(FlowSmolStr::new("key")),
                dict_key: Arc::new(Ty::Str),
                dict_value: Arc::new(Ty::Top),
            }),
            obj_def_loc: None,
            obj_props: vec![
                mk_field("ro", Polarity::Positive, Ty::Str),
                mk_field("wo", Polarity::Negative, Ty::Num),
            ]
            .into(),
        }));
        let opts = test_options();
        let result = string_of_t(&obj, &opts);
        assert_eq!(
            result,
            "{readonly [key: string]: unknown, readonly ro: string, writeonly wo: number}"
        );
    }

    #[test]
    fn test_type_param_bound_and_variance() {
        let func = Ty::<ALoc>::Fun(Box::new(FunT {
            fun_params: vec![].into(),
            fun_rest_param: None,
            fun_return: ReturnT::ReturnType(Arc::new(Ty::Void)),
            fun_type_params: Some(
                vec![TypeParam {
                    tp_name: FlowSmolStr::new("T"),
                    tp_bound: Some(Arc::new(Ty::Top)),
                    tp_polarity: Polarity::Positive,
                    tp_default: None,
                    tp_const: false,
                }]
                .into(),
            ),
            fun_static: Arc::new(Ty::Top),
            fun_effect: FunEffect::Arbitrary,
        }));
        let opts = test_options();
        let result = string_of_t(&func, &opts);
        assert_eq!(result, "<out T extends unknown>() => void");
    }
}
