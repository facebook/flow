/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;

use dupe::Dupe;
use flow_common::reason::Name;
use flow_common::reason::VirtualReasonDesc;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;

#[derive(Debug, Clone, Dupe, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Base {
    OrdinaryIdentifier(FlowSmolStr),
    This,
    Super,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Proj {
    Prop(FlowSmolStr),
    Elem(Box<Key>),
    PrivateField(FlowSmolStr),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Key(pub Base, pub Vec<Proj>);

impl fmt::Display for Key {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Key(base, projs) = self;
        match base {
            Base::OrdinaryIdentifier(name) => write!(f, "{}", name)?,
            Base::This => write!(f, "this")?,
            Base::Super => write!(f, "super")?,
        };
        for proj in projs.iter().rev() {
            match proj {
                Proj::Prop(name) => write!(f, ".{}", name)?,
                Proj::PrivateField(name) => write!(f, "private.{}", name)?,
                Proj::Elem(expr) => write!(f, "[{}]", expr)?,
            }
        }
        Ok(())
    }
}

impl Key {
    /// true if the given key uses the given property name
    pub fn uses_propname(&self, propname: &str, private_: bool) -> bool {
        let Key(_base, proj) = self;
        Self::proj_uses_propname(private_, propname, proj)
    }

    fn proj_uses_propname(private_: bool, propname: &str, proj: &[Proj]) -> bool {
        match proj {
            [Proj::Prop(name), tail @ ..] => {
                (name.as_str() == propname && !private_)
                    || Self::proj_uses_propname(private_, propname, tail)
            }
            [Proj::PrivateField(name), tail @ ..] => {
                (name.as_str() == propname && private_)
                    || Self::proj_uses_propname(private_, propname, tail)
            }
            [Proj::Elem(key), tail @ ..] => {
                key.uses_propname(propname, private_)
                    || Self::proj_uses_propname(private_, propname, tail)
            }
            [] => false,
        }
    }

    pub fn is_simple(&self) -> bool {
        let Key(_, ps) = self;
        ps.is_empty()
    }

    pub fn reason_desc<L: Dupe>(&self) -> VirtualReasonDesc<L> {
        use flow_common::reason::VirtualReasonDesc::*;

        let Key(base, projs) = self;
        match (base, projs.as_slice()) {
            (Base::OrdinaryIdentifier(name), []) => RIdentifier(Name::new(name.dupe())),
            (Base::This, []) => RThis,
            (Base::Super, []) => RSuper,
            (_, projs) => match projs.last().unwrap() {
                Proj::Prop(x) => RProperty(Some(Name::new(x.dupe()))),
                Proj::PrivateField(x) => RPrivateProperty(x.dupe()),
                Proj::Elem(_) => RProperty(None),
            },
        }
    }
}
