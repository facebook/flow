/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A clock identifying a point in a Flow server instance's life, returned by `flow query` and fed
//! back as `--since`. The daemon state that produces it lives in `flow_server_env::flow_clock`.

use serde::Deserialize;
use serde::Deserializer;
use serde::Serialize;
use serde::Serializer;
use serde::de::value::Error as ValueError;
use serde::de::value::StrDeserializer;
use uuid::Uuid;

const CLOCK_TAG: &str = "flow";
const CLOCK_VERSION: u32 = 1;

#[derive(Debug, Clone)]
pub struct FlowClock {
    instance_id: Uuid,
    counter: u64,
}

impl FlowClock {
    pub fn new(instance_id: Uuid, counter: u64) -> FlowClock {
        FlowClock {
            instance_id,
            counter,
        }
    }

    pub fn instance_id(&self) -> Uuid {
        self.instance_id
    }

    pub fn counter(&self) -> u64 {
        self.counter
    }
}

impl Serialize for FlowClock {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let token = format!(
            "{CLOCK_TAG}:{CLOCK_VERSION}:{}:{}",
            self.instance_id.simple(),
            self.counter
        );
        serializer.serialize_str(&token)
    }
}

impl<'de> Deserialize<'de> for FlowClock {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let token = String::deserialize(deserializer)?;
        let mut parts = token.split(':');
        let tag = parts.next();
        let version = parts.next().and_then(|v| v.parse::<u32>().ok());
        let instance_id = parts.next().and_then(|s| Uuid::try_parse(s).ok());
        let counter = parts.next().and_then(|s| s.parse::<u64>().ok());
        match (tag, version, instance_id, counter, parts.next()) {
            (Some(CLOCK_TAG), Some(CLOCK_VERSION), Some(instance_id), Some(counter), None) => {
                Ok(FlowClock::new(instance_id, counter))
            }
            _ => Err(serde::de::Error::custom(format!(
                "invalid flow_clock token: {token}"
            ))),
        }
    }
}

/// A `--since` clock: a recognized `FlowClock`, or `Unrecognized` for a token this server can't
/// parse (wrong version/encoding), which a `--since` query treats as "report everything".
#[derive(Debug, Clone)]
pub enum Since {
    Clock(FlowClock),
    Unrecognized(String),
}

impl Serialize for Since {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            Since::Clock(clock) => clock.serialize(serializer),
            Since::Unrecognized(token) => serializer.serialize_str(token),
        }
    }
}

impl<'de> Deserialize<'de> for Since {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let token = String::deserialize(deserializer)?;
        // A token this server can't parse is kept verbatim rather than failing the query.
        match FlowClock::deserialize(StrDeserializer::<ValueError>::new(&token)) {
            Ok(clock) => Ok(Since::Clock(clock)),
            Err(_) => Ok(Since::Unrecognized(token)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn flow_clock_round_trips_through_serde() {
        let clock = FlowClock::new(Uuid::from_u128(0x1234), 7);
        let value = serde_json::to_value(&clock).unwrap();
        assert_eq!(
            value,
            serde_json::json!(format!("flow:1:{:032x}:7", 0x1234u128))
        );
        let parsed: FlowClock = serde_json::from_value(value).unwrap();
        assert_eq!(parsed.instance_id(), clock.instance_id());
        assert_eq!(parsed.counter(), clock.counter());
    }

    #[test]
    fn since_decodes_clock_or_unrecognized() {
        let valid = format!("\"flow:1:{:032x}:0\"", 0u128);
        assert!(matches!(
            serde_json::from_str::<Since>(&valid).unwrap(),
            Since::Clock(_)
        ));
        // A wrong version/tag/encoding is Unrecognized, not a deserialize error.
        for bad in [
            "\"flow:999:0:0\"",
            "\"c:deadbeef:0\"",
            "\"flow:1:nothex:0\"",
        ] {
            assert!(matches!(
                serde_json::from_str::<Since>(bad).unwrap(),
                Since::Unrecognized(_)
            ));
        }
    }

    #[test]
    fn since_round_trips_as_a_string() {
        let valid = format!("flow:1:{:032x}:3", 0xabcu128);
        for token in [valid.as_str(), "garbage"] {
            let since: Since = serde_json::from_str(&format!("\"{token}\"")).unwrap();
            assert_eq!(
                serde_json::to_value(&since).unwrap(),
                serde_json::Value::String(token.to_string()),
            );
        }
    }
}
