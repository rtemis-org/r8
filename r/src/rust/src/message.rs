//! The messages of R8's functions in R, over `rtemis_r8::message`: the text
//! that the core words, and the line that R prints with base `message()`.
//!
//! spec: R8/raven#summaries

use extendr_api::prelude::*;
use extendr_api::{Error, Result};
use rtemis_r8::message::{Body, Path, Place, Size, line, map_ended, map_started};

/// The language of the host, in the texts of messages.
const HOST: &str = "R";

/// Whether a package registered the body of the function `qname` in compiled
/// code, by the lookup that a call uses.
pub(crate) fn in_rust(qname: &str) -> bool {
    std::ffi::CString::new(qname.as_bytes()).is_ok_and(|c| {
        let key = unsafe { extendr_ffi::Rf_install(c.as_ptr()) };
        crate::exports::native_body(key).is_some()
    })
}

/// Whether a package registered the body of each function in compiled code.
///
/// @param qnames Character: The qualified names, or a vector of length 0.
/// @return Logical: One for each name.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_body_in_rust(qnames: Robj) -> Logicals {
    // R's strings read element by element: the array of an empty vector is no
    // address that a slice takes.
    let registered: Vec<Rbool> = crate::rview::strings(unsafe { qnames.get() })
        .into_iter()
        .map(|q| Rbool::from(q.is_some_and(|q| in_rust(&q))))
        .collect();
    Logicals::from_values(registered)
}

/// The text of a message of a long map, and the engine of its work.
///
/// @param phase Character \{"start", "end"\}: The message.
/// @param fields List: `label`, the function's name; `path`, `"rust"` for a
///   table's map or `"host"` for a list's; `functions` and `in_rust`, the
///   functions called and whether each body is compiled code; for `"start"`,
///   `columns`, `groups`, `items`, and `calls`, each or `NULL`; for `"end"`,
///   `wall`, `cpu`, and `threads`.
/// @return List: `text` and `engine`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_map_message(phase: &str, fields: List) -> Result<List> {
    let fields: Robj = fields.into();
    let get = |name: &str| crate::rview::element(&fields, name);
    // A number: an integer or a double.
    let number = |name: &str| {
        let v = get(name);
        v.as_real().or_else(|| v.as_integer().map(f64::from))
    };
    let count = |name: &str| number(name).map(|x| x.max(0.0) as u64);
    let functions: Vec<String> = get("functions")
        .as_str_vector()
        .map(|v| v.iter().map(|s| s.to_string()).collect())
        .unwrap_or_default();
    let in_rust: Vec<bool> = get("in_rust")
        .as_logical_vector()
        .map(|v| v.iter().map(|b| b.is_true()).collect())
        .unwrap_or_default();
    let place = Place {
        path: if get("path").as_str() == Some("host") {
            Path::Host
        } else {
            Path::Rust
        },
        bodies: functions
            .into_iter()
            .zip(in_rust.into_iter().chain(std::iter::repeat(false)))
            .map(|(function, in_rust)| Body { function, in_rust })
            .collect(),
    };
    let label = get("label").as_str().unwrap_or("f").to_string();
    let text = match phase {
        "start" => map_started(
            &label,
            &Size {
                columns: count("columns"),
                groups: count("groups"),
                items: count("items"),
                calls: count("calls"),
            },
            &place,
            HOST,
        ),
        "end" => map_ended(
            number("wall").unwrap_or(0.0),
            number("cpu").unwrap_or(0.0),
            count("threads").unwrap_or(1).max(1),
            &place,
            HOST,
        ),
        other => return Err(Error::Other(format!("no message `{other}`"))),
    };
    Ok(list!(text = text, engine = place.engine().name()))
}

/// The line of a message, `<timestamp> <text> [<caller>]`.
///
/// @param timestamp Character: The time, as `"%Y-%m-%d %H:%M:%S"` writes it.
/// @param text Character: The message.
/// @param caller Character: The function that writes it, or `""`.
/// @return Character.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_message_line(timestamp: &str, text: &str, caller: &str) -> String {
    line(timestamp, text, Some(caller))
}

extendr_module! {
    mod message;
    fn r8_body_in_rust;
    fn r8_map_message;
    fn r8_message_line;
}
