#[path = "validate/ctx.rs"]
mod ctx;
#[path = "validate/helpers.rs"]
mod helpers;
#[path = "validate/switch_kind.rs"]
mod switch_kind;

#[path = "validate/ops/mod.rs"]
mod ops;

#[path = "validate/entry.rs"]
mod entry;
#[path = "validate/strict.rs"]
mod strict;

pub use entry::validate_module;
#[cfg(test)]
pub use strict::validate_module_strict;
