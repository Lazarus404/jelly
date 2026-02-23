macro_rules! assert_repo_snapshot {
    ($name:expr, $value:expr) => {{
        let mut settings = insta::Settings::clone_current();
        settings.set_snapshot_path("../../../../snapshots");
        settings.set_prepend_module_to_snapshot(false);
        settings.bind(|| {
            insta::assert_snapshot!($name, $value);
        });
    }};
}

mod bench;
mod errors;
mod imports;
mod lowering;
mod repl;
mod snapshots;
