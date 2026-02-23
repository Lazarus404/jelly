use std::collections::HashMap;

pub fn build_module_abi_blob(exports: &HashMap<String, u32>, import_keys: &[String]) -> Vec<u8> {
    fn wr_u32le(out: &mut Vec<u8>, v: u32) {
        out.extend_from_slice(&v.to_le_bytes());
    }

    let mut out: Vec<u8> = Vec::new();
    out.extend_from_slice(b"JLYMODABI1\0");

    let mut names: Vec<(&String, &u32)> = exports.iter().collect();
    names.sort_by(|a, b| a.0.cmp(b.0));
    wr_u32le(&mut out, names.len() as u32);
    for (name, tid) in names {
        wr_u32le(&mut out, name.len() as u32);
        out.extend_from_slice(name.as_bytes());
        wr_u32le(&mut out, *tid);
    }

    wr_u32le(&mut out, import_keys.len() as u32);
    for k in import_keys {
        wr_u32le(&mut out, k.len() as u32);
        out.extend_from_slice(k.as_bytes());
    }

    out
}
