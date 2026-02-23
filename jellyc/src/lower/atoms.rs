use crate::lower::LowerCtx;

pub(crate) fn intern_atom(name: &str, ctx: &mut LowerCtx) -> u32 {
    if let Some(&id) = ctx.atom_ids.get(name) {
        return id;
    }
    let id = ctx.atoms.len() as u32;
    ctx.atoms.push(name.as_bytes().to_vec());
    ctx.atom_ids.insert(name.to_string(), id);
    id
}
