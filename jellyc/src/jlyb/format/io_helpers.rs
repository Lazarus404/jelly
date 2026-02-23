use std::io::{self, Read, Write};

pub(super) fn wr_u8<W: Write>(w: &mut W, v: u8) -> io::Result<()> {
    w.write_all(&[v])
}

pub(super) fn wr_u16le<W: Write>(w: &mut W, v: u16) -> io::Result<()> {
    w.write_all(&v.to_le_bytes())
}

pub(super) fn wr_u32le<W: Write>(w: &mut W, v: u32) -> io::Result<()> {
    w.write_all(&v.to_le_bytes())
}

pub(super) fn rd_u8<R: Read>(r: &mut R) -> Result<u8, io::Error> {
    let mut b = [0u8; 1];
    r.read_exact(&mut b)?;
    Ok(b[0])
}

pub(super) fn rd_u16le<R: Read>(r: &mut R) -> Result<u16, io::Error> {
    let mut b = [0u8; 2];
    r.read_exact(&mut b)?;
    Ok(u16::from_le_bytes(b))
}

pub(super) fn rd_u32le<R: Read>(r: &mut R) -> Result<u32, io::Error> {
    let mut b = [0u8; 4];
    r.read_exact(&mut b)?;
    Ok(u32::from_le_bytes(b))
}
