mod imp;

use lib::{Pos, Size};
use libc::{c_int, c_ulong};
use std::{char, result};

#[derive(Clone, Copy, Debug)]
pub enum Error {
    Err,
    Null,
    Decode,
}

pub type Result<T> = result::Result<T, Error>;

fn check(ret: i32) -> Result<()> {
    match ret {
        imp::ERR => Err(Error::Err),
        _ => Ok(()),
    }
}

pub fn initscr() -> Result<()> {
    if unsafe { imp::initscr() }.is_null() {
        Err(Error::Null)
    } else {
        Ok(())
    }
}

pub fn cbreak() -> Result<()> {
    check(unsafe { imp::cbreak() })
}

pub fn noecho() -> Result<()> {
    check(unsafe { imp::noecho() })
}

pub fn endwin() -> Result<()> {
    check(unsafe { imp::endwin() })
}

pub fn addch(ch: char) -> Result<()> {
    check(unsafe { imp::addch(ch as c_ulong) })
}

pub fn addstr(st: &str) -> Result<()> {
    for ch in st.chars() {
        addch(ch)?;
    }
    Ok(())
}

pub fn getch() -> Result<char> {
    let ch = unsafe { imp::getch() };
    if ch == imp::ERR as c_ulong {
        Err(Error::Err)
    } else {
        char::from_u32(ch as u32).ok_or(Error::Decode)
    }
}

pub fn keypad(on: bool) -> Result<()> {
    check(unsafe { imp::keypad(imp::stdscr, if on { 1 } else { 0 }) })
}

pub fn move_to(pos: Pos) -> Result<()> {
    check(unsafe { imp::wmove(imp::stdscr, pos.line as c_int, pos.column as c_int) })
}

pub fn erase() -> Result<()> {
    check(unsafe { imp::erase() })
}

pub fn refresh() -> Result<()> {
    check(unsafe { imp::refresh() })
}

pub fn size() -> Size {
    Size {
        height: unsafe { imp::getmaxy(imp::stdscr) },
        width: unsafe { imp::getmaxx(imp::stdscr) },
    }
}
