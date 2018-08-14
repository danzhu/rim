use libc::{c_int, c_ulong};

pub enum Window {}

pub const ERR: c_int = -1;

#[link(name = "ncurses")]
extern "C" {
    pub fn initscr() -> *mut Window;
    pub fn cbreak() -> c_int;
    pub fn noecho() -> c_int;
    pub fn endwin() -> c_int;
    pub fn erase() -> c_int;
    pub fn timeout(time: c_int);
    pub fn addch(ch: c_ulong) -> c_int;
    pub fn getch() -> c_ulong;
    pub fn keypad(win: *mut Window, on: c_int) -> c_int;
    pub fn wmove(win: *mut Window, row: c_int, col: c_int) -> c_int;
    pub fn getmaxy(win: *mut Window) -> c_int;
    pub fn getmaxx(win: *mut Window) -> c_int;
    pub fn refresh() -> c_int;
    pub static stdscr: *mut Window;
}
