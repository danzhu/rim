extern crate libc;

// mod ncurses;
mod script;

// fn run() -> ncurses::Result<()> {
//     ncurses::initscr()?;
//     ncurses::cbreak()?;
//     ncurses::keypad(true)?;
//     ncurses::noecho()?;

//     ncurses::getch()?;

//     ncurses::endwin()
// }

fn main() {
    let mut script = script::Script::new();
    script.load_file("test.fin").expect("startup file");
    script.run();
    // run().expect("ncurses error");
}
