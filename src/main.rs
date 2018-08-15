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
    script.load_file("test.fin").expect("failed to load");
    match script.run() {
        Ok(_) => {}
        Err(err) => {
            script.runtime().dump();
            panic!(format!("failed to run: {:?}", err));
        }
    }
    // run().expect("ncurses error");
}
