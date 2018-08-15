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

fn load(script: &mut script::Script) -> script::Result<()> {
    script.load_file("test.fin")?;
    script.run()
}

fn main() {
    let mut script = script::Script::new();
    match load(&mut script) {
        Ok(_) => {}
        Err(err) => {
            script.memory().dump();
            println!("failed to run: {:?}", err);
        }
    }
    // run().expect("ncurses error");
}
