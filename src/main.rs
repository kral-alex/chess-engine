use std::fmt::Display;
use std::io;
use std::str::FromStr;
use regex::Regex;

use chess::{Game, Position, Color};
use log::warn;

macro_rules! or_continue {
    ($handle: expr, $res: expr) => {
        match $res {
            Ok(val) => val,
            Err(e) => {
                warn!("An error: {:?}.", e);
                write!($handle, "Wrong format. Try again: ")?;
                $handle.flush()?;
                continue;
            }
        }
    };
}

#[derive(Debug, PartialEq)]
struct NoPositionFound;

fn main() {
    let stdout = io::stdout(); // the global stdout entity
    let mut handle = io::BufWriter::new(stdout);
    match std::env::args().nth(1).expect("no first argument").as_str() {
        "play" => play_chess(handle).expect("IO error"),
        _ => {}
    }
}

fn play_chess<T: io::Write>(mut handle: T) -> Result<(), io::Error> {
    let mut counter: i32 = 0;
    writeln!(handle, "Hello, you are playing chess. \nWrite moves in format 'A1 to A2'")?;
    handle.flush()?;
    let mut game = Game::set_up();
    loop {
        let color = match counter % 2 {
            0 => Color::White,
            1 => Color::Black,
            _ => unreachable!()
        };
        counter += 1;
        write!(handle, "{}: ", color)?;
        handle.flush()?;
        loop {
            let regex = Regex::new(".*(?<before>[A-H][1-8]).*(?<after>[A-H][1-8])").expect("Initializing regex failed");
            let mut input = String::new();
            or_continue!(handle, io::stdin().read_line(&mut input));

            let positions = or_continue!(handle,
                regex.captures(&input)
                .ok_or(NoPositionFound));

            let (before, after) = or_continue!(handle,
                positions.name("before")
                .zip(positions.name("after"))
                .ok_or(NoPositionFound));

            println!("{}, {}", before.as_str(), after.as_str());

            let before = or_continue!(handle, Position::from_str(before.as_str()));
            let after = or_continue!(handle, Position::from_str(after.as_str()));
            match game.try_move(before, after, color) {
                Err(_) => {
                    write!(handle, "Not a valid move. Try again: ")?;
                    handle.flush()?;
                    continue
                }
                Ok(res) => break
            }
        }
    };
}

