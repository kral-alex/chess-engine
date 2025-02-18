use std::io;
use std::str::FromStr;
use regex::Regex;

use chess::{Game, Position, Color, PieceType};
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
    let handle = io::BufWriter::new(stdout);
    match std::env::args().nth(1).expect("no first argument").as_str() {
        "play" => play_chess(handle).expect("IO error"),
        _ => {}
    }
}

// print allowed moves
fn play_chess<T: io::Write>(mut handle: T) -> Result<(), io::Error> {
    let mut color = Color::White;
    writeln!(handle, "Hello, you are playing chess. \nWrite moves in format 'A1 to A2'")?;
    handle.flush()?;
    let mut game = Game::set_up();
    'main: loop {

        if game.get_allowed(color).is_empty() {
            write!(handle, "{} has won!", color.opposite())?;
            handle.flush()?;
            break
        }

        write!(handle, "{}: ", color)?;
        handle.flush()?;
        loop {
            let regex = Regex::new(".*(?<before>[A-H][1-8]).*(?<after>[A-H][1-8])").expect("Initializing regex failed");

            let mut input = String::new();
            or_continue!(handle, io::stdin().read_line(&mut input));
            let input = input.to_ascii_uppercase();

            if let Some(_) = input.find("/SHOW") {
                writeln!(handle, "{}", game)?;
                handle.flush()?;
                continue 'main
            }
            if let Some(_) = input.find("/HINT") {
                for m in game.get_allowed(color).iter() {
                    writeln!(handle, "{}", m)?;
                }
                handle.flush()?;
                continue 'main
            }

            let positions = or_continue!(handle,
                regex.captures(&input)
                .ok_or(NoPositionFound));

            let (before, after) = or_continue!(handle,
                positions.name("before")
                .zip(positions.name("after"))
                .ok_or(NoPositionFound));

            //println!("{}, {}", before.as_str(), after.as_str());

            let before = Position::from_str(before.as_str()).expect("Already approved in regex");
            let after = Position::from_str(after.as_str()).expect("Already approved in regex");
            match game.try_move(before, after, color) {
                Err(_) => {
                    write!(handle, "Not a valid move. Try again: ")?;
                    handle.flush()?;
                    continue
                }
                Ok(res) => {
                    if res.is_promotion() {
                        write!(handle, "Promote to the Queen, Knight, Rook, or Bishop: ")?;
                        handle.flush()?;

                        loop {
                            let mut input = String::new();
                            or_continue!(handle, io::stdin().read_line(&mut input));
                            if input.len() < 1 {or_continue!(handle, Err("Zero length input"))}
                            let piece_type = match input[0..1].to_uppercase().as_str() {
                                "Q" => PieceType::Queen,
                                "K" => PieceType::Knight,
                                "R" => PieceType::Rook { moved: true },
                                "B" => PieceType::Bishop,
                                _ => or_continue!(handle, Err("Not a valid piece type"))
                            };
                            game.promote_last(piece_type);
                            break
                        }
                    }
                    write!(handle, "{} moved: {}\n", color, res)?;
                    handle.flush()?;
                    break
                }
            }
        }
        color = color.opposite();
    };
    Ok(())
}

