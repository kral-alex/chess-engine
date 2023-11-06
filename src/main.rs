use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::str::FromStr;

use chess;
use chess::{Game, Position, convert_pos, print_moves};

fn main() {
    println!("Hello, world!");
    let mut board: Game = Game::set_up();
    let my_pos = Position::from_str("D2").expect("should be fine");
    println!("{:#?}", my_pos);
    println!("{:#?}", board);

    //let prev_move = board.do_move_unchecked(convert_pos("B2"), convert_pos("B3")).expect("");
    let prev_move = board.make_move_unsafe(convert_pos("B7"), convert_pos("B4")).expect("");
    let prev_move = board.make_move_unsafe(convert_pos("B2"), convert_pos("E4")).expect("");
    board.pieces.get_mut(&convert_pos("B4")).unwrap().piece_type = chess::PieceType::Pawn(chess::MovedState::Moved);
    let prev_move = board.make_move_unsafe(convert_pos("C2"), convert_pos("C4")).expect("");
    //let prev_move = board.do_move_unchecked(convert_pos("E2"), convert_pos("D3")).expect("");
    //let prev_move = board.do_move_unchecked(convert_pos("B1"), convert_pos("D5")).expect("");
    println!("{:#?}", board);
    let legals = board.find_possible_moves( board.pieces.get(
        &convert_pos("B4"))
        .unwrap());
    println!("{:?}", legals);
    println!("{:?}", print_moves(legals));
}

