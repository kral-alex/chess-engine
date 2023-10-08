use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::ops::Add;
use std::str::FromStr;
use crate::Color::{Black, White};
use crate::MovedState::Still;
use crate::PieceState::Alive;
use crate::PieceType::{Bishop, King, Knight, Pawn, Queen, Rook};

fn main() {
    println!("Hello, world!");
    let mut board: Board = Board::set_up();
    let my_pos = Position::from_str("D2").expect("should be fine");
    println!("{:#?}", my_pos);
    println!("{:#?}", board);

    //let prev_move = board.do_move_unchecked(convert_pos("B2"), convert_pos("B3"), 0).expect("");
    //let prev_move = board.do_move_unchecked(convert_pos("B7"), convert_pos("B5"), 0).expect("");
    //let prev_move = board.do_move_unchecked(convert_pos("C1"), convert_pos("C5"), 0).expect("");
    //let prev_move = board.do_move_unchecked(convert_pos("E2"), convert_pos("D3"), 0).expect("");
    let prev_move = board.do_move_unchecked(convert_pos("B1"), convert_pos("D5"), 0).expect("");
    println!("{:#?}", board);
    let legals = board.find_legal("D5".try_into().unwrap(), prev_move);
    println!("{:?}", legals);
    println!("{:?}", print_positions(legals));
}

fn print_positions(positions: Vec<Position>) -> String {
        positions.into_iter().map(|x| x.to_string() + ", ").collect::<String>()
}

fn convert_pos(s: &str) -> Position {
    Position::from_str(s).expect("could not convert string to Position")
}

#[derive(Debug)]
struct Piece {
    piece_type: PieceType,
    color: Color,
    state: PieceState
}

impl Piece {
    fn is_enemy(&self, other: &Piece) -> bool {
        self.color != other.color
    }

    fn is_pawn(&self) -> bool {
        match self.piece_type {
            Pawn(_) => true,
            _ => false
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum PieceState {
    Alive(Position),
    Dead
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Color {
    White,
    Black
}

#[derive(Debug, Copy, Clone)]
enum PieceType {
    Pawn(MovedState),
    Rook(MovedState),
    Knight,
    Bishop,
    Queen,
    King(MovedState)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum MovedState {
    Still,
    Moved
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct Position {
    x: i32,
    y: i32
}

#[derive(Debug)]
struct PositionParsingError;

impl FromStr for Position {
    type Err = PositionParsingError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() != 2 {return Err(PositionParsingError)};
        let x = match s.chars().nth(0) {
            None => return Err(PositionParsingError),
            Some(c) => {
                match c.to_ascii_uppercase() {
                    'A' => 0,
                    'B' => 1,
                    'C' => 2,
                    'D' => 3,
                    'E' => 4,
                    'F' => 5,
                    'G' => 6,
                    'H' => 7,
                    _ => return Err(PositionParsingError)
                }
            }
        };
        let second = s.chars().nth(1).ok_or(PositionParsingError)?.to_digit(10);
        let y: i32 = match second {
            Some(i) if matches!(i, 1..=8) => i as i32 - 1,
            _ => return Err(PositionParsingError)
        };
        Ok(Position {x, y})
    }
}

impl TryFrom<&str> for Position {
    type Error = PositionParsingError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::from_str(value)
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let letter = match self.x {
             0 => 'A',
             1 => 'B',
             2 => 'C',
             3 => 'D',
             4 => 'E',
             5 => 'F',
             6 => 'G',
             7 => 'H',
            _ => unreachable!()
        };
        let number: String = (self.y + 1).to_string();
        write!(f, "{}", letter.to_string() + &number)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct MoveVector {
    x: i32,
    y: i32
}

impl Add for MoveVector {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y
        }
    }
}

pub trait BoundsAdd<T> {
    type Output;
    fn add(self, rhs: T) -> Option<Self::Output>;
}

impl BoundsAdd<MoveVector> for Position {
    type Output = Self;

    fn add(self, rhs: MoveVector) -> Option<Self::Output> {
        match (self.x + rhs.x, self.y + rhs.y) {
            (0..=7, 0..=7) => Some(Self::Output {
                                    x: self.x + rhs.x,
                                    y: self.y + rhs.y
            }),
            _ => None
        }
    }
}

#[derive(Debug)]
struct Move {
    before: Position,
    after: Position,
    id: u32,
}

#[derive(Debug)]
struct Board {
    pieces: HashMap<Position, Piece>
}

impl Board {

    fn set_up() -> Self {
        let mut pieces: HashMap<Position, Piece> = HashMap::with_capacity(32);
        for i in 0..31 {
            let color = match i {
                0..=15 => White,
                16..=31 => Black,
                _ => unreachable!()
            };
            let piece_type = match i % 16 {
                0..=7 => Pawn(Still),
                8 | 15 => Rook(Still),
                9 | 14 => Knight,
                10 | 13 => Bishop,
                11 => King(Still),
                12 => Queen,
                _ => unreachable!()
            };
            let row = match (&color, &piece_type) {
                (White, Pawn(_)) => 1,
                (White, _) => 0,
                (Black, Pawn(_)) => 6,
                (Black, _) => 7
            };
            pieces.insert(Position{x: i % 8, y: row}, Piece {
                state: Alive(Position{x: i % 8, y: row}),
                piece_type,
                color
            });
        }
        Self {pieces}
    }

    fn do_move_unchecked(&mut self, before: Position, after: Position, id: u32) -> Option<Move> {
        let mut piece = self.pieces.remove(&before)?;
        //println!("{:?}", piece);
        /// change to match the whole remove thing. Returns none for both dead pieces and empty positions, good?
        match piece.state {
            Alive(pos) if pos == before => {piece.state = Alive(after)}
            PieceState::Dead => {return None}
            _ => unreachable!()
        }
        //println!("{:?}", piece);
        self.pieces.insert(after, piece);
        Some(
            Move {before, after, id}
        )
    }

    fn search_direction(&self, start_piece: &Piece, start_pos: Position, step: MoveVector) -> Vec<Position> {
        let mut possible_moves: Vec<Position> = Vec::new();
        let mut position = start_pos;
        loop {
            position = if let Some(pos) = position.add(step) { pos } else {
                break
            };
            match self.pieces.get(&position) {
                None => possible_moves.push(position),
                Some(piece) if start_piece.is_enemy(piece) => {
                    possible_moves.push(position);
                    break
                }
                _ => break
            };
        };
        possible_moves
    }

    fn find_legal(&self, at_position: Position, prev_move: Move) -> Vec<Position> {
        let mut legal_pos: Vec<Position> = Vec::new();
        let piece = match self.pieces.get(&at_position) {
            Some(i) => i,
            None => return legal_pos
        };
        match piece {
            Piece {piece_type: Rook(_), state: Alive(pos), ..} => {
                for &direction in [
                    MoveVector{x: 0, y: 1},
                    MoveVector{x: 1, y: 0},
                    MoveVector{x: -1, y: 0},
                    MoveVector{x: 0, y: -1}
                ].iter() {
                    legal_pos.append(&mut self.search_direction(piece, *pos, direction))
                }
            }
            Piece {piece_type: Bishop, state: Alive(pos), ..} => {
                for &direction in [
                    MoveVector { x: 1, y: 1 },
                    MoveVector { x: -1, y: 1 },
                    MoveVector { x: 1, y: -1 },
                    MoveVector { x: -1, y: -1 }
                ].iter() {
                    legal_pos.append(&mut self.search_direction(piece, *pos,direction))
                }
            }
            Piece {piece_type: Pawn(moved), state: Alive(pos), color, .. } => {
                let dir_switch = match color {
                    White => 1,
                    Black => -1
                };
                let single_vec = MoveVector {x: 0, y: dir_switch * 1};
                let take_right_vec = MoveVector {x: 1, y: dir_switch * 1};
                let take_left_vec = MoveVector {x: -1, y: dir_switch * 1};

                let en_passant_before = MoveVector {x: 0, y: dir_switch * 1};
                let en_passant_after = MoveVector {x: 0, y: dir_switch * -1};

                if let Some(single_move) = pos.add(single_vec) {
                    match self.pieces.get(&single_move) {
                        Some(_) => {}
                        None if *moved == MovedState::Still => {
                            legal_pos.push(single_move);
                            if let Some(double_move) = single_move.add(single_vec) {
                                match self.pieces.get(&double_move) {
                                    Some(_) => {}
                                    None => legal_pos.push(double_move)
                                };
                            }
                        }
                        None => {}
                    }
                }
                for &take_vec in [take_right_vec, take_left_vec].iter() {
                    if let Some(take_pos) = pos.add(take_vec) {
                        match self.pieces.get(&take_pos) {
                            Some(other) if piece.is_enemy(other) => legal_pos.push(take_pos),
                            Some(_) => {}
                            None => {
                                if let (Some(enp_before), Some(enp_after)) = (take_pos.add(en_passant_before), take_pos.add(en_passant_after)) {
                                    match &prev_move {
                                        Move { before, after,.. } if
                                        (&self.pieces[after].is_pawn())
                                        & (*before == enp_before)
                                        & (*after == enp_after) => {
                                            legal_pos.push(take_pos)
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Piece {piece_type: Knight, state: Alive(pos), ..} => {
                for &direction in [
                    MoveVector { x: 1, y: 2 },
                    MoveVector { x: -1, y: 2 },
                    MoveVector { x: 1, y: -2 },
                    MoveVector { x: -1, y: -2 },
                    MoveVector { x: 2, y: 1 },
                    MoveVector { x: -2, y: 1 },
                    MoveVector { x: 2, y: -1 },
                    MoveVector { x: -2, y: -1 }
                ].iter() {
                    let next_pos = if let Some(i) = pos.add(direction) { i } else {
                        continue
                    };
                    match self.pieces.get(&next_pos) {
                        None => legal_pos.push(next_pos),
                        Some(other) if piece.is_enemy(other) => {
                            legal_pos.push(next_pos);
                        }
                        _ => {}
                    }
                }
            }
            Piece {piece_type: Queen, state: Alive(pos), ..} => {
                for direction in [
                    MoveVector{x: 0, y: 1},
                    MoveVector{x: 1, y: 0},
                    MoveVector{x: -1, y: 0},
                    MoveVector{x: 0, y: -1},
                    MoveVector { x: 1, y: 1 },
                    MoveVector { x: -1, y: 1 },
                    MoveVector { x: 1, y: -1 },
                    MoveVector { x: -1, y: -1 }
                ].iter() {
                    legal_pos.append(&mut self.search_direction(piece, *pos,*direction))
                }
            }
            Piece {piece_type: King(moved), ..} => {todo!() }
            Piece{state: PieceState::Dead, ..} => unreachable!("dead piece")
        }
        legal_pos
    }
}



