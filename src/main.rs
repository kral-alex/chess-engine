use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::ops::Add;
use std::str::FromStr;
use crate::MoveType::Castle;
use crate::PieceType::{Bishop, King, Knight, Pawn, Queen, Rook};

macro_rules! add_move {
    ($set: expr, $before: expr, $after: expr, $move_type: expr) => {
        $set.insert(Move {before: $before, after: $after, kind: $move_type})
    }
}

fn main() {
    println!("Hello, world!");
    let mut board: Board = Board::set_up();
    let my_pos = Position::from_str("D2").expect("should be fine");
    println!("{:#?}", my_pos);
    println!("{:#?}", board);

    //let prev_move = board.do_move_unchecked(convert_pos("B2"), convert_pos("B3")).expect("");
    let prev_move = board.do_move_unchecked(convert_pos("B7"), convert_pos("B4")).expect("");
    let prev_move = board.do_move_unchecked(convert_pos("B2"), convert_pos("E4")).expect("");
    board.pieces.get_mut(&convert_pos("B4")).unwrap().piece_type = Pawn(MovedState::Moved);
    let prev_move = board.do_move_unchecked(convert_pos("C2"), convert_pos("C4")).expect("");
    //let prev_move = board.do_move_unchecked(convert_pos("E2"), convert_pos("D3")).expect("");
    //let prev_move = board.do_move_unchecked(convert_pos("B1"), convert_pos("D5")).expect("");
    println!("{:#?}", board);
    let legals = board.find_legal("B4".try_into().unwrap());
    println!("{:?}", legals);
    println!("{:?}", print_moves(legals));
}

fn print_moves(moves: HashSet<Move>) -> String {
        moves.into_iter().map(|x| x.after.to_string() + ", ").collect::<String>()
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
    fn direction(self) -> i32 {
        match self.color {
            Color::White => 1,
            Color::Black => -1
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

impl Color {
    fn opposite(self) -> Self {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White
        }
    }
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

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct Move {
    before: Position,
    after: Position,
    kind: MoveType,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum MoveType {
    Plain,
    Taking,
    EnPassant,
    Castle,
    Unchecked
}

#[derive(Debug)]
struct Board {
    pieces: HashMap<Position, Piece>,
    previous_move: Option<Move>
}

impl Board {

    fn set_up() -> Self {
        let mut pieces: HashMap<Position, Piece> = HashMap::with_capacity(32);
        for i in 0..31 {
            let color = match i {
                0..=15 => Color::White,
                16..=31 => Color::Black,
                _ => unreachable!()
            };
            let piece_type = match i % 16 {
                0..=7 => Pawn(MovedState::Still),
                8 | 15 => Rook(MovedState::Still),
                9 | 14 => Knight,
                10 | 13 => Bishop,
                11 => King(MovedState::Still),
                12 => Queen,
                _ => unreachable!()
            };
            let row = match (&color, &piece_type) {
                (Color::White, Pawn(_)) => 1,
                (Color::White, _) => 0,
                (Color::Black, Pawn(_)) => 6,
                (Color::Black, _) => 7
            };
            pieces.insert(Position{x: i % 8, y: row},Piece {
                state: PieceState::Alive(Position{x: i % 8, y: row}),
                piece_type,
                color
            });
        }
        Self {
            pieces,
            previous_move: None
        }
    }

    fn do_move_unchecked(&mut self, before: Position, after: Position) -> Option<Move> {
        let mut piece = self.pieces.remove(&before)?;
        //println!("{:?}", piece);
        // change to match the whole remove thing. Returns none for both dead pieces and empty positions, good?
        match piece.state {
            PieceState::Alive(pos) if pos == before => {piece.state = PieceState::Alive(after)}
            PieceState::Dead => {return None}
            _ => unreachable!()
        }
        //println!("{:?}", piece);
        self.pieces.insert(after, piece);
        self.previous_move = Some(
            Move {before, after, kind: MoveType::Unchecked}
        );
        self.previous_move
    }

    fn search_direction(&self, start_piece: &Piece, start_pos: Position, step: MoveVector) -> HashSet<Move> {
        let mut possible_moves: HashSet<Move> = HashSet::new();
        let mut position = start_pos;
        loop {
            position = if let Some(pos) = position.add(step) { pos } else {
                break
            };
            match self.pieces.get(&position) {
                None => add_move!(possible_moves, start_pos, position, MoveType::Plain),
                Some(piece) if start_piece.is_enemy(piece) => {
                    add_move!(possible_moves, start_pos, position, MoveType::Taking);
                    break
                }
                _ => break
            };
        };
        possible_moves
    }

    fn find_legal(&self, at_position: Position) -> HashSet<Move> { // TODO have initial_piece argument
        let mut legal_pos: HashSet<Move> = HashSet::with_capacity(8); // usually about 8 moves available for piece
        let piece = match self.pieces.get(&at_position) {
            Some(i) => i,
            None => return legal_pos
        };
        match piece {
            Piece {piece_type: Rook(_), state: PieceState::Alive(pos), ..} => {
                for &direction in [
                    MoveVector{x: 0, y: 1},
                    MoveVector{x: 1, y: 0},
                    MoveVector{x: -1, y: 0},
                    MoveVector{x: 0, y: -1}
                ].iter() {
                    legal_pos.extend(self.search_direction(piece, *pos, direction).into_iter())
                }
            }
            Piece {piece_type: Bishop, state: PieceState::Alive(pos), ..} => {
                for &direction in [
                    MoveVector { x: 1, y: 1 },
                    MoveVector { x: -1, y: 1 },
                    MoveVector { x: 1, y: -1 },
                    MoveVector { x: -1, y: -1 }
                ].iter() {
                    legal_pos.extend(self.search_direction(piece, *pos,direction).into_iter())
                }
            }
            Piece {piece_type: Pawn(moved), state: PieceState::Alive(pos), color, .. } => {
                let dir_switch = match color {
                    Color::White => 1,
                    Color::Black => -1
                };
                let single_vec = MoveVector {x: 0, y: dir_switch * 1};
                let take_right_vec = MoveVector {x: 1, y: dir_switch * 1};
                let take_left_vec = MoveVector {x: -1, y: dir_switch * 1};

                let en_passant_before = MoveVector {x: 0, y: dir_switch * 1};
                let en_passant_after = MoveVector {x: 0, y: dir_switch * -1};

                if let Some(single_move) = pos.add(single_vec) {
                    match self.pieces.get(&single_move) {
                        Some(_) => {}
                        None => {
                            add_move!(legal_pos, *pos, single_move, MoveType::Plain);
                            if let Some(double_move) = single_move.add(single_vec) {
                                match self.pieces.get(&double_move) {
                                    Some(_) => {}
                                    None if *moved == MovedState::Still => { add_move!(legal_pos, *pos, double_move, MoveType::Plain);}
                                    None => {}
                                };
                            }
                        }
                    }
                }
                for &take_vec in [take_right_vec, take_left_vec].iter() {
                    if let Some(take_pos) = pos.add(take_vec) {
                        match self.pieces.get(&take_pos) {
                            Some(other) if piece.is_enemy(other) => { add_move!(legal_pos, *pos, take_pos, MoveType::Taking);},
                            Some(_) => {}
                            None => {
                                if let (Some(enp_before), Some(enp_after)) = (take_pos.add(en_passant_before), take_pos.add(en_passant_after)) {
                                    match &self.previous_move {
                                        None => {}
                                        Some(Move { before, after,.. }) if
                                        (&self.pieces[after].is_pawn())
                                        & (*before == enp_before)
                                        & (*after == enp_after) => {
                                            { add_move!(legal_pos, *pos, take_pos, MoveType::EnPassant);}
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Piece {piece_type: Knight, state: PieceState::Alive(pos), ..} => {
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
                        None => { add_move!(legal_pos, *pos, next_pos, MoveType::Plain);},
                        Some(other) if piece.is_enemy(other) => {
                            add_move!(legal_pos, *pos, next_pos, MoveType::Taking);
                        }
                        _ => {}
                    }
                }
            }
            Piece {piece_type: Queen, state: PieceState::Alive(pos), ..} => {
                for &direction in [
                    MoveVector{x: 0, y: 1},
                    MoveVector{x: 1, y: 0},
                    MoveVector{x: -1, y: 0},
                    MoveVector{x: 0, y: -1},
                    MoveVector { x: 1, y: 1 },
                    MoveVector { x: -1, y: 1 },
                    MoveVector { x: 1, y: -1 },
                    MoveVector { x: -1, y: -1 }
                ].iter() {
                    legal_pos.extend(self.search_direction(piece, *pos,direction).into_iter())
                }
            }
            Piece {piece_type: King(moved_state), state: PieceState::Alive(pos),..} => {
                let mut possible_wo_attack: HashSet<Move> = HashSet::with_capacity(10);
                for &direction in [
                    MoveVector { x: 0, y: 1 },
                    MoveVector { x: 1, y: 0 },
                    MoveVector { x: -1, y: 0 },
                    MoveVector { x: 0, y: -1 },
                    MoveVector { x: 1, y: 1 },
                    MoveVector { x: -1, y: 1 },
                    MoveVector { x: 1, y: -1 },
                    MoveVector { x: -1, y: -1 }
                ].iter() {
                    let next_pos = if let Some(i) = pos.add(direction) { i } else {
                        continue
                    };
                    match self.pieces.get(&next_pos) {
                        None => { add_move!(possible_wo_attack, *pos, next_pos, MoveType::Plain); },
                        Some(other) if piece.is_enemy(other) => {
                            add_move!(possible_wo_attack, *pos, next_pos, MoveType::Taking);
                        }
                        _ => {}
                    }
                }
                if moved_state == &MovedState::Still {
                    {
                        match self.make_small_castle(*pos) {
                            Some(i) => { possible_wo_attack.insert(i); }
                            None => {}
                        }
                        match self.make_big_castle(*pos) {
                            Some(i) => { possible_wo_attack.insert(i); }
                            None => {}
                        }
                    }
                }

            }
            Piece{state: PieceState::Dead, ..} => unreachable!("dead piece")
        }
        legal_pos
    }

    fn make_small_castle(&self, king_pos: Position) -> Option<Move> {
        let r_rook = self.pieces.get(
            &king_pos.add(MoveVector { x: 3, y: 0 })
                .expect("King did not move so small castle rook position should be valid")
        )?;
        let r_knight = self.pieces.get(
            &king_pos.add(MoveVector { x: 2, y: 0 })
                .expect("King did not move so small castle knight position should be valid")
        );
        let r_bishop = self.pieces.get(
            &king_pos.add(MoveVector { x: 1, y: 0 })
                .expect("King did not move so small castle bishop position should be valid")
        );
        match (r_rook, r_knight, r_bishop) {
            (&Piece {piece_type: Rook(MovedState::Still), state: PieceState::Alive(rook_pos), ..}, None, None) => {
                Some(Move {before: king_pos, after: king_pos.add(MoveVector { x: 2, y: 0 }).unwrap(), kind: Castle})
            }
            _ => None
        }
    }
    fn make_big_castle(&self, king_pos: Position) -> Option<Move> {
        let l_rook = self.pieces.get(
            &king_pos.add(MoveVector { x: -4, y: 0 })
                .expect("King did not move so big castle rook position should be valid")
        )?;
        let l_knight = self.pieces.get(
            &king_pos.add(MoveVector { x: -3, y: 0 })
                .expect("King did not move so big castle knight position should be valid")
        );
        let l_bishop = self.pieces.get(
            &king_pos.add(MoveVector { x: -2, y: 0 })
                .expect("King did not move so big castle bishop position should be valid")
        );
        let queen = self.pieces.get(
            &king_pos.add(MoveVector { x: -1, y: 0 })
                .expect("King did not move so big castle queen position should be valid")
        );
        match (l_rook, l_knight, l_bishop, queen) {
            (&Piece {piece_type: Rook(MovedState::Still), state: PieceState::Alive(rook_pos), ..}, None, None, None) => {
                Some(Move {before: king_pos, after: king_pos.add(MoveVector { x: 2, y: 0 }).unwrap(), kind: Castle})
            }
            _ => None
        }
    }
}



