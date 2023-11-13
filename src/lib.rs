use std::borrow::Borrow;
use std::collections::HashSet;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Add;
use std::str::FromStr;

macro_rules! add_move {
    ($set: expr, $before: expr, $after: expr, $move_type: expr) => {
        $set.insert(Move {pos: ($before, $after), move_type: $move_type})
    }
}

pub fn print_moves<T: IntoIterator<Item = Move>>(moves: T) -> String {
    moves.into_iter().map(|x| {
        x.pos.0.to_string() + "->" + &x.pos.1.to_string() + ", "
    }).collect::<String>()
}

pub fn convert_pos(s: &str) -> Position {
    Position::from_str(s).expect("could not convert string to Position")
}

#[derive(Debug, Clone)]
pub struct Piece {
    piece_type: PieceType,
    color: Color,
    position: Position
}

impl Display for Piece {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
                Self {piece_type, color: Color::White, ..} => format!("{}", piece_type).to_ascii_uppercase(),
                Self {piece_type, color: Color::Black, ..} => format!("{}", piece_type).to_ascii_lowercase()
            }
        )
    }
}

impl Piece {
    fn is_enemy(&self, other: &Piece) -> bool {
        self.color != other.color
    }

    fn is_pawn(&self) -> bool {
        matches!(self.piece_type, PieceType::Pawn { moved: _ })
    }

    fn be_moved(&mut self, new_pos: Position) {
        self.position = new_pos;
        match self.piece_type {
            PieceType::Pawn {ref mut moved}
            | PieceType::King {ref mut moved}
            | PieceType::Rook {ref mut moved} => { *moved = true },
            _ => {}
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Color {
    White,
    Black
}

impl Color {
    pub fn opposite(self) -> Self {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White
        }
    }

    fn direction(self) -> i32 {
        match self {
            Color::White => 1,
            Color::Black => -1
        }
    }
}

impl Display for Color {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Color::White => write!(f, "White"),
            Color::Black => write!(f, "Black")
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum PieceType {
    Pawn{moved: bool},
    Rook{moved: bool},
    Knight,
    Bishop,
    Queen,
    King{moved: bool}
}

impl Display for PieceType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::Pawn { .. } => "P",
            Self::Rook { .. } => "R",
            Self::Knight => "N",
            Self::Bishop => "B",
            Self::Queen => "Q",
            Self::King { .. } => "K"
        })
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Position {
    x: i32,
    y: i32
}

#[derive(Debug, PartialEq)]
pub struct PositionParsingError;

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
pub struct Move {
    pos: (Position, Position),
    move_type: MoveType,
}

impl Borrow<(Position, Position)> for Move {
    fn borrow(& self) -> &(Position, Position) {
        &self.pos
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}->{}: {}", self.pos.0, self.pos.1, match self.move_type {
            MoveType::NonChecking(non_checking_type) => format!("{:?}", non_checking_type),
            MoveType::Checking {..} => format!("Normal")
        })
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, )]
enum MoveType {
    NonChecking(NonCheckingMove),
    Checking{ allowed: bool }, // an empty square or enemy piece
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum NonCheckingMove {
    EnPassant,
    PawnPush,
    Promotion,
    SmallCastle,
    BigCastle
}

impl Move {
    pub fn is_promotion(self) -> bool {
        matches!(self.move_type, MoveType::NonChecking(NonCheckingMove::Promotion))
    }

    fn is_available(self) -> bool {
        match self.move_type {
            MoveType::NonChecking(_) => true,
            //MoveType::EnPassant => true,
            MoveType::Checking { allowed: true} => true,
            MoveType::Checking { allowed: false} => false
        }
    }

    fn is_checking(self) -> bool {
        match self.move_type {
            MoveType::Checking {..} => true,
            MoveType::NonChecking(..) => false
        }
    }
}

trait BoardOps {
    fn get(&self, pos: &Position) -> Option<&Piece>;

    fn get_mut(&mut self, pos: &Position) -> Option<&mut Piece>;
    fn remove(&mut self, pos: &Position) -> Option<Piece>;
    fn insert(&mut self, pos: Position, piece: Piece) -> Option<Piece>;
    fn move_to(&mut self, before: Position, after: Position) -> Option<Piece>;
    }

#[derive(Debug, Default, Clone)]
struct Board {
    arr: [[Option<Piece>; 8]; 8]
}

impl BoardOps for Board {
    fn get(&self, pos: &Position) -> Option<&Piece> {
        self.arr[pos.x as usize][pos.y as usize].as_ref()
    }

    fn get_mut(&mut self, pos: &Position) -> Option<&mut Piece> {
        self.arr[pos.x as usize][pos.y as usize].as_mut()
    }

    fn remove(&mut self, pos: &Position) -> Option<Piece> {
        self.arr[pos.x as usize][pos.y as usize].take()
    }

    fn insert(&mut self, pos: Position, piece: Piece) -> Option<Piece> {
        self.arr[pos.x as usize][pos.y as usize].replace(piece)
    }

    fn move_to(&mut self, before: Position, after: Position) -> Option<Piece> {
        let killed = self.remove(&after);
        let mut moving = self.remove(&before).expect("before position argument has to have a piece");
        moving.be_moved(after);
        self.insert(after, moving);
        killed
    }
}

impl<'a> IntoIterator for &'a Board {
    type Item = &'a Piece;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.arr
            .as_slice()
            .iter()
            .flatten()
            .filter_map(|x| x.as_ref())
            .collect::<Vec<Self::Item>>()
            .into_iter()
    }
}


impl Display for Board {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let letters = "ABCDEFGH";
        write!(f, "     1     2     3     4     5     6     7     8   \n")?;
        for (x_index, column) in self.arr.iter().enumerate() {
            write!(f, "  {:-<49}\n", "")?;
            write!(f, "{} ", letters.chars().nth(x_index).expect("Index must be maximum 7"))?;
            for piece in column.iter() {
                write!(f, "|{}", match piece {
                    None => format!("     "),
                    Some(res) => format!("  {}  ", res)
                })?
            }
            write!(f, "|\n")?;
        };
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Game {
    pieces: Board,
    previous_move: Option<Move>
}

#[derive(Debug, PartialEq)]
pub struct IllegalMoveError;

impl Game {

    pub fn set_up() -> Self {
        let mut pieces: Board = Default::default();
        for i in 0..=31 {
            let color = match i {
                0..=15 => Color::White,
                16..=31 => Color::Black,
                _ => unreachable!()
            };
            let piece_type = match i % 16 {
                0..=7 => PieceType::Pawn{moved: false},
                8 | 15 => PieceType::Rook{moved: false},
                9 | 14 => PieceType::Knight,
                10 | 13 => PieceType::Bishop,
                11 => PieceType::Queen,
                12 => PieceType::King{moved: false},
                _ => unreachable!()
            };
            let row = match (&color, &piece_type) {
                (Color::White, PieceType::Pawn { moved: _ }) => 1,
                (Color::White, _) => 0,
                (Color::Black, PieceType::Pawn { moved: _ }) => 6,
                (Color::Black, _) => 7
            };
            pieces.insert(Position{x: i % 8, y: row}, Piece {
                position: Position{x: i % 8, y: row},
                piece_type,
                color
            });
        }
        Self {
            pieces,
            previous_move: None
        }
    }

    pub fn jump_unchecked(&mut self, before: Position, after: Position) {
        // Change position of any piece. Might break the game session
        let mut piece = self.pieces.remove(&before).expect("There is not a piece in this position");
        piece.position = after;
        self.pieces.insert(after, piece);
        self.previous_move = None;
    }

    // todo if get_allowed empty lost (and implement max 3x repetition)
    pub fn get_allowed(&self, color: Color) -> HashSet<Move> {
        let mut allowed: HashSet<Move> = HashSet::with_capacity(32);
        for piece in self.pieces.into_iter()
            .filter(|x| x.color == color) {
            for possible_move in self.find_possible_moves(piece).into_iter()
                .filter(|x| x.is_available())
            {
                let mut game_clone = self.clone();
                game_clone.make_move_unchecked(&possible_move);
                let king = game_clone.pieces.into_iter()
                    .find(
                    |&x| matches!(x, Piece { color: col, piece_type: PieceType::King { moved: _ }, ..} if *col == color)
                ).unwrap();
                if ! game_clone.get_checked(king.color).contains(&king.position) {
                    allowed.insert(possible_move);
                }
            }
        };
        allowed
    }

    pub fn try_move(&mut self, before: Position, after: Position, player_color: Color) -> Result<Move, IllegalMoveError> {
        let piece = self.pieces.get(&before).ok_or(IllegalMoveError)?;
        if piece.color != player_color { return Err(IllegalMoveError) }

        match self.find_possible_moves(piece)
            .into_iter()
            .filter(|x| x.is_available())
            .find(|x| x.pos == (before, after))
        {
            Some(possible_move) => {
                let mut game_clone = self.clone();
                game_clone.make_move_unchecked(&possible_move);
                let king = game_clone.pieces.into_iter()
                    .find(
                    |&x| matches!(x, Piece { color: col, piece_type: PieceType::King { moved: _ }, ..} if *col == piece.color)
                ).unwrap();
                if game_clone.get_checked(king.color).contains(&king.position) { return Err(IllegalMoveError) }

                *self = game_clone;
                self.previous_move = Some(possible_move);
                Ok(possible_move)
            }
            _ => Err(IllegalMoveError)
        }
    }

    fn make_move_unchecked(&mut self, m: &Move) {
        // undefined behaviour when move is not allowed
        // panics if there is no piece at move before position or if move is unavailable
        match self.pieces.remove(&m.pos.0) {
            None => panic!("Should not receive move from empty position"),
            Some(mut piece) => {
                match m.move_type {
                    MoveType::Checking { allowed: false } => panic!("Should not receive unavailable move"),
                    MoveType::Checking { allowed: true } => {
                        drop(self.pieces.remove(&m.pos.1))
                    }
                    MoveType::NonChecking(NonCheckingMove::PawnPush) => {}
                    MoveType::NonChecking(NonCheckingMove::EnPassant) => {
                        drop(self.pieces.remove(&m.pos.1
                            // if it is white playing, target's position is in the black direction
                            .add(MoveVector{x: 0, y: -piece.color.direction()})
                            .expect("En Passant was already confirmed. Target square must be in bounds.")
                        )
                            .expect("En Passant was already confirmed. Target piece has to exist.")
                        );
                    }
                    MoveType::NonChecking(NonCheckingMove::SmallCastle) => {
                        let mut r_rook = self.pieces
                            .remove(
                                &m.pos.0
                                .add(MoveVector { x: 3, y: 0 })
                                .expect("")
                            ).expect("");
                        let r_rook_new_pos = m.pos.1.add(MoveVector { x: -1, y: 0 }).expect("");
                        r_rook.be_moved(r_rook_new_pos);
                        self.pieces.insert(r_rook_new_pos, r_rook);
                    }
                    MoveType::NonChecking(NonCheckingMove::BigCastle) => {
                        let mut l_rook = self.pieces.remove(
                            &m.pos.0
                            .add(MoveVector { x: -4, y: 0 })
                            .expect("")
                        ).expect("");
                        let l_rook_new_pos = m.pos.1.add(MoveVector { x: 1, y: 0 }).expect("");
                        l_rook.be_moved(l_rook_new_pos);
                        self.pieces.insert(l_rook_new_pos, l_rook);
                    }
                    MoveType::NonChecking(NonCheckingMove::Promotion) => {}
                };
                piece.be_moved(m.pos.1);
                self.pieces.insert(m.pos.1, piece);
            }
        }
    }

    pub fn promote_last(&mut self, new_piece_type: PieceType) {
        if let Some(Move {move_type: MoveType::NonChecking(NonCheckingMove::Promotion), pos }) = self.previous_move {
            self.pieces.get_mut(&pos.1).expect("").piece_type = new_piece_type;
        } else {
            panic!("Has to follow a promotion move")
        }
    }

    pub fn get_checked(&self, color: Color) -> HashSet<Position> {
        let mut checked: HashSet<Position> = HashSet::with_capacity(32);
        for piece in self.pieces.into_iter() {
            if piece.color.opposite() == color {
                checked.extend(self.find_legal(piece).iter().filter(
                    |&x| x.is_checking()
                ).map(
                    |&x: &Move| x.pos.1
                ))
            }
        };
        checked
    }

    pub fn find_possible_moves(&self, piece: &Piece) -> HashSet<Move> {
        // Cleans out moves the king cannot do because of check.
        // Outside find_legal because the King can check positions it cannot go to itself
        let mut possible_moves: HashSet<Move> = self.find_legal(piece);
        if let Piece { position: pos, piece_type: PieceType::King{moved}, ..} = piece {
                let checked = self.get_checked(piece.color);
                for legal_move in possible_moves.clone() {
                    if checked.contains(&legal_move.pos.1) {
                        possible_moves.remove(&legal_move);
                    }
                }
                if !*moved {
                    {
                        if let Some(i) = self.make_small_castle(*pos, &checked) {possible_moves.insert(i);}
                        if let Some(i) = self.make_big_castle(  *pos, &checked) {possible_moves.insert(i);}
                    }
                }
            };
        possible_moves
    }

    fn find_legal(&self, piece: &Piece) -> HashSet<Move> {
        let mut legal_moves: HashSet<Move> = HashSet::with_capacity(16); // usually no more than 16 moves available for piece
        match piece {
            Piece {piece_type: PieceType::Rook { moved: _ }, position: pos, ..} => {
                for &direction in [
                    MoveVector{x: 0, y: 1},
                    MoveVector{x: 1, y: 0},
                    MoveVector{x: -1, y: 0},
                    MoveVector{x: 0, y: -1}
                ].iter() {
                    legal_moves.extend(self.search_direction(piece, *pos, direction).into_iter())
                }
            }
            Piece {piece_type: PieceType::Bishop, position: pos, ..} => {
                for &direction in [
                    MoveVector { x: 1, y: 1 },
                    MoveVector { x: -1, y: 1 },
                    MoveVector { x: 1, y: -1 },
                    MoveVector { x: -1, y: -1 }
                ].iter() {
                    legal_moves.extend(self.search_direction(piece, *pos, direction).into_iter())
                }
            }
            Piece {piece_type: PieceType::Queen, position: pos, ..} => {
                for &direction in [
                    MoveVector { x: 0, y: 1},
                    MoveVector { x: 1, y: 0},
                    MoveVector { x: -1, y: 0},
                    MoveVector { x: 0, y: -1},
                    MoveVector { x: 1, y: 1 },
                    MoveVector { x: -1, y: 1 },
                    MoveVector { x: 1, y: -1 },
                    MoveVector { x: -1, y: -1 }
                ].iter() {
                    legal_moves.extend(self.search_direction(piece, *pos, direction).into_iter())
                }
            }
            Piece {piece_type: PieceType::Pawn { moved }, position: pos, color, .. } => {
                let single_vec = MoveVector {x: 0, y: color.direction()};
                let take_right_vec = MoveVector {x: 1, y: color.direction()};
                let take_left_vec = MoveVector {x: -1, y: color.direction()};

                let en_passant_before = MoveVector {x: 0, y: color.direction()};
                let en_passant_after = MoveVector {x: 0, y: - color.direction()};

                if let Some(single_move) = pos.add(single_vec) {
                    match self.pieces.get(&single_move) {
                        Some(_) => {}
                        None if single_move.y == 0 || single_move.y == 7 => {
                            add_move!(legal_moves, *pos, single_move, MoveType::NonChecking(NonCheckingMove::Promotion));
                        }
                        None => {
                            add_move!(legal_moves, *pos, single_move, MoveType::NonChecking(NonCheckingMove::PawnPush));
                            if let Some(double_move) = single_move.add(single_vec) {
                                match self.pieces.get(&double_move) {
                                    Some(_) => {}
                                    None if !*moved => { add_move!(legal_moves, *pos, double_move, MoveType::NonChecking(NonCheckingMove::PawnPush));}
                                    _ => {}
                                };
                            }
                        }
                    }
                }
                for &take_vec in [take_right_vec, take_left_vec].iter() {
                    if let Some(take_pos) = pos.add(take_vec) {
                        match self.pieces.get(&take_pos) {
                            Some(other) => { add_move!(legal_moves, *pos, take_pos, MoveType::Checking {allowed: piece.is_enemy(other)});},
                            None => {
                                add_move!(legal_moves, *pos, take_pos, MoveType::Checking {allowed: false});
                                // En passant
                                if let (Some(enp_before), Some(enp_after)) = (take_pos.add(en_passant_before), take_pos.add(en_passant_after)) {
                                    match &self.previous_move {
                                        None => {}
                                        Some(Move { pos: (before, after), .. }) if
                                        self.pieces.get(after).unwrap().is_pawn()
                                            & (*before == enp_before)
                                            & (*after == enp_after) => {
                                            { add_move!(legal_moves, *pos, take_pos, MoveType::NonChecking(NonCheckingMove::EnPassant));}
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Piece {piece_type: PieceType::Knight, position: pos, ..} => {
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
                        None => { add_move!(legal_moves, *pos, next_pos, MoveType::Checking{ allowed: true });},
                        Some(other) => {
                            add_move!(legal_moves, *pos, next_pos, MoveType::Checking { allowed: piece.is_enemy(other) });
                        }
                    }
                }
            }
            Piece {piece_type: PieceType::King { moved: _ }, position: pos,..} => {
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
                        None => { add_move!(legal_moves, *pos, next_pos, MoveType::Checking{ allowed: true }); }, //add_move!(possible_wo_attack, *pos, next_pos, MoveType::NonChecking(NonCheckingMove::Plain));
                        Some(other) if piece.is_enemy(other) => {
                            add_move!(legal_moves, *pos, next_pos, MoveType::Checking { allowed: piece.is_enemy(other) });
                        }
                        _ => {}
                    }
                }
            }
        }
        legal_moves
    }

    fn make_small_castle(&self, king_pos: Position, checked: &HashSet<Position>) -> Option<Move> {
        let r_rook = king_pos.add(MoveVector { x: 3, y: 0 })
            .expect("King did not move so small castle rook position should be valid");
        let r_knight = king_pos.add(MoveVector { x: 2, y: 0 })
            .expect("King did not move so small castle knight position should be valid");
        let r_bishop = king_pos.add(MoveVector { x: 1, y: 0 })
            .expect("King did not move so small castle bishop position should be valid");
        if
        checked.contains(&r_rook)
            || checked.contains(&r_knight)
            || checked.contains(&r_bishop)
            || checked.contains(&king_pos)

        {
            return None
        }
        match (
            self.pieces.get(&r_rook)?,
            self.pieces.get(&r_knight),
            self.pieces.get(&r_bishop)) {
            (&Piece {piece_type: PieceType::Rook { moved: false }, position: _, ..}, None, None) => {
                Some(Move {pos: (king_pos, king_pos.add(MoveVector { x: 2, y: 0 }).unwrap()), move_type: MoveType::NonChecking(NonCheckingMove::SmallCastle)})
            }
            _ => None
        }
    }

    fn make_big_castle(&self, king_pos: Position, checked: &HashSet<Position>) -> Option<Move> {
        let l_rook = king_pos.add(MoveVector { x: -4, y: 0 })
            .expect("King did not move so big castle rook position should be valid");
        let l_knight = king_pos.add(MoveVector { x: -3, y: 0 })
            .expect("King did not move so big castle knight position should be valid");
        let l_bishop = king_pos.add(MoveVector { x: -2, y: 0 })
            .expect("King did not move so big castle bishop position should be valid");
        let queen = king_pos.add(MoveVector { x: -1, y: 0 })
            .expect("King did not move so big castle queen position should be valid");
        if
            checked.contains(&l_rook)
                || checked.contains(&l_knight)
                || checked.contains(&l_bishop)
                || checked.contains(&queen)
                || checked.contains(&king_pos)
        {
            return None
        }
        match (
            self.pieces.get(&l_rook)?,
            self.pieces.get(&l_knight),
            self.pieces.get(&l_bishop),
            self.pieces.get(&queen)) {
            (&Piece {piece_type: PieceType::Rook { moved: false }, position: _, ..}, None, None, None) => {
                Some(Move {pos: (king_pos, king_pos.add(MoveVector { x: -2, y: 0 }).unwrap()), move_type: MoveType::NonChecking(NonCheckingMove::BigCastle)})
            }
            _ => None
        }
    }

    fn search_direction(&self, start_piece: &Piece, start_pos: Position, step: MoveVector) -> HashSet<Move> {
        let mut possible_moves: HashSet<Move> = HashSet::new();
        let mut position = start_pos;
        loop {
            position = if let Some(pos) = position.add(step) { pos } else {
                break
            };
            match self.pieces.get(&position) {
                None => add_move!(possible_moves, start_pos, position, MoveType::Checking { allowed: true }),
                Some(piece) => {
                    add_move!(possible_moves, start_pos, position, MoveType::Checking { allowed: start_piece.is_enemy(piece) });
                    break
                }
            };
        };
        possible_moves
    }
}

impl Display for Game {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.pieces)
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test() {
        assert!(true)
    }

    #[test]
    fn position_from_str() {
        assert_eq!(Position::try_from("A1"), Ok(Position { x: 0, y: 0 }));
        assert_eq!(Position::try_from("H8"), Ok(Position { x: 7, y: 7 }));
        assert_eq!(Position::try_from("D5"), Ok(Position { x: 3, y: 4 }));

        assert_eq!(Position::try_from("J8"), Err(PositionParsingError));
        assert_eq!(Position::try_from("H9"), Err(PositionParsingError));
        assert_eq!(Position::try_from("A0"), Err(PositionParsingError));
    }

    #[test]
    fn initial_pawns_white() {
        let board: Game = Game::set_up();
        let white_row = [
            Position::try_from("A2").unwrap(),
            Position::try_from("B2").unwrap(),
            Position::try_from("C2").unwrap(),
            Position::try_from("D2").unwrap(),
            Position::try_from("E2").unwrap(),
            Position::try_from("F2").unwrap(),
            Position::try_from("G2").unwrap(),
            Position::try_from("H2").unwrap()
        ];
        for pos in white_row {
            let mut control: HashSet<Move> = HashSet::new();
            control.insert(Move {pos: (pos, pos.add(MoveVector {x: 0, y: 1}).unwrap()), move_type: MoveType::NonChecking(NonCheckingMove::PawnPush) });
            control.insert(Move {pos: (pos, pos.add(MoveVector {x:0, y: 2}).unwrap()), move_type: MoveType::NonChecking(NonCheckingMove::PawnPush) });
            if let Some(left_pos) = pos.add(MoveVector {x: -1, y: 1}) {
                control.insert(Move { pos: (pos, left_pos), move_type: MoveType::Checking { allowed: false } });
            }
            if let Some(right_pos) = pos.add(MoveVector {x: 1, y: 1}) {
                control.insert(Move { pos: (pos, right_pos), move_type: MoveType::Checking { allowed: false } });
            }

            assert_eq!(
                board.find_possible_moves(board.pieces.get(&pos).unwrap()),
                control
            )
        };
    }

    #[test]
    fn initial_pawns_black() {
        let board: Game = Game::set_up();
        let black_row = [
            Position::try_from("A7").unwrap(),
            Position::try_from("B7").unwrap(),
            Position::try_from("C7").unwrap(),
            Position::try_from("D7").unwrap(),
            Position::try_from("E7").unwrap(),
            Position::try_from("F7").unwrap(),
            Position::try_from("G7").unwrap(),
            Position::try_from("H7").unwrap()
        ];
        for pos in black_row {
            let mut control: HashSet<Move> = HashSet::new();
            control.insert(Move {pos: (pos, pos.add(MoveVector {x: 0, y: -1}).unwrap()), move_type: MoveType::NonChecking(NonCheckingMove::PawnPush) });
            control.insert(Move {pos: (pos, pos.add(MoveVector {x:0, y: -2}).unwrap()), move_type: MoveType::NonChecking(NonCheckingMove::PawnPush) });
            if let Some(left_pos) = pos.add(MoveVector {x: -1, y: -1}) {
                control.insert(Move { pos: (pos, left_pos), move_type: MoveType::Checking { allowed: false } });
            }
            if let Some(right_pos) = pos.add(MoveVector {x: 1, y: -1}) {
                control.insert(Move { pos: (pos, right_pos), move_type: MoveType::Checking { allowed: false } });
            }

            assert_eq!(
                board.find_possible_moves(board.pieces.get(&pos).unwrap()),
                control
            )
        }
    }

    #[test]
    fn small_castle_white() {
        let mut board: Game = Game::set_up();
        let _ = board.jump_unchecked(Position { x: 6, y: 0 }, Position { x: 6, y: 2 });
        let _ = board.jump_unchecked(Position { x: 5, y: 0 }, Position { x: 5, y: 2 });
        let king_pos = Position { x: 4, y: 0 };
        let res = board.find_possible_moves(board.pieces.get(&king_pos).unwrap());
        println!("{:}", print_moves(res.clone()));
        println!("{:#?}", res.clone());
        assert!(res.contains(&Move { pos: (king_pos, Position { x: 5, y: 0 }), move_type: MoveType::Checking { allowed: true} }));
        assert!(res.contains(&Move { pos: (king_pos, Position { x: 6, y: 0 }), move_type: MoveType::NonChecking(NonCheckingMove::SmallCastle) }));
    }

    #[test]
    fn small_castle_black() {
        let mut board: Game = Game::set_up();
        let _ = board.jump_unchecked(Position {x: 6, y: 7}, Position {x: 6, y: 5});
        let _ = board.jump_unchecked(Position {x: 5, y: 7}, Position {x: 5, y: 5});
        let king_pos = Position {x: 4, y: 7};
        let res = board.find_possible_moves(board.pieces.get(&king_pos).unwrap());
        println!("{:}", print_moves(res.clone()));
        println!("{:#?}", res.clone());
        assert!(res.contains(&Move {pos: (king_pos, Position {x: 5, y: 7}), move_type: MoveType::Checking { allowed: true }}));
        assert!(res.contains(&Move {pos: (king_pos, Position {x: 6, y: 7}), move_type: MoveType::NonChecking(NonCheckingMove::SmallCastle)}))
    }

    #[test]
    fn big_castle_white() {
        let mut board: Game = Game::set_up();
        let _ = board.jump_unchecked(Position { x: 3, y: 0 }, Position { x: 3, y: 2 });
        let _ = board.jump_unchecked(Position { x: 2, y: 0 }, Position { x: 2, y: 2 });
        let _ = board.jump_unchecked(Position { x: 1, y: 0 }, Position { x: 1, y: 2 });
        let king_pos = Position { x: 4, y: 0 };
        let res = board.find_possible_moves(board.pieces.get(&king_pos).unwrap());
        println!("{:}", print_moves(res.clone()));
        println!("{:#?}", res.clone());
        assert!(res.contains(&Move { pos: (king_pos, Position { x: 3, y: 0 }), move_type: MoveType::Checking { allowed: true } }));
        assert!(res.contains(&Move { pos: (king_pos, Position { x: 2, y: 0 }), move_type: MoveType::NonChecking(NonCheckingMove::BigCastle) }));
    }

    #[test]
    fn big_castle_black() {
        let mut board: Game = Game::set_up();

        let _ = board.jump_unchecked(Position {x: 3, y: 7}, Position {x: 3, y: 5});
        let _ = board.jump_unchecked(Position {x: 2, y: 7}, Position {x: 2, y: 5});
        let _ = board.jump_unchecked(Position {x: 1, y: 7}, Position {x: 1, y: 5});
        let king_pos = Position {x: 4, y: 7};
        let res = board.find_possible_moves(board.pieces.get(&king_pos).unwrap());
        println!("{:}", print_moves(res.clone()));
        println!("{:#?}", res.clone());
        assert!(res.contains(&Move {pos: (king_pos, Position { x: 3, y: 7 }), move_type: MoveType::Checking { allowed: true}}));
        assert!(res.contains(&Move {pos: (king_pos, Position { x: 2, y: 7 }), move_type: MoveType::NonChecking(NonCheckingMove::BigCastle)}))
    }

    #[test]
    fn en_passant() {
        let mut board: Game = Game::set_up();
        let _ = board.jump_unchecked(convert_pos("B7"), convert_pos("B4"));
        let prev_move = board.jump_unchecked(convert_pos("C2"), convert_pos("C4"));
        let res = board.find_possible_moves(board.pieces.get(&convert_pos("B4")).unwrap());
        println!("{:#?}", print_moves(res))

    }

    #[test]
    fn num_of_allowed() {
        let game = Game::set_up();
        let allowed_white = game.get_allowed(Color::White);
        let allowed_white_count = allowed_white.iter().count();
        let allowed_black_count = game.get_allowed(Color::Black).iter().count();
        println!("{}", print_moves(allowed_white));
        assert_eq!(20, allowed_white_count);
        assert_eq!(20, allowed_black_count);
    }

    #[test]
    fn display_board() {
        let game = Game::set_up();
        println!("{}", game.pieces)
    }
}
