// ...

use std::fs;
use std::error;
use std::collections::HashMap;
use std::vec::IntoIter;
use std::str::{FromStr, Chars};
use std::slice::Iter;
use std::iter::Peekable;
use std::path::PathBuf;

pub type Error = Box<dyn error::Error>;

#[derive(Debug, Clone)]
pub struct Code {
    pub tokens: Vec<Token>,
}

impl FromStr for Code {
    type Err = Error;
    fn from_str(s: &str) -> Result<Code, Error> {
        parse(s)
    }
}

impl Code {
    pub fn process(&self, macros: &HashMap<String, Box<dyn Invoker>>) -> Result<Code, Error> {
        let (mut code, n) = process(self, macros)?;
        if n == 0 {
            return Ok(code);
        }

        let mut depth = 0;
        while depth <= 32 {
            let rs = process(&code, macros)?;
            if rs.1 == 0 {
                return Ok(rs.0);
            }
            code = rs.0;
            depth += 1;
        }

        Err("recursive macro invocation reach the limit 32.".into())
    }
}

fn process(code: &Code, macros: &HashMap<String, Box<dyn Invoker>>) -> Result<(Code, usize), Error> {
    let mut n = 0;
    let mut tokens = Vec::with_capacity(code.tokens.len());
    let mut iter = code.tokens.iter().peekable();
    loop {
        match iter.next() {
            Some(token) => {
                match token {
                    Token::Ident(ref s) => {
                        match macros.get(s) {
                            // #1 macro(params) ...
                            Some(r#macro) if r#macro.info().1.is_some() => {
                                match paren(&mut iter) {
                                    Ok(code) => {
                                        let params = split_by_comma(code)
                                            .into_iter()
                                            .map(|x| Code { tokens: x })
                                            .collect();
                                        tokens.extend({
                                            n += 1;
                                            r#macro.invoke(macros, Some(params))?
                                        });
                                    }
                                    Err(code) => {
                                        tokens.push(token.clone());
                                        tokens.extend(code.process(macros)?);
                                    }
                                }
                            }
                            // #2 macro ...
                            Some(r#macro) if r#macro.info().1.is_none() => {
                                tokens.extend({
                                    n += 1;
                                    r#macro.invoke(macros, None)?
                                });
                            }
                            _ => tokens.push(token.clone())
                        }
                    }
                    _ => tokens.push(token.clone())
                }
            }
            None => break,
        }
    }
    Ok((Code { tokens }, n))
}

fn paren(iter: &mut Peekable<Iter<Token>>) -> Result<Code, Code> {
    let mut tokens = Vec::new();

    match iter.peek() {
        Some(Token::Symbol(Symbol::LParen)) => { iter.next(); }
        _ => return Err(Code { tokens })
    }

    let mut n = 0;
    loop {
        match iter.next() {
            Some(Token::Symbol(Symbol::RParen)) if n == 0 => {
                return Ok(Code { tokens });
            }
            Some(Token::Symbol(Symbol::LParen)) => {
                n += 1;
                tokens.push(Token::Symbol(Symbol::LParen));
            }
            Some(Token::Symbol(Symbol::RParen)) => {
                n -= 1;
                tokens.push(Token::Symbol(Symbol::RParen));
            }
            Some(token) => {
                tokens.push(token.clone())
            }
            None => {
                tokens.insert(0, Token::Symbol(Symbol::LParen));
                return Err(Code { tokens });
            }
        }
    }
}

fn split_by_comma(code: Code) -> Vec<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut iter = code.tokens.iter().peekable();

    let mut paren_n = 0;
    let mut brace_n = 0;
    let mut bracket_n = 0;
    loop {
        let mut inner = Vec::new();

        loop {
            match iter.next() {
                Some(next) => {
                    match next {
                        Token::Symbol(Symbol::Comma) if paren_n == 0 && brace_n == 0 && bracket_n == 0 => break,
                        Token::Symbol(Symbol::LParen) => paren_n += 1,
                        Token::Symbol(Symbol::RParen) => paren_n -= 1,
                        Token::Symbol(Symbol::LBrace) => brace_n += 1,
                        Token::Symbol(Symbol::RBrace) => brace_n -= 1,
                        Token::Symbol(Symbol::LBracket) => bracket_n += 1,
                        Token::Symbol(Symbol::RBracket) => bracket_n -= 1,
                        _ => ()
                    }
                    inner.push(next.clone());
                }
                None => break
            }
        }

        tokens.push(inner);
        if iter.peek().is_none() {
            break;
        }
    }

    tokens
}

fn trim(tokens: &mut Vec<Token>) {
    loop {
        match tokens.first() {
            Some(Token::Ws(_)) => { tokens.remove(0); }
            _ => break,
        }
    }

    loop {
        match tokens.last() {
            Some(Token::Ws(_)) => { tokens.pop(); }
            _ => break,
        }
    }
}

impl IntoIterator for Code {
    type Item = Token;
    type IntoIter = IntoIter<Token>;
    fn into_iter(self) -> IntoIter<Token> {
        self.tokens.into_iter()
    }
}

impl ToString for Code {
    fn to_string(&self) -> String {
        self.tokens.iter()
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join("")
    }
}

impl From<Token> for Code {
    fn from(x: Token) -> Code {
        Code { tokens: Box::new([x]).to_vec() }
    }
}

#[derive(Debug, Clone)]
pub enum Token {
    KeyWord(Keyword),
    Ident(String),
    Literal(Literal),
    Symbol(Symbol),
    Ws(Ws),
}

impl ToString for Token {
    fn to_string(&self) -> String {
        match self {
            Token::KeyWord(k) => k.to_string(),
            Token::Ident(s) => s.clone(),
            Token::Literal(l) => l.to_string(),
            Token::Symbol(s) => s.to_string(),
            Token::Ws(w) => w.to_string(),
        }
    }
}

macro_rules! keyword {
    ( $( ( $k:ident, $n:literal ) ),* $(,)?) => {

        #[derive(Debug, Clone)]
        pub enum Keyword {
            $($k,)*
        }

        impl FromStr for Keyword {
            type Err = ();
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    $($n => Ok(Keyword::$k),)*
                    _ => Err(()),
                }
            }
        }

        impl ToString for Keyword {
            fn to_string(&self) -> String {
                match self {
                    $(Keyword::$k => $n.to_string(),)*
                }
            }
        }

    };
}

keyword![
    // go keyword
    (Var, "var"),
    (Const, "const"),
    (Package, "package"),
    (Import, "import"),
    (Func, "func"),
    (Return, "return"),
    (Defer, "defer"),
    (Go, "go"),
    (Select, "select"),
    (Interface, "interface"),
    (Struct, "struct"),
    (Break, "break"),
    (Case, "case"),
    (Continue, "continue"),
    (For, "for"),
    (Fallthrough, "fallthrough"),
    (Else, "else"),
    (If, "if"),
    (Switch, "switch"),
    (Goto, "goto"),
    (Default , "default "),
    (Chan, "chan"),
    (Type, "type"),
    (Map, "map"),
    (Range, "range"),

    // macro keyword
    (Define, "define"),
    (Enddef, "enddef"),
    (Include, "include"),
];


#[derive(Debug, Clone)]
pub enum Literal {
    Byt(String),
    Str(String),
    Txt(String),
    Int(usize),
    Flt(f64),
    Bol(bool),
}

impl ToString for Literal {
    fn to_string(&self) -> String {
        match self {
            Literal::Byt(s) => s.clone(),
            Literal::Str(s) => s.clone(),
            Literal::Txt(s) => s.clone(),
            Literal::Int(i) => (*i).to_string(),
            Literal::Flt(f) => (*f).to_string(),
            Literal::Bol(b) => (*b).to_string(),
        }
    }
}

macro_rules! symbol {
    ( $( ( $s:ident, $v:literal ) ),* $(,)?) => {

        #[derive(Debug, Clone)]
        pub enum Symbol {
            $($s,)*
        }

        impl Symbol {
            fn from_char(ch: char) -> Result<Symbol, Error> {
                match ch {
                    $($v => Ok(Symbol::$s),)*
                    other => Err(format!("invalid symbol `{}`.", other).into()),
                }
            }
        }

        impl ToString for Symbol {
            fn to_string(&self) -> String {
                match self {
                    $(Symbol::$s => $v.to_string(),)*
                }
            }
        }

    };
}

symbol![
    (Cmd, '#'),

    (Comma, ','),
    (Colon, ':'),
    (Smco, ';'),
    (Dot, '.'),
    (Asg, '='),

    (Add, '+'),
    (Sub, '-'),
    (Pls, '*'),
    (Div, '/'),
    (Mod, '%'),
    (And, '&'),
    (Or, '|'),
    (Xor, '^'),

    (Lt, '<'),
    (Gt, '>'),
    (Not, '!'),

    (LBracket, '['),
    (RBracket, ']'),
    (LBrace, '{'),
    (RBrace, '}'),
    (LParen, '('),
    (RParen, ')'),
];

#[derive(Debug, Clone)]
pub enum Ws {
    Space,
    Tab,
    Lf,
}

impl ToString for Ws {
    fn to_string(&self) -> String {
        match self {
            Ws::Space => " ".to_string(),
            Ws::Tab => "\t".to_string(),
            Ws::Lf => "\n".to_string(),
        }
    }
}

pub trait Invoker {
    fn invoke(&self, macros: &HashMap<String, Box<dyn Invoker>>, params: Option<Vec<Code>>) -> Result<Code, Error>;
    fn info(&self) -> (String, Option<Vec<String>>, Code);
}

#[derive(Debug)]
pub struct Macro {
    pub name: String,
    pub args: Option<Vec<String>>,
    pub code: Code,
}

impl Invoker for Macro {
    fn invoke(&self, macros: &HashMap<String, Box<dyn Invoker>>, mut params: Option<Vec<Code>>) -> Result<Code, Error> {
        match (&self.args, &mut params) {
            (Some(args), Some(params)) => {
                if args.len() != params.len() {
                    return Err(format!("the macro `{}` accepts {} arguments, but {} were given.",
                                       self.name, args.len(), params.len()).into());
                }

                let params: HashMap<&String, Code> = args.iter()
                    .zip(params.iter_mut()
                        .map(|x| {
                            trim(&mut x.tokens);
                            x.process(macros)
                        })
                        .collect::<Result<Vec<Code>, Error>>()?)
                    .collect();

                Ok(invoke(&self.code, &params).process(macros)?)
            }
            (None, None) => Ok(self.code.process(macros)?),
            _ => unreachable!()
        }
    }

    fn info(&self) -> (String, Option<Vec<String>>, Code) {
        (self.name.clone(), self.args.clone(), self.code.clone())
    }
}

fn invoke(code: &Code, params: &HashMap<&String, Code>) -> Code {
    let mut tokens = Vec::with_capacity(code.tokens.len());
    let mut iter = code.tokens.iter();
    loop {
        match iter.next() {
            Some(Token::Ident(s)) if params.contains_key(s) => {
                tokens.extend(params[s].clone());
            }
            Some(token) => tokens.push(token.clone()),
            None => break,
        }
    }
    Code { tokens }
}

#[derive(Debug)]
pub struct Concat;

impl Invoker for Concat {
    fn invoke(&self, macros: &HashMap<String, Box<dyn Invoker>>, params: Option<Vec<Code>>) -> Result<Code, Error> {
        let s = match params {
            Some(mut params) => {
                params.iter_mut()
                    .map(|x| {
                        trim(&mut x.tokens);
                        x.process(macros)
                    })
                    .collect::<Result<Vec<Code>, Error>>()?
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join("")
            }
            None => unreachable!()
        };
        Ok(parse(&s)?.process(macros)?)
    }

    fn info(&self) -> (String, Option<Vec<String>>, Code) {
        ("concat".to_string(), Some(Vec::new()), Code { tokens: Vec::new() })
    }
}

#[derive(Debug)]
pub struct Str;

impl Invoker for Str {
    fn invoke(&self, _: &HashMap<String, Box<dyn Invoker>>, params: Option<Vec<Code>>) -> Result<Code, Error> {
        let s = match params {
            Some(params) => {
                params.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(",")
            }
            None => return Ok(Code { tokens: Vec::new() })
        };
        Ok(Token::Literal(Literal::Str(format!("\"{}\"", s))).into())
    }

    fn info(&self) -> (String, Option<Vec<String>>, Code) {
        ("str".to_string(), Some(Vec::new()), Code { tokens: Vec::new() })
    }
}

const CH: [char; 86] = [
    // 0
    '_', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
    'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C',
    'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
    'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
    'X', 'Y', 'Z',
    // 53
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    // 63
    '#', ',', ':', ';', '.', '=', '+', '-', '*', '/',
    '%', '&', '|', '^', '<', '>', '!', '[', ']', '{',
    '}', '(', ')'
    // 86
];

#[inline]
fn is_ident_first(ch: char) -> bool {
    CH[0..53].contains(&ch)
}

#[inline]
fn is_ident(ch: char) -> bool {
    CH[0..63].contains(&ch)
}

#[inline]
fn is_digit(ch: char) -> bool {
    CH[53..63].contains(&ch)
}

#[inline]
fn is_symbol(ch: char) -> bool {
    CH[63..86].contains(&ch)
}

pub fn parse(s: &str) -> Result<Code, Error> {
    let mut tokens = Vec::new();
    let mut iter = s.chars().peekable();
    let mut buffer = String::new();
    loop {
        match iter.peek() {
            // comments
            Some('/') => {
                drop_comments(&mut iter, &mut tokens)?;
            }
            // ident/keyword
            Some(&ch) if is_ident_first(ch) => {
                parse_ident(&mut iter, &mut buffer, &mut tokens)?;
            }
            // number
            Some(&ch) if is_digit(ch) => {
                parse_number(&mut iter, &mut buffer, &mut tokens)?;
            }
            // string/text/byte
            Some('\"') => {
                parse_string(&mut iter, &mut buffer, &mut tokens)?;
            }
            Some('`') => {
                parse_text(&mut iter, &mut buffer, &mut tokens)?;
            }
            Some('\'') => {
                parse_byte(&mut iter, &mut buffer, &mut tokens)?;
            }
            // symbol
            Some(&ch) if is_symbol(ch) => {
                tokens.push(Token::Symbol(Symbol::from_char(ch)?));
                iter.next();
            }
            // ws
            Some(' ') => {
                tokens.push(Token::Ws(Ws::Space));
                iter.next();
            }
            Some('\t') => {
                tokens.push(Token::Ws(Ws::Tab));
                iter.next();
            }
            Some('\n') => {
                tokens.push(Token::Ws(Ws::Lf));
                iter.next();
            }
            // invalid
            Some(ch) => return Err(format!("invalid character `{}`", ch).into()),
            // end
            None => break
        }
    }
    Ok(Code { tokens })
}

fn parse_ident(iter: &mut Peekable<Chars>, buffer: &mut String, tokens: &mut Vec<Token>) -> Result<(), Error> {
    loop {
        match iter.peek() {
            Some(&ch) if is_ident(ch) => {
                match iter.next() {
                    Some(ch) => buffer.push(ch),
                    None => unreachable!()
                }
            }
            _ => break
        }
    }

    match buffer.parse::<Keyword>() {
        Ok(keyword) => tokens.push(Token::KeyWord(keyword)),
        Err(()) => {
            match buffer.as_str() {
                "true" => {
                    tokens.push(Token::Literal(Literal::Bol(true)));
                }
                "false" => {
                    tokens.push(Token::Literal(Literal::Bol(false)));
                }
                _ => {
                    tokens.push(Token::Ident(buffer.clone()))
                }
            }
        }
    }

    buffer.clear();
    Ok(())
}

fn parse_number(iter: &mut Peekable<Chars>, buffer: &mut String, tokens: &mut Vec<Token>) -> Result<(), Error> {
    loop {
        match iter.peek() {
            Some('0'..='9') => {
                match iter.next() {
                    Some(ch) => buffer.push(ch),
                    None => unreachable!()
                }
            }
            _ => break
        }
    }

    match iter.peek() {
        Some('.') => {
            buffer.push('.');
            iter.next();
            match iter.peek() {
                Some('0'..='9') => {
                    match iter.next() {
                        Some(ch) => buffer.push(ch),
                        None => unreachable!()
                    }
                }
                Some(&ch) if is_ident(ch) => {
                    buffer.pop();
                    match buffer.parse::<usize>() {
                        Ok(i) => tokens.push(Token::Literal(Literal::Int(i))),
                        Err(_) => return Err(format!("error parsing `{}` to integer.", buffer).into())
                    }
                    tokens.push(Token::Symbol(Symbol::Dot));
                    buffer.clear();
                    return Ok(());
                }
                _ => return Err(format!("error parsing `{}` to token", buffer).into())
            }
            loop {
                match iter.peek() {
                    Some('0'..='9') => {
                        match iter.next() {
                            Some(ch) => buffer.push(ch),
                            None => unreachable!()
                        }
                    }
                    _ => break
                }
            }
        }
        _ => ()
    }

    match iter.peek() {
        Some('e') | Some('E') => {
            match iter.next() {
                Some(ch) => buffer.push(ch),
                None => unreachable!()
            }
            match iter.peek() {
                Some('+') | Some('-') => {
                    match iter.next() {
                        Some(ch) => buffer.push(ch),
                        None => unreachable!()
                    }
                }
                _ => ()
            }
            loop {
                match iter.peek() {
                    Some('0'..='9') => {
                        match iter.next() {
                            Some(ch) => buffer.push(ch),
                            None => unreachable!()
                        }
                    }
                    _ => break
                }
            }
        }
        _ => ()
    }

    if buffer.contains('.') {
        match buffer.parse::<f64>() {
            Ok(f) => tokens.push(Token::Literal(Literal::Flt(f))),
            Err(_) => return Err(format!("error parsing `{}` to float.", buffer).into())
        }
    } else {
        match buffer.parse::<usize>() {
            Ok(i) => tokens.push(Token::Literal(Literal::Int(i))),
            Err(_) => return Err(format!("error parsing `{}` to integer.", buffer).into())
        }
    }

    buffer.clear();
    Ok(())
}

fn parse_string(iter: &mut Peekable<Chars>, buffer: &mut String, tokens: &mut Vec<Token>) -> Result<(), Error> {
    match iter.next() {
        Some('\"') => buffer.push('\"'),
        _ => unreachable!(),
    }

    loop {
        match iter.peek() {
            Some('\"') => {
                buffer.push('\"');
                iter.next();
                break;
            }
            Some('\\') => {
                buffer.push('\\');
                iter.next();
                match iter.next() {
                    Some(ch) => buffer.push(ch),
                    None => ()
                }
            }
            Some(&ch) => {
                buffer.push(ch);
                iter.next();
            }
            None => return Err("unclosed string literal.".into())
        }
    }

    tokens.push(Token::Literal(Literal::Str(buffer.clone())));
    buffer.clear();
    Ok(())
}

fn parse_text(iter: &mut Peekable<Chars>, buffer: &mut String, tokens: &mut Vec<Token>) -> Result<(), Error> {
    match iter.next() {
        Some('`') => buffer.push('`'),
        _ => unreachable!()
    }

    loop {
        match iter.peek() {
            Some('`') => {
                buffer.push('`');
                iter.next();
                break;
            }
            Some(&ch) => {
                buffer.push(ch);
                iter.next();
            }
            None => return Err("unclosed text literal.".into())
        }
    }

    tokens.push(Token::Literal(Literal::Txt(buffer.clone())));
    buffer.clear();
    Ok(())
}

fn parse_byte(iter: &mut Peekable<Chars>, buffer: &mut String, tokens: &mut Vec<Token>) -> Result<(), Error> {
    match iter.next() {
        Some('\'') => buffer.push('\''),
        _ => unreachable!(),
    }

    loop {
        match iter.peek() {
            Some('\'') => {
                buffer.push('\'');
                iter.next();
                break;
            }
            Some('\\') => {
                buffer.push('\\');
                iter.next();
                match iter.next() {
                    Some(ch) => buffer.push(ch),
                    None => ()
                }
            }
            Some(&ch) => {
                buffer.push(ch);
                iter.next();
            }
            None => return Err("unclosed byte literal.".into())
        }
    }

    tokens.push(Token::Literal(Literal::Byt(buffer.clone())));
    buffer.clear();
    Ok(())
}

fn drop_comments(iter: &mut Peekable<Chars>, tokens: &mut Vec<Token>) -> Result<(), Error> {
    match iter.next() {
        Some('/') => (),
        _ => unreachable!()
    }

    match iter.peek() {
        Some('/') => {
            iter.next();
            loop {
                match iter.peek() {
                    Some('\n') | None => break,
                    _ => { iter.next(); }
                }
            }
        }
        Some('*') => {
            iter.next();
            loop {
                match iter.next() {
                    Some('*') => {
                        match iter.next() {
                            Some('/') => break,
                            _ => ()
                        }
                    }
                    None => return Err("unclosed multi-line comments.".into()),
                    _ => ()
                }
            }
        }
        _ => tokens.push(Token::Symbol(Symbol::Div))
    }

    Ok(())
}

pub fn read_path(stack: Vec<&PathBuf>, macros: &mut HashMap<String, Box<dyn Invoker>>) -> Result<Code, Error> {
    let path = stack.last().expect("missing path");
    let mut code = fs::read_to_string(path)
        .expect(&format!("error reading code on path: `{}`", path.to_str().expect("error convert to str")))
        .parse::<Code>()?;

    read_code(&mut code, &stack, macros)?;
    Ok(code)
}

fn read_code(code: &mut Code, stack: &Vec<&PathBuf>, macros: &mut HashMap<String, Box<dyn Invoker>>) -> Result<(), Error> {
    let mut tokens = Vec::with_capacity(code.tokens.len());
    let mut iter = code.tokens.iter().peekable();

    loop {
        match iter.next() {
            Some(Token::Symbol(Symbol::Cmd)) => {
                match iter.next() {
                    Some(Token::KeyWord(Keyword::Define)) => {
                        let r#macro = read_macro(&mut iter)?;
                        macros.insert(r#macro.name.clone(), Box::new(r#macro));
                    }
                    Some(Token::KeyWord(Keyword::Include)) => {
                        read_include(&mut iter, stack.clone(), macros)?;
                    }
                    Some(other) => return Err(format!("invalid command: `{}`", other.to_string()).into()),
                    None => return Err("expect a keyword after # command.".into())
                }
            }
            Some(token) => tokens.push(token.clone()),
            None => break
        }
    }

    *code = Code { tokens };
    Ok(())
}

fn read_macro(iter: &mut Peekable<Iter<Token>>) -> Result<Macro, Error> {
    // remove ws
    loop {
        match iter.peek() {
            Some(Token::Ws(_)) => { iter.next(); }
            _ => break
        }
    }

    let name = match iter.next() {
        Some(Token::Ident(name)) => name.clone(),
        Some(other) => return Err(format!("expect an ident as macro name, got `{}`.", other.to_string()).into()),
        None => return Err("expect an ident as macro name.".into())
    };

    let args = match iter.peek() {
        Some(Token::Symbol(Symbol::LParen)) => {
            match paren(iter) {
                Ok(code) => {
                    let args: Vec<String> = code.tokens
                        .split(|x| {
                            match x {
                                Token::Symbol(Symbol::Comma) => true,
                                _ => false
                            }
                        })
                        .map(|x| -> Result<String, Error> {
                            let mut x = x.to_vec();
                            // remove ws
                            trim(&mut x);

                            if x.len() != 1 {
                                return Err(format!("macro arg should be a single token, got {}.", x.len()).into());
                            }

                            match x.first() {
                                Some(Token::Ident(s)) => Ok(s.clone()),
                                Some(other) => Err(format!("macro arg should be an ident, got `{}`", other.to_string()).into()),
                                None => unreachable!()
                            }
                        })
                        .collect::<Result<Vec<String>, Error>>()?;
                    Some(args)
                }
                Err(_) => return Err("unclosed parenthesis on macro definition.".into())
            }
        }
        _ => None,
    };

    // remove ws
    loop {
        match iter.peek() {
            Some(Token::Ws(_)) => { iter.next(); }
            _ => break
        }
    }

    let mut tokens = Vec::new();
    loop {
        match iter.next() {
            Some(Token::Symbol(Symbol::Cmd)) => {
                match iter.next() {
                    Some(Token::KeyWord(Keyword::Enddef)) => break,
                    Some(other) => return Err(format!("expect `enddef` to finish macro definition, got `{}`.", other.to_string()).into()),
                    None => return Err("expect a keyword after # command.".into())
                }
            }
            Some(token) => tokens.push(token.clone()),
            None => return Err("unclosed macro definition.".into())
        }
    }

    // remove ws
    loop {
        match tokens.last() {
            Some(Token::Ws(_)) => { tokens.pop(); }
            _ => break
        }
    }

    let code = Code { tokens };

    Ok(Macro { name, args, code })
}

fn read_include(iter: &mut Peekable<Iter<Token>>, stack: Vec<&PathBuf>, macros: &mut HashMap<String, Box<dyn Invoker>>) -> Result<(), Error> {
    // remove ws
    loop {
        match iter.peek() {
            Some(Token::Ws(_)) => { iter.next(); }
            _ => break
        }
    }

    let path: PathBuf = match iter.next() {
        Some(Token::Literal(Literal::Str(path))) => path.trim_matches('\"').into(),
        _ => return Err("expect a string as path after `include` command.".into())
    };

    if path.is_dir() {
        // directory process
        for entry in path.read_dir().expect("error read directory") {
            let path = entry.expect("error read file").path();
            if !path.is_dir() {
                match path.extension() {
                    Some(s) if s == "go" => {
                        // prevent recursive include
                        if stack.contains(&&path) {
                            return Err(format!("recursive include: `{}`", path.to_str().expect("error convert to str")).into());
                        }
                        let mut stack = stack.clone();
                        stack.push(&path);
                        read_path(stack, macros)?;
                    }
                    _ => ()
                }
            }
        }
    } else {
        // single file process
        // prevent recursive include
        if stack.contains(&&path) {
            return Err(format!("recursive include: `{}`", path.to_str().expect("error convert to str")).into());
        }
        let mut stack = stack.clone();
        stack.push(&path);
        read_path(stack, macros)?;
    }

    // remove ws
    loop {
        match iter.peek() {
            Some(Token::Ws(Ws::Lf)) | None => break,
            Some(Token::Ws(_)) => { iter.next(); }
            Some(other) => return Err(format!("invalid token `{}` after including path.", other.to_string()).into()),
        }
    }

    Ok(())
}