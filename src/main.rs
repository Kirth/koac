pub mod lex;

use lex::*;

#[derive(Clone, Debug)]
pub enum Object {
    Nil,
    Int(i64),
    String(String),
    List(Vec<Object>),
    //Func(Box<dyn Block>)
}

#[derive(Debug)]
pub enum Expression {
    IntLit(i64),
    StringLit(String),
    Identifier(String),
    Call(Box<Expression>, Vec<Box<Expression>>),
    List(Vec<Box<Expression>>),
    Assign(Box<Expression>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
    For(Box<Expression>, Box<Expression>, Box<Expression>), // pattern, iterator, body
    Chain(Box<Expression>, Vec<(String, Box<Expression>)>),
    Fn(String, Box<Expression>)
}

#[derive(Debug)]
struct Parser {
    tokens: Vec<TokenLoc>,
    position: usize
}

impl Parser {
    fn peek(&self) -> Option<TokenLoc> {
        self.tokens.get(self.position).cloned()
    }

    fn peek_token(&self) -> Option<Token> {
        self.peek().map(|t| t.token)
    }

    fn ignore(&mut self, expected: Token) -> bool /* Found? */ {
        if self.peek_token().as_ref() == Some(&expected) {
            self.position += 1;
            return true
        }

        return false
    }

    fn require(&mut self, expected: Token, message: String) -> Result<(), String> {
        let peeked = self.peek_token();

        if peeked.as_ref() != Some(&expected) {
            return Err(format!("{}; required {:?}, got {:?}", message, expected, peeked))
        } else {
            self.position += 1;
            return Ok(())
        }
    }

    fn atom(&mut self) -> Result<Expression, String> {
        //println!("atom call: {:?}", self.peek_token());
        if let Some(tokenloc) = self.peek() {
            match tokenloc.token {
                Token::IntLit(n) => {
                    self.position += 1;
                    Ok(Expression::IntLit(n))
                },
                Token::StringLit(s) => {
                    self.position += 1;
                    Ok(Expression::StringLit(s))
                },
                Token::Identifier(s) => {
                    self.position += 1;
                    Ok(Expression::Identifier(s))
                },
                Token::LParen => {
                    self.position += 1;
                    let e = self.expression()?;
                    self.require(Token::RParen, "parenthesized expression".to_string())?;
                    Ok(e)
                },
                Token::RParen => Err(format!("unexpected RPAREN at {:?}", tokenloc)),
                Token::LBrace => {
                    self.position += 1;
                    let e = self.expression()?;
                    self.require(Token::RBrace, "unterminated block".to_string())?;
                    Ok(e)
                }
                Token::If => {
                    self.position += 1;
                    self.ignore(Token::LParen);
                    let condition = self.expression()?;
                    self.ignore(Token::RParen);
                    let lbrace = self.ignore(Token::LBrace);
                    //self.require(Token::LBrace, "if body must start with {".to_string())?;
                    let body = self.expression()?;
                    //
                    if lbrace { 
                        self.require(Token::RBrace, "if body must end with }".to_string())?; 
                    } else {
                        self.ignore(Token::RBrace);
                    }
                    
                    let else_body = if self.peek_token() == Some(Token::Else) {
                        self.position += 1; 
                        Some(Box::new(self.assignment()?))
                    } else {
                        None
                    };

                    Ok(Expression::If(Box::new(condition), Box::new(body), else_body))
                },
                Token::For => { // so far we only support for(i := 0; i < 1337; i++);
                    self.position += 1;
                    self.require(Token::LParen, "for-statements MUST start with (".to_string())?;
                    let pattern = self.pattern()?; 
                    self.position += 1;
                    let iteratee = self.pattern()?;
                    self.require(Token::RParen, "for-statement declarations MUST end with )".to_string())?;
                    let body = self.assignment()?;
                    
                    Ok(Expression::For(Box::new(pattern), Box::new(iteratee), Box::new(body)))
                },
                Token::Fn => {
                    self.position += 1;
                    println!("fn! {:?}", self.peek_token());
                    let name = match self.peek_token() {
                        Some(Token::Identifier(s))  => s,
                        _ => "<anonymous_fn>".to_string()
                    };

                    println!("name: {}", &name);

                    self.position += 1;
                    self.require(Token::LParen, "fn-def expected ( opening argument list".to_string())?;
                    let args = self.expression()?;
                    println!("ARGS: {:?}", args);
                    self.require(Token::RParen, "fn-def expected ) closing argument list".to_string())?;

                    println!("{:?}", self.peek_token());
                    let body = self.expression()?;
                    println!("BODAY {:?}", body);

                    Ok(Expression::Fn(name, Box::new(body)))
                },
                _ => Err(format!("unexpected token: {:?}", tokenloc)),
            }
        } else {
            Err("atom: unexpected EOF".to_string())
        }
    }

    fn operand(&mut self) -> Result<Expression, String> {
        let mut cur = self.atom()?;

        while self.peek_token() == Some(Token::LParen) {
            self.position += 1;
            let (cs, _) = self.comma_separate_chains()?;
            self.require(Token::RParen, "call expression".to_string())?;
            cur = Expression::Call(Box::new(cur), cs);
        }

        Ok(cur)
    }

    fn peek_stopper(&self) -> bool {
        match self.peek_token() {
            Some(Token::RParen) => true,
            Some(Token::RBrace) => true,
            Some(Token::Else) => true,
            None => true,
            _ => false
        }
    }

    fn peek_csc_stopper(&self) -> bool {
        self.peek_stopper() || match self.peek_token() {
            Some(Token::Assign) => true,
            _ => false,
        }
    }

    fn peek_chain_stopper(&self) -> bool {
        self.peek_csc_stopper() || match self.peek_token() {
            Some(Token::Comma) => true,
            Some(Token::Colon) => true,
            _ => false,
        }
    }

    fn chain(&mut self) -> Result<Expression, String> {
        let op1 = self.operand()?;
        if self.peek_chain_stopper() {
            return Ok(op1)
        }

        let op2 = self.atom()?;
        /*
        // old implementation:
        let op1 = self.operand()?;
        let mut ops = Vec::<(String, Box<Expression>)>::new();

        while let Some(Token::Identifier(op)) = self.peek_token() {
            self.position += 1;
            ops.push((op.to_string(), Box::new(self.operand()?)));
        }

        if ops.is_empty() {
            return Ok(op1);
        }

        Ok(Expression::Chain(Box::new(op1), ops))  */
    }

    // returns parsed form of e.g. `(foo(bar), baz + 1)` , comma helps discern `(x)` from `(x,)`
    fn comma_separate_chains(&mut self) -> Result<(Vec<Box<Expression>>, bool), String> {
        if match self.peek_token() {
            Some(Token::RParen | Token::RBrace | Token::Assign) => true,
            _ => false } {
            return Ok((Vec::new(), false))
        }

        let mut xs = vec![Box::new(self.chain()?)];
        let mut comma = false;

        while self.peek_token() == Some(Token::Comma) {
            self.position += 1;
            comma = true;

            if match self.peek_token() 
              {
                Some(Token::RParen | Token::RBrace | Token::Assign) => true,
                _ => false } {
                    return Ok((xs, comma));  // we're done and probably inside another chain!
            }

            xs.push(Box::new(self.chain()?));
        }

        println!("dropping out of csc, {:?} is not a comma", self.peek_token());

        return Ok((xs, comma))
    }

    // a pattern is comma separated, no ;  e.g. (_"foo", 2_)
    fn pattern(&mut self) -> Result<Expression, String> {
        let (mut xs, comma) = self.comma_separate_chains()?;

        if !comma && xs.len() == 1 {
            return Ok(*xs.swap_remove(0)) // there's only one expression so return it directly
        }
        
        return Ok(Expression::List(xs))
    }



    fn assignment(&mut self) -> Result<Expression, String> {
        let pattern = self.pattern()?;

        println!("assigned, went past patt, peeking {:?}", self.peek_token());

        if self.peek_token() == Some(Token::Assign) {
            self.position += 1;
            return Ok(Expression::Assign(Box::new(pattern), Box::new(self.pattern()?)))
        }

        Ok(pattern)
    }

    fn expression(&mut self) -> Result<Expression, String> {
        let mut exprs = vec![Box::new(self.assignment()?)]; // does not imply every expression starts with an assignment
        let mut ending_semicolon = false;

        while self.peek_token() == Some(Token::Semicolon) {
            self.position += 1;
            if match self.peek_token() {
                Some(Token::RParen | Token::RBrace | Token::Assign) => true,
                _ => false } {
                // TODO: we bounced against a semicolon; there's nothing in this block past this, handle that
                print!("BUMP");
                ending_semicolon = true;
                break;
            }                

            if self.peek_token() != None {
                println!("pushing assignment based on peeked {:?}", self.peek_token());
                exprs.push(Box::new(self.assignment()?));
            }
        }

        if exprs.len() == 1 && ending_semicolon {
            return Ok(*exprs.remove(0))
        }

        println!("expression with len {}", exprs.len());
        for i in &exprs { 
            println!("> {:?}", i);
        }
        return Ok(Expression::List(exprs))
    }
}

fn parse(tokens: Vec<TokenLoc>) -> Result<Expression, String> {
    let mut parser: Parser = Parser {
        tokens, position: 0
    };

    parser.expression()
}


use std::{fs, io, option, net::TcpStream};
fn main() -> io::Result<()> {
    //let code = fs::read_to_string("examples/fizzbuzz.kc")?;
    let code = fs::read_to_string("examples/hello_world.kc")?;
    
    let tokens = lex::lex(code);

    println!("{:#?}", tokens);

    let parsed = parse(tokens).unwrap();

    match parsed {
        Expression::List(e) => {
            for i in e {
                println!("{:?}", i);
            }
        }
        _ => {
            println!("parsed expression wasn't a list");
        }

    }

    Ok(())
}
