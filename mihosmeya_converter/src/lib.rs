#[test]
fn test1() {
    assert_eq!(
        convert("KESERIMA TONADORAPAMO HIFI, MAHOSMA NINIBINIYANA,").unwrap(),
        "かぃさぃりま となろらぱも いヒ,まおしま ににぴにいあな,"
    );
}
#[test]
fn test2() {
    assert_eq!(
        convert("HAYONTI MA NINIBINIYAFI, TONADORAMINIYA.").unwrap(),
        "あいおんち ま ににぴにいあヒ,となろらみにいあ."
    );
}
#[test]
fn test3() {
    assert_eq!(
        convert("SEMIGOHA, PIYA MA HOMI MEHIGAMIFI,").unwrap(),
        "さぃみこあ,ぴいあ ま おみ まぃいがみヒ,"
    );
}
#[test]
fn test4() {
    assert_eq!(
        convert("SANGAPAMO TONAMIYAFI MOHONIYA.").unwrap(),
        "さんがぱも となみいあヒ もおにいあ."
    );
}
#[test]
fn test5() {
    assert_eq!(
        convert("MIHOSMEYA SANTSEGIPAMO HIME,").unwrap(),
        "みおしまぃいあ さんさぃきぱも いまぃ,"
    );
}
#[test]
fn test6() {
    assert_eq!(
        convert("MEGAYEDI HOMI HINA TONADORAMINI.").unwrap(),
        "まぃがいあぃり おみ いな となろらみに."
    );
}

#[test]
fn test7() {
    assert_eq!(
        convert("HASTE, MAHOSMA, DIRETSO, SAMEGO, MOHONTA, HAYONTI MA SERIMIYA. OMMAFI MIRA, SEMIGOHA, PIYA MA SEGORIME SAMBATI SAMBATI GATONA").unwrap(),
        "あしたぃ,まおしま,りらぃそ,さまぃこ,もおんた,あいおんち ま さぃりみいあ.おんまヒ みら,さぃみこあ,ぴいあ ま さぃこりまぃ さんばち さんばち がとな"
    )
}

use log::warn;
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Error(String);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Onset {
    Hor0,
    P,
    B,
    T,
    DorR,
    K,
    G,
    F,
    SorTs,
    M,
    N,
    Y,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Vowel {
    A,
    I,
    E,
    O,
}

enum ParserState {
    WordInitial,
    OpenSyllParsed,
    SkipSpaces,
    OnsetParsed(Onset),
    N,
    S,
}

mod lex;

pub fn convert(a: &str) -> Result<String, Error> {
    use lex::Token::*;
    let mut ans = String::new();
    let mut state = ParserState::WordInitial;
    let lexed = lex::lex(a)?;
    let mut iter = lexed.iter();

    while let Some(c) = iter.next() {
        match c {
            Space | Comma | Period => {
                if let ParserState::SkipSpaces = state {
                    if *c != Space {
                        warn!("duplicate punctuation");
                        ans.push((*c).into());
                    }
                } else {
                    state = ParserState::SkipSpaces;
                    ans.push((*c).into());
                }
            }
            N => match state {
                ParserState::N => {
                    ans += "ん";
                    state = ParserState::OnsetParsed(Onset::N);
                }

                ParserState::S => {
                    ans += "し";
                    state = ParserState::OnsetParsed(Onset::N);
                }

                ParserState::OnsetParsed(o) => {
                    return Err(Error(format!("invalid N encountered after {:?}", o)));
                }

                ParserState::WordInitial
                | ParserState::OpenSyllParsed
                | ParserState::SkipSpaces => {
                    state = ParserState::N;
                }
            },

            S => match state {
                ParserState::N => {
                    ans += "ん";
                    state = ParserState::OnsetParsed(Onset::SorTs);
                }

                ParserState::S => {
                    ans += "し";
                    state = ParserState::OnsetParsed(Onset::SorTs);
                }

                ParserState::OnsetParsed(o) => {
                    return Err(Error(format!("invalid S encountered after {:?}", o)));
                }

                ParserState::WordInitial
                | ParserState::OpenSyllParsed
                | ParserState::SkipSpaces => {
                    state = ParserState::S;
                }
            },
            P | B | D | K | G | F | Y => {
                if let ParserState::OnsetParsed(c2) = state {
                    return Err(Error(format!(
                        "impossible consonant cluster detected: {:?} followed by {:?}",
                        c2, c
                    )));
                } else {
                    if let ParserState::N = state {
                        ans += "ん"
                    } else if let ParserState::S = state {
                        ans += "し"
                    }
                    state = ParserState::OnsetParsed(match c {
                        P => Onset::P,
                        B => Onset::B,
                        D => Onset::DorR,
                        K => Onset::K,
                        G => Onset::G,
                        F => Onset::F,
                        Y => Onset::Y,
                        _ => panic!("Cannot happen"),
                    });
                }
            }

            T | R | H | M => {
                match state {
                    ParserState::N => {
                        ans += "ん";
                    }

                    ParserState::S => {
                        ans += "し";
                    }
                    _ => {}
                };

                let onset = match c {
                    T => Onset::T,
                    R => Onset::DorR,
                    H => Onset::Hor0,
                    M => Onset::M,
                    _ => panic!("cannot happen"),
                };

                match (c, iter.next()) {
                    (_, Some(A)) => {
                        ans += make_syllable(onset, Vowel::A);
                        state = ParserState::OpenSyllParsed;
                    }
                    (_, Some(E)) => {
                        ans += make_syllable(onset, Vowel::E);
                        state = ParserState::OpenSyllParsed;
                    }
                    (_, Some(I)) => {
                        ans += make_syllable(onset, Vowel::I);
                        state = ParserState::OpenSyllParsed;
                    }
                    (_, Some(O)) => {
                        ans += make_syllable(onset, Vowel::O);
                        state = ParserState::OpenSyllParsed;
                    }

                    (T, Some(S)) => state = ParserState::OnsetParsed(Onset::SorTs),

                    (R, Some(R)) => {
                        ans += "ん";
                        state = ParserState::OnsetParsed(Onset::DorR)
                    }

                    (H, Some(H)) => {
                        ans += "し";
                        state = ParserState::OnsetParsed(Onset::Hor0)
                    }

                    (M, Some(M)) => {
                        ans += "ん";
                        state = ParserState::OnsetParsed(Onset::M)
                    }
                    (M, Some(P)) => {
                        ans += "ん";
                        state = ParserState::OnsetParsed(Onset::P)
                    }
                    (M, Some(B)) => {
                        ans += "ん";
                        state = ParserState::OnsetParsed(Onset::B)
                    }
                    (M, Some(F)) => {
                        ans += "ん";
                        state = ParserState::OnsetParsed(Onset::F)
                    }

                    (_, a) => {
                        return Err(Error(format!(
                            "Unexpected {:?} encountered after {:?}",
                            a, c
                        )))
                    }
                }
            }

            A | I | E | O => {
                let vowel = match c {
                    A => Vowel::A,
                    I => Vowel::I,
                    E => Vowel::E,
                    O => Vowel::O,
                    _ => panic!("cannot happen"),
                };

                match state {
                    ParserState::OnsetParsed(c2) => {
                        ans += make_syllable(c2, vowel);
                    }
                    ParserState::S => {
                        ans += make_syllable(Onset::SorTs, vowel);
                    }
                    ParserState::N => {
                        ans += make_syllable(Onset::N, vowel);
                    }
                    ParserState::WordInitial
                    | ParserState::OpenSyllParsed
                    | ParserState::SkipSpaces => ans += make_syllable(Onset::Hor0, vowel),
                }

                state = ParserState::OpenSyllParsed;
            }
        }
    }

    if let ParserState::N = state {
        return Err(Error(
            "Unexpected end of input encontered after N".to_string(),
        ));
    } else if let ParserState::S = state {
        return Err(Error(
            "Unexpected end of input encontered after S".to_string(),
        ));
    }

    Ok(ans)
}

fn make_syllable(c2: Onset, vowel: Vowel) -> &'static str {
    use Onset::*;
    use Vowel::*;
    match (c2, vowel) {
        (Hor0, A) => "あ",
        (Hor0, E) => "あぃ",
        (Hor0, I) => "い",
        (Hor0, O) => "お",
        (P, A) => "ぱ",
        (P, E) => "ぱぃ",
        (P, I) | (B, I) => "ぴ",
        (P, O) => "ぽ",
        (B, A) => "ば",
        (B, E) => "ばぃ",
        (B, O) => "ぼ",
        (T, A) => "た",
        (T, E) => "たぃ",
        (T, I) => "ち",
        (T, O) => "と",
        (DorR, A) => "ら",
        (DorR, E) => "らぃ",
        (DorR, I) => "り",
        (DorR, O) => "ろ",
        (K, A) => "か",
        (K, E) => "かぃ",
        (K, I) | (G, I) => "き",
        (K, O) | (G, O) => "こ",
        (G, A) => "が",
        (G, E) => "がぃ",
        (F, A) => "ハ",
        (F, E) => "ハぃ",
        (F, I) => "ヒ",
        (F, O) => "ホ",
        (SorTs, A) => "さ",
        (SorTs, E) => "さぃ",
        (SorTs, I) => {
            warn!("si / tsi detected. Replacing it with い");
            "い"
        }
        (SorTs, O) => "そ",
        (M, A) => "ま",
        (M, E) => "まぃ",
        (M, I) => "み",
        (M, O) => "も",
        (N, A) => "な",
        (N, E) => "なぃ",
        (N, I) => "に",
        (N, O) => "の",
        (Y, A) => "いあ",
        (Y, E) => "いあぃ",
        (Y, I) => "い", // intentional
        (Y, O) => "いお",
    }
}
