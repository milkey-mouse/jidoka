use memmap2::Mmap;
use std::{
    cmp::min,
    convert::Infallible,
    fs::File,
    io::{self, BufRead, BufReader, ErrorKind},
    str::Utf8Error,
};
use thiserror::Error;

// TODO: handle EINTR
// TODO: pub enum TakeCharError

#[derive(Error, Debug)]
pub enum TakeCharError {
    #[error("error reading input")]
    IO(#[from] io::Error),
    #[error("invalid UTF-8")]
    Utf8(#[from] Utf8Error),
    // TODO: ParseError::IO(io::Error::from(io::ErrorKind::UnexpectedEof)) or custom variant?
    #[error("unexpected EOF")]
    UnexpectedEOF, // TODO: necessary?
}

pub enum PeekableFile<T: BufRead, const N: usize> {
    Stream {
        stream: T,
        peeked: [u8; N],
        peeked_pos: usize,
    },
    Mmap {
        map: Mmap,
        pos: usize,
    },
}

impl<T: BufRead, const N: usize> PeekableFile<T, N> {
    pub fn take_while<R>(
        &mut self,
        mut should_take: impl FnMut(u8) -> bool,
        // TODO: impl FnMut(Cow<u8>) so no copy on new vec case?
        mut map_taken: impl FnMut(&[u8]) -> R,
    ) -> io::Result<Option<R>> {
        match self {
            Self::Stream {
                stream,
                peeked,
                peeked_pos,
            } => {
                let mut v = Vec::new();
                if *peeked_pos != N {
                    if let Some(len) = peeked[*peeked_pos..].iter().position(|b| !should_take(*b)) {
                        let start = *peeked_pos;
                        *peeked_pos += len;
                        return Ok(Some(map_taken(&peeked[start..*peeked_pos])));
                    } else {
                        v.extend_from_slice(&peeked[*peeked_pos..]);
                        *peeked_pos = N;
                    }
                } else {
                    // peeked_pos = N, so we don't need to worry about prepending peeked bytes
                    let buf = stream.fill_buf()?;
                    if buf.is_empty() {
                        // end of file
                        return Ok(None);
                    } else if let Some(len) = buf.iter().position(|b| !should_take(*b)) {
                        // let's optimistically assume we probably only need
                        // one buffer fill and thus can call map_taken directly
                        // on a slice from buf
                        let ret = map_taken(&buf[..len]);
                        stream.consume(len);
                        return Ok(Some(ret));
                    } else {
                        v.extend_from_slice(buf);
                        let len = buf.len();
                        stream.consume(len);
                    }
                }

                loop {
                    let buf = stream.fill_buf()?;
                    if buf.is_empty() {
                        // end of file
                        return Ok(None);
                    } else if let Some(len) = buf.iter().position(|b| !should_take(*b)) {
                        v.extend_from_slice(&buf[..len]);
                        stream.consume(len);
                        return Ok(Some(map_taken(&v)));
                    } else {
                        v.extend_from_slice(buf);
                        let len = buf.len();
                        stream.consume(len);
                    }
                }
            }
            Self::Mmap { map, pos } => match map[*pos..].iter().position(|b| !should_take(*b)) {
                Some(len) => {
                    let start = *pos;
                    *pos += len;

                    Ok(Some(map_taken(&map[start..*pos])))
                }
                None => Ok(None),
            },
        }
    }

    // factored into separate function from try_take mainly
    // to avoid lots of identical monomorphized versions
    pub fn try_take_slice<R>(
        &mut self,
        m: usize,
        f: impl FnOnce(&[u8]) -> (R, usize),
    ) -> io::Result<Option<R>> {
        match self {
            Self::Stream {
                stream,
                peeked,
                peeked_pos,
            } => {
                if *peeked_pos == N {
                    // bypass writing to peeked if nothing's in it
                    // and buf is big enough on its own
                    let buf = stream.fill_buf()?;
                    if m <= buf.len() {
                        let (ret, consumed) = f(&buf[..m]);
                        stream.consume(consumed);
                        return Ok(Some(ret));
                    }
                }

                if *peeked_pos <= N - m {
                    // peeked is full enough that we can take
                    // m values from peeked without refilling
                    let (ret, consumed) = f(&peeked[*peeked_pos..*peeked_pos + m]);
                    *peeked_pos += consumed;
                    return Ok(Some(ret));
                }

                // we need to replenish peeked from stream
                peeked.copy_within(*peeked_pos.., 0);
                let mut written = N - *peeked_pos;
                while written < N {
                    let buf = stream.fill_buf()?;
                    if buf.is_empty() {
                        // end of file
                        break;
                    }

                    let copy_len = min(N - written, buf.len());
                    peeked[written..written + copy_len].copy_from_slice(&buf[..copy_len]);
                    // stream.consume is not the same as self.consume:
                    // the former only consumes the values from BufRead's perspective,
                    // so here we are "transferring ownership" from stream to peeked
                    stream.consume(copy_len);
                }

                if written >= m {
                    let (ret, consumed) = f(&peeked[..m]);
                    *peeked_pos = min(consumed, m);
                    Ok(Some(ret))
                } else {
                    Ok(None)
                }
            }
            Self::Mmap { map, pos } => {
                if let Some(b) = map.get(*pos..*pos + N) {
                    let (ret, consumed) = f(b);
                    *pos = min(*pos + consumed, map.len());
                    Ok(Some(ret))
                } else {
                    Ok(None)
                }
            }
        }
    }

    pub fn try_take_char<R>(
        &mut self,
        f: impl FnOnce(char) -> Option<R>,
    ) -> io::Result<Result<Option<R>, Utf8Error>> {
        let mut f = Some(f);
        for i in 1..=4 {
            return match self.try_take_slice(i, |raw| {
                match std::str::from_utf8(raw) {
                    Ok(s) => {
                        // there is certainly at least one char to unwrap
                        // in a string of length >= 1 of full UTF-8 chars
                        let c = s.chars().next().unwrap();
                        match (f.take().unwrap())(c) {
                            Some(r) => (Some(Ok(r)), i),
                            None => (None, 0),
                        }
                    }
                    Err(ref e) if e.error_len().is_none() => (None, 0),
                    Err(e) => (Some(Err(e)), 0),
                }
            }) {
                // TODO: real error handling for EOF here
                Ok(None) => panic!("EOF reading char"),
                Ok(Some(None)) => Ok(Ok(None)),
                Ok(Some(Some(Ok(r)))) => Ok(Ok(Some(r))),
                Ok(Some(Some(Err(e)))) => Ok(Err(e)),
                Err(e) => Err(e),
            };
        }

        unreachable!("4 bytes should always contain at least one UTF-8 char")
    }

    pub fn try_take<R, const M: usize>(
        &mut self,
        f: impl FnOnce(&[u8; M]) -> (R, usize),
    ) -> io::Result<Option<R>> {
        /*const _: () =*/
        assert!(M <= N);

        // TODO: explicitly inline?
        self.try_take_slice(M, |s| {
            // TODO: just cast the function pointer?
            // SAFETY: _try_take only calls f if its
            // slice parameter is of length at least m
            let arr = unsafe { &*(s.as_ptr() as *const [u8; M]) };
            f(arr)
        })
    }

    pub fn take_slice_if(&mut self, m: usize, f: impl FnOnce(&[u8]) -> bool) -> io::Result<bool> {
        Ok(self
            .try_take_slice(m, |s| match f(s) {
                true => (Some(()), m),
                false => (None, 0),
            })?
            .is_some())
    }

    pub fn take_char_if(
        &mut self,
        f: impl FnOnce(char) -> bool,
    ) -> io::Result<Result<bool, Utf8Error>> {
        self.try_take_char(|c| match f(c) {
            true => Some(()),
            false => None,
        })
        .map(|r| r.map(|o| o.is_some()))
    }

    pub fn take_if<const M: usize>(
        &mut self,
        f: impl FnOnce(&[u8; M]) -> bool,
    ) -> io::Result<bool> {
        match self.try_take(|s| match f(s) {
            true => (true, M),
            false => (false, 0),
        }) {
            Ok(Some(taken)) => Ok(taken),
            Ok(None) => Ok(false),
            Err(e) => Err(e),
        }
    }

    pub fn take_if_eq<const M: usize>(&mut self, b: &[u8; M]) -> io::Result<bool> {
        /*println!("want {}", unsafe { std::str::from_utf8_unchecked(b) });
        Ok(self.try_take(|a| {
            println!("got {}", unsafe { std::str::from_utf8_unchecked(a) });
            if a == b {
                (true, M)
            } else {
                (false, 0)
            }
        })? == Some(true))
        // TODO*/
        self.take_if(|a| a == b)
    }

    // take_char_if_eq(b'c') would be redundant, as we could just do take_if_eq(b"c")

    pub fn take_slice_if_eq(&mut self, b: &[u8]) -> io::Result<bool> {
        self.take_slice_if(b.len(), |a| a == b)
    }
}

impl<T: BufRead, const N: usize> Iterator for PeekableFile<T, N> {
    type Item = io::Result<u8>;
    fn next(&mut self) -> Option<io::Result<u8>> {
        self.try_take(|[b]| (*b, 1)).transpose()
    }
}

impl<T: BufRead, const N: usize> From<T> for PeekableFile<T, N> {
    fn from(stream: T) -> Self {
        Self::Stream {
            stream,
            peeked: [0u8; N],
            peeked_pos: N,
        }
    }
}

impl<const N: usize> From<File> for PeekableFile<BufReader<File>, N> {
    fn from(file: File) -> Self {
        match unsafe { Mmap::map(&file) } {
            Ok(map) => Self::Mmap { map, pos: 0 },
            Err(_) => Self::from(BufReader::new(file)),
        }
    }
}
