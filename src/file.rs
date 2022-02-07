use memmap2::Mmap;
use std::{
    cmp::min,
    convert::Infallible,
    fs::File,
    io::{self, BufRead, BufReader, ErrorKind},
    str::Utf8Error,
};

// TODO: handle EINTR

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
    // TODO: peek(n: usize) where n <= N
    // and then have try_take(b'str') -> bool
    // try_take(impl FnOnce([u8; N]))
    // take_until()

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
                    if buf.len() == 0 {
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

    //pub fn take_if<const M: usize>(&mut self, 

    pub fn take_if_eq<const M: usize>(&mut self, b: &[u8; M]) -> io::Result<bool> {
        println!("want {}", unsafe { std::str::from_utf8_unchecked(b) });
        Ok(self.try_take(|a| {
            println!("got {}", unsafe { std::str::from_utf8_unchecked(a) });
            if a == b { (true, M) } else { (false, 0) }
        })? == Some(true))
    }

    pub fn peek<const M: usize>(&mut self) -> io::Result<Option<[u8; M]>> {
        // TODO: the copy makes this inefficient, don't use this fn
        self.try_take(|b| (*b, 0))
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
                Ok(None) => panic!("EOF reading char"),
                Ok(Some(None)) => Ok(Ok(None)),
                Ok(Some(Some(Ok(r)))) => Ok(Ok(Some(r))),
                Ok(Some(Some(Err(e)))) => Ok(Err(e)),
                Err(e) => Err(e),
            };
        }

        unreachable!("4 bytes should always contain at least one UTF-8 char")
    }

    /*pub fn take_char_if_eq<const M: usize>(&mut self, b: char) -> io::Result<Result<bool, Utf8Error>> {
        self.try_take_char(|a| a == b).map(|r| r.map(Option::is_some))
    }*/

    pub fn consume(&mut self, n: usize) {
        match self {
            Self::Stream {
                stream,
                peeked: _,
                peeked_pos,
            } => {
                // TODO: this is wrong but does it matter?
                let new_peeked_pos = min(*peeked_pos + n, N);
                stream.consume(new_peeked_pos - *peeked_pos);
                *peeked_pos = new_peeked_pos;
            }
            Self::Mmap { map: _, pos } => {
                *pos += n;
            }
        }
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
