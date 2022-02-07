use memmap2::Mmap;
use std::{
    cmp::min,
    convert::Infallible,
    fs::File,
    io::{self, BufRead, BufReader, ErrorKind, Result},
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
    pub fn peek(&mut self) -> Result<&[u8]> {
        Ok(match self {
            Self::Stream {
                stream,
                peeked,
                peeked_pos,
            } => {
                /*if *peeked_pos == N {
                    // bypass writing to peeked if nothing's in it
                    let buf = stream.fill_buf()?;
                    if let Some(b) = buf.get(..N) {
                        return Ok(b);
                    } else if buf.len() == 0 {
                        return Ok(&[]);
                    }
                }*/

                // some of peeked should be reused if 0 < peeked_pos < N
                peeked.copy_within(*peeked_pos.., 0);

                let mut remaining = &mut peeked[N - *peeked_pos..];
                while remaining.len() > 0 {
                    let buf = stream.fill_buf()?;
                    if buf.len() == 0 {
                        return Ok(&[]);
                    } else if *peeked_pos == N {

                    }

                    let consumed = min(remaining.len(), buf.len());
                    remaining[..consumed].copy_from_slice(&buf[..consumed]);
                    stream.consume(consumed);

                    remaining = &mut remaining[consumed..];
                }

                *peeked_pos = 0;
                peeked
            }
            Self::Mmap { map, pos } => &map[*pos..min(*pos + N, map.len())],
        })
    }

    pub fn peek_one(&mut self) -> Result<Option<u8>> {
        match self {
            Self::Stream {
                stream,
                peeked,
                peeked_pos,
            } => {
                if *peeked_pos < N {
                    let byte = peeked[*peeked_pos];
                    Ok(Some(byte))
                } else {
                    let mut byte = [0; 1];
                    match stream.read(&mut byte) {
                        Ok(0) => Ok(None),
                        Ok(_) => Ok(Some(byte[0])),
                        Err(e) => Err(e),
                    }
                }
            }
            Self::Mmap { map, pos } => Ok(map.get(*pos).map(|b| *b)),
        }
    }

    pub fn consume(&mut self, n: usize) {
        match self {
            Self::Stream {
                stream,
                peeked: _,
                peeked_pos,
            } => {
                let new_peeked_pos = min(*peeked_pos + n, N);
                stream.consume(new_peeked_pos - *peeked_pos);
                *peeked_pos = new_peeked_pos;
            }
            Self::Mmap { map: _, pos } => {
                *pos += n;
            }
        }
    }

    pub fn next_if(&mut self, func: impl FnOnce(u8) -> bool) -> Option<Result<u8>> {
        match self.peek_one() {
            Ok(Some(b)) if func(b) => {
                self.consume(1);
                Some(Ok(b))
            }
            Ok(_) => None,
            Err(e) => Some(Err(e)),
        }
    }
}

impl<T: BufRead, const N: usize> Iterator for PeekableFile<T, N> {
    type Item = Result<u8>;
    fn next(&mut self) -> Option<Result<u8>> {
        self.peek_one()
            .map(|peeked| {
                self.consume(1);
                peeked
            })
            .transpose()

        /*match self {
            Self::Stream {
                stream,
                peeked,
                peeked_pos,
            } => {
                if *peeked_pos < N {
                    let byte = peeked[*peeked_pos];
                    *peeked_pos += 1;
                    Some(Ok(byte))
                } else {
                    let mut byte = [0; 1];
                    loop {
                        return match stream.read(&mut byte) {
                            Ok(0) => None,
                            Ok(_) => Some(Ok(byte[0])),
                            Err(ref e) if e.kind() == ErrorKind::Interrupted => continue,
                            Err(e) => Some(Err(e)),
                        };
                    }
                }
            }
            Self::Mmap { map, pos } => map.get(*pos).map(|b| {
                *pos = *pos + 1;
                Ok(*b)
            }),
        }*/
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
