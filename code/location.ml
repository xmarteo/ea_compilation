type 'a t = {
  startpos: Lexing.position;
  endpos: Lexing.position;
  content: 'a
}

let make startpos endpos content = {
  startpos = startpos;
  endpos = endpos;
  content = content
}

let content { content = content } =
  content

let startpos { startpos = startpos } =
  startpos

let endpos { endpos = endpos } =
  endpos

