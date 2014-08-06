fs = require('fs')
P = require('comparse')

class YangStatement
  constructor: (@prf, @kw, @arg, @substmts) -> 

yLineComment =
  (P.string '//').bind ->
    P.anyChar.manyTill(P.char '\n').bind (cs) ->
      P.unit cs.join ''

yBlockComment =
  (P.string '/*').bind ->
    P.anyChar.manyTill(P.string '*/').bind (cs) ->
      P.unit cs.join ''

yComment = yLineComment.orElse yBlockComment

sep = (P.space.orElse yComment).skipMany 1
optSep = (P.space.orElse yComment).skipMany()

yIdentifier =
  (P.letter.orElse P.char '_').bind (fst) ->
    (P.alphanum.orElse P.oneOf '.-').many().bind (tail) ->
      P.unit fst + tail.join ''

yKeyword =
  (yIdentifier.bind (prf) -> P.char(':').bind ->
    P.unit prf).option().bind (pon) ->
      yIdentifier.bind (kw) ->
        P.unit [pon, kw]

uArg =
  (P.noneOf(" '\"\n\t\r;{}/").orElse \
    P.char('/').notFollowedBy(P.oneOf '/*')).many(1).bind (str) ->
      P.unit str.join ''

sqLit =
  P.sat((c) -> c != "'").many().between(
    P.char("'"), P.char("'")).bind (cs) -> P.unit cs.join ''

yEscape = P.char('\\').bind ->
  esc =
    't': '\t'
    'n': '\n'
    '"': '"'
    '\\': '\\'
  P.oneOf('tn"\\').bind (c) -> P.unit esc[c]

dqChar = P.noneOf('"\\').orElse yEscape

dqTrailing = 

dqString = (lim) ->
  trimLead = (str) ->
    left = lim
    sptab = '        '
    i = 0
    while left > 0
      c = str[i++]
      if c == ' '
        left -= 1
      else if c == '\t'
        return sptab[...8-left] + str[i..] if left < 8
        left -= 8
      else
        return str[(i-1)..]
    str[i..]
  dqChar.manyTill(P.char '"').bind (cs) ->
    res = []
    lines = cs.join('').split('\n')
    for ln in lines[..-2]
      mo = trimLead(ln).match /(.*\S)?\s*/
      res.push mo[1]
    res.push trimLead lines.pop()
    P.unit res.join('\n')

dqLit =
  P.char('"').bind -> P.coordinates.bind (col) ->
    dqString col[1]

qLit = dqLit.orElse sqLit

qArg = qLit.bind (lft) ->
  (P.char('+').between(optSep, optSep).bind -> qArg).option().bind (rt) ->
    P.unit lft + rt

yArgument = uArg.orElse(qArg).option()

yStatement = yKeyword.bind (kw) ->
  sep.bind -> yArgument.bind (arg) ->
    optSep.bind -> semiOrBlock.bind (sst) ->
      P.unit new YangStatement kw[0], kw[1], arg, sst

stmtBlock = P.char('{').bind ->
  (optSep.bind -> yStatement).manyTill optSep.bind -> P.char('}')

semiOrBlock = (P.char(';').bind -> P.unit []).orElse stmtBlock

parseYang = (text, top=null) ->
  yst = yStatement.between(optSep, optSep).parse text
  if top? and yst.kw != top
    throw P.error "Wrong top-level statement", 0
  yst

parseModule = (fname, top=null) ->
  text = fs.readFileSync fname, "utf8"
  parseYang text, "module"

yam = '''
container bar {
  description
    "This module contains a collection of YANG definitions for the
     retrieval of information from a NETCONF server.

     Copyright (c) 2013 IETF Trust and the persons identified as
     authors of the code.  All rights reserved.";

  leaf foo { // line comment
    type uint8;
    default 42;
  }
}
'''

try
  console.log parseYang yam
  #console.log parseModule "/Users/lhotka/Projects/yang/interfaces/ietf-interfaces.yang"
  #console.log parseModule "/Users/lhotka/sandbox/YANG/minimal.yang"
catch e
  if e.name is "ParsingError"
    console.log "Parsing failed at", e.offset, "(line", e.coords[0], "column", e.coords[1] + ")"
  else
    throw e
