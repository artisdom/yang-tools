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
    P.unit prf).option('').bind (pon) ->
      yIdentifier.bind (kw) ->
        P.unit [pon, kw]

uArg =
  (P.noneOf(" '\"\n\t\r;{}/").orElse \
    P.char('/').notFollowedBy(P.oneOf '/*')).many(1).bind (str) ->
      P.unit str.join ''

sqArg =
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
    sptab = '        '
    i = 0
    while lim > 0
      c = str[i++]
      if c == ' '
        lim -= 1
      else if c == '\t'
        return sptab[...8-lim] + str[i..] if lim < 8
        lim -= 8
      else
        return str[(i-1)..]
    str[i..]
  dqChar.manyTill(P.char '"').bind (cs) ->
    res = []
    lines = cs.join('').split('\n')
    for ln in lines[..-2]
      mo = trimLead(ln).match /(.*\S)\s*/
      res.push mo[1]
    res.push trimLead lines.pop() 
    P.unit res.join('\n')

dqArg =
  P.char('"').bind -> P.coordinates.bind (col) ->
    dqString col[1]

yArgument = P.choice uArg, dqArg, sqArg

semiOrBlock = P.char(';').bind -> P.unit []

yStatement = yKeyword.bind (kw) ->
  sep.bind -> yArgument.bind (arg) ->
    optSep.bind -> semiOrBlock.bind (sst) ->
      P.unit new YangStatement kw[0], kw[1], arg, sst

p = P.skipSpace.bind -> yArgument

console.log yStatement.parse '''
description "Agoj troubo!";
'''
 
