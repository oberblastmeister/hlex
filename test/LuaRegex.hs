module LuaRegex where

import Hlex.Regex qualified as R

rWhitespace = R.alt [R.set [' ', '\t', '\n'], "\r\n"]

varStartSet = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_']

rVarStart = R.set varStartSet

rVarContinue = R.set $ varStartSet ++ ['0' .. '9']

rHexDigit = R.set $ ['0' .. '9'] ++ ['a' .. 'f'] ++ ['A' .. 'F']

rNumber = R.cat [R.some R.digit, R.optional rFloatSuffix]

rFloatSuffix =
  R.cat
    [ R.optional ".",
      R.some R.digit,
      R.optional rScientificSuffix
    ]

rScientificSuffix =
  R.cat
    [ R.set ['e', 'E'],
      R.optional $ R.set ['+', '-'],
      R.some R.digit
    ]
