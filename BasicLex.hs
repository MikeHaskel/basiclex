-- Copyright 2013 Michael Haskel
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
-- http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- |Basic, generic, somewhat inflexible lexer intended for user with new
-- languages.
-- 
-- The main feature of the lexer is that it is fully Unicode-based.  Based on
-- Unicode Standard Annex 31 "Alternative Identifier Syntax"
-- (<http://www.unicode.org/reports/tr31/#Alternative_Identifier_Syntax>), it
-- divides input characters into four classes:
-- 
--  * Whitespace characters.  These are ignored other than to separate
--  identifiers.
-- 
--  * Syntax characters.  Individual syntax characters form their own tokens.
-- 
--  * Invalid characters.  These are not allowed, except in comments.
-- 
--  * Everything else.  These characters combine arbitrarily to form
--  identifiers.
-- 
-- In order to encourage and support Unicode canonical equivalence, the lexer
-- provides identifiers wrapped in their own type ('Identifier'), whose 'Eq'
-- and 'Ord' instances support the preferred Unicode algorithms (see
-- 'Data.Text.ICU.compare').
-- 
-- Comments begin with a single character, end with a different character, and
-- may span multiple lines (in fact, the lexer does not distinguish between
-- line breaks and other white space).  The user of this module must specify
-- which characters these should be.  These characters cannot be used in any
-- way in the input other than in their capacity as comment delimiters.
-- Comments nest.
module Text.BasicLex (Identifier(..),
                      Token(..),
                      LexerState,
                      basicLex,
                      basicLexEasy) where
import Control.Monad
import Control.Monad.Error.Class
import qualified Data.Text as T
import qualified Data.Text.ICU as I
import qualified Data.Text.ICU.Char as I


-- |Unicode string which uses Unicode canonical equivalence and ordering
newtype Identifier = Identifier { getIdentifier :: T.Text }

instance Eq Identifier where
  x == y = compare x y == EQ

instance Ord Identifier where
  compare (Identifier x) (Identifier y) = I.compare [] x y

instance Show Identifier where
  showsPrec p (Identifier x) = showsPrec p x

instance Read Identifier where
  readsPrec p r = do (x, r) <- readsPrec p r
                     return (Identifier x, r)


-- |Lexical units
data Token
  -- |An individual character with syntactic meaning
  = SyntaxToken !Char
  -- |A sequence of characters used as an identifier
  | IdentifierToken !Identifier
  deriving (Eq, Ord, Show)


data LexicalCharacterClass
  = WhitespaceChar
  | SyntaxChar
  | InvalidChar
  | IdentifierChar
  deriving (Eq, Show)

classify :: Char -> LexicalCharacterClass
classify c
  | I.property I.PatternWhiteSpace c = WhitespaceChar
  | I.property I.PatternSyntax c = SyntaxChar
  | I.property I.GeneralCategory c == I.PrivateUseChar = InvalidChar
  | I.property I.GeneralCategory c == I.Surrogate = InvalidChar
  | I.property I.GeneralCategory c == I.ControlChar = InvalidChar
  | I.property I.NonCharacter c = InvalidChar
  | otherwise = IdentifierChar


-- |Complete interface to the lexer
-- 
-- Input is parameters for how comments look and instructions for what to do
-- with lexing results.  Output is instructions for what to do with characters
-- and an extraction function for grabbing the desired output when finished
-- processing.
basicLex ::
  Char -- ^Character that begins comments
  -> Char -- ^Character that ends comments
          -- 
          -- Cannot be the same as the start comment character.
  -> (String -> z) -- ^Error handler
  -> (Token -> z -> z) -- ^Token handler
  -> z -- ^End of output handler (no more tokens)
  -> (Char -> LexerState z -> LexerState z,
      LexerState z,
      LexerState z -> z) -- ^Output character-level handlers
                         -- 
                         -- (1) Input character handler
                         -- 
                         -- (1) End of input handler (no more characters)
                         -- 
                         -- (1) Convert internal lexer state to desired output.
                         -- Use this after processing the entire input.
                         -- 
                         -- This output should be suitable for a
                         -- 'Data.List.foldr'-like interface.
basicLex commentStart commentEnd handleError handleToken handleEndTokens =
  (handleChar, handleEndChars, extractUserState)
  where handleChar c tailState =
          LexerState { lexerStateInBody = goInBody,
                       lexerStateInIdentifier = goInIdentifier,
                       lexerStateInComment = goInComment }
          where goInBody =
                  case (c == commentStart, c == commentEnd, classify c) of
                    (True, _, _) -> lexerStateInComment tailState 0
                    (_, True, _) -> handleError $
                                    "basicLex: unmatched end of comment"
                    (_, _, WhitespaceChar) -> lexerStateInBody tailState
                    (_, _, SyntaxChar) -> handleToken (SyntaxToken c) $
                                          lexerStateInBody tailState
                    (_, _, InvalidChar) ->
                      handleError $
                      "basicLex: invalid character: " ++ show c
                    (_, _, IdentifierChar) ->
                      let (cs, tailUserState) =
                            lexerStateInIdentifier tailState
                      in handleToken (IdentifierToken $ Identifier $
                                      T.pack $ c:cs) $
                         tailUserState
                goInIdentifier =
                  case (c == commentStart, c == commentEnd, classify c) of
                    (True, _, _) -> ("", lexerStateInComment tailState 0)
                    (_, True, _) -> ("", handleError $
                                         "basicLex: unmatched end of comment")
                    (_, _, WhitespaceChar) -> ("", lexerStateInBody tailState)
                    (_, _, SyntaxChar) -> ("", handleToken (SyntaxToken c) $
                                               lexerStateInBody tailState)
                    (_, _, InvalidChar) ->
                      ("", handleError $
                           "basicLex: invalid character: " ++ show c)
                    (_, _, IdentifierChar) ->
                      let (cs, tailUserState) =
                            lexerStateInIdentifier tailState
                      in (c:cs, tailUserState)
                goInComment n =
                  case (c == commentStart, c == commentEnd) of
                    (True, _) -> lexerStateInComment tailState (n+1)
                    (_, True) -> if n <= 0
                                 then lexerStateInBody tailState
                                 else lexerStateInComment tailState (n-1)
                    _ -> lexerStateInComment tailState n
        handleEndChars =
          LexerState {
            lexerStateInBody = handleEndTokens,
            lexerStateInIdentifier = ("", handleEndTokens),
            lexerStateInComment = const $ handleError $
                                  "basicLex: end of input during comment"
            }
        extractUserState = lexerStateInBody

-- |Internal state for 'basicLex'
-- 
-- This type should be opaque to other functions.
data LexerState z = LexerState { lexerStateInBody :: z,
                                 lexerStateInIdentifier :: (String, z),
                                 lexerStateInComment :: Int -> z }

-- |A less flexible, easy-to-use version of 'basicLex'
-- 
-- The main limitations of this function are that it must process the entire
-- input at once in order to report errors up-front and that it may incur
-- some slight overhead if conversion to a 'String' is required (for example,
-- if input is a 'Data.Text.Text').
basicLexEasy :: (Error e, MonadError e m) =>
                Char -> Char -> String -> m [Token]
basicLexEasy commentStart commentEnd =
  let (handleChar, handleEndChars, extractUserState) =
        basicLex commentStart commentEnd
        handleError handleToken handleEndTokens
  in extractUserState . foldr handleChar handleEndChars
  where handleError = throwError . strMsg
        handleToken tok doToks = doToks >>= return . (tok:)
        handleEndTokens = return []
