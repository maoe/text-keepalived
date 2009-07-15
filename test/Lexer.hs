import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Checkers
import Text.Keepalived.Lexer

import Text.Parsec hiding (token)
import Text.Parsec.Pos

instance Arbitrary TokenType where
  arbitrary = oneof [ Identifier <$> elements identifiers
                    , BlockId    <$> elements blockIdentifiers
                    , pure OpenBrace
                    , pure CloseBrace
                    , Quoted <$> arbitrary
                    , Value <$> arbitrary
                    ]

main :: IO ()
main = quickBatch batch

batch :: TestBatch
batch = ("lexer tests", [ ("identifier", monadicIO prop_tIdentifier)
                        , ("blockId",    monadicIO prop_tBlockId)
                        , ("openBrace",  monadicIO prop_tOpenBrace)
                        , ("closeBrace", monadicIO prop_tCloseBrace)
                        , ("value",      monadicIO prop_tValue)
                        , ("quoted",     monadicIO prop_tQuoted)
                        ])

prop_tIdentifier :: PropertyM IO Bool
prop_tIdentifier =
  forAllM (elements identifiers)
          (\ident -> run $ do
             parsed <- runParserT token () "" ident
             case parsed of
               Right (_, Identifier _) -> return True
               Right _                 -> return False
               Left err                -> return False)

prop_tBlockId :: PropertyM IO Bool
prop_tBlockId =
  forAllM (elements blockIdentifiers)
          (\ident -> run $ do
             parsed <- runParserT token () "" ident
             case parsed of
               Right (_, BlockId _) -> return True
               Right _              -> return False
               Left err             -> return False)

prop_tOpenBrace :: PropertyM IO Bool
prop_tOpenBrace =
  forAllM (pure "{")
          (\brace -> run $ do
             parsed <- runParserT token () "" brace
             case parsed of
               Right (_, OpenBrace) -> return True
               Right _              -> return False
               Left err             -> return False)

prop_tCloseBrace :: PropertyM IO Bool
prop_tCloseBrace =
  forAllM (pure "}")
          (\brace -> run $ do
             parsed <- runParserT token () "" brace
             case parsed of
               Right (_, CloseBrace) -> return True
               Right _               -> return False
               Left err              -> return False)

prop_tValue :: PropertyM IO Bool
prop_tValue = forAllM (arbitrary :: Gen String)
                      (\value -> run $ do
                         parsed <- runParserT token () "" value
                         case parsed of
                           Right (_, Value _) -> return True
                           Right _            -> return False
                           Left _             -> return False)

prop_tQuoted :: PropertyM IO Bool
prop_tQuoted = forAllM (do { str <- (arbitrary :: Gen String)
                           ; return $ "\"" ++ str ++ "\"" })
                     (\quoted -> run $ do
                        parsed <- runParserT token () "" quoted
                        case parsed of
                          Right (_, Quoted q) -> return True
                          Right _             -> return False
                          Left _              -> return False)
