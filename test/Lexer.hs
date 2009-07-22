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
          (\i -> run $ do
             parsed <- runParserT token () "" i
             case parsed of
               Right (_, Identifier i') -> return (i == i')
               _                        -> return False)

prop_tBlockId :: PropertyM IO Bool
prop_tBlockId =
  forAllM (elements blockIdentifiers)
          (\i -> run $ do
             parsed <- runParserT token () "" i
             case parsed of
               Right (_, BlockId i') -> return (i == i')
               _                     -> return False)

prop_tOpenBrace :: PropertyM IO Bool
prop_tOpenBrace =
  forAllM (pure "{")
          (\b -> run $ do
             parsed <- runParserT token () "" b
             case parsed of
               Right (_, OpenBrace) -> return True
               _                    -> return False)

prop_tCloseBrace :: PropertyM IO Bool
prop_tCloseBrace =
  forAllM (pure "}")
          (\b -> run $ do
             parsed <- runParserT token () "" b
             case parsed of
               Right (_, CloseBrace) -> return True
               _                     -> return False)

prop_tValue :: PropertyM IO Bool
prop_tValue = forAllM (arbitrary :: Gen String)
                      (\v -> run $ do
                         parsed <- runParserT token () "" v
                         case parsed of
                           Right (_, Value v') -> return (v == v')
                           _                   -> return False)

prop_tQuoted :: PropertyM IO Bool
prop_tQuoted = forAllM (do { str <- (arbitrary :: Gen String)
                           ; return $ "\"" ++ str ++ "\"" })
                     (\q -> run $ do
                        parsed <- runParserT token () "" q
                        case parsed of
                          Right (_, Quoted q') -> return (q == q')
                          _                    -> return False)
