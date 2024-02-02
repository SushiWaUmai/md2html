module Main where
import System.Directory (createDirectoryIfMissing)
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as L
import Data.List (dropWhileEnd, isPrefixOf)
import Data.Char (intToDigit, isSpace)

type HtmlTag = String

data Structure = Header !Int !String | Code !String !String | Ul ![String] | Ol ![String] | Paragraph !String deriving Show

main :: IO ()
main = do
  contents <- readFile "./README.md"
  putStrLn $ convert contents
  createDirectoryIfMissing False "./build"
  simpleHttp "https://cdn.tailwindcss.com?plugins=typography" >>= L.writeFile "./build/tailwind.js"
  writeFile "./build/index.html" $ convert contents

trimWhitespace :: String -> String
trimWhitespace = dropWhile isSpace . dropWhileEnd isSpace

convert :: String -> String
convert = toHtml . parseMd

parseMd :: String -> [Structure]
parseMd mdString = parseTag (map trimWhitespace (lines mdString))

parseTag :: [String] -> [Structure]
parseTag [] = []
parseTag (x:xs)
  | "#" `isPrefixOf` x = let (level, content) = span (=='#') x
                             i = length level
                      in Header i (trimWhitespace content) : parseTag xs
  | "-" `isPrefixOf` x = let (items, rest) = span (isPrefixOf "-") (x:xs)
                             contents = map (trimWhitespace . drop 1) items
                      in Ul contents : parseTag rest
  | "```" `isPrefixOf` x = let (items, rest) = break (isPrefixOf "```") xs
                               content = unlines $ map trimWhitespace items
                          in Code (trimWhitespace $ drop 3 x) content : parseTag (drop 1 rest)
  | otherwise = Paragraph x : parseTag xs


toHtml :: [Structure] -> String
toHtml x = wrapHtml $ htmlHead <> htmlBody
  where
    htmlHead = wrapHead $ wrapAttrTag "script" [("src", "/tailwind.js")] ""
    htmlBody = wrapBody $ foldr ((<>) . matchTag) "" x

matchTag :: Structure -> String
matchTag (Header i content) = wrapTag ("h" <> [intToDigit i]) content
matchTag (Ul content) = wrapTag "ul" (foldr ((<>) . wrapTag "li") "" content)
matchTag (Ol content) = wrapTag "ol" (foldr ((<>) . wrapTag "li") "" content)
matchTag (Code _ content) = wrapTag "pre" $ wrapTag "code" content
matchTag (Paragraph content) = if content /= "" then wrapTag "p" content else ""

wrapAttrTag :: HtmlTag -> [(String, String)] -> String  -> String
wrapAttrTag tag attr content  = "<" <> tag <> attrString <> ">" <> content <> "</" <> tag <> ">"
  where
    attrString = foldr ((\x y -> " " <> x <> y) . (\(x,y) -> x <> "=" <> "\"" <> y <> "\"")) "" attr


wrapTag :: HtmlTag -> String -> String
wrapTag tag = wrapAttrTag tag []

wrapHtml :: String -> String
wrapHtml content = "<!DOCTYPE html>" <> wrapTag "html" content

wrapBody :: String -> String
wrapBody = wrapAttrTag "body" [("class", "prose mx-auto p-4")]

wrapHead :: String -> String
wrapHead = wrapTag "head"
