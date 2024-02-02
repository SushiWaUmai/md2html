{-# LANGUAGE ViewPatterns #-}
module Main where
import System.Directory (createDirectoryIfMissing)
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as L

type HtmlTag = String

data Structure = Header !Int !String | Code !String !String | Ul ![String] | Ol ![String] | Paragraph !String

main :: IO ()
main = do
  contents <- readFile "./README.md"
  putStrLn $ convert contents
  createDirectoryIfMissing False "./build"
  simpleHttp "https://cdn.tailwindcss.com?plugins=typography" >>= L.writeFile "./build/tailwind.js"
  writeFile "./build/index.html" $ convert contents

hasPrefix :: String -> String -> Bool
hasPrefix [] _ = True
hasPrefix _ [] = False
hasPrefix (x:xs) (y:ys) = (x == y) && hasPrefix xs ys

stripPrefix :: String -> String -> Maybe String
stripPrefix [] x = Just x
stripPrefix _ [] = Nothing
stripPrefix (x:xs) (y:ys) = if x == y then stripPrefix xs ys else Nothing

stripPrefixForce :: String -> String -> String
stripPrefixForce [] x = x
stripPrefixForce _ [] = []
stripPrefixForce (x:xs) (y:ys) = if x == y then stripPrefixForce xs ys else ys

trimWhitespace :: String -> String
trimWhitespace = trimPostfix . trimPrefix
  where
    trimPrefix :: String -> String
    trimPrefix (' ':xs) = trimPrefix xs
    trimPrefix x = x

    trimPostfix :: String -> String
    trimPostfix x = reverse $ trimPrefix $ reverse x

intToChar :: Int -> Char
intToChar x
  | x < 10 = toEnum (x + fromEnum '0')
  | otherwise = '0'

convert :: String -> String
convert = toHtml . parseMd

parseMd :: String -> [Structure]
parseMd mdString = parseTag (map trimWhitespace (lines mdString))

parseTag :: [String] -> [Structure]
parseTag [] = []
parseTag ((stripPrefix "######" -> Just content):xs) = Header 6 (trimWhitespace content) : parseTag xs
parseTag ((stripPrefix "#####" -> Just content):xs) = Header 5 (trimWhitespace content) : parseTag xs
parseTag ((stripPrefix "####" -> Just content):xs) = Header 4 (trimWhitespace content) : parseTag xs
parseTag ((stripPrefix "###" -> Just content):xs) = Header 3 (trimWhitespace content) : parseTag xs
parseTag ((stripPrefix "##" -> Just content):xs) = Header 2 (trimWhitespace content) : parseTag xs
parseTag ((stripPrefix "#" -> Just content):xs) = Header 1 (trimWhitespace content) : parseTag xs
parseTag ((stripPrefix "-" -> Just content):xs) = Ul contents : parseTag rest
  where
    contents = map trimWhitespace $ content:map (stripPrefixForce "-") (takeWhile (hasPrefix "-") xs)
    rest = drop ((length contents) - 1) xs
parseTag ((stripPrefix "```" -> Just lang):xs) = Code lang content : parseTag rest
  where
    contentData = map trimWhitespace $ takeWhile (not . hasPrefix "```") xs
    rest = drop ((length contentData) + 1) xs
    content = foldr (\x y -> x <> "\n" <> y) "" contentData
parseTag (x:xs) = Paragraph x : parseTag xs


toHtml :: [Structure] -> String
toHtml x = wrapHtml $ htmlHead <> htmlBody
  where
    htmlHead = wrapHead $ wrapAttrTag "script" [("src", "/tailwind.js")] ""
    htmlBody = wrapBody $ foldr ((<>) . matchTag) "" x

matchTag :: Structure -> String
matchTag (Header i content) = wrapTag ("h" <> [intToChar i]) content
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
