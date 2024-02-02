{-# LANGUAGE ViewPatterns #-}
module Main where
import System.Directory

type HtmlTag = String

data MarkdownStructure = MdHeader Int | MdCode | MdUl | MdString String
data HtmlStructure = HtmlHeader Int String | HtmlCode String | HtmlUl [String] | HtmlOl [String] | HtmlParagraph String

main :: IO ()
main = do
  contents <- readFile "./README.md"
  putStrLn $ convert contents
  createDirectory "./build"
  writeFile "./build/index.html" $ convert contents

isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = if x == y then isPrefix xs ys else False

stripPrefix :: String -> String -> Maybe String
stripPrefix [] x = Just x
stripPrefix _ [] = Nothing
stripPrefix (x:xs) (y:ys) = if x == y then stripPrefix xs ys else Nothing

intToChar :: Int -> Char
intToChar x
  | x < 10 = toEnum (x + fromEnum '0')
  | otherwise = '0'

convert :: String -> String
convert = toHtml . convertTag . parseMd

parseMd :: String -> [MarkdownStructure]
parseMd mdString = foldr (<>) [] (map parseTag (lines mdString))

parseTag :: String -> [MarkdownStructure]
parseTag (stripPrefix "# " -> Just content) =    [MdHeader 1, MdString content]
parseTag (stripPrefix "## " -> Just content) =   [MdHeader 2, MdString content]
parseTag (stripPrefix "### " -> Just content) =  [MdHeader 3, MdString content]
parseTag (stripPrefix "#### " -> Just content) = [MdHeader 4, MdString content]
parseTag (stripPrefix "- " -> Just content) = [MdUl, MdString content]
parseTag (stripPrefix "```" -> Just _) = [MdCode]
parseTag content = [MdString content]

convertTag :: [MarkdownStructure] -> [HtmlStructure]
convertTag [] = []
convertTag ((MdHeader i):(MdString content):xs) = HtmlHeader i content : convertTag xs
convertTag ((MdUl):(MdString content):xs) = HtmlUl [content] : convertTag xs
convertTag ((MdCode):xs) = HtmlCode content : convertTag rest
  where
    contentData = takeWhile (not . isMdCode) xs
    content = foldr (\x y -> x <> "\n" <> y) "" (map getContent contentData)
    rest = drop ((length contentData) + 1) xs

    isMdCode :: MarkdownStructure -> Bool
    isMdCode MdCode = True
    isMdCode _ = False

    getContent :: MarkdownStructure -> String
    getContent (MdString x) = x
    getContent _ = ""

convertTag (MdString content:xs) = HtmlParagraph content : convertTag xs

toHtml :: [HtmlStructure] -> String
toHtml x = wrapHtml $ htmlHead <> htmlBody
  where
    htmlHead = wrapHead $ wrapAttrTag "script" [("src", "https://cdn.tailwindcss.com?plugins=typography")] ""
    htmlBody = wrapBody $ foldr (<>) "" (map matchTag x)

matchTag :: HtmlStructure -> String
matchTag (HtmlHeader i content) = wrapTag ("h" <> [intToChar i]) content
matchTag (HtmlUl content) = wrapTag "ul" (foldr (<>) "" (map (wrapTag "li") content))
matchTag (HtmlOl content) = wrapTag "ol" (foldr (<>) "" (map (wrapTag "li") content))
matchTag (HtmlCode content) = wrapTag "pre" $ wrapTag "code" content
matchTag (HtmlParagraph content) = if content /= "" then wrapTag "p" content else ""

wrapAttrTag :: HtmlTag -> [(String, String)] -> String  -> String
wrapAttrTag tag attr content  = "<" <> tag <> attrString <> ">" <> content <> "</" <> tag <> ">"
  where
    attrString = foldr (\x y -> " " <> x <> y) "" (map (\(x,y) -> x <> "=" <> "\"" <> y <> "\"") attr)


wrapTag :: HtmlTag -> String -> String
wrapTag tag content = wrapAttrTag tag [] content

wrapHtml :: String -> String
wrapHtml content = wrapTag "html" content

wrapBody :: String -> String
wrapBody content = wrapAttrTag "body" [("class", "prose mx-auto p-4")] content

wrapHead :: String -> String
wrapHead content = wrapTag "head" content
