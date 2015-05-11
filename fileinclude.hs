#!/usr/bin/env runhaskell

{-
The MIT License (MIT)

Copyright (c) 2015 Wasif Hasan Baig <pr.wasif@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-}

{-|
  A Pandoc filter that replaces Image Links having *.md extension with the
  parsed contents of the specified markdown file. In your markdown, include the
  markdown file as shown below.

  >  ![This section heading will be ignored](section-two.md)
-}

import Data.List        (isSuffixOf)
import Text.Pandoc
import Text.Pandoc.JSON (toJSONFilter)

main :: IO ()
main = toJSONFilter includeFileContent

includeFileContent :: [String] -> Block -> IO [Block]
includeFileContent exts (Para [(Image l (f, _))])
    | or $ map (`isSuffixOf` f) $ "md":exts
    = do
        contents <- readFile f
        return . pandocToBlocks . readMarkdown def $ contents
includeFileContent _ x = return [x]

-- | Extracts [Block] from Pandoc
pandocToBlocks :: Pandoc -> [Block]
pandocToBlocks (Pandoc _ bs) = bs
