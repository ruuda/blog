module Site.Pandoc (pandocCompiler) where

import Data.Set (insert, delete)
import Hakyll hiding (pandocCompiler, pandocCompilerWith)
import Text.Pandoc.Options

pandocReaderOptions :: ReaderOptions
pandocReaderOptions = def
  {
    readerSmart      = True,
    -- Yes, HTML5 figures are fancy, but I prefer a simple p > img.
    -- I also need fenced code blocks.
    readerExtensions = delete Ext_implicit_figures     $
                       insert Ext_backtick_code_blocks $
                       readerExtensions def
  }

pandocWriterOptions :: WriterOptions
pandocWriterOptions = def
  {
    writerHtml5          = True,
    writerHighlight      = True,
    writerHTMLMathMethod = MathML Nothing
  }

-- Pandoc cannot handle CRLF on systems that use LF line endings, so we
-- remove the CR before feeding things into Pandoc.
removeCr :: String -> String
removeCr = filter (/= '\r')

getBodyWithoutCr :: Compiler (Item String)
getBodyWithoutCr = fmap (fmap removeCr) getResourceBody

pandocCompilerWith :: ReaderOptions -> WriterOptions -> Compiler (Item String)
pandocCompilerWith ropt wopt =
  cached "Hakyll.Web.Pandoc.pandocCompilerWith" $
    writePandocWith wopt <$> (readPandocWith ropt =<< getBodyWithoutCr)

pandocCompiler :: Compiler (Item String)
pandocCompiler = pandocCompilerWith pandocReaderOptions pandocWriterOptions
