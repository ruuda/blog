module Site.Pandoc (pandocCompiler) where

import Data.Set (insert, delete)
import Hakyll hiding (pandocCompiler)
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

pandocCompiler :: Compiler (Item String)
pandocCompiler = pandocCompilerWith pandocReaderOptions pandocWriterOptions
