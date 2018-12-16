{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Latex where

import           Data.Text (Text)
import qualified Data.Text as T
import           System.Process (CreateProcess(..), readCreateProcess, CmdSpec(..), StdStream(..))
import           System.Directory (createDirectory, removePathForcibly)

class Latexable a where
    latex :: a -> Text

instance Latexable String where
    latex s = T.pack s

instance Latexable Text where
    latex s = s

instance {-# OVERLAPS #-} Latexable a => Latexable [a] where
    latex = T.unlines . (map latex)

latexPreview :: Latexable a => a -> IO ()
latexPreview x = do
    removePathForcibly "/tmp/ccg_latex"
    createDirectory "/tmp/ccg_latex"
    latexToFile x "/tmp/ccg_latex/stuff.tex"
    _ <- readCreateProcess (CreateProcess
        { cmdspec            = ShellCommand
            "( latexmk -pdf -pdflatex=\"xelatex\" -halt-on-error &> log.txt ) \
            \ && ( evince stuff.pdf &>/dev/null & )"
        , cwd                = Just "/tmp/ccg_latex"
        , env                = Nothing
        , std_in             = Inherit
        , std_out            = Inherit
        , std_err            = Inherit
        , close_fds          = False
        , create_group       = False
        , detach_console     = False
        , create_new_console = False
        , delegate_ctlc      = False
        , new_session        = False
        , child_group        = Nothing
        , child_user         = Nothing
        , use_process_jobs   = False
        }) ""
    pure ()

latexToFile :: Latexable a => a -> FilePath -> IO ()
latexToFile x f = writeFile f $ T.unpack $ wrap $ latex x
    -- fixme - use packed data

wrap :: Text -> Text
wrap body = " \n \
    \ % !TEX program = xelatex \n \
    \ \\documentclass[12pt, varwidth]{standalone} \n \
    \ \\usepackage[T1]{fontenc} \n \
    \ \\usepackage{fontspec} \n \
    \ \\usepackage{libertine} \n \
    \  \n \
    \ \\defaultfontfeatures{Ligatures=TeX} \n \
    \  \n \
    \ \\usepackage[bulgarian,english]{babel} \n \
    \ \\usepackage{indentfirst} \n \
    \ \\usepackage[a4paper, portrait, margin = 2.5 cm]{geometry} \n \
    \ \\usepackage{url} \n \
    \ \\usepackage{color} \n \
    \ \\usepackage{xcolor} \n \
    \ \\usepackage{amsthm} \n \
    \ \\usepackage{amssymb} \n \
    \ \\usepackage{amsmath} \n \
    \ \\usepackage{graphicx} \n \
    \ \\usepackage{adjustbox} \n \
    \ \\usepackage{listings} \n \
    \ \\usepackage{syntax} \n \
    \ \\usepackage{tikz-qtree} \n \
    \ \\usepackage{tikz-qtree-compat} \n \
    \ \\renewcommand{\\baselinestretch}{1.1} \n \
    \ \\setlength{\\emergencystretch}{3em} \n \
    \ \\setlength{\\parskip}{5pt} \n \
    \ \\setlength{\\parindent}{0pt} \n \
    \  \n \
    \ \\usepackage{color} \n \
    \ \\definecolor{Bluish}{rgb}{0.39,0.55,0.78} \n \
    \ \\definecolor{light-gray}{gray}{0.9} \n \
    \  \n \
    \ \\begin{document} \n \
    \ \n" <> body <> "  \n \
    \ \\end{document}  \n"
