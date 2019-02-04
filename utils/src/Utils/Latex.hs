{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.Latex where

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import           System.Process (CreateProcess(..), readCreateProcess, CmdSpec(..), StdStream(..))
import           System.Directory (createDirectory, removePathForcibly)
import           Control.Exception (catch, SomeException)

import Paths_utils

class Latexable a where
    latex :: a -> Text

instance Latexable Text where
    latex s = s

instance Latexable a => Latexable [a] where
    latex = T.unlines . map latex

latexPreview :: Latexable a => a -> IO ()
latexPreview x = do
    removePathForcibly "/tmp/ccg_latex"
    createDirectory "/tmp/ccg_latex"
    latexToFile x "/tmp/ccg_latex/stuff.tex"
    _ <- readCreateProcess (CreateProcess
        { cmdspec            = ShellCommand
            "bash -c '( latexmk -pdf -pdflatex=\"xelatex\" -halt-on-error &> log.txt ) \
            \ && ( evince stuff.pdf &>/dev/null & )'"
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

latexSVG :: Latexable a => a -> IO Text
latexSVG x = do
    removePathForcibly "/tmp/ccg_latex"
    createDirectory "/tmp/ccg_latex"
    latexToFile x "/tmp/ccg_latex/stuff.tex"

    content <- catch (
        do
            _ <- readCreateProcess (CreateProcess
                { cmdspec            = ShellCommand
                    "bash -c '( latexmk -pdf -pdflatex=\"xelatex\" -halt-on-error \
                        \ && pdf2svg stuff.pdf out.svg ) &> log.txt'"
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
            B.readFile "/tmp/ccg_latex/out.svg"
                ) (\(_ :: SomeException) ->
        do
            filename <- getDataFileName "data/latex_error.svg"
            B.readFile filename
                )

    pure $ T.decodeUtf8 $ B64.encode content

latexToFile :: Latexable a => a -> FilePath -> IO ()
latexToFile x f = T.writeFile f $ wrap $ latex x
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
    \ \\usepackage{minibox} \n \
    \ \\usepackage{adjustbox} \n \
    \ \\usepackage{listings} \n \
    \ \\usepackage{syntax} \n \
    \ \\usepackage{tikz-qtree} \n \
    \ \\usepackage{tikz-qtree-compat} \n \
    \ \\renewcommand{\\baselinestretch}{1.1} \n \
    \ \\setlength{\\emergencystretch}{3em} \n \
    \ \\setlength{\\parskip}{5pt} \n \
    \ \\setlength{\\parindent}{0pt} \n \
    \ \\lstset{breaklines=true} \n \
    \  \n \
    \ \\usepackage{color} \n \
    \ \\definecolor{Bluish}{rgb}{0.39,0.55,0.78} \n \
    \ \\definecolor{light-gray}{gray}{0.9} \n \
    \  \n \
    \ \\begin{document} \n \
    \ \n" <> body <> "  \n \
    \ \\end{document}  \n"
