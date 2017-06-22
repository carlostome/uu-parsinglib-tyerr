\documentclass{article}
% necesssary header for lhs2TeX
%include custom.fmt
\usepackage{hyperref}
\renewcommand{\hscodestyle}{\footnotesize}
\usepackage{xargs}
\usepackage[pdftex,dvipsnames]{xcolor}
\usepackage[colorinlistoftodos,prependcaption,textsize=tiny]{todonotes}
\begin{document}
\newcommand{\sibling}{\textit{sibling}}

\author{Carlos Tom\'e Corti\~nas}
\title{Type error customization in uu-parsinglib\footnote{\url{https://hackage.haskell.org/package/uu-parsinglib}}}
\maketitle

\section{Introduction}

Hola que ase

\section{Text.ParserCombinators.UU.Derived}

%include src/Text/ParserCombinators/UU/TyErr/Derived.lhs

\section{Text.ParserCombinators.UU.Core}

%include src/Text/ParserCombinators/UU/TyErr/Core.lhs

\appendix

\section{GHC.TypeErrors.Utils}

%include src/GHC/TypeErrors/Utils.lhs

\section{GHC.TypeErrors.PP}

%include src/GHC/TypeErrors/PP.lhs

\end{document}
