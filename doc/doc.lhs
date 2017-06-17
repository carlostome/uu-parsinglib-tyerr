\documentclass{article}
%include custom.fmt
\usepackage[titletoc,title]{appendix}
\renewcommand{\hscodestyle}{\footnotesize}
\usepackage{xargs}                    
\usepackage[pdftex,dvipsnames]{xcolor}
\usepackage[colorinlistoftodos,prependcaption,textsize=tiny]{todonotes}
\newcommandx{\unsure}[2][1=]{\todo[linecolor=red,backgroundcolor=red!25,bordercolor=red,#1]{#2}}
\newcommandx{\change}[2][1=]{\todo[linecolor=blue,backgroundcolor=blue!25,bordercolor=blue,#1]{#2}}
\newcommandx{\info}[2][1=]{\todo[linecolor=OliveGreen,backgroundcolor=OliveGreen!25,bordercolor=OliveGreen,#1]{#2}}
\newcommandx{\improvement}[2][1=]{\todo[linecolor=Plum,backgroundcolor=Plum!25,bordercolor=Plum,#1]{#2}}
\newcommandx{\thiswillnotshow}[2][1=]{\todo[disable,#1]{#2}}

\begin{document}

\section{Text.ParserCombinators.UU.Derived}
%include src/Text/ParserCombinators/UU/TyErr/Derived.lhs

\appendix

\section{GHC.TypeErrors.Utils}

%include src/GHC/TypeErrors/Utils.lhs

\end{document}