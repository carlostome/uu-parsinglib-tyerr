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

In this document we present the implementation of a wrapper over the
uu-parsinglib library for giving domain specific error messages to the
user.

We have divided this document into several sections regarding the customization
of each module included in the library. Moreover, in the appendices we include
the modules that define some extra functionality for pretty printing (is not
specific to this library) and also some common error combinators for the parsing
library (domain specific).

\section{Text.ParserCombinators.UU.Core}
\label{sec:Core}

%include src/Text/ParserCombinators/UU/TyErr/Core.lhs

\section{Text.ParserCombinators.UU.Derived}

%include src/Text/ParserCombinators/UU/TyErr/Derived.lhs


\section{General remarks and conclusions}

\begin{itemize}
\item
The addition of new siblings to a customized error message is
not composable. It involves hardcoding in the correct place of the type
the conditions that must be met in order to hint the user with a proper
suggestion that when applied will make the expression well typed.

An example of this can be seen in the encoding of combinators for the
|Functor|, |Applicative|, |Alternative| and |ExtAlternative| as explained
in \ref{sec:Core}.

\item
Encoding requirements over type classes in the error messages is weak.
We cannot be sure that at a certain point in the error message for example
a type |p| is indeed an instance of the class |IsParser|. Therefore, we must
take additional measures to ensure that we rule out cases we know do not belong 
to the class. Moreover, this additional measurements poison the type of
the combinator that no longer can be reduced to a type similar to the original one.

\item
The impossibility to include some form of reified expression where the type
error is generated forbids the DSL writer to specify precisely in the type error
message the source of the error. For now, the only way to refer to it is
hardcode the name of the function involved and number the arguments (which
indeed leaves room open for a lot of misleading in the errors in case it is wrong).

\item
The type |ErrorMessage| provided by GHC does not work as smoothly as it should be.
For example, printing a type with |ShowType| with a complicated type at the
end of a sentence makes it unreadable. As an improvement to this mechanism
I would like to have access to all the pretty printing machinery implemented
in GHC through a type level API that can allow for much better formatting of
the error messages.
\end{itemize}

\appendix

\section{GHC.TypeErrors.Utils}

%include src/GHC/TypeErrors/Utils.lhs

\section{GHC.TypeErrors.PP}

%include src/GHC/TypeErrors/PP.lhs

\end{document}
