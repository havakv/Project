% -------------------------------------------------------------------
%  @LaTeX-class-file{
%     filename        = "project-class.cls",
%     version         = "0",
%     date            = "23 May 2015",
%     codetable       = "ISO/ASCII",
%     docstring       = "A thesis class for project at NTNU."
% --------------------------------------------------------------------

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{myclass}[2014/05/23 My custom project class]
\LoadClass[11pt, a4paper]{book}

\RequirePackage[utf8]{inputenc} % Norwegian letters
\RequirePackage{amsmath,amsfonts,amssymb}
\RequirePackage{natbib} % Need to use bibiliography other than plain.
\RequirePackage{bm}
\RequirePackage{float}  % Used for minipage and stuff.
\RequirePackage{wrapfig} % Wrap text around figure wrapfig [tab]
\RequirePackage{afterpage}
\RequirePackage{graphicx}
\RequirePackage{subcaption}
\RequirePackage[usenames,dvipsnames,svgnames,table]{xcolor}
\RequirePackage{enumerate} % Use e.g. \begin{enumerate}[a)]
\RequirePackage[font={small, it}]{caption} % captions on figures and tables
\RequirePackage[toc]{appendix} % Page make Appendices title, toc fix table of content 
\RequirePackage{todonotes} % Notes. Use \todo{"text"}. Comment out \listoftodos
\RequirePackage{microtype} % Improves spacing. Include AFTER fonts
\RequirePackage[backref=page]{hyperref} % Use \autoref{} and \nameref{}
\hypersetup{backref,
  colorlinks=true,
  breaklinks=true,
  %hidelinks, %uncomment to make links black
  linkcolor=blue,
  urlcolor=SeaGreen,
  citecolor=blue,
  %linktoc=all,
  linktocpage=true
}
\RequirePackage[all]{hypcap} % Makes hyperref jup to top of pictures and tables

%---------------------------------------------------------------------------------------------
% Set page reference in bibliography.
\renewcommand*{\backref}[1]{}
\renewcommand*{\backrefalt}[4]{%
  \ifcase #1 (Not cited.)%
  \or        (Cited on page~#2.)%
  \else      (Cited on pages~#2.)%
\fi}
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Page layout
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%\RequirePackage{showframe} % Uncomment if you want the margin frames
\RequirePackage{fullpage}
\setlength{\marginparwidth}{2cm} % Needed when using todos and book toughether.
%\topmargin=-0.25in
%\evensidemargin=-0.3in
%\oddsidemargin=-0.3in
%\textwidth=6.9in
%\textheight=9.5in
\headsep=0.25in
\footskip=0.50in

%-------------------------------------------------------------------------------
% Header and footer
\RequirePackage{lastpage} % To be able to add last page in footer.
\RequirePackage{fancyhdr} % Custom headers and footers
%\pagestyle{fancy} % Use "fancyplain" for header in all pages
\pagestyle{fancyplain}
\renewcommand{\chaptermark}[1]{ \markboth{\thechapter.\ \ #1}{} } % Usefull for book?
\renewcommand{\sectionmark}[1]{ \markright{\thesection\ #1}{} } % Remove formating and nr.
\fancyhead[LE]{\thepage \quad \quad \nouppercase{\leftmark}}
\fancyhead[RO]{\nouppercase{\rightmark} \quad \quad \thepage}
%\fancyhead[LE]{\thepage \quad \quad \MakeUppercase{\leftmark}}
%\fancyhead[RO]{\rightmark \quad \quad \thepage}
\fancyhead[LO]{}
\fancyhead[RE]{}

\fancyfoot{}
%\fancyfoot[L]{} % Empty left footer
%\fancyfoot[C]{} % Empty center footer
%\fancyfoot[R]{Page\ \thepage\ of\ \protect\pageref*{LastPage}} % Page numbering for right footer
%\renewcommand{\headrulewidth}{0.5pt} % header underlines
\renewcommand{\headrulewidth}{0pt} % header underlines
%\renewcommand{\footrulewidth}{0.5pt} % footer underlines
\setlength{\headheight}{13.6pt} % Customize the height of the header

% Remove page numbering from chapter pages
\makeatletter
\let\ps@plain\ps@empty
\makeatother

%-------------------------------------------------------------------------------
% Suppose to make it easier for LaTeX to place figures and tables where I want.
\setlength{\abovecaptionskip}{0pt plus 1pt minus 2pt} % Makes caption come closer to figure.
%\setcounter{totalnumber}{5}
%\renewcommand{\textfraction}{0.05}
%\renewcommand{\topfraction}{0.95}
%\renewcommand{\bottomfraction}{0.95}
%\renewcommand{\floatpagefraction}{0.35}
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Titlepage
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand{\horrule}[1]{\rule{\linewidth}{#1}} % Create horizontal rule command with 1 argument of height



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstract
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newenvironment{abstract}
{
  \thispagestyle{empty}
  \null\null\null\null
  \null\null\null\null
  \begin{center}
    \setlength{\parskip}{0pt}
    {\underline{ABSTRACT} \par}
  \end{center}
  \vspace{-1cm}
  \list{}{
    \setlength{\leftmargin}{1.5cm}%
    \setlength{\rightmargin}{\leftmargin}%
  }%
  \item\relax
  {\endlist}
}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Math short cuts for expectation, variance and covariance
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand{\E}{\mathrm{E}}
\newcommand{\Var}{\mathrm{Var}}
\newcommand{\Cov}{\mathrm{Cov}}
% Commands for argmin and argmax
\DeclareMathOperator*{\argmin}{arg\,min}
\DeclareMathOperator*{\argmax}{arg\,max}

\newcommand*{\medcap}{\mathbin{\scalebox{1.5}{\ensuremath{\cap}}}}% And symbol, U (uppside down), for statistics



