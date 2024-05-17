% based on bioinfo.cls and ncc.cls
\newcommand\classname{jcsh}
\newcommand\lastmodifieddate{2023/08/13}
\newcommand\versionnumber{0.1}

\NeedsTeXFormat{LaTeX2e}
%\ProvidesClass{ncc}[19/06/2017, v1.0]
\ProvidesClass{jchs}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions*
\LoadClass{article}

%\RequirePackage[utf8]{inputenc}
\RequirePackage[english]{babel}

% for huxtable
%\RequirePackage{array}
%\RequirePackage{caption}%gives error abovescaptionskip already defined, posssibly loaded elsewhere
%\RequirePackage{supertabular}
\RequirePackage{graphicx}
\RequirePackage{siunitx}
\RequirePackage[normalem]{ulem}
%\RequirePackage{colortbl}
%\RequirePackage{multirow}
\RequirePackage{hhline}
\RequirePackage{calc}
%\RequirePackage{tabularx}
%\RequirePackage{threeparttable}
%\RequirePackage{wrapfig}
\RequirePackage{adjustbox}
\RequirePackage{cuted}



%\RequirePackage{longtable}

%\RequirePackage{hyperref}


%\RequirePackage{supertabular}
\RequirePackage{xltabular}


\RequirePackage{ifthen}
\RequirePackage{calc}
\AtEndOfClass{\RequirePackage{microtype}}



%%%%%%%%%% font stuff %%%%%%%%%%%%%
%\RequirePackage{opensans}
%\RequirePackage{tgpagella}

\RequirePackage{fontspec}

% load font from manual location to enbale embedded shipping
\setmainfont{texgyrepagella-regular.otf}[
Path = fonts/pagella/,
BoldFont = texgyrepagella-bold.otf,
ItalicFont = texgyrepagella-italic.otf,
BoldItalicFont  = texgyrepagella-bolditalic.otf]


\setsansfont{OpenSans-Regular.ttf}[
Path = fonts/opensans/,
BoldFont = OpenSans-Bold.ttf,
ItalicFont = OpenSans-Italic.ttf,
BoldItalicFont  = OpenSans-BoldItalic.ttf]



%\usepackage[T1]{fontenc}
%\RequirePackage[default]{opensans}
\RequirePackage{setspace}

\RequirePackage{ragged2e}
\RequirePackage{float}


\DeclareTextFontCommand{\fontAffils}{\fontsize{50}{54}\selectfont}

%\renewcommand{\normalsize}{\fontsize{10}{11}\selectfont}
%\renewcommand{\large}{\fontsize{11}{13}\selectfont}
%\renewcommand{\Large}{\fontsize{12}{14}\selectfont}
%\renewcommand{\footnotesize}{\fontsize{7}{8}\selectfont}
%\setstretch{1.1}


\RequirePackage[hang,flushmargin]{footmisc}

\RequirePackage{tcolorbox}




\RequirePackage{ifpdf}

\RequirePackage{amsmath,amsfonts,amssymb}
\RequirePackage{graphicx}
\RequirePackage[table]{xcolor}
%\RequirePackage{booktabs}
\usepackage{graphicx}


%\twocolumn
\RequirePackage[left=1.5cm,%
right=1.5cm,%
top=2.5cm,%
bottom=2.5cm,%
headheight=12pt,%
a4paper]{geometry}%

\RequirePackage{multicol}


\setlength{\columnsep}{12mm}%distance between columns

\RequirePackage[labelfont={bf,sf},%
labelsep=period,%
justification=raggedright]{caption}



% author/ affil setup
\RequirePackage{authblk}
\setcounter{Maxaffil}{0}% always footnote mode
\setlength{\affilsep}{6.5mm}%distance between authors and affiliations
\renewcommand\Authsep{, }% author separation string
\renewcommand\Authand{ and }% if two authors
\renewcommand\Authands{, }% before last author if more than two

\renewcommand\Authfont{\fontsize{12}{14}\selectfont}
\renewcommand\Affilfont{\fontsize{9}{12}\selectfont}


% https://tex.stackexchange.com/questions/496334/including-orcid-in-plain-latex-using-authblk
\newbox{\myorcidaffilbox}
\sbox{\myorcidaffilbox}{\large\smash{\includegraphics[height=14.5pt]{ORCIDiD_iconvector}}}
\newcommand{\orcidaffil}[1]{%
	\href{https://orcid.org/#1}{\hspace*{1pt}\usebox{\myorcidaffilbox}}}



% use - this instead: https://ctan.org/pkg/vancouver
% https://bookdown.org/yihui/bookdown/citations.html
% https://bookdown.org/yihui/rmarkdown-cookbook/bibliography.html

%
% writeLaTeX logo
%
\newcommand\wllogo{%
	\renewcommand*\rmdefault{ugq}\normalfont\upshape{}write%
	\renewcommand*\rmdefault{cmr}\normalfont\upshape{\bf\LaTeX}}

%
% headers and footers
%
\RequirePackage{lastpage}  % Number of pages in the document
\RequirePackage{fancyhdr}  % custom headers/footers
\pagestyle{fancy}
\fancyhf{}% Clear all headers/footers

%% Headers
%\lhead{}%
%\chead{}%
%\rhead{}%
% Footers
%\lfoot{}%
%\cfoot{}%
%\rfoot{\small\sffamily\bfseries\thepage/\pageref{LastPage}}%
\renewcommand{\headrulewidth}{0pt}% % No header rule
\renewcommand{\footrulewidth}{0pt}% % No footer rule

%
% section/subsection/paragraph set-up
%
\RequirePackage[explicit]{titlesec}
\titleformat{\section}
{\color{jchsheadercolor}\Large\bfseries}
% {\thesection} with numbers
{}
{1pt}
{\MakeUppercase{#1}}
[]
\titleformat{name=\section,numberless}
{\color{jchsheadercolor}\Large\bfseries}
{}
{0em}
{\MakeUppercase{#1}}
[]
\titleformat{\subsection}
{\color{jchsheadercolor}\bfseries}
{}
{1pt}
{#1}
[]
\titleformat{\subsubsection}
{\color{jchsheadercolor}\small\bfseries\itshape}
{}
{1pt}
{#1}
[]
\titleformat{\paragraph}[runin]
{\sffamily\small\bfseries}
{}
{0em}
{#1}
\titlespacing*{\section}{0pc}{3ex \@plus4pt \@minus3pt}{0pt}
\titlespacing*{\subsection}{0pc}{2.5ex \@plus3pt \@minus2pt}{0pt}
\titlespacing*{\subsubsection}{0pc}{2ex \@plus2.5pt \@minus1.5pt}{0pt}
\titlespacing*{\paragraph}{0pc}{1.5ex \@plus2pt \@minus1pt}{10pt}
\RequirePackage{indentfirst}
%
% tableofcontents set-up
%
\usepackage{titletoc}
\contentsmargin{0cm}
\titlecontents{section}[\tocsep]
{\addvspace{4pt}\small\bfseries\sffamily}
{\contentslabel[\thecontentslabel]{\tocsep}}
{}
{\hfill\thecontentspage}
[]
\titlecontents{subsection}[\tocsep]
{\addvspace{2pt}\small\sffamily}
{\contentslabel[\thecontentslabel]{\tocsep}}
{}
{\ \titlerule*[.5pc]{.}\ \thecontentspage}
[]
\titlecontents*{subsubsection}[\tocsep]
{\footnotesize\sffamily}
{}
{}
{}
[\ \textbullet\ ]

% no footnote line seperator
\renewcommand*\footnoterule{}

\RequirePackage{enumitem}

\RequirePackage{xurl}

\PassOptionsToPackage{hyphens}{url}
\RequirePackage[colorlinks=true,luatex,breaklinks=true]{hyperref}
\hypersetup{breaklinks=true}
\urlstyle{same}% do not use monospace for links


%\setlist{nolistsep} % Uncomment to remove spacing between items in lists (enumerate, itemize)


% for  author
\newcommand{\corrauthor}[3][]{%
	\def\thecorrauthor{%
		{\fontsize{9}{12}\selectfont{\bfseries $^{#1}$} #2 \mbox{#3}\par}
	}
}

% dates
%\newcommand{\articledates}[3]{
	%	\def\thearticledates{
		%		{\fontsize{9}{12}\selectfont#1 #2 #3\par}
		%	}
	%}
% version with one  argument
\newcommand{\articledates}[1]{\def\thearticledates{
		{\fontsize{9}{12}\selectfont#1\par}
}}
\newcommand{\journalvolume}[1]{\def\thejournalvolume{#1}}
\newcommand{\journaltitle}[1]{\def\thejournaltitle{#1}}
\newcommand{\journaldoi}[1]{\def\thejournaldoi{#1}}

%
% article meta data
%
\newcommand{\keywords}[1]{\def\@keywords{#1}}



\def\xabstract{abstract}
\long\def\abstract#1\end#2{\def\two{#2}\ifx\two\xabstract
	\long\gdef\theabstract{\ignorespaces#1}
	\def\go{\end{abstract}}\else
\typeout{^^J^^J PLEASE DO NOT USE ANY \string\begin\space \string\end^^J
	COMMANDS WITHIN ABSTRACT^^J^^J}#1\end{#2}
	\gdef\theabstract{\vskip12pt BADLY FORMED ABSTRACT: PLEASE DO
NOT USE {\tt\string\begin...\string\end} COMMANDS WITHIN
THE ABSTRACT\vskip12pt}\let\go\relax\fi
\go}



%
% custom title page
%
\renewcommand{\@maketitle}{
{
\raggedright%left align
%osfamily % use open sans
%\fontspec{Open Sans}
\sffamily
\begin{minipage}{0.13\textwidth}
	{\includegraphics[width = 20mm]{jchs.pdf}}
\end{minipage}
\begin{minipage}[]{0.49\textwidth}
	{\footnotesize
		\thejournaltitle\\
		\thejournalvolume\\
		\thejournaldoi}
\end{minipage}
\begin{minipage}[t][][t]{0.37\textwidth}% align both the minipage itself and its contents on top
	\textls[-14]{%
		\begin{flushright}%
			\vspace{-13mm} % somehow vspace etc insert a linebreak in the minipage environment, so we need to work with a minus offset\\
			\colorlet{varlinkcolor}{blue}%
			\begin{tcolorbox}[colframe=jchsorange, colback=white, halign=center,fontupper=\fontsize{10}{12}\selectfont, boxsep=0pt,width=63mm, left=0mm, right=0mm]
				We are a \href{https://www.fairopenaccess.org/}{\underline{\smash{Fair Open Access}}} journal
			\end{tcolorbox}%
			\colorlet{varlinkcolor}{black}%
	\end{flushright}}%
\end{minipage}

\vskip5.2mm


{% we do not want a intent for the title, and we want it centered
	\setstretch{1.18}\setlength{\JustifyingParindent}{0pt}%
	\justifying\bfseries\fontsize{16}{18}\selectfont\color{jchsheadercolor}{\textls[20]{\@title}}\par%
}


\vskip7.5mm
{  \@author}
\vskip5mm
{\thecorrauthor}
\vskip5mm
{\thearticledates}
\vskip3mm

\begin{tcolorbox}[colframe=jchslightorange, colback=jchslightorange, sharp corners,boxsep=4mm,top=3.5mm,left=3.8mm,right=2.0mm]
	{
		\noindent
		{\textbf{ABSTRACT}}
	}
	\vskip 1.5mm
	{
		\noindent
		
		\parbox{\dimexpr\linewidth-2\fboxsep\relax}
		{
			{\fontsize{10}{12}\selectfont\setstretch{1.14}\theabstract\par}
		}
		
		\vskip 0mm
		\noindent
		\parbox{\dimexpr\linewidth-2\fboxsep\relax}
		{
			{\fontsize{10}{12}\selectfont\setstretch{1.14}{\bfseries\color{color1}Keywords:} \@keywords}
		}
	}
\end{tcolorbox}
\vskip25pt
}
}
%-----------------------------------------------

\setlength{\fboxrule}{0.75pt} % Width of the border around the abstract

\definecolor{color1}{RGB}{0,0,0} % Color of section headings

\definecolor{jchsorange}{HTML}{ffc000} % color aroiund 'we are a fair open access journal'
\definecolor{urlcolor}{HTML}{0563c1} % url color
\definecolor{jchslightorange}{HTML}{fbe5d6} % abstract background color
\definecolor{jchsheadercolor}{HTML}{ed7d31} % abstract background color

\definecolor{jchshlightgray}{HTML}{d9d9d9}

% help variable so we can conveniently change the color in links )
\definecolor{varlinkcolor}{RGB}{126,169,105}

%\renewcommand\Authfont{\fontsize{12}{12}\usefont{OT1}{phv}{b}{n}}
%\renewcommand\Affilfont{\fontsize{10}{10}\usefont{OT1}{phv}{b}{n}}
\newcommand{\keywordsfont}{\normalfont\rmfamily\fontsize{7}{10}\selectfont}

\newlength{\tocsep}
\setlength\tocsep{1.5pc} % Sets the indentation of the sections in the table of contents
\setcounter{tocdepth}{3} % Show only three levels in the table of contents section: sections, subsections and subsubsections

%-----------------------------------------------
%% Automatically abbreviate journal names
%\RequirePackage{jabbrv}
