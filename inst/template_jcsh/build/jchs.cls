% based on bioinfo.cls and ncc.cls
\newcommand\classname{jcsh}
\newcommand\lastmodifieddate{2023/08/13}
\newcommand\versionnumber{0.1}

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{jchs}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions*
\LoadClass{article}

\RequirePackage[english]{babel}

\RequirePackage{graphicx}
\RequirePackage{siunitx}
\RequirePackage[normalem]{ulem}
\RequirePackage{hhline}
\RequirePackage{calc}
\RequirePackage{adjustbox}
\RequirePackage{cuted}
\RequirePackage{abspos}
\RequirePackage{hyphenat}
\RequirePackage{wrapfig}
\RequirePackage{etoolbox}
\RequirePackage{placeins}
\RequirePackage{framed}

% tables
\RequirePackage{xltabular}


\RequirePackage{ifthen}
\RequirePackage{calc}
\AtEndOfClass{\RequirePackage{microtype}}
\RequirePackage{wrapfig}



%%%%%%%%%% font stuff %%%%%%%%%%%%%
\RequirePackage{fontspec}

% fallbacks in case main font does not contain something
	\directlua{ luaotfload.add_fallback("fallbacks", { "[fonts/notosanssymbols2/NotoSansSymbols2-Regular.ttf]:style=Regular;" , "[fonts/notosanscherokee/NotoSansCherokee-Regular.ttf]:style=Regular;"   }) }


% load font from manual location to enbale embedded shipping
\setmainfont{texgyrepagella-regular.otf}[
Path = fonts/pagella/,
BoldFont = texgyrepagella-bold.otf,
ItalicFont = texgyrepagella-italic.otf,
BoldItalicFont  = texgyrepagella-bolditalic.otf,
RawFeature={fallback=fallbacks}]


\setsansfont{OpenSans-Regular.ttf}[
Path = fonts/opensans/,
BoldFont = OpenSans-Bold.ttf,
ItalicFont = OpenSans-Italic.ttf,
BoldItalicFont  = OpenSans-BoldItalic.ttf,
RawFeature={fallback=fallbacks}]


%\usepackage[T1]{fontenc}
%\RequirePackage[default]{opensans}
\RequirePackage{setspace}

\RequirePackage{ragged2e}
\RequirePackage{float}


\DeclareTextFontCommand{\fontAffils}{\fontsize{50}{54}\selectfont}

%%%%% end ont stuff %%%%%%%%%

\RequirePackage[hang,flushmargin]{footmisc}

\RequirePackage[breakable]{tcolorbox}


\RequirePackage{ifpdf}

\RequirePackage{amsmath,amsfonts,amssymb}
\RequirePackage{graphicx}
\RequirePackage[table]{xcolor}
\usepackage{graphicx}


%page setup
\RequirePackage[left=1.6cm,%
right=1.6cm,%
top=2.5cm,%
bottom=2.5cm,%
headheight=12pt,%
a4paper]{geometry}%
\RequirePackage{pdflscape}
\RequirePackage{afterpage}


\RequirePackage{multicol}


\setlength{\columnsep}{8mm}%distance between columns

\RequirePackage[labelfont={bf,sf},%
labelsep=period,%
justification=raggedright, skip = 2pt]{caption}



% author/ affil setup
\RequirePackage{authblk}
\setcounter{Maxaffil}{0}% always footnote mode
\setlength{\affilsep}{4.5mm}%distance between authors and affiliations
\renewcommand\Authsep{}% author separation string
\renewcommand\Authand{}% if two authors
\renewcommand\Authands{}% before last author if more than two

\renewcommand\Authfont{\fontsize{12}{14}\selectfont}
\renewcommand\Affilfont{\fontsize{9}{12}\selectfont}

% https://tex.stackexchange.com/questions/496334/including-orcid-in-plain-latex-using-authblk
\newbox{\myorcidaffilbox}
\sbox{\myorcidaffilbox}{\large\smash{\includegraphics[height=9pt]{ORCIDiD_iconvector}}}
	\newcommand{\orcidaffil}[1]{%
	\href{https://orcid.org/#1}{\hspace*{1pt}\usebox{\myorcidaffilbox}}}


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

\renewcommand{\headrulewidth}{0pt}% % No header rule
\renewcommand{\footrulewidth}{0pt}% % No footer rule


%
% section/subsection/paragraph set-up
%
\RequirePackage[explicit]{titlesec}
\titleformat{\section}
{\color{themesectionheadingcolor}\Large\bfseries}
% {\thesection} with numbers
{}
{0pt}
{\MakeUppercase{#1}}
[]
\titleformat{name=\section,numberless}
{\color{themesectionheadingcolor}\Large\bfseries}
{}
{-0.5pt}
{\MakeUppercase{#1}}
[]
\titleformat{\subsection}
{\color{themesectionheadingcolor}\bfseries}
{}
{-0.5pt} % not sure why needed but otherwise 	subsections sem to have some  indent. 
{#1}
[]
\titleformat{\subsubsection}
{\color{themesectionheadingcolor}\small\bfseries\itshape}
{}
{-0.5pt}
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
\RequirePackage{csquotes}
%\RequirePackage[begintext=``, endtext='']{quoting}
\RequirePackage{xurl}

\PassOptionsToPackage{hyphens}{url}
\RequirePackage[colorlinks=true,luatex,breaklinks=true]{hyperref}
\hypersetup{breaklinks=true}
\urlstyle{same}% do not use monospace for links




% for  author
\newcommand{\corrauthor}[3][]{%
	\def\thecorrauthor{%
		{\fontsize{9}{12}\selectfont{\bfseries $^{#1}$} #2  \nohyphens{\sloppy #3}\par}
	}
}

% dates
% version with one  argument
\newcommand{\articledates}[1]{\def\thearticledates{
		{\fontsize{9}{12}\selectfont#1}
}}
\newcommand{\journalvolumestring}[1]{\def\thejournalvolumestring{#1}}
\newcommand{\journaltitle}[1]{\def\thejournaltitle{#1}}
\newcommand{\journaldoi}[1]{\def\thejournaldoi{#1}}
\newcommand{\journalarticletype}[1]{\def\thejournalarticletype{#1}}
\newcommand{\abstractmainlangtitle}[1]{\def\theabstractmainlangtitle{#1}}




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
{\absput[pg-tl, tl, x=1cm, y=-1.2cm,width=1\textwidth]{
\begin{minipage}{0.13\textwidth}
	\href{https://journals.ub.umu.se/index.php/jcsh}{\includegraphics[width = 20mm]{jchs.pdf}}
\end{minipage} 
\begin{minipage}{0.49\textwidth}
	{\sffamily\footnotesize\color{black}
		\thejournaltitle\\
		\thejournalvolumestring\\
		\thejournaldoi \vspace*{3mm}\\
		{\fontsize{11}{13}\selectfont\textbf{\thejournalarticletype}}}
\end{minipage}
\begin{minipage}[t][][t]{0.37\textwidth}% align both the minipage itself and its contents on top
	\textls[-14]{%
	\begin{flushright}%
		\vspace{-13mm} % somehow vspace etc insert a linebreak in the minipage environment, so we need to work with a minus offset\\
		\colorlet{varlinkcolor}{blue}%
		\begin{tcolorbox}[colframe=themecolor1, colback=white, halign=center,fontupper=\fontsize{10}{12}\selectfont, boxsep=0pt,width=63mm, left=0mm, right=0mm]
			\sffamily We are a {\textbf{Fair Open Access}} journal
		\end{tcolorbox}%
		\colorlet{varlinkcolor}{black}%
\end{flushright}}%
\end{minipage}}}
\vspace*{7mm}
%~\\[7mm]
{\sffamily\raggedright%left align

{% we do not want a intent for the title, and we want it centered
	\setstretch{1.08}\setlength{\JustifyingParindent}{0pt}%
	\raggedright \sloppy \bfseries\fontsize{17}{19}\selectfont\color{themesectionheadingcolor}{\textls[20]{\@title}}\par%
}
\vskip3mm
{  \@author}

\ifdef{\thecorrauthor}
{\vskip1.5mm
\thecorrauthor}{} % empty otherwise
\ifdef{\thearticledates}
{\vskip2mm
\thearticledates} {}% empty otherwise
\ifdef{\theabstract}
{
	\vskip2mm
	\begin{tcolorbox}[colframe=themecolor2, colback=themecolor2, sharp corners,boxsep=4mm,top=0mm,left=3.8mm,right=2.0mm,bottom=1mm, breakable]
	{
		\noindent
		\ifdef{\theabstractmainlangtitle}{\raggedright\bfseries \theabstractmainlangtitle\par}{}
	}
	
	\noindent
			
	{\fontsize{10}{12}\selectfont\setstretch{1.14}\theabstract}
	\end{tcolorbox}
}{} % if empty ldo not print anything to page
}
\begin{center}
\ifdef{\abstracthint}{{\bfseries \abstracthint}}{}
\end{center}
\vfill
}
%-----------------------------------------------

\setlength{\fboxrule}{0.75pt} % Width of the border around the abstract

\definecolor{color1}{RGB}{0,0,0} % Color of section headings

\definecolor{themecolor1}{HTML}{ffc000} % color aroiund 'we are a  open access journal'
\definecolor{urlcolor}{HTML}{0563c1} % url color
\definecolor{themecolor2}{HTML}{ffe8d9} %{fbe5d6} % abstract background color
\definecolor{themesectionheadingcolor}{HTML}{ed7d31} % orange title text color

\definecolor{jchshlightgray}{HTML}{d9d9d9}

% help variable so we can conveniently change the color in links )
\definecolor{varlinkcolor}{RGB}{126,169,105}

\newlength{\tocsep}
\setlength\tocsep{1.5pc} % Sets the indentation of the sections in the table of contents
\setcounter{tocdepth}{3} % Show only three levels in the table of contents section: sections, subsections and subsubsections

