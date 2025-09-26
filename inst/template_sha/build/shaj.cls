% based on bioinfo.cls and ncc.cls
\newcommand\classname{shaj}
\newcommand\lastmodifieddate{2023/08/13}
\newcommand\versionnumber{0.1}

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{shaj}

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
\RequirePackage{changepage}
\RequirePackage{atbegshi}

% tables
\RequirePackage{xltabular}


\RequirePackage{ifthen}
\RequirePackage{calc}
\AtEndOfClass{\RequirePackage{microtype}}
\RequirePackage{wrapfig}



%%%%%%%%%% font stuff %%%%%%%%%%%%%
\RequirePackage{fontspec}
	\directlua{ luaotfload.add_fallback("fallbacks", { "[fonts/linux/LinBiolinum_Rah.ttf]:style=Regular;" }) }


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


\newfontfamily{\sanssemibold}{OpenSans-SemiBold.ttf}
[Path = fonts/opensans/,
ItalicFont=OpenSans-SemiBold.ttf,
RawFeature={fallback=fallbacks}]

%\usepackage[T1]{fontenc} 
%\RequirePackage[default]{opensans}
\RequirePackage{setspace}

\RequirePackage{ragged2e}
\RequirePackage{float}


\DeclareTextFontCommand{\fonts}{\fontsize{50}{54}\selectfont}

%%%%% end ont stuff %%%%%%%%%

\RequirePackage[hang,flushmargin]{footmisc}

\RequirePackage[breakable]{tcolorbox}


\RequirePackage{ifpdf}

\RequirePackage{amsmath,amsfonts,amssymb}
\RequirePackage{graphicx}
\RequirePackage[table]{xcolor}
\usepackage{graphicx}


%page setup
\RequirePackage[a4paper]{geometry}%
\RequirePackage{pdflscape}
\RequirePackage{afterpage}


\RequirePackage{multicol}

\RequirePackage{paracol}
\setlength{\columnsep}{8mm}%distance between columns

\RequirePackage[labelfont={bf,sf},%
labelsep=period,%
justification=raggedright, skip = 2pt]{caption}



% author/ affil setup
\RequirePackage{authblk}
\setcounter{Maxaffil}{0}% always footnote mode
\setlength{\affilsep}{1.5mm}%distance between authors and affiliations
\renewcommand\Authsep{}% author separation string
\renewcommand\Authand{}% if two authors
\renewcommand\Authands{}% before last author if more than two

\renewcommand\Authfont{\fontsize{8.8}{6}\selectfont\sanssemibold}
\renewcommand\Affilfont{\fontsize{8.2}{8}\selectfont\sffamily}


\newcommand{\textsuperscriptAffil}[1]{\raisebox{0.7ex}{\scalebox{0.74}{#1}}}

\newcommand{\textsuperscriptAuthor}[1]{\raisebox{0.7ex}{\scalebox{1}{#1}}}

%\makeatletter
\renewcommand\AB@affilsepx{, \protect\Affilfont}
%\makeatother



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

\renewcommand{\headwidth}{17cm}%  width of header
\renewcommand{\headrulewidth}{0pt} % No header rule
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
{#1}
[]
\titleformat{name=\section,numberless}
{\color{themesectionheadingcolor}\Large\bfseries}
{}
{-0.5pt}
{#1}
[]
\titleformat{\subsection}
{\color{themesectionheadingcolor}\bfseries\itshape}
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
%\RequirePackage{indentfirst} % enable to indent also first paragraph after a seciton headings
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
\newcommand{\articledates}[1]{\def\thearticledates{#1}}
\newcommand{\keywordscust}[1]{\def\thekeywords{#1}}
\newcommand{\respeditor}[1]{\def\therespeditor{#1}}

\newcommand{\journalvolumestring}[1]{\def\thejournalvolumestring{#1}}
\newcommand{\journaltitle}[1]{\def\thejournaltitle{#1}}
\newcommand{\journaldoi}[1]{\def\thejournaldoi{#1}}
\newcommand{\journalarticletype}[1]{\def\thejournalarticletype{#1}}




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
{\absput[pg-tl, tl, x=2cm, y=-1.7cm,width=1\textwidth]{
{\color{themecolor1}\noindent\hrulefill\\}
\noindent\begin{minipage}[t][2cm][t]{0.3\textwidth} \setstretch{0.7}
	\href{https://journals.ub.umu.se/index.php/shaj}{\includegraphics[width = 39.25mm]{logo_svg.pdf}}
	\vspace{1mm}\\
	\textls[-10]{\sffamily\fontsize{7.2}{6}\selectfont\color{black}
		\textbf{\MakeUppercase{\thejournaltitle}}\\
		\thejournalvolumestring\\
		\thejournaldoi}
\end{minipage}
\raisebox{9.8mm}{\begin{minipage}[t][2cm][b]{0.7\textwidth}% align both the minipage itself and its contents on top
	\begin{flushright}
	\includegraphics[width = 19.2mm]{oaccesslogo.pdf}
	\end{flushright}
\end{minipage}}
}}
\vspace*{11.5mm}
{\color{themecolor1}\noindent\hrulefill\\}\vspace*{-2.8mm}\\
{\fontsize{10}{13}\sffamily\selectfont\sanssemibold{\textls[5]{\thejournalarticletype}}}\\
\vspace*{-1.52mm}
{\sffamily\raggedright%left align

{% we do not want a intent for the title, and we want it centered
	\emergencystretch=\maxdimen
	\hyphenpenalty=10000
	%\setlength{\JustifyingParfillskip}{15mm}
	\setlength{\RaggedRightRightskip}{7mm}	
	\setstretch{1.11}\setlength{\JustifyingParindent}{0pt}%
	\RaggedRight\sloppy\bfseries\fontsize{17.5}{19}\selectfont\color{themesectionheadingcolor}{\textls[15]{\@title}}\par%
}
\vspace{2.2mm}
%{\@author}
{\begin{spacing}{0.1}
		\fontsize{8.6}{7}\selectfont\sanssemibold\custauths
\end{spacing}
}
\vspace{5mm}
\begin{spacing}{0.1}
{\fontsize{8}{6}\selectfont\custaffil}
\end{spacing}
%\ifdef{\thecorrauthor}
%{\vskip1.5mm
%\thecorrauthor}{} % empty otherwise
\vspace{6mm}
% trick: insert minipage shifted to right and with 0 width/depth and height
\begin{adjustbox}{minipage=[][0cm][]{37.75mm}, right, set height=0pt,set depth=0pt}
\vspace{45mm}
\begin{spacing}{1.6}
	\fontsize{7.2}{6}\selectfont
	\ifdef{\thearticledates}{\thearticledates}{}% empty otherwise
	\vskip2mm
	\ifdef{\therespeditor}{\therespeditor}{}% empty otherwise
	\vskip2mm
	\ifdef{\thekeywords}{\thekeywords}{}% empty otherwise
\end{spacing}
\end{adjustbox}



\ifdef{\theabstract}
{
	%\vspace{-5cm}
	\begin{tcolorbox}[colframe=themecolor2, colback=themecolor2, sharp corners,boxsep=4mm,top=0mm,left=2.0mm,right=2.0mm,breakable, width=0.74\textwidth]
	{
		\noindent
		{\fontsize{8}{12}\selectfont\raggedright\bfseries\color{themecolor1}ABSTRACT\par}
	}
	\vskip1.5mm
	\noindent
			
	{\setstretch{1.14}\fontsize{7.7}{8.75}\selectfont\theabstract\par}
			
	\end{tcolorbox}
	
	\vskip3mm
}{} % if empty ldo not print anything to page

}
	
%\begin{center}
%\ifdef{\abstracthint}{{\bfseries \abstracthint}}{}
%\end{center}
\vfill
}
%-----------------------------------------------

\setlength{\fboxrule}{0.75pt} % Width of the border around the abstract

\definecolor{sectionheadingcolor}{RGB}{0,0,0} % Color of section headings

\definecolor{themecolor1}{HTML}{328a67} % color aroiund 'we are a  open access journal'
\definecolor{urlcolor}{HTML}{0563c1} % url color
\definecolor{themecolor2}{HTML}{e9f3f0} %{fbe5d6} % abstract background color  jshlightorange before
\definecolor{themesectionheadingcolor}{HTML}{328a67} % orange title text color jslheadercoor before

\definecolor{jchshlightgray}{HTML}{d9d9d9}

% help variable so we can conveniently change the color in links )
\definecolor{varlinkcolor}{RGB}{126,169,105}

\newlength{\tocsep}
\setlength\tocsep{1.5pc} % Sets the indentation of the sections in the table of contents
\setcounter{tocdepth}{3} % Show only three levels in the table of contents section: sections, subsections and subsubsections

