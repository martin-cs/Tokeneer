%% Praxis Z styles and tools
%%
%% Copyright (c) 2008, Praxis High Integrity Systems Limited
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%%  * Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%%
%%  * Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in
%%    the documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
%% PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
%% OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
%% EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
%% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%% PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%% LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


%% -*-LaTeX-*-
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{pxcsdoc}[1997/08/20 Standard PxCS Document (aap/jarh/jah)]

\RequirePackage{ifthen}
\renewcommand{\sfdefault}{phv}
\renewcommand{\rmdefault}{ptm}

% Chapter gives new page unless compact option used
\newboolean{chapternewpage}
\setboolean{chapternewpage}{true}

\DeclareOption{compact}{%
  \typeout{(Compact option)}%
  \setboolean{chapternewpage}{false}%
}

\DeclareOption*{%
  \PassOptionsToClass{\CurrentOption}{report}%
}

\ProcessOptions
\LoadClass[a4paper]{report}

\RequirePackage{parskip}
\RequirePackage[dvips]{graphics}

% "To be supplied" marker
\newcommand{\ToBeSupplied}[1]
{\textit{(To be supplied: #1)}%
 \ClassWarning{praxdoc}{Material to be supplied: "#1"}}
% Need to improve the rest of this:
%   only specify changes from report class
%   use vmargin to set page size
%   use fancyheadings to set page styles (correct gap below header)

% BUGS...
%
% - level 1 lists are feeble
%
%   etc etc

%  The type size option is handled by reading a different file for each 
%  size, as follows, to define font size-specific commands:
%       10pt : PRAX10, 11pt : PRAX11, 12pt : PRAX12
%  Implemented by \def'ing \@ptsize to last digit of file name.
%

\def\@ptsize{1}               % Default is PRAX11.STY
\@namedef{ds@10pt}{\def\@ptsize{0}} % 10pt option reads in PRAX10.STY
\@namedef{ds@12pt}{\def\@ptsize{2}} % 12pt option reads in PRAX12.STY

%  Two-side or one-side printing.
%
% \@twosidefalse               %  Default is one-sided printing.
\def\ds@twoside{\@twosidetrue  %  Defines twoside option.
           \@mparswitchtrue}   %    Marginpars go on outside of page.
%  draft option
%
% \overfullrule = 0pt             % Default is don't mark overfull hboxes.
\def\ds@draft{\overfullrule 5pt}  % Causes overfull hboxes to be marked.

% The \@options command causes the execution of every command \ds@FOO 
% which is defined and for which the user typed the FOO option in his
% \documentstyle command.  For every option BAR he typed for which
% \ds@BAR is not defined, the file BAR.sty will be read after the present
% (main) .STY file is executed.

\@options

%% \input prax1\@ptsize.sty\relax
%% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

%    ****************************************
%    *   PRAX11.STY                         *
%    ****************************************
%
%    Included whilst under development (makes editing easier)
%
% PRAXIS DOCUMENT STYLE -- TKR 4 January 1988
% based on ...

% REPORT DOCUMENT STYLE -- released 23 September 1985
%    for LaTeX version 2.09
% Copyright (C) 1985 by Leslie Lamport

%  ****************************************
%  *               FONTS                  *
%  ****************************************
%

\lineskip 1pt            % \lineskip is 1pt for all font sizes.
\normallineskip 1pt
\def\baselinestretch{1}

% Each size-changing command \SIZE executes the command
%        \@setsize\SIZE{BASELINESKIP}\FONTSIZE\@FONTSIZE
% where:
%   BASELINESKIP = Normal value of \baselineskip for that size.  (Actual 
%                  value will be \baselinestretch * BASELINESKIP.)
%
%  \FONTSIZE     = Name of font-size command.  The currently available
%                  (preloaded) font sizes are: \vpt (5pt), \vipt (6pt),
%                  \viipt (etc.), \viiipt, \ixpt, \xpt, \xipt, \xiipt,
%                  \xivpt, \xviipt, \xxpt, \xxvpt.
%  \@FONTSIZE    = The same as the font-size command except with an
%                  '@' in front---e.g., if \FONTSIZE = \xivpt then
%                  \@FONTSIZE = \@xivpt.
%
% For reasons of efficiency that needn't concern the designer,
% the document style defines \@normalsize instead of \normalsize .  This is
% done only for \normalsize, not for any other size-changing commands.

\def\normalsize{\@setsize\normalsize{13pt}\xipt\@xipt
\abovedisplayskip 11pt plus3pt minus6pt%
\belowdisplayskip \abovedisplayskip
\abovedisplayshortskip  \z@ plus3pt%   
\belowdisplayshortskip  6.5pt plus3.5pt minus3pt}

\def\small{\@setsize\small{12pt}\xpt\@xpt
\abovedisplayskip 10pt plus2pt minus5pt%
\belowdisplayskip \abovedisplayskip
\abovedisplayshortskip  \z@ plus3pt%   
\belowdisplayshortskip  6pt plus3pt minus3pt
\def\@listi{\topsep 6pt plus 2pt minus 2pt\parsep 3pt plus 2pt minus 1pt
\itemsep \parsep}}

\def\footnotesize{\@setsize\footnotesize{11pt}\ixpt\@ixpt
\abovedisplayskip 8pt plus 2pt minus 4pt%
\belowdisplayskip \abovedisplayskip
\abovedisplayshortskip \z@ plus 1pt%
\belowdisplayshortskip 4pt plus 2pt minus 2pt
\def\@listi{\topsep 4pt plus 2pt minus 2pt\parsep 2pt plus 1pt minus 1pt
\itemsep \parsep}}

\def\scriptsize{\@setsize\scriptsize{9.5pt}\viiipt\@viiipt}
\def\tiny{\@setsize\tiny{7pt}\vipt\@vipt}
\def\large{\@setsize\large{14pt}\xiipt\@xiipt}
\def\Large{\@setsize\Large{18pt}\xivpt\@xivpt}
\def\LARGE{\@setsize\LARGE{22pt}\xviipt\@xviipt}
\def\huge{\@setsize\huge{25pt}\xxpt\@xxpt}
\def\Huge{\@setsize\Huge{30pt}\xxvpt\@xxvpt}


%  ****************************************
%  *            PAGE LAYOUT               *
%  ****************************************
%
% All margin dimensions measured from a point one inch from top and side
% of page.  

% NB all margins reduced by 0.5in (36pt) - TKR
% except topmargin -21pt not -31pt coz text falls off top of LZR-2665

% SIDE MARGINS:
\if@twoside                 % Values for two-sided printing:
   \oddsidemargin 0pt       %   Left margin on odd-numbered pages.
   \evensidemargin 38pt     %   Left margin on even-numbered pages.
   \marginparwidth 64pt     %   Width of marginal notes. (?)
\else                       % Values for one-sided printing:
   \oddsidemargin 4.5pt      %   Note that \oddsidemargin = \evensidemargin
   \evensidemargin 4.5pt
   \marginparwidth 47pt 
\fi
\marginparsep 10pt          % Horizontal space between outer margin and 
                            % marginal note


% VERTICAL SPACING:        
                         % Top of page:
\topmargin -36pt         %    Nominal distance from top of page to top of
                         %    box containing running head.
\headheight 35pt         %    Height of box containing running head.
\headsep 13mm             %    Space between running head and text.
% \topskip = 10pt        %    '\baselineskip' for first line of page.
                         % Bottom of page:
\footskip 30pt           %    Distance from baseline of box containing foot 
                         %    to baseline of last line of text.


% DIMENSION OF TEXT:

\textheight 9in          % Height of text (including footnotes and figures, 
                         % excluding running head and foot).
\textwidth 6in           % Width of text line.
                         % For two-column mode: 
\columnsep 10pt          %    Space between columns 
\columnseprule 0pt       %    Width of rule between columns.

% A \raggedbottom command causes 'ragged bottom' pages: pages set to
% natural height instead of being stretched to exactly \textheight.

% FOOTNOTES:

\footnotesep 7.7pt    % Height of strut placed at the beginning of every
                      % footnote = height of normal \footnotesize strut,
                      % so no extra space between footnotes.

\skip\footins 10pt plus 4pt minus 2pt  % Space between last line of text and 
                                       % top of first footnote.

% FLOATS: (a float is something like a figure or table)
%
%  FOR FLOATS ON A TEXT PAGE:
%
%    ONE-COLUMN MODE OR SINGLE-COLUMN FLOATS IN TWO-COLUMN MODE:
\floatsep 12pt plus 2pt minus 2pt        % Space between adjacent floats moved
                                         % to top or bottom of text page.
\textfloatsep 20pt plus 2pt minus 4pt    % Space between main text and floats
                                         % at top or bottom of page.
\intextsep 12pt plus 2pt minus 2pt       % Space between in-text figures and 
                                         % text.
%    TWO-COLUMN FLOATS IN TWO-COLUMN MODE:
\dblfloatsep 12pt plus 2pt minus 2pt     % Same as \floatsep for double-column
                                         % figures in two-column mode.
\dbltextfloatsep 20pt plus 2pt minus 4pt % \textfloatsep for double-column 
                                         % floats.

%  FOR FLOATS ON A SEPARATE FLOAT PAGE OR COLUMN:
%    ONE-COLUMN MODE OR SINGLE-COLUMN FLOATS IN TWO-COLUMN MODE:
\@fptop 0pt plus 1fil    % Stretch at top of float page/column. (Must be    
                         % 0pt plus ...)                                    
\@fpsep 8pt plus 2fil    % Space between floats on float page/column.       
\@fpbot 0pt plus 1fil    % Stretch at bottom of float page/column. (Must be 
                         % 0pt plus ... )                                   

%   DOUBLE-COLUMN FLOATS IN TWO-COLUMN MODE.
\@dblfptop 0pt plus 1fil % Stretch at top of float page. (Must be 0pt plus ...)
\@dblfpsep 8pt plus 2fil % Space between floats on float page.
\@dblfpbot 0pt plus 1fil % Stretch at bottom of float page. (Must be 
                         % 0pt plus ... )                                   
% MARGINAL NOTES:
%
\marginparpush 5pt       % Minimum vertical separation between two marginal 
                         % notes.


%  ****************************************
%  *           PARAGRAPHING               *
%  ****************************************
%
\parskip 10pt plus 1pt              % Extra vertical space between paragraphs.
\parindent 0pt                    % Width of paragraph indentation.
\topsep 9pt plus 3pt minus 5pt     % Extra vertical space, in addition to 
                                   % \parskip, added above and below list and
                                   % paragraphing environments.
\partopsep 3pt plus 1pt minus 2pt  % Extra vertical space, in addition to 
                                   % \parskip and \topsep, added when user
                                   % leaves blank line before environment.
\itemsep 4.5pt plus 2pt minus 1pt  % Extra vertical space, in addition to
                                   % \parskip, added between list items.

% The following page-breaking penalties are defined

\@lowpenalty   51      % Produced by \nopagebreak[1] or \nolinebreak[1]
\@medpenalty  151      % Produced by \nopagebreak[2] or \nolinebreak[2]
\@highpenalty 301      % Produced by \nopagebreak[3] or \nolinebreak[3]

\@beginparpenalty -\@lowpenalty    % Before a list or paragraph environment.
\@endparpenalty   -\@lowpenalty    % After a list or paragraph environment.
\@itempenalty     -\@lowpenalty    % Between list items.

% \clubpenalty         % 'Club line'  at bottom of page.
% \widowpenalty        % 'Widow line' at top of page.
% \displaywidowpenalty % Math display widow line.
% \predisplaypenalty   % Breaking before a math display.
% \postdisplaypenalty  % Breaking after a math display.
% \interlinepenalty    % Breaking at a line within a paragraph.
% \brokenpenalty       % Breaking after a hyphenated line.


%    ****************************************
%    *        CHAPTERS AND SECTIONS         *
%    ****************************************
%

\def\part{\cleardoublepage   % Starts new page.
   \thispagestyle{plain}     % Page style of part page is 'plain'
  \if@twocolumn              % IF two-column style
     \onecolumn              %  THEN \onecolumn
     \@tempswatrue           %       @tempswa := true
    \else \@tempswafalse     %  ELSE @tempswa := false
  \fi                        % FI
  \hbox{}\vfil               % Add fil glue to center title  
  \bgroup  \centering        % BEGIN centering
  \secdef\@part\@spart}     


\def\@part[#1]#2{\ifnum \c@secnumdepth >-2\relax  % IF secnumdepth > -2
        \refstepcounter{part}                     %   THEN step part counter
        \addcontentsline{toc}{part}{\thepart      %        add toc line
        \hspace{1em}#1}\else                      %   ELSE add unnumbered line
        \addcontentsline{toc}{part}{#1}\fi        % FI
   \markboth{}{}
    \ifnum \c@secnumdepth >-2\relax  % IF secnumdepth > -2
      \huge\bfseries Part \thepart         %   THEN Print 'Part' and number
    \par                             %         in \huge bold.
    \vskip 20pt \fi                  %        Add space before title.
    \Huge \bfseries                        % FI
    #1\@endpart}                     % Print Title in \Huge bold.
    
    
% \@endpart finishes the part page
%
\def\@endpart{\par\egroup     % END centering 
   \vfil\newpage              % End page with 1fil glue.
   \if@twoside                % IF twoside printing
       \hbox{}                %   THEN Produce totally blank page
       \thispagestyle{empty}  
       \newpage        
   \fi                        % FI
   \if@tempswa                % IF @tempswa = true       
     \twocolumn               %   THEN \twocolumn 
   \fi}                       % FI

\def\@spart#1{\Huge \bfseries                      % Print title in \huge boldface
   #1\@endpart}


% \@makechapterhead {TEXT} : Makes the heading for the \chapter command.
%

\def\@makechapterhead#1{%             % Heading for \chapter command
  \vspace*{0pt}%                      % wierd but necessary
  { \parindent 0pt%
    \hbox{\normalsize\bfseries\hbox to 0pt{\hskip -1.5cm
        \thechapter\hss}\@chapapp{}\uppercase{#1}\hss\par 
    }
    \nobreak                         % TeX penalty to prevent page break.
    \vskip 1mm                      % Space between title and text.
  } }

% \@makeschapterhead {TEXT} : Makes the heading for the \chapter* command.
%

\def\@makeschapterhead#1{             % Heading for \chapter* command
  \vspace*{0pt}                      % wierd but necessary
  { \parindent 0pt
    \hbox{\normalsize\bfseries\hbox to 0pt{\hskip -1.5cm
        \thechapter\hss}\@chapapp{}\uppercase{#1}\hss\par 
    }
    \nobreak                         % TeX penalty to prevent page break.
    \vskip 1mm                      % Space between title and text.
  } }

% \secdef{UNSTARCMDS}{STARCMDS} :
%    When defining a \chapter or \section command without using
%    \@startsection, you can use \secdef as follows:
%       \def\chapter { ... \secdef \CMDA \CMDB }
%       \def\CMDA    [#1]#2{ ...   % Command to define \chapter[...]{...}
%       \def\CMDB    #1{ ...       % Command to define \chapter*{...}

\ifthenelse{\boolean{chapternewpage}}{%
  \def\chapter{%
   \clearpage%
   \global\@topnum\z@        % Prevents figures from going at top of page.
   \@afterindentfalse        % Suppresses indent in first paragraph.  Change
   \secdef\@chapter\@schapter}   % to \@afterindenttrue to have indent.
}{%
  \def\chapter{%
   \global\@topnum\z@        % Prevents figures from going at top of page.
   \@afterindentfalse        % Suppresses indent in first paragraph.  Change
   \secdef\@compactchapter\@schapter}   % to \@afterindenttrue to have indent.
}

\def\@chapter[#1]#2{\ifnum \c@secnumdepth >\m@ne
        \refstepcounter{chapter}
        \typeout{\@chapapp\thechapter.}
        \addcontentsline{toc}{chapter}{\protect
        \numberline{\thechapter}#1}\else
      \addcontentsline{toc}{chapter}{#1}\fi
   \chaptermark{#1}
   \addtocontents{lof}{\protect\addvspace{10pt}} % Adds between-chapter space
   \addtocontents{lot}{\protect\addvspace{10pt}} % to lists of figs & tables.
   \if@twocolumn                                 % Tests for two-column mode.  
           \@topnewpage[\@makechapterhead{#2}]  
     \else \@makechapterhead{#2}
           \@afterheading                  % Routine called after chapter and
     \fi}                                  % section heading.

\def\@compactchapter[#1]#2{\ifnum \c@secnumdepth >\m@ne
        \refstepcounter{chapter}
        \typeout{\@chapapp\thechapter.}
        \addcontentsline{toc}{chapter}{\protect
        \numberline{\thechapter}#1}\else
      \addcontentsline{toc}{chapter}{#1}\fi
   \vspace{5mm}\chaptermark{#1}
   \addtocontents{lof}{\protect\addvspace{10pt}} % Adds between-chapter space
   \addtocontents{lot}{\protect\addvspace{10pt}} % to lists of figs & tables.
   \if@twocolumn                                 % Tests for two-column mode.  
           \@topnewpage[\@makechapterhead{#2}]  
     \else \@makechapterhead{#2}
           \@afterheading                  % Routine called after chapter and
     \fi}                                  % section heading.

\def\@schapter#1{\if@twocolumn \@topnewpage[\@makeschapterhead{#1}]
        \else \@makeschapterhead{#1} 
              \@afterheading\fi}

% \@startsection {NAME}{LEVEL}{INDENT}{BEFORESKIP}{AFTERSKIP}{STYLE} 
%            optional * [ALTHEADING]{HEADING}
%    Generic command to start a section.  
%    NAME       : e.g., 'subsection'
%    LEVEL      : a number, denoting depth of section -- e.g., chapter=1,
%                 section = 2, etc.  A section number will be printed if
%                 and only if LEVEL < or = the value of the secnumdepth
%                 counter.
%    INDENT     : Indentation of heading from left margin
%    BEFORESKIP : Absolute value = skip to leave above the heading.  
%                 If negative, then paragraph indent of text following 
%                 heading is suppressed.
%    AFTERSKIP  : if positive, then skip to leave below heading,
%                       else - skip to leave to right of run-in heading.
%    STYLE      : commands to set style
%  If '*' missing, then increments the counter.  If it is present, then
%  there should be no [ALTHEADING] argument.  A sectioning command
%  is normally defined to \@startsection + its first six arguments.

%\def\section{\@startsection {section}{1}{\z@}{-3.5ex plus -1ex minus 
\def\section{%
  \@startsection {section}{1}{-1.5cm}{-1pt}{1pt}{\normalsize\bfseries}}
\def\subsection{\@startsection{subsection}{2}{-1.5cm}{1pt}{1pt}{\normalsize}}
\def\subsubsection{\@startsection{subsubsection}{3}{-1.5cm}{-3.25ex plus
 -1ex minus -.2ex}{1.5ex plus .2ex}{\normalsize\bfseries}}
\def\paragraph{\@startsection
     {paragraph}{4}{\z@}{3.25ex plus 1ex minus .2ex}{-1em}{\normalsize\bfseries}}
\def\subparagraph{\@startsection
     {subparagraph}{4}{\parindent}{3.25ex plus 1ex minus 
     .2ex}{-1em}{\normalsize\bfseries}}

% Default initializations of \...mark commands.  (See below for their
% us in defining page styles.
%

\def\chaptermark#1{}
% \def\sectionmark#1{}           % Preloaded definitions
% \def\subsectionmark#1{}
% \def\subsubsectionmark#1{}
% \def\paragraphmark#1{}
% \def\subparagraphmark#1{}

% The value of the counter secnumdepth gives the depth of the
% highest-level sectioning command that is to produce section numbers.
%

\setcounter{secnumdepth}{2}

% APPENDIX
%
% The \appendix command must do the following:
%    -- reset the chapter counter to zero
%    -- set \@chapapp to Appendix (for messages)
%    -- redefine the chapter counter to produce appendix numbers
%    -- reset the section counter to zero
%    -- redefine the \chapter command if appendix titles and headings are
%       to look different from chapter titles and headings.
%    -- add a line to table of contents
%       {NB if we printed page numbers this would be wrong}
%    -- \appendix must know whether or not we're in appendices

\newcounter{inappendices}

\def\appendix{\par
  \setcounter{inappendices}{1}
  \setcounter{chapter}{0}
  \setcounter{section}{0}
  \setcounter{subsection}{0}
  \def\@chapapp{APPENDIX:\space}
  \def\thechapter{\Alph{chapter}}
  \addcontentsline{toc}{chapter}{\protect{\numberline{\space}Appendix:}}}

\def\appendices{\par
  \setcounter{inappendices}{1}
  \setcounter{chapter}{0}
  \setcounter{section}{0}
  \setcounter{subsection}{0}
  \def\@chapapp{APPENDIX:\space}
  \def\thechapter{\Alph{chapter}}
  \addcontentsline{toc}{chapter}{\protect{\numberline{\space}Appendices:}}}


%    ****************************************
%    *                LISTS                 *
%    ****************************************
%

% The following commands are used to set the default values for the list
% environment's parameters. See the LaTeX manual for an explanation of
% the meanings of the parameters.  Defaults for the list environment are
% set as follows.  First, \rightmargin, \listparindent and \itemindent
% are set to 0pt.  Then, for a Kth level list, the command \@listK is
% called, where 'K' denotes 'i', 'ii', ... , 'vi'.  (I.e., \@listiii is
% called for a third-level list.)  By convention, \@listK should set
% \leftmargin to \leftmarginK.
%
% For efficiency, level-one list's values are defined at top level, and
% \@listi is defined to set only \leftmargin.

\leftmargini 2.5em
\leftmarginii 2.2em     % > \labelsep + width of '(m)'
\leftmarginiii 1.87em   % > \labelsep + width of 'vii.'
\leftmarginiv 1.7em     % > \labelsep + width of 'M.'
\leftmarginv 1em
\leftmarginvi 1em

\leftmargin\leftmargini
\labelwidth\leftmargini\advance\labelwidth-\labelsep
\labelsep .5em
\parsep 0pt

\def\@listi{\leftmargin\leftmargini}

\def\@listii{\leftmargin\leftmarginii
   \labelwidth\leftmarginii\advance\labelwidth-\labelsep
   \topsep 4.5pt plus 2pt minus 1pt
   \parsep 2pt plus 1pt minus 1pt
   \itemsep \parsep}

\def\@listiii{\leftmargin\leftmarginiii
    \labelwidth\leftmarginiii\advance\labelwidth-\labelsep
    \topsep 2pt plus 1pt minus 1pt 
    \parsep \z@ \partopsep 1pt plus 0pt minus 1pt
    \itemsep \topsep}

\def\@listiv{\leftmargin\leftmarginiv
     \labelwidth\leftmarginiv\advance\labelwidth-\labelsep}

\def\@listv{\leftmargin\leftmarginv
     \labelwidth\leftmarginv\advance\labelwidth-\labelsep}

\def\@listvi{\leftmargin\leftmarginvi
     \labelwidth\leftmarginvi\advance\labelwidth-\labelsep}

%% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


%    ****************************************
%    *                LISTS                 *
%    ****************************************
%

% ENUMERATE
%  Enumeration is done with four counters: enumi, enumii, enumiii
%  and enumiv, where enumN controls the numbering of the Nth level
%  enumeration.  The label is generated by the commands \labelenumi 
%  ... \labelenumiv.  The expansion of \p@enumN\theenumN defines the 
%  output of a \ref command.  

\def\labelenumi{\arabic{enumi}.}    
\def\theenumi{\arabic{enumi}}     
 
\def\labelenumii{(\alph{enumii})}
\def\theenumii{\alph{enumii}}
\def\p@enumii{\theenumi}

\def\labelenumiii{\roman{enumiii}.}
\def\theenumiii{\roman{enumiii}}
\def\p@enumiii{\theenumi(\theenumii)}

\def\labelenumiv{\Alph{enumiv}.}
\def\theenumiv{\Alph{enumiv}}     
\def\p@enumiv{\p@enumiii\theenumiii}

% ITEMIZE
% Itemization is controlled by four commands: \labelitemi, \labelitemii,
% \labelitemiii, and \labelitemiv, which define the labels of the various 
% itemization levels.

\def\labelitemi{$\bullet$}
\def\labelitemii{\bfseries --}
\def\labelitemiii{$\ast$}
\def\labelitemiv{$\cdot$}


% VERSE
%   The verse environment is defined by making clever use of the
%   list environment's parameters.  The user types \\ to end a line.
%   This is implemented by \let'in \\ equal \@centercr.
%
\def\verse{\let\\=\@centercr  
  \list{}{\itemsep\z@ \itemindent -1.5em\listparindent \itemindent 
          \rightmargin\leftmargin\advance\leftmargin 1.5em}\item[]}
\let\endverse\endlist

% QUOTATION
%   Fills lines
%   Indents paragraph
%   
\def\quotation{\list{}{\listparindent 1.5em
    \itemindent\listparindent
    \rightmargin\leftmargin\parsep 0pt plus 1pt}\item[]}
\let\endquotation=\endlist

% QUOTE -- same as quotation except no paragraph indentation,
%
\def\quote{\list{}{\rightmargin\leftmargin}\item[]}
\let\endquote=\endlist

% DESCRIPTION 
%
%  To change the formatting of the label, you must redefine 
%  \descriptionlabel.  

\def\descriptionlabel#1{\hspace\labelsep \bfseries #1}
\def\description{\list{}{\labelwidth\z@ \itemindent-\leftmargin
       \let\makelabel\descriptionlabel}}

\let\enddescription\endlist

\newdimen\descriptionmargin
\descriptionmargin=3em

%    ****************************************
%    *         OTHER ENVIRONMENTS           *
%    ****************************************
%
%
% THEOREM 
% \@begintheorem ... \@endtheorem are the commands executed at the
% beginning and end of a (user-defined) theorem-like environment.
% Except \@opargbegintheorem is executed when an optional argument is
% given.  Cf. LATEX.TEX.
%
% \def\@begintheorem#1#2{\it \trivlist \item[\hskip \labelsep{\bfseries #1\ #2}]}
% \def\@opargbegintheorem#1#2#3{\it \trivlist
%       \item[\hskip \labelsep{\bfseries #1\ #2\ (#3)}]}
% \def\@endtheorem{\endtrivlist}


% TITLEPAGE
%  In the normal environments, the titlepage environment does nothing but 
%  start and end a page, and inhibit page numbers.  It also resets the
%  page number to zero.  In two-column style, it still makes a one-column
%  page.
\def\titlepage{\@restonecolfalse\if@twocolumn\@restonecoltrue\onecolumn
     \else \newpage \fi \thispagestyle{frontpage}\c@page\z@}

\def\endtitlepage{\if@restonecol\twocolumn \else \newpage \fi}

% ARRAY AND TABULAR
%

\arraycolsep 5pt     % Half the space between columns in an array environment.
\tabcolsep 6pt       % Half the space between columns in a tabular environment.
\arrayrulewidth .4pt % Width of rules in array and tabular environment.
\doublerulesep 2pt   % Space between adjacent rules in array or tabular env.

% TABBING
%
\tabbingsep \labelsep   % Space used by the \' command.  (See LaTeX manual.)

% MINIPAGE
%  \@minipagerestore is called upon entry to a minipage environment to
%  set up things that are to be handled differently inside a minipage
%  environment. In the current styles, it does nothing.
%
% \skip\@mpfootins : plays same role for footnotes in a minipage as
%                    \skip\footins does for ordinary footnotes

\skip\@mpfootins = \skip\footins

% FRAMEBOX
%
\fboxsep = 3pt    % Space left between box and text by \fbox and \framebox.
\fboxrule = .4pt  % Width of rules in box made by \fbox and \framebox.


%    ****************************************
%    *        CHAPTERS AND SECTIONS         *
%    ****************************************
%
% DEFINE COUNTERS:
%
% \newcounter{NEWCTR}[OLDCTR] : Defines NEWCTR to be a counter, which is 
%                               reset to zero when counter OLDCTR is stepped.  
%                               Counter OLDCTR must already be defined.

% For any counter CTR, \theCTR is a macro that defines the printed version
% of counter CTR.  It is defined in terms of the following macros:
%
%  \arabic{COUNTER} : The value of COUNTER printed as an arabic numeral.
%  \roman{COUNTER}  : Its value printed as a lower-case roman numberal.
%  \Roman{COUNTER}  : Its value printed as an upper-case roman numberal.
%  \alph{COUNTER}   : Value of COUNTER printed as a lower-case letter:
%                         1 = a, 2 = b, etc.
%  \Alph{COUNTER}   : Value of COUNTER printed as an upper-case letter:
%                           1 = A, 2 = B, etc.
%
%  \arabicz is like \arabic except that 0 -> 0 not null (see latex.tex)
%
\def\arabicz#1{\number\@nameuse{c@#1}}

\def\thepart          {\Roman{part}}
\def\thechapter       {\arabicz{chapter}}
\def\thesection       {\thechapter.\arabicz{section}}
\def\thesubsection    {\thesection.\arabic{subsection}}
\def\thesubsubsection {\thesubsection .\arabic{subsubsection}}
\def\theparagraph     {\thesubsubsection.\arabic{paragraph}}
\def\thesubparagraph  {\theparagraph.\arabic{subparagraph}}
                     
% \@chapapp is initially defined to be null.  The \appendix
% command redefines it to be 'Appendix'.
%
\def\@chapapp{}

% extracted from latex.tex and @svsec changed...
%

% \def\@pxstartsection#1#2#3#4#5#6{\if@noskipsec \leavevmode \fi
%    \par \@tempskipa #4\relax
%    \@afterindenttrue
%    \ifdim \@tempskipa <\z@ \@tempskipa -\@tempskipa \@afterindentfalse\fi
%    \if@nobreak \everypar{}\else
%      \addpenalty{\@secpenalty}\addvspace{\@tempskipa}\fi \@ifstar
%      {\@ssect{#3}{#4}{#5}{#6}}{\@dblarg{\@pxsect{#1}{#2}{#3}{#4}{#5}{#6}}}}

\def\@sect#1#2#3#4#5#6[#7]#8{\ifnum #2>\c@secnumdepth
     \def\@svsec{}\else 
     % was...
     % \refstepcounter{#1}\edef\@svsec{\csname the#1\endcsname\hskip 1em }\fi
     \refstepcounter{#1}\edef\@svsec{\hbox to 1.5cm{\csname the#1\endcsname\hss }}\fi
     \@tempskipa #5\relax
      \ifdim \@tempskipa>\z@ 
        \begingroup #6\relax
          \@hangfrom{\hskip #3\relax\@svsec}{\interlinepenalty \@M #8\par}
        \endgroup
       \csname #1mark\endcsname{#7}\addcontentsline
         {toc}{#1}{\ifnum #2>\c@secnumdepth \else
                      \protect\numberline{\csname the#1\endcsname}\fi
                    #7}\else
        \def\@svsechd{#6\hskip #3\@svsec #8\csname #1mark\endcsname
                      {#7}\addcontentsline
                           {toc}{#1}{\ifnum #2>\c@secnumdepth \else
                             \protect\numberline{\csname the#1\endcsname}\fi
                       #7}}\fi
     \@xsect{#5}}


%    ****************************************
%    *         TABLE OF CONTENTS, ETC.      *
%    ****************************************
%
% A \subsection command writes a 
%       \contentsline{subsection}{TITLE}{PAGE}
% command on the .toc file, where TITLE contains the contents of the
% entry and PAGE is the page number.  If subsections are being numbered,
% then TITLE will be of the form
%       \numberline{NUM}{HEADING}
% where NUM is the number produced by \thesubsection.  Other sectioning
% commands work similarly.  
%
% A \caption command in a 'figure' environment writes
%    \contentsline{figure}{\numberline{NUM}{CAPTION}}{PAGE}
% on the .lof file, where NUM is the number produced by \thefigure and
% CAPTION is the figure caption.  It works similarly for a 'table' environment.
%
% The command \contentsline{NAME} expands to \l@NAME.  So, to specify
% the table of contents, we must define \l@chapter, \l@section, 
% \l@subsection, ... ; to specify the list of figures, we must define
% \l@figure; and so on.  Most of these can be defined with the
% \@dottedtocline command, which works as follows.
%
% \@dottedtocline{LEVEL}{INDENT}{NUMWIDTH}{TITLE}{PAGE}
%    LEVEL    : An entry is produced only if LEVEL < or = value of 
%               'tocdepth' counter.  Note, \chapter is level 0, \section
%               is level 1, etc.
%    INDENT   : The indentation from the outer left margin of the start of 
%               the contents line.
%    NUMWIDTH : The width of a box in which the section number is to go,
%               if TITLE includes a \numberline command.
%   
% This command uses the following three parameters, which are set
% with a \def (so em's can be used to make them depend upon the font).
%   \@pnumwidth : The width of a box in which the page number is put.
%   \@tocrmarg  : The right margin for multiple line entries.  One
%                 wants \@tocrmarg > or = \@pnumwidth
%   \@dotsep    : Separation between dots, in mu units.  Should be \def'd to
%                 a number like 2 or 1.7

\def\@pnumwidth{1.55em}
\def\@tocrmarg {2.55em}
\def\@dotsep{4.5}
\setcounter{tocdepth}{0}
\def\@toccmd{\tableofcontents}
\def\@contentsinside{0}
\def\@appA{0}
\def\@appB{0}
\def\@appC{0}

% TABLEOFCONTENTS
%  In report style, \tableofcontents, \listoffigures, etc. are always
%  set in single-column style.  @restonecol


\def\tableofcontents{\@restonecolfalse\if@twocolumn\@restonecoltrue\onecolumn
  \fi
  \ifnum\@contentsinside=1\parskip 0pt 
  \pagebreak\chapter{Table Of Contents}
  \@starttoc{toc}\if@restonecol\twocolumn\fi\parskip 10pt
  \else\@starttoc{toc}\if@restonecol\twocolumn\fi\fi}

\def\l@part#1#2{\addpenalty{-\@highpenalty}
   \addvspace{2.25em plus 1pt}  % space above part line
   \begingroup
   \@tempdima 3em         % width of box holding part number, used by 
     \parindent \z@ \rightskip \@pnumwidth             %% \numberline
     \parfillskip -\@pnumwidth   
     {\large \bfseries          % set line in \large boldface
     \leavevmode          % TeX command to enter horizontal mode.       
     #1\hfil \hbox to\@pnumwidth{\hss #2}}\par
     \nobreak             % Never break after part entry
   \endgroup}


\def\l@chapter#1#2{ % \pagebreak[3] 
% arg 1 is chapter name, arg 2 is page number
   \ifnum\@contentsinside=1
   \vskip 1.0em plus 1pt  % space above chapter line
   \fi
   \@tempdima 1.5em       % width of box holding chapter number
   \begingroup
     \parindent \z@ \rightskip \@pnumwidth 
     \parfillskip -\@pnumwidth   
%     \bfseries                  % Boldface.
     \leavevmode          % TeX command to enter horizontal mode.       
%     #1\hfil \hbox to\@pnumwidth{\hss #2}\par
     #1\hfil \hbox to\@pnumwidth{\hss}\par
   \endgroup}
      
\def\l@section{\@dottedtocline{1}{1.5em}{2.3em}}
\def\l@subsection{\@dottedtocline{2}{3.8em}{3.2em}}
\def\l@subsubsection{\@dottedtocline{3}{7.0em}{4.1em}}
\def\l@paragraph{\@dottedtocline{4}{10em}{5em}}
\def\l@subparagraph{\@dottedtocline{5}{12em}{6em}}

% LIST OF FIGURES
%
\def\listoffigures{\@restonecolfalse\if@twocolumn\@restonecoltrue\onecolumn
  \fi\chapter*{List of Figures\@mkboth
   {LIST OF FIGURES}{LIST OF FIGURES}}\@starttoc{lof}\if@restonecol\twocolumn
  \fi}

\def\l@figure{\@dottedtocline{1}{1.5em}{2.3em}}

% LIST OF TABLES
%
\def\listoftables{\@restonecolfalse\if@twocolumn\@restonecoltrue\onecolumn
  \fi\chapter*{List of Tables\@mkboth
   {LIST OF TABLES}{LIST OF TABLES}}\@starttoc{lot}\if@restonecol\twocolumn
  \fi}

\let\l@table\l@figure



%    ****************************************
%    *             BIBLIOGRAPHY             *
%    ****************************************
%
% The thebibliography environment executes the following commands:
%
%  \def\newblock{\hskip .11em plus .33em minus -.07em} --
%      Defines the `closed' format, where the blocks (major units of 
%      information) of an entry run together.
%
%  \sloppy  -- Used because it's rather hard to do line breaks in 
%      bibliographies,
% 
%  \sfcode`\.=1000\relax --
%      Causes a `.' (period) not toproduce an end-of-sentence space.

\def\thebibliography#1{\list
  {\arabic{enumi}}{\settowidth\labelwidth{#1}\leftmargin\labelwidth
    \advance\leftmargin\labelsep
    \usecounter{enumi}}
    \def\newblock{\hskip .11em plus .33em minus -.07em}
    \sloppy
    \sfcode`\.=1000\relax}

\let\endthebibliography=\endlist

% \def\@biblabel#1{[#1]\hfill}  % Produces the label for a \bibitem[...]
                                % command. 
% \def\@cite#1{[#1]}            % Produces the output of the \cite command.



%    ****************************************
%    *              THE INDEX               *
%    ****************************************
%
% THE THEINDEX ENVIRONMENT
% Produces double column format, with each paragraph a separate entry.
% The user commands \item, \subitem and \subsubitem are used to
% produce the entries, and \indexspace adds an extra vertical space
% that's the right size to put above the first entry with a new letter
% of the alphabet.
%
% Index changes for Sholis format:
%       1. single column as names tend to be very long;
%       2. made a numbered section so it appears in contents;
%       3. removed explicit page headers and footers.

\newif\if@restonecol

\def\theindex{\parindent\z@
 \parskip\z@ plus .3pt\relax\let\item\@idxitem}

\def\@idxitem{\par\hangindent 40pt}
\def\subitem{\par\hangindent 40pt \hspace*{20pt}}
\def\subsubitem{\par\hangindent 40pt \hspace*{30pt}}
\def\endtheindex{}
\def\indexspace{\par \vskip 10pt plus 5pt minus 3pt\relax}


%    ****************************************
%    *             FOOTNOTES                *
%    ****************************************
%
% \footnoterule is a macro to draw the rule separating the footnotes from
% the text.  It should take zero vertical space, so it needs a negative
% skip to compensate for any positive space taken by the rule.  (See
% PLAIN.TEX.)

\def\footnoterule{\kern-3\p@   
  \hrule width .4\columnwidth 
  \kern 2.6\p@}                 % The \hrule has default height of .4pt .

% \newcounter{footnote}
\@addtoreset{footnote}{chapter}  % Numbers footnotes within chapters

%   \@makefntext{NOTE} :
%        Must produce the actual footnote, using \@thefnmark as the mark 
%        of the footnote and NOTE as the text.  It is called when effectively
%        inside a \parbox of width \columnwidth (i.e., with \hsize = 
%        \columnwidth).  
%
%        The following macro indents all lines of the footnote by 10pt, and 
%        indents the first line of a new paragraph by 1em.  To change these 
%        dimensions, just substitute the desired value for '10pt' [in both 
%        places] or '1em'.  The mark is flushright against the footnote. 
%          \long\def\@makefntext#1{\@setpar{\@@par\@tempdima \hsize 
%             \advance\@tempdima-10pt\parshape \@ne 10pt \@tempdima}\par
%             \parindent 1em\noindent \hbox to \z@{\hss$^{\@thefnmark}$}#1}
%
%        A simpler macro is used, in which the footnote text is
%        set like an ordinary text paragraph, with no indentation except
%        on the first line of a paragraph, and the first line of the 
%        footnote.  Thus, all the macro must do is set \parindent
%        to the appropriate value for succeeding paragraphs and put the 
%        proper indentation before mark.

\long\def\@makefntext#1{\parindent 1em\noindent 
            \hbox to 1.8em{\hss$^{\@thefnmark}$}#1}

% \@makefnmark : A macro to generate the footnote marker that goes
%    in the text.  Default used.
%



%    ****************************************
%    *         FIGURES AND TABLES           *
%    ****************************************
% 
% Float placement parameters.  See LaTeX manual for their definition.
%
\setcounter{topnumber}{2}
\def\topfraction{.7}
\setcounter{bottomnumber}{1}
\def\bottomfraction{.3}
\setcounter{totalnumber}{3}
\def\textfraction{.2}
\def\floatpagefraction{.5}
\setcounter{dbltopnumber}{2}
\def\dbltopfraction{.7}
\def\dblfloatpagefraction{.5}

% \@makecaption{NUMBER}{TEXT} : Macro to make a figure or table caption.  
%      NUMBER : Figure or table number--e.g., 'Figure 3.2' 
%      TEXT   : The caption text.
%  Macro should be called inside a \parbox of right width, with \normalsize.

\long\def\@makecaption#1#2{
   \vskip 10pt 
   \setbox\@tempboxa\hbox{#1: #2}
   \ifdim \wd\@tempboxa >\hsize   % IF longer than one line:
       \unhbox\@tempboxa\par      %   THEN set as ordinary paragraph.
     \else                        %   ELSE  center.
       \hbox to\hsize{\hfil\box\@tempboxa\hfil}  
   \fi}

% To define a float of type TYPE (e.g., TYPE = figure), the document style 
% must define the following.
%
%  \fps@TYPE   : The default placement specifier for floats of type TYPE.
%
%  \ftype@TYPE : The type number for floats of type TYPE.  Each TYPE has
%                associated a unique positive TYPE NUMBER, which is a power 
%                of two.  E.g., figures might have type number 1, tables type 
%                number 2, programs type number 4, etc.
%
%  \ext@TYPE   : The file extension indicating the file on which the 
%                contents list for float type TYPE is stored.  For example, 
%                \ext@figure = 'lof'.
%
%  \fnum@TYPE  : A macro to generate the figure number for a caption.
%                For example, \fnum@TYPE == Figure \thefigure.
%
%  The actual float-making environment commands--e.g., the commands
%  \figure and \endfigure--are defined in terms of the macros \@float
%  and \end@float, which are described below.
%
%  \@float{TYPE}[PLACEMENT] : Macro to begin a float environment for a 
%     single-column float of type TYPE with PLACEMENT as the placement 
%     specifier.  The default value of PLACEMENT is defined by \fps@TYPE.   
%     The environment is ended by \end@float.  
%     E.g., \figure == \@float{figure}, \endfigure == \end@float.


% FIGURE

\def\thefigure{\thechapter.\@arabic\c@figure}

\def\fps@figure{tbp}
\def\ftype@figure{1}
\def\ext@figure{lof}
\def\fnum@figure{Figure \thefigure}
\def\figure{\@float{figure}}
\let\endfigure\end@float
\@namedef{figure*}{\@dblfloat{figure}}
\@namedef{endfigure*}{\end@dblfloat}

% TABLE
%
\def\thetable{\thechapter.\@arabic\c@table}

\def\fps@table{tbp}
\def\ftype@table{2}
\def\ext@table{lot}
\def\fnum@table{Table \thetable}
\def\table{\@float{table}}
\let\endtable\end@float
\@namedef{table*}{\@dblfloat{table}}
\@namedef{endtable*}{\end@dblfloat}


%    ****************************************
%    *         TITLE AND ABSTRACT           *
%    ****************************************
%
%

% \def\maketitle{\begin{titlepage}
%  \let\footnotesize\small      % Local definitions to make \thanks produce
%  \let\footnoterule\relax      % \normalsize footnotes with no separating rule.
%  \setcounter{page}{0}
%  \null
%  \vfil
%  \vskip 60pt                   % To adjust centering.
%  \begin{center}
%    {\LARGE \@title \par}        % Set title in \LARGE size.
%    \vskip 3em                  % Vertical space after title.
%    {\large                     % Set author in \large size.
%      \lineskip .75em
%      \begin{tabular}[t]{c}\@author 
%      \end{tabular}\par}      
%     \vskip 1.5em               % Vertical space after author.
%    {\large \@date \par}        % Set date in \large size.
% \end{center} \par
% \tableofcontents      % YOW!!
% \vfil
% \@thanks
% \vfil
% \null
% \end{titlepage}
% \setcounter{footnote}{0}       % Footnotes start at zero again.
% \let\thanks\relax
% \gdef\@thanks{}\gdef\@author{}\gdef\@title{}\let\maketitle\relax}    
% 
% \def\abstract{\titlepage
% \null\vfil
% \begin{center}
% {\bfseries Abstract}
% \end{center}}
% 
% \def\endabstract{\par\vfil\null\endtitlepage}

\def\maketitle{\begin{titlepage}
\setcounter{page}{1}    % yes! the Praxis frontsheet is page 1.
\vbox to 0.2in{}
\hbox to \textwidth{\hbox to 110pt{\bfseries Document Set\hfil}\@documentset
  \hfil Reference \@reference}

\begin{list}{}{
\parsep=0pt
\itemsep=18pt plus 2pt
\leftmargin=1.5in
\labelwidth=1.25in
\labelsep=0.25in    % NB labelwidth+labelsep = leftmargin
\rightmargin=0.5in
}

\def\makelabel##1{\hbox to 1.25in{\bfseries##1\hfil:}}

% \item[Document Set]
% \begin{tabbing}
% \@documentset \= \` {\bfseries Reference} \@reference
% \end{tabbing}

\item[Title]
\@title

\item[Synopsis]
{\raggedright \@synopsis}

\item[Contents]
\begin{minipage}[t]{100mm}
% the \vtop is necessary to avoid misalignment of Contents: and
% 0 Document Control by about half a line ...
\vtop{\@toccmd}
\end{minipage}

\item[Status]
\@status

\item[Issue Number]
\@issuenumber

\item[Date]
\@date

\item[Copied To] 
\begin{minipage}[t]{4.0in}
\@copiedto
\end{minipage}

\item[Front Sheet]
\begin{minipage}[t]{216pt}
\@frontsheet
\end{minipage}

\item[\@originator]
\begin{minipage}[t]{300pt}
\hbox{\hbox to 145pt{\vtop{\@author}\hss}\vtop{
  \hbox to 80pt{\bfseries Signed\hfil:}}}
\end{minipage}

\ifnum\@appA=1%
\item[\@applabelA]
\begin{minipage}[t]{300pt}
\hbox{\hbox to 145pt{\vtop{\@approverA}\hss}\vtop{
  \hbox to 80pt{\bfseries Approved\hfil:}}}
\end{minipage}
\fi
\ifnum\@appB=1%
\item[\@applabelB]
\begin{minipage}[t]{300pt}
\hbox{\hbox to 145pt{\vtop{\@approverB}\hss}\vtop{
  \hbox to 80pt{\bfseries Approved\hfil:}}}
\end{minipage}
\fi
\ifnum\@appC=1%
\item[\@applabelC]
\begin{minipage}[t]{300pt}
\hbox{\hbox to 145pt{\vtop{\@approverC}\hss}\vtop{
  \hbox to 80pt{\bfseries Approved\hfil:}}}
\end{minipage}
\fi

\end{list}

\end{titlepage}
\setcounter{footnote}{0}       % Footnotes start at zero again.
}


%    ****************************************
%    *            PAGE STYLES               *
%    ****************************************
%
% The page style 'foo' is defined by defining the command \ps@foo.  This
% command should make only local definitions.  There should be no stray
% spaces in the definition, since they could lead to mysterious extra
% spaces in the output.
%
% The \ps@... command defines the macros \@oddhead, \@oddfoot,
% \@evenhead, and \@evenfoot to define the running heads and
% feet---e.g., \@oddhead is the macro to produce the contents of the
% heading box for odd-numbered pages.  It is called inside an \hbox of
% width \textwidth.
%
% To make headings determined by the sectioning commands, the page style
% defines the commands \chaptermark, \sectionmark, ... , where
% \chaptermark{TEXT} is called by \chapter to set a mark, and so on.
% The \...mark commands and the \...head macros are defined with the
% help of the following macros.  (All the \...mark commands should be
% initialized to no-ops.)
%
% MARKING CONVENTIONS:
% LaTeX extends TeX's \mark facility by producing two kinds of marks
% a 'left' and a 'right' mark, using the following commands:
%     \markboth{LEFT}{RIGHT} : Adds both marks.
%     \markright{RIGHT}      : Adds a 'right' mark.
%     \leftmark  : Used in the \@oddhead, \@oddfoot, \@evenhead or \@evenfoot
%                  macro, gets the current 'left'  mark.  Works like TeX's 
%                  \botmark command.
%     \rightmark : Used in the \@oddhead, \@oddfoot, \@evenhead or \@evenfoot
%                  macro, gets the current 'right'  mark.  Works like TeX's 
%                  \firstmark command.
% The marking commands work reasonably well for right marks 'numbered 
% within' left marks--e.g., the left mark is changed by a \chapter command and
% the right mark is changed by a \section command.  However, it does
% produce somewhat anomalous results if two \bothmark's occur on the same page.
% 
% Commands like \tableofcontents that should set the marks in some page styles
% use a \@mkboth command, which is \let by the pagestyle command (\ps@...)
% to \markboth for setting the heading or \@gobbletwo to do nothing.

\mark{{}{}}   % Initializes TeX's marks

% \ps@empty and \ps@plain defined in LATEX.TEX

% Definition of 'praxis' page style 
%

\if@twoside         % If two-sided printing.
\def\ps@praxis{\let\@mkboth\markboth
\def\@oddfoot{\bfseries\@confidentiality}\def\@evenfoot{\bfseries\@confidentiality}%       No feet.
\def\@evenhead{\rm \thepage\hfil \sl \leftmark}%        Left heading.
\def\@oddhead{\hbox{}\sl \rightmark \hfil \rm\thepage}% Right heading.
\def\chaptermark##1{\markboth {\uppercase{\ifnum \c@secnumdepth >\m@ne
      \@chapapp\ \thechapter. \ \fi ##1}}{}}%
\def\sectionmark##1{\markright {\uppercase{\ifnum \c@secnumdepth >\z@
   \thesection. \ \fi ##1}}}}
\else               % If one-sided printing.
\def\ps@praxis{\let\@mkboth\markboth
\def\@oddfoot{\bfseries\hfil\@confidentiality\hfil}
\def\@evenfoot{}
\def\@oddhead{%
\parbox[t]{1.1in}{%
\em Praxis\hfil\\High Integrity\hfil\\Systems
\hfil}%
\parbox[t]{3.15in}{\raggedright\@documentset\\\@title}%
\parbox[t]{1.75in}{\raggedright Reference \@reference\\ Issue \@issuenumber\\ Page \thepage}\hfil}
% The final \hfil wouldn't be necessary if TeX did its arithemtic better!

\def\chaptermark##1{\markright {\uppercase{\ifnum \c@secnumdepth >\m@ne
  \@chapapp\ \thechapter. \ \fi ##1}}}}
\fi


%  `frontpage' style
%

\def\ps@frontpage{\let\@mkboth\@gobbletwo
     \def\@oddhead{\parbox[t]{23mm}
                   {\em Praxis\hfil\\High Integrity\hfil\\Systems\hfil}\hfil}
     \def\@oddfoot{\bfseries\hfil\@confidentiality\hfil}
     \def\@evenhead{}\let\@evenfoot\@oddfoot}


%    ****************************************
%    *            MISCELLANEOUS             *
%    ****************************************
%
% DATE
%
% Changed for SHOLIS to get right format and correct day extension
\def\today{\number\day\ifnum\day=1 st\else\ifnum\day=21 st\else
                      \ifnum\day=31 st\else
                      \ifnum\day=2 nd\else\ifnum\day=22 nd\else
                      \ifnum\day=3 rd\else\ifnum\day=23 rd\else th
                      \fi\fi\fi\fi\fi\fi\fi\space
           \ifcase\month\or January\or February\or March\or April
           \or May\or June\or July\or August\or September\or October
           \or November\or December\fi\space\number\year}

% EQUATION and EQNARRAY -- put here because it must follow \chapter definition
%
% \newcounter{equation}    
%
\@addtoreset{equation}{chapter}   % Makes \chapter reset 'equation' counter.

\def\theequation{\thechapter.\arabic{equation}}

% \jot = 3pt      % Extra space added between lines of an eqnarray environment

% The macro \@eqnnum defines how equation numbers are to appear in equations.
%
% \def\@eqnnum{(\theequation)} 
% 


%    ****************************************
%    *           PRAXIS STUFF               *
%    ****************************************
%
\newcommand{\documentset}[1]{\def\@documentset{#1}}
\newcommand{\reference}[1]{\def\@reference{#1}}
\newcommand{\synopsis}[1]{\def\@synopsis{#1}}
\newcommand{\status}[1]{\def\@status{#1}}
\newcommand{\issuenumber}[1]{\def\@issuenumber{#1}}
\newcommand{\copiedto}[1]{\def\@copiedto{#1}}
\newcommand{\frontsheet}[1]{\def\@frontsheet{#1}}
\newcommand{\originator}[1]{\def\@originator{Originator}\author{#1}}
\newcommand{\originators}[1]{\def\@originator{Originators}\author{#1}}
\newcommand{\confidentiality}[1]{\def\@confidentiality{#1}}
\newcommand{\approverA}[2]{\def\@applabelA{#1}\def\@approverA{#2}\def\@appA{1}}
\newcommand{\approverB}[2]{\def\@applabelB{#1}\def\@approverB{#2}\def\@appB{1}}
\newcommand{\approverC}[2]{\def\@applabelC{#1}\def\@approverC{#2}\def\@appC{1}}

\newcommand{\documentcontrol}{\setcounter{chapter}{-1}\chapter{Document Control}}
\newcommand{\contentsinside}{\setcounter{tocdepth}{1}\def\@contentsinside{1}\def\@toccmd{See
table of contents}}

%    ****************************************
%    *           INITIALIZATION             *
%    ****************************************
%
% Default initializations

\ps@praxis                  % 'praxis' page style
\pagenumbering{arabic}      % Arabic page numbers
\onecolumn                  % Single-column.
\if@twoside\else\raggedbottom\fi % Ragged bottom unless twoside option.

\documentset{Unknown}
\reference{Unknown}
\synopsis{}
\status{Draft}
\issuenumber{0.0}
\copiedto{Nobody}
\frontsheet{}
\originator{J Random Nerd}
\confidentiality{}

% INTERACTIVE FILE INCLUSION FOR LARGE DOCUMENTS
% 
\newcommand{\AskForIncludes}[1]{%
\typeout{ }
\typein[\filestoinclude]{Files to include? (comma-separated list, or ALL)}
\ifthenelse{\equal{\filestoinclude}{ALL}}
  {}
  {\includeonly{#1\filestoinclude}}}

