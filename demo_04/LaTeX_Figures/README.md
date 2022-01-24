# Automatic Document Generation


## Figures

The notation for including figures uses the... ```figure``` environment. 
To display figures, you need to declare the ```graphicx``` package in the preamble 
of the markup script (before the ```\begin{document}``` command). 
Depending on the platform, you might also have to specify how to
translate images in ```eps``` format into ```pdf``` format. 
Using ```eps``` figures is worthwhile because they contain text code for making quick adjustments and the images are rendered more clearly, since
no translation of the images is required. 
To handle cases on different platforms, you can use ```if``` statements, 
much like you can in other languages. 


We add a figure to a script called ```Paper_2.tex```.

```
\documentclass[11pt]{article}

\ifx\pdftexversion\undefined
    \usepackage[dvips]{graphicx}
\else
    \usepackage[pdftex]{graphicx}
    \usepackage{epstopdf}
    \epstopdfsetup{suffix=}
\fi

\begin{document}
This is my document.

In Figure \ref{fig:example}, there is an image. 

\begin{figure}
\centering
\includegraphics[width=\textwidth]{../Figures/name_of_figure.eps}
\caption{Caption Goes Here}
\label{fig:example}
\end{figure}

\end{document}
```

### Code Snippets

You can also display the commands you used to generate some output, 
which might be useful to help explain your analysis. 
First, you have to declare a few more packages 
in the preamble before the ```\begin{document}``` command. 

```
% Packages for displaying code:
\usepackage{listings}
\usepackage{textcomp}
\usepackage{color}

% Color settings used in the code below:
\definecolor{dkgreen}{rgb}{0,0.6,0}
\definecolor{gray}{rgb}{0.5,0.5,0.5}
\definecolor{mauve}{rgb}{0.58,0,0.82}

% Settings for the formatting of the code on display:
\lstset{frame=tb,
  language=R,
  aboveskip=3mm,
  belowskip=3mm,
  showstringspaces=false,
  columns=flexible,
  basicstyle={\small\ttfamily},
  numbers=none,
  numberstyle=\tiny\color{gray},
  keywordstyle=\color{blue},
  commentstyle=\color{dkgreen},
  stringstyle=\color{mauve},
  breaklines=true,
  breakatwhitespace=true,
  tabsize=3
}
```
The first is the set of packages to display code, 
the main one being ```listings```. 
The rest are for customized settings, such as the color
and format of the syntax within the code.

A block of R code is displayed with the following commands.

```
\begin{lstlisting}[language=R]
R> # Generate a random variable.
    epsilon <- rnorm(1000)
\end{lstlisting}
```


This is all collected with an example of a figure
in the script ```Paper_w_Figure.tex```. 



