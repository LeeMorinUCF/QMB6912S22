# Automatic Document Generation


## File Organization

Some analysts follow the "kitchen-sink" approach to organization of files within a single folder. 
But since we're generating the documents automatically, why not be more organized? 
After all, it takes a little bit of the OCD personality to write documents this way in the first place. 
A sensible approach is to divide files into the following headings: 

- Code
- Data
- Figures
- Paper
- Tables
- Text

This system of file organization is used in the following examples. 

### Example

The script ```Paper.tex``` contains all of these components.
To build the ```pdf``` file from these scripts, 
you will have to run a program such as ```pdflatex```. 

```
pdflatex Paper.tex
pdflatex Paper.tex
```

This will produce a ```pdf``` document called ```Paper.pdf```, along with some other intermediate files
that are sometimes useful for troubleshooting
(although Google is often more useful for that). 

The command is shown twice because there are references to the tables,
figures and equations in the script. 
The first time it will produce a document with question marks 
in place of all the reference numbers. 
After this accounting exercise, the numbers are filled in
in the second pass. 


## Putting it All Together:

The shell script ```DoWork.sh``` fully automates the 
document-generation process by also running
the code that generates tables and figures for a document
and then builds the pdf document. 

In a stepwise format, the instructions are as follows.


1. Run ```DoWork.sh``` in a terminal window from within this folder. 
This program generates a pdf document from automated data analysis. 
1. This shell script calls the ```R``` program in the ```Code``` folder, 
which creates tables, figures and text for the document, 
by analyzing a dataset read in from ```Data```. 
1. At the end of the shell script, a call to ```pdflatex``` is placed 
to build a pdf document from the ```TeX``` script in the ```Paper``` folder. 
1. The completed paper will appear as the file ```Paper.pdf``` 
within the ```Paper``` folder. 
