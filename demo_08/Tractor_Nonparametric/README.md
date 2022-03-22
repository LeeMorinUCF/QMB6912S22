# Nonparametric Estimation

*Nonparametric methods* are used to fit statistical models 
with arbitrary forms of nonlinearity.
We will discuss two forms: kernel smoothing and tree methods. 
Tree methods are often used to determine 
which variables should be in the model:
methods for *dimension reduction*.  
In contrast, kernel smoothing methods are often used 
with a small set of candidate variables
to fine-tune the way the variables fit in the model:
a method for choosing the *functional form* of a regression model. 
Strictly speaking, this produces 
a type of *semiparametric* model: 
it has a linear specification that includes
a variable that is a nonparametric fit of a variable, 
thus it is part parametric and part nonparametric. 


## Instructions:

Run the script ```DoWork.sh```, which performs the following:

1. Run the script ```Tractor_Nonparametric..R```
which reads the dataset ```TRACTOR7.csv```
and estimates semiparametric nonlinear regression models,
saving the results in ```.pdf``` files in the 
```Figures``` folder and tables of regression results
in ```tex``` files in the ```Tables``` folder.

1. Run the command ```pdflatex```
on the ```.tex``` script ```Tractor_Nonparametric..tex```
in the ```Paper``` folder.
This generates the document ```Tractor_Nonparametric..pdf```,
which is saved in the ```Paper``` folder.
