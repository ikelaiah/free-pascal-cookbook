# Math with LMath

!!! Tip "Credit"
    Huge thanks to Viatcheslav Nesterov (Glorfin) for maintaining **LMath** package for Free Pascal and Lazarus users.


!!! Info
    The content of this page is adapted from the official document on [Sourceforge](https://sourceforge.net/projects/lmath-library/files/DOC/LMath06/).

## Introduction

> LMath, further development of DMath library from Jean Debord, is a general purpose mathematical library for FreePascal (FPC) and Lazarus. LMath provides routines and demo programs for numerical analysis, including mathematical functions, probabilities, matrices, optimization, linear and nonlinear equations, integration, Fast Fourier Transform, random numbers, curve fitting, statistics and graphics. It is organized as a set of lazarus packages. Such organization makes it easily extensible and helps to include only really needed features in your project. 
> 
> DMath stands for Delphi Math, and is a continuation of an earlier work which was named TPMath, for Turbo Pascal Math. Continuing this tradition, this this
library is called LMath: Lazarus Math.
> 
> Source: [LMath 0.6 Official Doc](https://sourceforge.net/projects/lmath-library/files/DOC/LMath06/).

Check the [official document](https://sourceforge.net/projects/lmath-library/files/DOC/LMath06/) for what is new in LMath compared to DMath.

License: LGPL 3.0

### What does it offer?

LMath has 13 relatively small packages:

1. **`lmGenMath`**. This package defines several important data structures, used later in the whole library, some utility functions, basic math functions and special functions. All other packages of the library depend on `lmGenMath`.
2. **`lmMathUtil`**. Various function for manipulations with arrays, sorting and formatting. Depends on `lmGenMath`.
3. **`lmLinearAlgebra`**. Operations over vectors and matrices. Depends on `lmMathUtil` and `lmGenMath`.
4. **`lmPolynoms`**. Evaluation of polynomials, polynomial roots finding, polynomial critical points finding. Depends on `lmGenMath` and `lmLinearAlgebra`.
5. **`lmIntegrals`**. Numeric integrating and solving differential equations. Depends on `lmGenMath`.
6. **`lmRandoms`**. Generation of random numbers. Depends on `lmGenMath`.
7. **`lmMathStat`** Descriptive statistics, hypothesis testing, collection of various distributions. Depends on `lmGenMath` and `lmLinearAlgebra`.
8. **`lmOptimum`**. Algorithms of function minimization. Somewhat artificially, unit `uEval` for evaluation of an expression, is included into this package. Depends on `lmGenMath`, `lmLinearAlgebra`, `lmRandoms`, `lmMathStat`.
9. **`lmNonLinEq`**. Finding of roots of non-linear equations. Depends on `lmGenMath`, `lmLinearAlgebra`, `lmOptimum`.
10. **`lmDSP`**. Functions for digital signal processing. Collection of filters and Fourier Transform procedures.
11. **`lmRegression`**. Functions for linear and non-linear regression, curve fitting. Collection of common models. Unit `uFFT` for fast Fourier Transform is located also here. Depends on `lmLinearAlgebra`, `lmPolynoms`, `lmOptimum`, `lmMathUtil`.
12. **`lmSpecRegress`**. Collection of field-specific models for data fitting. Depends on `lmGenMath` and `lmRegression`.
13. **`lmPlotter`**. Routines for data and functions plotting. Depends on `lmGenMath`, `lmMathUtil` and `LCL`.


## Installation, Compilation and use of the library

### Option #1: Download, unpack and compile

1. Download and unpack file `LMath and Components06.zip`. 
2. Open and compile packages in this order; `lmGenMath`, `lmMathUtil`, `lmLinearAlgebra`, `lmPolynoms`, `lmIntegrals`, `lmRandoms`, `lmMathStat`, `lmOptimum`, `lmNonLinEq`, `lmRegression`, `lmSpecRegress`, `lmDSP`, `lmPlotter`. 
3. After it you may compile `LMath.pkg`, which simply depends on all these packages. Therefore, if you add dependency on LMath
to your project, it is not necessary to add every single package.

### Option #2: Download, use the Project Group package

Alternatively, if you have `Project Groups` package installed in your Lazarus IDE, you can open and compile LMath project group, select LMath target and compile it.

### LMComponents

If you are going to use `LMComponents`, open `LMComponents` package, compile and install it.

`LMComponents` is an object-oriented extension of `LMath`. It contains `TCoordSys` component which serves for graphical representation of functions or data, several dialogs and input controls, as well as DSP filters implemented as components.

### Note on Compilation

For the sake of maximally broad compatibility, rather conservative optimization options are selected in LMath packages (optimization level 2, no options for modern processors selected). Depending on your system, you may want to increase this level and use options for modern processors. 

For example, you may want to define the following to effectively use AVX2 registers.

```
-CfAVX2
-CpCOREAVX2
-OoFASTMATH
-OoLOOPUNROLL
```

**Do not generate debug information** is selected by default in the options of LMath packages. If you want to step into LMath procedures debugging your programs, change it. If you find a bug in LMath, please, report it at the official site at Sourseforge.

## Examples

Coming soon.