# Numerical with NumLib

The notes on this page is based on [Free Pascal NumLib official doc](https://wiki.freepascal.org/NumLib_Documentation).

!!! Tip "Credits"

    Thanks to Kees van Ginneken, Wil Kortsmit and Loek van Reij of the Computational centre of the Eindhoven University of Technology for making it available for the Free Pascal project.

    Thanks to Marco van de Voort ([marco@freepascal.org](mailto:marco@freepascal.org)) and Michael van Canneyt ([michael@freepascal.org](mailto:michael@freepascal.org)) for porting to FPC and documenting NumLib.


## Summary

| Unit  | Routine  | Operation      | Expression / Note                                    |
|-------| ---------| -------------- | ---------------------------------------------------- |
| `omv` | `omvinp`  | Dot product    | $\mathbf{a} \cdot \mathbf{b} = \sum_{i=1}^n a_i b_i = a_1 b_1 + a_2 b_2 + \dots + a_n b_n$ |
| `omv` | `omvmmm` | Product of two matrices | $C_{ij} = \sum_{k=0}^n A_{ik} B_{kj}$       |
| `omv` | `omvmmv` | Product of a matrix and a vector | \(\mathbf{c} = A\ \mathbf{b} = \left[ \begin{array}{cccc} a_{11} & a_{12} & a_{13} & \ldots & a_{1n} \\ a_{21} & a_{22} & a_{23} & \ldots & a_{2n} \\ \vdots & \vdots & \vdots & \ddots & \vdots \\ a_{m1} & a_{m2} & a_{m3} & \ldots & a_{mn} \end{array} \right] \left[ \begin{array}{c} b_1 \\ b_2 \\ b_3 \\ \vdots \\ b_n \end{array} \right] = \left[ \begin{array}{c} a_{11}b_1 + a_{12}b_2 + a_{13}b_3 + \cdots + a_{1n}b_n \\ a_{21}b_1 + a_{22}b_2 + a_{23}b_3 + \cdots + a_{2n}b_n \\ \vdots \\ a_{m1}b_1 + a_{m2}b_2 + a_{m3}b_3 + \cdots + a_{mn}b_n \end{array} \right]\) | `omvmmv` | `omv` |
| `omv` | omvtrm| Transpose matrix | $A = \left[ \begin{array}{cccc} a_{11} & a_{12} & a_{13} & \ldots & a_{1n} \\ a_{21} & a_{22} & a_{23} & \ldots & a_{2n} \\ \vdots & \vdots & \vdots & \ddots & \vdots \\ a_{m1} & a_{m2} & a_{m3} & \ldots & a_{mn} \end{array} \right] \quad A^T = \left[ \begin{array}{cccc} a_{11} & a_{21} & \ldots & a_{m1} \\ a_{12} & a_{22} & \ldots & a_{m2} \\ a_{13} & a_{23} & \ldots & a_{m3} \\ \vdots & \vdots & \ddots & \vdots \\ a_{1n} & a_{2n} & \ldots & a_{mn} \end{array} \right]$ |
| `omv` | `omvn1v` | 1-norm of a vector | $\|a\|_1 = \sum_{i=1}^n &#124;{a_i}&#124;$ |
| `omv` | `omvn2v` | 2-norm of a vector | $\|a\|_2 = \sqrt{\sum_{i=1}^n {a_i}^2}$ |
| `omv` | `omvnmv` | Maximum infinite norm of a vector| $\|a\|_\infty = \max({a_1}, {a_2}, ... {a_n})$ |
| `omv` | `omvn1m` | 1-norm of a matrix | $\|M\|_1 = \max_{1 \le j \le {n}} \sum_{i=1}^m&#124;M_{ij}&#124;$ |
| `omv` | `omvnmm` | Maximum infinite norm of a matrix | $\|M\|_\infty = \max_{1 \le i \le\ m} \sum_{j=1}^n &#124;M_{ij}&#124;$ |
| `omv` | `omvnfm` | Frobenius norm of a matrix | $\|M\|_F = \sqrt{\sum_{i=1}^m \sum_{j=1}^n {M_{ij}}^2}$ |
| `det` | `detgen`, `detgsy`, `detgpd` | Determinant of a standard matrix | |
| `det` | `detgba` | Determinant of a band matrix     | |
| `det` | `detgpb` | Determinant of a symmetric positive definite band matrix | |
| `det` | `detgtr` | Determinant of a tridiagonal matrix | | 
| `inv` | `invgen`, `invgsy`, `invgpd` | Inverse of a matrix  | |
| `sle` | `slegen`, `slegsy`, `slegpd` | Solve linear equations - Square matrices | |
| `sle` | `slegba` | Solve linear equations - Band matrix | |
| `sle` | `slegpb` | Solve linear equations - symmetric positive definite band matrix | Optimised approach for a [symmetric positive band matrix](https://wiki.freepascal.org/NumLib_Documentation#Symmetric_positive_definite_band_matrix) |
| `sle` | `sledtr`, `slegtr` | Solve linear equations - Tridiagonal matrix | |
| `sle` | `slegls` | Solve linear equations - Least squares | Solves linear systems of a rectangular matrix (has more equations than unknowns). |
| `eig` | `...` | ... | |
| `roo` | `...` | ... | |
| `roo` | `...` | ... | |
| `roo` | `...` | ... | |
| `roo` | `...` | ... | |
| `roo` | `...` | ... | |
| `int` | `...` | ... | |
| `ode` | `...` | ... | |
| `ode` | `...` | ... | |
| `ipf` | `...` | ... | |
| `spe` | `...` | ... | |

## Operations with matrices and vectors

### Inner product of two vectors

$$
\mathbf{a} \cdot \mathbf{b} = \sum_{i=1}^n a_i b_i = a_1 b_1 + a_2 b_2 + \dots + a_n b_n
$$

NumLib provides the function `omvinp` for calculation of the inner product:

```pascal
function omvinp(var a, b: ArbFloat; n: ArbInt): ArbFloat;
```

- `a` and `b` are the first elements of 1-dimensional arrays representing the vectors $a$ and $b$, respectively.
- `n` defines the dimension of the vectors (count of array elements). Both vectors must have the same number of elements.

**Example**

Calculate the dot product of these vectors.

$$
a = \begin{pmatrix} 0 \\ 1  \\ 2  \\ 2 \\ -1 \end{pmatrix}, \quad
b = \begin{pmatrix} 3 \\ -1 \\ -2 \\ 2 \\ -1 \end{pmatrix}
$$

```pascal linenums="1" hl_lines="33"
program InnerProductVectors;

{$mode objfpc}{$H+}{$J-}

{
 An example of inner product of two vectors using omvinp in numlib.

 function omvinp(var a, b: ArbFloat; n: ArbInt): ArbFloat;

 - `a` and `b` are the first elements of 1-dimensional arrays representing
    the vectors a and b, respectively.
 - `n` defines the dimension of the vectors (count of array elements).
 -  Both vectors must have the same number of elements.
}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  typ,
  omv;

var
  a: array[0..4] of ArbFloat = (0, 1, 2, 2, -1);
  b: array[0..4] of Arbfloat = (3, -1, -2, 2, -1);
  ab: ArbFloat;
  i: integer;

begin

  // Perform dot product
  ab := omvinp(a[0], b[0], high(a));

  // Print vector a
  Write('Vector a = [');
  for i := Low(a) to High(a) do
    Write(a[i]: 5: 0);
  WriteLn('  ]');

  // Print vector b
  Write('Vector b = [');
  for i := Low(b) to High(b) do
    Write(b[i]: 5: 0);
  WriteLn('  ]');

  // Print a . b
  Write('Dot product a . b = ');
  WriteLn(ab: 4: 0);

  WriteLn;
  WriteLn('Press enter key to quit');
  ReadLn;
end.
```

## Solving Systems of Linear Equations

A system of linear equations (or linear system) is a collection of two or more linear equations involving the same set of variables, $x_1 ... x_n$.

$$
\displaystyle{  \begin{cases}
    a_{11} x_1 + a_{12} x_2 + \dots + a_{1n} x_n = b_1 \\
    a_{21} x_1 + a_{22} x_2 + \dots + a_{2n} x_n = b_n \\
    \ \ \ \vdots \\
    a_{m1} x_1 + a_{m2} x_2 + \dots + a_{mn} x_n = b_m
  \end{cases} 
 }
$$

In order to find a solution which satisfies all equations simultaneously the system of equations is usually converted to a matrix equation $A x = b$, where

$$
\displaystyle{  
    A = \left [ \begin{array}{ccc}
                  a_{11} & a_{12} & \dots  & a_{1n} \\
                  a_{21} & a_{22} & \dots  & a_{2n} \\
                  \vdots & \vdots & \ddots & \vdots \\
                  a_{m1} & a_{m2} & \dots  & a_{mn}
                \end{array}
        \right ],
   \
   \mathbf{x} = \left [ \begin{array}{c}
                  x_1    \\
                  x_2    \\
                  \vdots \\
                  x_n
               \end{array}
       \right ],
   \
   \mathbf{b} = \left [ \begin{array}{c}
                  b_1    \\
                  b_2    \\
                  \vdots \\
                  b_m
               \end{array}
       \right ]
 }
$$


NumLib has several procedures to solve the matrix equation depending on the properties of the matrix.



### Square matrices

Pass the matrix to the procedures in the standard NumLib way as the first element of a 2D or 1D array. The 2D array must be dimensioned to contain at least $n$ rows and $n$ columns, the 1D array must contain at least $n^2$ elements.

```pascal
procedure slegen(n, rwidth: ArbInt; var a, b, x, ca: ArbFloat; var term: ArbInt);   // generic matrix
procedure slegsy(n, rwidth: ArbInt; var a, b, x, ca: ArbFloat; var term: ArbInt);   // symmetric matrix
procedure slegpd(n, rwidth: ArbInt; var a, b, x, ca: ArbFloat; var term: ArbInt);   // symmetric positive definite matrix
```

`slegen` is the General-purpose for any square matrix (Gaussian elimination with partial pivoting).

`slegsy` is specialised for symmetric matrices (more stable reduction to tridiagonal form).

`slegpd` is optimisedfor symmetric positive definite matrices (Cholesky decomposition).

- `n` is the matrix size.
- `rwidth` is the number of allocated columns (must be `n <= rwidth`). 
- `a` is the first element of the coefficient matrix (unchanged during calculation). 
- `b` is the constant vector (also unchanged).
- `x` is where the solution vector will be stored. 
- `ca` indicates solution accuracy.
- `term` returns an error code:
    - `1`: Success, solution in `x`.
    - `2`: Matrix is nearly singular, no solution.
    - `3`: Invalid input, `n < 1`.

**Example**

```pascal linenums="1" hl_lines="60-63"
program SolveLinearEq;

{$mode objfpc}{$H+}{$J-}

{
  This is an example for solving linear equations; A x = b, using slgen in numlib.

  5 x_1 +  7 x_2 +  6 x_3 +  5 x_4 = 57         |5  7 6 5|      |57|
  7 x_1 + 10 x_2 +  8 x_3 +  7 x_4 = 79  => A = |7 10 8 7|, b = |79|
  6 x_1 +  8 x_2 + 10 x_3 +  9 x_4 = 88         |6 8 10 9|      |88|
  5 x_1 +  7 x_2 +  9 x_3 + 10 x_4 = 86         |5 7 9 10|      |86|

  Lastly, it tests the result, x, by multiplying A x (from slgen's output) and
  the result must be equal to b.
  }

uses
  NumLib,
  typ,
  sle,
  omv;

var
  A: array[1..4, 1..4] of ArbFloat = (
  (5, 7, 6, 5),
  (7, 10, 8, 7),
  (6, 8, 10, 9),
  (5, 7, 9, 10));
  b: array[1..4] of ArbFloat = (57, 79, 88, 86);
  x: array[1..4] of ArbFloat;
  b_test: array[1..4] of ArbFloat;
  ca: ArbFloat;
  n, i, j: integer;
  term: integer;

begin

  // Define dimensions of the matrix
  n := High(A[1]);   // Number of columns

  WriteLn('Solve matrix system A x = b');
    WriteLn;

    // Print A
    WriteLn('Matrix A = ');
    for i:= 1 to n do begin
      for j := 1 to n do
        Write(A[i, j]:10:0);
      WriteLn;
    end;
    WriteLn;

    // Print b
    WriteLn('Vector b = ');
    for i:= 1 to n do
      Write(b[i]:10:0);
    WriteLn;
    WriteLn;

    // Solve - select one of these methods
    slegen(n, n, A[1, 1], b[1], x[1], ca, term);
    //slegsy(n, n, A[1, 1], b[1], x[1], ca, term);
    //slegpd(n, n, A[1, 1], b[1], x[1], ca, term);

  if term = 1 then begin
      WriteLn('Solution vector x = ');
      for i:= 1 to n do
        Write(x[i]:10:0);
      WriteLn;
      WriteLn;

      omvmmv(A[1,1], n, n, n, x[1], b_test[1]);

      WriteLn('Check result: A x = (must be equal to b)');
      for i:= 1 to n do
        Write(b_test[i]:10:0);
      WriteLn;
    end
    else
      WriteLn('Error');

  WriteLn('Press enter key to exit');
  ReadLn; // Wait for user input before closing
end.
```