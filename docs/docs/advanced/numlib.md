# Numerical with NumLib | Examples

The notes on this page is based on the [Free Pascal NumLib official doc](https://wiki.freepascal.org/NumLib_Documentation).

!!! Tip "Credits"

    Thanks to Kees van Ginneken, Wil Kortsmit and Loek van Reij of the Computational centre of the Eindhoven University of Technology for making it available for the Free Pascal project.

    Thanks to Marco van de Voort ([marco@freepascal.org](mailto:marco@freepascal.org)) and Michael van Canneyt ([michael@freepascal.org](mailto:michael@freepascal.org)) for porting to FPC and documenting NumLib.


**Routines in NumLib**

- Basic operations with matrices and vectors - unit `omv`
- Calculation of determinants - unit `det`
- Matrix inversion - unit `inv`
- Solution of systems of linear equations - unit `sle`
- Calculation of eigenvalues - unit `eig`
- Finding roots of equations - unit `roo`
- Calculates integrals - unit `int`
- Oridnary differential equations - unit `ode`
- Fitting routines - unit `ipf`
- Calculation of special functions - unit `spe`

## Unit `omv` - Operations with matrices and vectors

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

```pascal linenums="1" hl_lines="22"
program omvinp_demo;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  typ,
  omv;

var
  vector_a: array[1..5] of ArbFloat = (0, 1, 2, 2, -1);
  vector_b: array[1..5] of Arbfloat = (3, -1, -2, 2, -1);
  product_ab: ArbFloat;
  i: integer;

begin

  // Perform dot product
  product_ab := omvinp(vector_a[1], vector_b[1], high(vector_a));

  // Print vector vector_a
  Write('Vector a = [');
  for i := Low(vector_a) to High(vector_a) do
    Write(vector_a[i]: 4: 0);
  WriteLn('  ]');

  // Print vector vector_b
  Write('Vector b = [');
  for i := Low(vector_b) to High(vector_b) do
    Write(vector_b[i]: 4: 0);
  WriteLn('  ]');

  // Print vector_a . b
  Write('Dot product a . b = ');
  WriteLn(product_ab: 4: 0);

  WriteLn;
  WriteLn('Press enter key to quit');
  ReadLn;
end.
```

### Product of a matrix with a vector

**Example**

Calculate the product of the following.

$$
\displaystyle{ \begin{align*}
  \mathbf{c} = A\ \mathbf{b}=
  \left[
    \begin{array}{cccc}
      3 & 5 & 4 & 1 & -4\\
      -2 & 3 & 4 & 1 & 0\\
      0 & 1 & -1 & -2 & 5
    \end{array}
  \right]
  \ \ 
  \left[
    \begin{array}{c}
      3\\
      0\\
      -1\\
      -2\\
      1
    \end{array}
  \right]
\end{align*} }
$$


```pascal linenums="1" hl_lines="24-28"
program omvmmv_demo;

{$mode objfpc}{$H+}{$J-}

uses
  typ,
  omv;

var
  matrix_A: array[1..3, 1..5] of ArbFloat = (
    (3, 5, 4, 1, -4),
    (-2, 3, 4, 1, 0),
    (0, 1, -1, -2, 5));
  vector_b: array[1..5] of ArbFloat = (3, 0, -1, -2, 1);
  vector_c: array[1..5] of ArbFloat;
  m_row, n_column, i, j: integer;

begin

  m_row := High(matrix_A) - Low(matrix_A) + 1;
  n_column := High(matrix_A[1]) - Low(matrix_A[1]) + 1;

  // Calculate product vector_c = matrix_A vector_b
  omvmmv(
    matrix_A[1, 1], m_row, n_column, n_column,
    vector_b[1],
    vector_c[1]
    );

  // Print matrix_A
  WriteLn('A = ');
  for i := 1 to m_row do
  begin
    for j := 1 to n_column do
      Write(matrix_A[i, j]: 4: 0);
    WriteLn;
  end;
  WriteLn;

  // Print vector vector_b
  WriteLn('b = ');
  for i := 1 to n_column do
    WriteLn(vector_b[i]: 4: 0);
  WriteLn;
  WriteLn;

  // Print result vector vector_c
  WriteLn('c = A b');
  WriteLn('c = ');
  for i := 1 to m_row do
    WriteLn(vector_c[i]: 4: 0);
  WriteLn;

  // Pause console
  WriteLn('Press enter key to exit');
  ReadLn;
end.
```

### Transpose matrix

The transpose matrix $A^{T}$ of matrix $A$ is obtained by flipping rows and columns:

$$
\displaystyle{ \begin{align*}
  {A} = \left[
    \begin{array}{cccc}
      a_{11} & a_{12} & a_{13} & \ldots & a_{1n}\\
      a_{21} & a_{22} & a_{23} & \ldots & a_{2n}\\
      \vdots & \vdots & \vdots & \ddots & \vdots\\
      a_{m1} & a_{m2} & a_{m3} & \ldots & a_{mn}
    \end{array}
  \right]
  \ \ \
  {A^T} = 
  \left[
    \begin{array}{cccc}
      a_{11} & a_{21} & \ldots & a_{m1}\\
      a_{12} & a_{22} & \ldots & a_{m2}\\
      a_{13} & a_{23} & \ldots & a_{m3} \\
      \vdots & \vdots & \ddots & \vdots \\
      a_{1n} & a_{2n} & \ldots & a{mn}
    \end{array}
  \right]
\end{align*} }
$$
 

Use the procedure omvtrm to perform this operation with NumLib:

```pascal
procedure omvtrm(
  var a: ArbFloat; m, n, rwa: ArbInt; 
  var c: ArbFloat; rwc: ArbInt
);
```

- `a` is the first element of the input matrix $A$. The elements of this array **are not** changed by the procedure.
- `m` is the number of rows of matrix $A$.
- `n` is the number of columns of matrix $A$.
- `rwa` is the number of allocated columns of $A$. This way the array of $A$ can be larger than acutally needed.
- `c` is the first element of the transposed matrix $A^{T}$. It has $n$ rows and $m$ columns.
- `rwc` is the number of allocated columns for the transposed matrix.

**Example**

Transpose the following 2 x 4 matrix.

$$
\displaystyle{ {A} = \left[
    \begin{array}{cccc}
      1  & 2  & 3  & 4 \\
      11 & 22 & 33 & 44
    \end{array}
  \right]
 }
$$

```pascal linenums="1" hl_lines="26-29"
program omvtrm_demo;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, 
  typ,
  omv;

var
  matrix_A: array[1..2, 1..4] of ArbFloat = (
    ( 1,  2,  3,  4),
    (11, 22, 33, 44));
  transposed_A: array[1..4, 1..2] of ArbFloat;
  m_row_matrix_A, n_col_matrix_A, i, j: integer;

begin

  m_row_matrix_A := High(matrix_A) - Low(matrix_A) + 1;
  n_col_matrix_A := High(matrix_A[1]) - Low(matrix_A[1]) + 1;

  // Transpose matrix_A
  omvtrm(
    matrix_A[1,1], m_row_matrix_A, n_col_matrix_A, n_col_matrix_A,
    transposed_A[1,1], m_row_matrix_A
  );

  // Print matrix_A
  WriteLn('A = ');
  for i:= 1 to m_row_matrix_A do begin
    for j := 1 to n_col_matrix_A do
      Write(matrix_A[i, j]:8:0);
    WriteLn;
  end;
  WriteLn;

  // Print transposed_A
  WriteLn('Aᵀ = ');
  for i:= 1 to n_col_matrix_A do begin
    for j := 1 to m_row_matrix_A do
      Write(transposed_A[i, j]:8:0);
    WriteLn;
  end;

  WriteLn;
  WriteLn('Press enter key to quit');
  ReadLn;
end.
```

### Norm of vectors and matrices

A norm assigns a positive "length" to a vector or matrix.

For vectors, `NumLib` supports these norms:

- The 1-norm is the sum of the absolute values of vector components, also known as the "Taxicab" or "Manhattan" norm because it represents the distance a taxi drives in a grid.

$$
\displaystyle{ \|a\|_1 = \sum_{i=1}^n |{a_i}| }
$$

- The 2-norm (Euclidean norm) is the distance from point a to the origin.

$$
\displaystyle{ \|a\|_2 = \sqrt{\sum_{i=1}^n {a_i}^2}  }
$$

- The infinity norm is the largest absolute value of the vector components.

$$
\displaystyle{ \|a\|_\infty = \max({a_1}, {a_2}, ... {a_n}) }
$$

For matrices, `NumLib` defines norms based on rows or columns:

- The 1-norm is the largest absolute column sum.

$$
\displaystyle{ \|M\|_1 = \max_{1 \le j \le {n}} \sum_{i=1}^m|M_{ij}| }
$$
 
- The infinity norm is the largest absolute row sum.

$$
\displaystyle{ \|M\|_\infty = \max_{1 \le i \le\ m} \sum_{j=1}^n |M_{ij}|  }
$$
 
- The Frobenius norm sums the squares of all matrix elements, similar to the vector 2-norm.

$$
\displaystyle{ \|M\|_F = \sqrt{\sum_{i=1}^m \sum_{j=1}^n {M_{ij}}^2}  }
$$
 
These are the NumLib routines for calculating norms:

```pascal
function omvn1v(var a: ArbFloat; n: ArbInt): ArbFloat;               // 1-norm of a vector
function omvn1m(var a: ArbFloat; m, n, rwidth: ArbInt): ArbFloat;    // 1-norm of a matrix
function omvn2v(var a: ArbFloat; n: ArbInt): ArbFloat;               // 2-norm of a vector
function omvnfm(Var a: ArbFloat; m, n, rwidth: ArbInt): ArbFloat;    // Frobenius norm of a matrix
function omvnmv(var a: ArbFloat; n: ArbInt): ArbFloat;               // Maximum infinite norm of a vector
function omvnmm(var a: ArbFloat; m, n, rwidth: ArbInt): ArbFloat;    // Maximum infinite norm of a matrix
```

- `a`: First element of the vector or matrix for norm calculation.
- `m`: Number of rows (for matrix norms).
- `n`: Number of elements (for vector norms) or columns (for matrix norms).
- `rwidth`: Allocated column count, allowing for a larger matrix than required.

**Example**

```pascal linenums="1" hl_lines="31-33 43-45"
program omv_norm_demo;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  typ,
  omv;

var
  vector_a: array[0..3] of ArbFloat = (0, 1, 2, -3);
  matrix_b: array[0..3, 0..1] of Arbfloat = ((3, -1),
                                             (-2, 2),
                                             (0, -1),
                                             (2, 1));
  n_vec, n_col_matrix, m_row_matrix, i, j: integer;

begin
  // Get length of vector and the size of the matrix
  n_vec := High(vector_a) - Low(vector_a) + 1;
  m_row_matrix := High(matrix_b) - Low(matrix_b) + 1;
  n_col_matrix := High(matrix_b[1]) - Low(matrix_b[1]) + 1;

  Write('Vector a = [');
  for i := Low(vector_a) to High(vector_a) do
    Write(vector_a[i]: 5: 0);
  WriteLn('  ]');
  WriteLn('        1-norm of vector a: ', omvn1v(vector_a[0], n_vec): 0: 3);
  WriteLn('        2-norm of vector a: ', omvn2v(vector_a[0], n_vec): 0: 3);
  WriteLn('  maximum norm of vector a: ', omvnmv(vector_a[0], n_vec): 0: 3);
  WriteLn;

  WriteLn('Matrix b = ');
  for i := 0 to m_row_matrix - 1 do
  begin
    for j := 0 to n_col_matrix - 1 do
      Write(matrix_b[i, j]: 5: 0);
    WriteLn;
  end;
  WriteLn('        1-norm of matrix b: ', omvn1m(matrix_b[0, 0], m_row_matrix, n_col_matrix, n_col_matrix): 0: 3);
  WriteLn('Forbenius norm of matrix b: ', omvnfm(matrix_b[0, 0], m_row_matrix, n_col_matrix, n_col_matrix): 0: 3);
  WriteLn('  maximum norm of matrix b: ', omvnmm(matrix_b[0, 0], m_row_matrix, n_col_matrix, n_col_matrix): 0: 3);

  WriteLn;
  WriteLn('Press enter key to quit');
  ReadLn;
end.
```

## Unit `sle` - Solving Systems of Linear Equations

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

Solve this system of linear equations:

$$
\displaystyle{  \begin{cases}
    5 x_1 +  7 x_2 +  6 x_3 +  5 x_4 = 57  \\
    7 x_1 + 10 x_2 +  8 x_3 +  7 x_4 = 79  \\
    6 x_1 +  8 x_2 + 10 x_3 +  9 x_4 = 88  \\
    5 x_1 +  7 x_2 +  9 x_3 + 10 x_4 = 86
  \end{cases}
  \qquad \Rightarrow \qquad
  A=
  \left[
    \begin{array}{rrrr}
       5 &  7 &  6 &  5    \\
       7 & 10 &  8 &  7 \\
       6 &  8 & 10 &  9 \\
       5 &  7 &  9 & 10
    \end{array}
  \right]
  , \;
  \mathbf{b} =
  \left[
    \begin{array}{r}
      57 \\
      79 \\
      88 \\
      86
    \end{array}
  \right]
 }
$$

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

### Band matrix

`slegba` is specially implemented for a band matrix, i.e. a matrix in which all elements are zero outside a band of width `l` below and of width `r` above the main diagonal.

```pascal
procedure slegba(n, l, r: ArbInt; var a, b, x, ca: ArbFloat; var term:ArbInt);
```

!!! Warning
    Note that a 2D dynamic array (`array of array of ArbFloat`) cannot be used for this routine.

- `n` is the number of columns and rows of the matrix (it must be a square matrix).
- `l` is the left bandwidth, i.e. the number of diagonals the band extends below (or to the left of) the main diagonal.
- `r` is the right bandwidth, i.e. the number of diagonals the band extends above (or to the right of) the main diagonsl.
- `a` is the first element of a 1D array which contains the elements of the diagonal band, see this. This array contains only the band elements and is obtained by running across the rows of the band matrix from left to right and top to bottom, starting at element $A_11$. 
    - It must be dimensioned to contain at least `n*(l+1+r) - (r*(r+1)+l*(l+1)) div 2` elements. 
    - **Note that a 2D array cannot be used for this routine**.
- `b` is the first element of the array containing the constant vector $b$. The array length at least must be equal to $n$. The vector will not be changed during the calculation.
- `x` is the first element of the array to receive the solution vector $x$. It must be allocated to contain at least $n$ values.
- `ca` is a parameter to describe the accuracy of the solution.
- `term` returns an error code:
    - 1 - successful completion, the solution vector $x$ is valid
    - 2 - the solution could not have been determined because the matrix is (almost) singular.
    - 3 - error in input values: n < 1, l < 0, l >= n, r < 0, or r >= n


**Example**

Solve this system of linear equations:

$$
\displaystyle{  \begin{array}{ccccccc}
    5 x_1 & - & 4 x_2 & + &   x_3 &   &       &   &       &   &       &   &       & = 0  \\
   -4 x_1 & + & 6 x_2 & - & 4 x_3 & + &   x_4 &   &       &   &       &   &       & = 0  \\
      x_1 & - & 4 x_2 & + & 6 x_3 & - & 4 x_4 &   &       &   &       &   &       & = 0  \\
          &   &   x_2 & - & 4 x_3 & + & 6 x_4 & - & 4 x_5 &   &       &   &       & = 1  \\
          &   &       &   &   x_3 & - & 4 x_4 & + & 6 x_5 & - & 4 x_6 & + &   x_7 & = 0  \\
          &   &       &   &       &   &   x_4 & - & 4 x_5 & + & 6 x_6 & - & 4 x_7 & = 0  \\
          &   &       &   &       &   &       &   &   x_5 & - & 4 x_6 & + & 5 x_7 & = 0  \\
  \end{array}
  \qquad \Rightarrow \qquad
  A=
  \left[
    \begin{array}{rrrr}
       5 & -4 &  1 &  0 &  0 &  0 &  0  \\
      -4 &  6 & -4 &  1 &  0 &  0 &  0  \\
       1 & -4 &  6 & -4 &  1 &  0 &  0  \\
       0 &  1 & -4 &  6 & -4 &  1 &  0  \\
       0 &  0 &  1 & -4 &  6 & -4 &  1  \\
       0 &  0 &  0 &  1 & -4 &  6 & -4  \\
       0 &  0 &  0 &  0 &  1 & -4 &  5
    \end{array}
  \right]
  , \;
  \mathbf{b} =
  \left[
    \begin{array}{r}
      0 \\
      0 \\
      0 \\
      1 \\
      0 \\
      0 \\
      0
    \end{array}
  \right]
 }
$$

```pascal linenums="1" hl_lines="51 95-100"
program slegba_demo;

{$mode objfpc}{$H+}{$J-}

uses
  typ, sle, omv;

const
  n = 7;  // For Square matrices, n_col = m_row
  L = 2;
  R = L;  // For square matrices, L = R
  n_band_elements = n * (L + 1 + R) - (L * (L + 1) + R * (R + 1)) div 2;

var
  // Band elements arranged in rows, elements outside of the band omitted!
  mat_a_band_elements: array[1..n_band_elements] of Arbfloat = (
             5, -4, 1,
         -4, 6, -4, 1,
      1, -4, 6, -4, 1,
      1, -4, 6, -4, 1,
      1, -4, 6, -4, 1,
      1, -4, 6, -4,
      1, -4, 5
    );
  vec_b: array[1..n] of ArbFloat = (
    0, 0, 0, 1, 0, 0, 0
  );
  soln_vec_x: array[1..n] of ArbFloat;
  mat_a_test: array[1..n, 1..n] of ArbFloat;
  vec_b_test: array[1..n] of ArbFloat;
  ca: ArbFloat;
  i, j, k: integer;
  term: integer;

begin
  WriteLn('Solve matrix system A x = b where A is a band matrix');
  WriteLn;

  // Write diagonal elements
  WriteLn('Matrix A:');
  WriteLn('  n = ', n);
  Writeln('  Left band width L = ', L);
  WriteLn('  Right band width R = ', R);
  WriteLn('  Diagonal elements of A = ');
  for k := 1 to n_band_elements do
    Write(mat_a_band_elements[k]:5:0);
  WriteLn;
  WriteLn;

  // Solve
  slegba(n, l, r, mat_a_band_elements[1], vec_b[1], soln_vec_x[1], ca, term);

  if term = 1 then begin
    // Optional -- Test if tyhe result of matrix A with the solution vector x equals to vector b.
    // Since mutliplication of a matrix stored like a band matrix is quite
    // cumbersome we copy the band matrix into a standard matrix.
    FillChar(mat_a_test, SizeOf(mat_a_test), 0);
    k := 1;
    for i:=1 to L do
      for j := i-L to i+R do
        if (j >= 1) and (j <= n) then begin
          mat_a_test[i,j] := mat_a_band_elements[k];
          inc(k);
        end;

    for i:= L+1 to n-R do
      for j := i-L to i+R do begin
        mat_a_test[i,j] := mat_a_band_elements[k];
        inc(k);
      end;
    for i := n-R+1 to n do
      for j := i-L to i+R do
        if j <= n then begin
          mat_a_test[i,j] := mat_a_band_elements[k];
          inc(k);
        end;

    // Optional -- Print matrix A
    WriteLn('Matrix A =');
    for i:=1 to n do begin
      for j:=1 to n do
        Write(mat_a_test[i,j]:5:0);
      WriteLn;
    end;
    WriteLn;

    // Optional -- Print Vector b
    WriteLn('Vector b = ');
    for i:= 1 to n do
      Write(vec_b[i]:10:0);
    WriteLn;
    WriteLn;

    // Print solution
    WriteLn('Solution: vector x = ');
    for i:= 1 to n do
      Write(soln_vec_x[i]:10:3);
    WriteLn;
    WriteLn;

    // Optional -- Double-check result by multiply mat_a_text and the soln_vec_x
    // The result should be the same as vec_b
    omvmmv(mat_a_test[1,1], n, n, n, soln_vec_x[1], vec_b_test[1]);

    WriteLn('Check result: A x = (must be equal to b)');
    for i:= 1 to n do
      Write(vec_b_test[i]:10:0);
    WriteLn;
  end
  else
    WriteLn('Error');


  // Pause console
  WriteLn('Press enter to quit');
  ReadLn;
end.
```