# Numerical with NumLib | Examples

The notes and examples on this page are based on the [Free Pascal NumLib official doc](https://wiki.freepascal.org/NumLib_Documentation).

!!! Tip "Credits"

    Thanks to Kees van Ginneken, Wil Kortsmit and Loek van Reij of the Computational centre of the Eindhoven University of Technology for making it available for the Free Pascal project.

    Thanks to Marco van de Voort ([marco@freepascal.org](mailto:marco@freepascal.org)) and Michael van Canneyt ([michael@freepascal.org](mailto:michael@freepascal.org)) for porting to FPC and documenting NumLib.

!!! Note
    The code snippets are not exactly the same as the ones in the [Free Pascal NumLib official doc](https://wiki.freepascal.org/NumLib_Documentation). I modified them for my learning.

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

## Unit `omv` - Operations with Matrices and Vectors

### Inner Product of Two Vectors

$$
\mathbf{a} \cdot \mathbf{b} = \sum_{i=1}^n a_i b_i = a_1 b_1 + a_2 b_2 + \dots + a_n b_n
$$

NumLib provides the function `omvinp` for calculation of the inner product:

```pascal
function omvinp(var a, b: ArbFloat; n: ArbInt): ArbFloat;
```

**Parameters**

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

### Product of a Matrix with a Vector

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

### Transpose Matrix

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

**Parameters**

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

### Norm of Vectors and Matrices

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

**Parameters**

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

## Determinant of a Matrix

Coming soon.

## Inverse of a Matrix

Let's say we have a square matrix \( A \). The matrix \( A^{-1} \) is called the **inverse** of \( A \) if, when you multiply \( A^{-1} \) by \( A \), you get the **identity matrix** \( I \). The identity matrix is special because it has 1s on the diagonal (from the top left to bottom right) and 0s everywhere else.

$$
\displaystyle{  A^{-1} A = I = 
     \begin{bmatrix}
       1  &         & 0 \\
          & \ddots  &   \\
       0  &         & 1
     \end{bmatrix}
 }
$$

The inverse of a matrix only exists if the **determinant** of \( A \) is not zero. If the determinant is zero, \( A \) doesn't have an inverse.

### Using NumLib to Find the Inverse

NumLib is a tool that has several ways (called routines) to calculate the inverse of a matrix. Which method it uses depends on what kind of matrix you have. All the routines expect the matrix to be stored in a common way—as a 2D array of real numbers, or a 1D array of numbers arranged in rows and columns.

Here are some routines that NumLib uses:

- **invgen**: This routine works for any square matrix.
- **invgsy**: This routine works for matrices that are **symmetric** (meaning the left side mirrors the right side).
- **invgpd**: This routine works for **symmetric positive definite** matrices (a special kind of symmetric matrix).

```pascal
procedure invgen(n, rwidth: ArbInt; var ai: ArbFloat; var term: ArbInt);   // generic matrix
procedure invgsy(n, rwidth: ArbInt; var ai: ArbFloat; var term: ArbInt);   // symmetric matrix
procedure invgpd(n, rwidth: ArbInt; var ai: ArbFloat; var term: ArbInt);   // symmetric positive definite matrix
```

**Parameters**


- \( n \): The size of the matrix.
- \( rwidth \): The width of the rows (usually, this is the same as \( n \)).
- \( ai \): The first element of the matrix (the top-left corner).
- \( term \): A number that tells if the calculation was successful:
  - 1: Success! The inverse was found.
  - 2: The inverse couldn't be found because the matrix is too close to being singular (meaning its determinant is almost zero).
  - 3: There was a mistake with the input data (like if \( n \) was less than 1). 

**Example**

Calculate the inverse of the symmetric matrix.

$$
\displaystyle{ A = 
 \left[
 \begin{array}{rrrr}
   5 & 7 & 6 & 5  \\
   7 & 10 & 8 & 7 \\
   6 & 8 & 10 & 9 \\
   5 & 7 & 9 & 10
 \end{array}
 \right]
 }
$$
 
Note, this matrix is **symmetric** and **positive definite** (the product $b^{T} A b$ with any vector $b = [b1, b2, b3]^{T}$ cannot become zero or negative since all elements of this matrix are positive). Therefore, `invgpd` is best-suited for this task although the other routines can be used as well (uncomment their calls below).

```pascal linenums="1" hl_lines="41-44"
program inverse_matrix;

{$mode objfpc}{$H+}{$J-}

uses
  typ, inv, omv;

const
  n_mat_size = 4;
  decimals = 5;

var
  // a is the input matrix to be inverted
  // Note that this is matrix must be symmetric positive definite.
  mat_a: array[1..n_mat_size, 1..n_mat_size] of ArbFloat = (
    (5,  7,  6,  5),
    (7, 10,  8,  7),
    (6,  8, 10,  9),
    (5,  7,  9, 10)
  );
  mat_b: array[1..n_mat_size, 1..n_mat_size] of Arbfloat;
  mat_c: array[1..n_mat_size, 1..n_mat_size] of ArbFloat;
  term: integer = 0;
  i, j: integer;

begin
  // Write input matrix
  WriteLn('a = ');
  for i := 1 to n_mat_size do begin
    for j := 1 to n_mat_size do
      Write(mat_a[i, j]:10:decimals);
    WriteLn;
  end;
  WriteLn;

  // Store input matrix because it will be overwritten by the inverse of mat_a
  for i := 1 to n_mat_size do
    for j := 1 to n_mat_size do
      mat_b[i, j] := mat_a[i, j];

  // Calculate inverse  -- uncomment the procedure to be used.
  //invgen(n_mat_size, n_mat_size, mat_a[1, 1], term);
  //invgsy(n_mat_size, n_mat_size, mat_a[1, 1], term);
  invgpd(n_mat_size, n_mat_size, mat_a[1, 1], term);

  // Write inverse
  WriteLn('a^(-1) = ');
  for i := 1 to n_mat_size do begin
    for j := 1 to n_mat_size do
      Write(mat_a[i, j]:10:decimals);
    WriteLn;
  end;
  WriteLn;

  // Check validity of result by multiplying inverse with saved input matrix
  omvmmm(mat_a[1, 1], n_mat_size, n_mat_size, n_mat_size,
         mat_b[1, 1], n_mat_size, n_mat_size,
         mat_c[1, 1], n_mat_size);

  // Write inverse
  WriteLn('a^(-1) x a = ');
  for i := 1 to n_mat_size do begin
    for j := 1 to n_mat_size do
      Write(mat_c[i, j]:10:decimals);
    WriteLn;
  end;

  // Pause console
  WriteLn('Press enter to quit');
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

### Square Matrices

Pass the matrix to the procedures in the standard NumLib way as the first element of a 2D or 1D array. The 2D array must be dimensioned to contain at least $n$ rows and $n$ columns, the 1D array must contain at least $n^2$ elements.

```pascal
procedure slegen(n, rwidth: ArbInt; var a, b, x, ca: ArbFloat; var term: ArbInt);   // generic matrix
procedure slegsy(n, rwidth: ArbInt; var a, b, x, ca: ArbFloat; var term: ArbInt);   // symmetric matrix
procedure slegpd(n, rwidth: ArbInt; var a, b, x, ca: ArbFloat; var term: ArbInt);   // symmetric positive definite matrix
```

**Parameters**

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
program solve_linear_eq;

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

const
  n_col = 4; // n (col) = m (row) for square matrice

var
  mat_A: array[1..n_col, 1..n_col] of ArbFloat = (
  (5, 7, 6,  5),
  (7, 10, 8, 7),
  (6, 8, 10, 9),
  (5, 7, 9, 10));
  vec_b: array[1..n_col] of ArbFloat = (57, 79, 88, 86);
  soln_vec_x: array[1..n_col] of ArbFloat;
  vec_b_test: array[1..n_col] of ArbFloat;
  ca: ArbFloat;
  i, j: integer;
  term: integer;

begin

  WriteLn('Solve matrix system A x = b');
  WriteLn;

    // Print mat_A
    WriteLn('Matrix A = ');
    for i:= 1 to n_col do begin
      for j := 1 to n_col do
        Write(mat_A[i, j]:10:0);
      WriteLn;
    end;
    WriteLn;

    // Print vec_b
    WriteLn('Vector b = ');
    for i:= 1 to n_col do
      Write(vec_b[i]:10:0);
    WriteLn;
    WriteLn;

    // Solve - select one of these methods
    slegen(n_col, n_col, mat_A[1, 1], vec_b[1], soln_vec_x[1], ca, term);
    //slegsy(n_col, n_col, mat_A[1, 1], vec_b[1], soln_vec_x[1], ca, term);
    //slegpd(n_col, n_col, mat_A[1, 1], vec_b[1], soln_vec_x[1], ca, term);

  if term = 1 then begin
      WriteLn('Solution vector x = ');
      for i:= 1 to n_col do
        Write(soln_vec_x[i]:10:0);
      WriteLn;
      WriteLn;

      omvmmv(mat_A[1,1], n_col, n_col, n_col, soln_vec_x[1], vec_b_test[1]);

      WriteLn('Check result: A x = (must be equal to b)');
      for i:= 1 to n_col do
        Write(vec_b_test[i]:10:0);
      WriteLn;
    end
    else
      WriteLn('Error');

  WriteLn('Press enter key to exit');
  ReadLn; // Wait for user input before closing
end.
```

### Band Matrix

`slegba` is the optimised solution for band matrices, i.e. matrices in which all elements are zero outside a band of width `l` below and of width `r` above the main diagonal.

```pascal
procedure slegba(n, l, r: ArbInt; var a, b, x, ca: ArbFloat; var term:ArbInt);
```

!!! Warning
    Note that this routine cannot work with a 2D array.

**Parameters**

- `n` is the number of columns and rows of the matrix (it must be a square matrix).
- `l` is the left bandwidth, i.e. the number of diagonals the band extends below (or to the left of) the main diagonal.
- `r` is the right bandwidth, i.e. the number of diagonals the band extends above (or to the right of) the main diagonsl.
- `a` is the first element of a 1D array which contains the elements of the diagonal band, see this.
    - This array **contains only the band elements and is obtained by running across the rows of the band matrix from left to right and top to bottom, starting at** $A_11$. 
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
program solve_band;

{$mode objfpc}{$H+}{$J-}

uses
  typ, sle, omv;

const
  n = 7;  // For Square matrices, n_col = m_row
  L = 2;  // Number of diagonals the band extends below (or to the left of) the main diagonal.
  R = L;  // Number of diagonals the band extends above (or to the right of) the main diagonal.
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


### Symmetric Positive Definite Band Matrix

`slegpb` is the optimised solution for symmetric positive band matrices.

```pascal
procedure slegpb(n, w: ArbInt; var a, b, x, ca: ArbFloat; var term: ArbInt);
```

!!! Warning
    Note that this routine cannot work with a 2D array.

**Parameters**

- `n` is the number of rows and columns in the matrix (the matrix must be square).
- `w` is the bandwidth (one-sided), which is the number of diagonals that extend below (or to the left of) the main diagonal. This value is also the number of diagonals that extend above (or to the right of) the main diagonal.
- `a` points to the first element of a 1D array that contains the elements of the diagonal band. 
    - This array holds only the band elements and is constructed by traversing the rows of the band matrix from top-left to bottom-right, stopping at the main diagonal. 
    - As a result, **it contains only the left band and the main diagonal elements**.
    - The array must be dimensioned to hold at least `n*(w+1) - (w*(w+1)) div 2` elements. 
    - **Note that this routine cannot work with a 2D array**.
- `b` points to the first element of the array holding the constant vector $b$. The length of this array must be at least $n$, and the vector remains unchanged during the computation.
- `x` is the first element of the array where the solution vector $x$ will be stored. It must be large enough to hold at least `n` values. If term is not equal to 1, the solution array may contain invalid data.
- `ca` specifies the accuracy of the solution.
- `term` is an error code that indicates the result of the calculation:
    - 1: The computation was successful, and the solution vector $x$ is valid.
    - 2: The matrix is (nearly) singular, so a solution could not be determined.
    - 3: There was an error in the input values (e.g., `n < 1`, `w < 0`, or `w >= n`).

**Example**

Solve this system of linear equations.

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

```pascal linenums="1" hl_lines="50"
program solve_spd;    // "spd" = symmetric positive definite

{$mode objfpc}{$H+}{$J-}

uses
  typ, sle, omv;

const
  n = 7;
  w = 2;
  n_band_elements = n * (w + 1) - (w * (w + 1)) div 2;

var
  // Band elements arranged in rows
  // Elements outside of the band as well as right diagonals are omitted!
  mat_a: array[1..n_band_elements] of Arbfloat = (
             5,
            -4, 6,
             1, -4, 6,
                 1, -4, 6,
                     1, -4, 6,
                       1, -4, 6,
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
  WriteLn('Solve matrix system A x = b where A is a symmetric positive definite band matrix');
  WriteLn;

  // Write diagonal elements
  WriteLn('Matrix A:');
  WriteLn('  n = ', n);
  Writeln('  (One-sided) band width w = ', w);
  WriteLn('  Diagonal elements of A = ');
  for k := 1 to n_band_elements do
    Write(mat_a[k]:5:0);
  WriteLn;
  WriteLn;

  // Solve
  slegpb(n, w, mat_a[1], vec_b[1], soln_vec_x[1], ca, term);

  if term = 1 then begin
    // To test the result we multiply a with the solution vector x and check
    // whether the result is equal to b.
    // Since mutliplication of a matrix stored like a band matrix is quite
    // cumbersome we copy the band matrix into a standard matrix.
    FillChar(mat_a_test, SizeOf(mat_a_test), 0);
    i := 1;
    k := 1;
    while (k <= n_band_elements) and (i <= n) do begin
      for j := i - w to i do
        if (j >= 1) and (j <= n) then begin
          mat_a_test[i, j] := mat_a[k];
          mat_a_test[j, i] := mat_a[k];
          inc(k);
        end;
      inc(i);
    end;

    // Print mat_a
    WriteLn('Matrix A =');
    for i:=1 to n do begin
      for j:=1 to n do
        Write(mat_a_test[i,j]:5:0);
      WriteLn;
    end;
    WriteLn;

    // Print vec_b
    WriteLn('Vector b = ');
    for i:= 1 to n do
      Write(vec_b[i]:10:0);
    WriteLn;
    WriteLn;

    // Print solution vec_x
    WriteLn('Solution: vector x = ');
    for i:= 1 to n do
      Write(soln_vec_x[i]:10:3);
    WriteLn;
    WriteLn;

    omvmmv(mat_a_test[1,1], n, n, n, soln_vec_x[1], vec_b_test[1]);

    WriteLn('Check result: A x = (must be equal to b)');
    for i:= 1 to n do
      Write(vec_b_test[i]:10:3);
    WriteLn;
  end
  else
    WriteLn('Error');

  // Pause Console
  WriteLn('Press enter to quit');
  ReadLn;
end.
```


### Tridiagonal Matrix
NumLib provides two special procedures to solve linear equations based on a tridiagonal matrix. These procedures use an efficient way to store tridiagonal matrices, with slight differences in their calculation methods and handling of certain cases.

```pascal
sledtr(n: ArbInt; var l, d, u, b, x: ArbFloat; var term: ArbInt)
slegtr(n: ArbInt; var l, d, u, b, x, ca: ArbFloat; var term: ArbInt)
```

**Parameters**

- `n` is the number of unknown variables. It must be the same as the number of columns and rows of the coefficent matrix.
- `l` specifies the first element in the subdiagonal of the matrix $A$. This 1D array must be dimensioned to at least `n-1` elements.
- `d` specifies the first element along the main diagonal of the matrix $A$. This 1D array must be dimensioned to contain at least `n` elements.
- `u` specifies the first element along the superdiagonal of the matrix $A$. The array must be dimensioned to have at least `n-1` elements.
- `b` is the first element of the array containing the constant vector $b$. The array length at least must be equal to `n`. The vector will not be changed during the calculation.
- `x` is the first element of the array to receive the solution vector $x$. It must be allocated to contain at least `n` values.
- `ca` is a parameter to describe the accuracy of the solution.
- `term` returns an error code:
    - `1` - successful completion, the solution vector $x$ is valid
    - `2` - the solution could not have been determined because the matrix is (almost) singular.
    - `3` - error in input values: `n < 1`.

The `sledtr` routine is numerically stable if matrix $A$ fulfills one of these conditions:

- Matrix $A$ is regular (i.e. its inverse matrix exists), and $A$ is columnar-diagonally dominant, this means:
    - |d1| ≥ |l2|,
    - |di| ≥ |ui-1| + |li+1|, i = 2, ..., n-1,
    - |dn| ≥ |un-1|
- Matrix $A$ is regular, and $A$ is diagonally dominant, this means:
    - |d1| ≥ |l2|,
    - |di| ≥ |ui| + |li|, i = 2, ..., n-1,
    - |dn| ≥ |un|
- Matrix $A$ is symmetric and positive-definite.

Note, the `sledtr` routine does not provide the parameter `ca` from which the accuracy of the determined solution can be evaluated. If this is needed the (less stable) procedure `slegtr` must be used.


**Example**

Solve this tridiagonal system of linear equations where $n=8$.

$$
\displaystyle{  \begin{cases}
   188 x_1 - 100 x_2                    = 0   \\
   -100 x_1 + 188 x_2 -100 x_3          = 0 \\
   \vdots  \\
   -100 x_{n-2} + 188 x_{n-1} - 100 x_n = 0 \\          
   -100 x_{n-1} + 188 x_n               = 0 
  \end{cases}
  \qquad \Rightarrow \qquad
  A=
  \left[
    \begin{array}{rrrrr}
       188 & -100   &        &        & 0    \\
      -100 &  188   & -100   &        &      \\
           & \ddots & \ddots & \ddots &      \\
           &        & -100   &   188  & -100 \\
           &        &        &  -100  &  188 \\
    \end{array}
  \right]
  , \;
  \mathbf{b} =
  \left[
    \begin{array}{r}
       88 \\
      -12 \\
      \vdots \\
      -12 \\
       88
    \end{array}
  \right]
 }
$$


```pascal linenums="1" hl_lines="47"
program solve_tridiag_matrix;

{$mode objfpc}{$H+}{$J-}

uses
  typ, sle;

const
  n = 8;

var
  u_super_diag_items: array[1..n-1] of ArbFloat = (-100, -100, -100, -100, -100, -100, -100      );
  d_diag_items      : array[1..n]   of ArbFloat = ( 188,  188,  188,  188,  188,  188,  188,  188);
  l_sub_diag_items  : array[1..n-1] of ArbFloat = (      -100, -100, -100, -100, -100, -100, -100);
  vec_b             : array[1..n]   of ArbFloat = (  88,  -12,  -12,  -12,  -12,  -12,  -12,   88);
  soln_vec_x: array[1..n] of ArbFloat;
  ca: ArbFloat;
  i, term: integer;
  vec_b_test: array[1..n] of ArbFloat;

begin
  WriteLn('Solve tridiagonal matrix system A x = b');
  WriteLn;

  Write('Superdiagonal of A:':25);
  for i := 1 to n-1 do
    Write(u_super_diag_items[i]:10:0);
  WriteLn;

  Write('Main diagonal of A:':25);
  for i:= 1 to n do
    Write(d_diag_items[i]:10:0);
  WriteLn;

  Write('Subdiagonal of A:':25);
  Write('':10);
  for i:=2 to n do
    Write(l_sub_diag_items[i]:10:0);
  WriteLn;

  Write('Vector b:':25);
  for i:=1 to n do
    Write(vec_b[i]:10:0);
  WriteLn;

  // Solve for vector x
  slegtr(n, l_sub_diag_items[2], d_diag_items[1], u_super_diag_items[1], vec_b[1], soln_vec_x[1], ca, term);
  // Alternatively,
  // sledtr(n, l_sub_diag_items[2], d_diag_items[1], u_super_diag_items[1], vec_b[1], soln_vec_x[1], term);


  if term = 1 then begin
    Write('Solution vector x:':25);
    for i:= 1 to n do
      Write(soln_vec_x[i]:10:0);
    WriteLn;

    // Multiply A with soln_vec_x to test the result
    // NumLib does not have a routine to multiply a tridiagonal matrix with a
    // vector... Let's do it manually.
    vec_b_test[1] := d_diag_items[1]*soln_vec_x[1] + u_super_diag_items[1]*soln_vec_x[2];
    for i := 2 to n-1 do
      vec_b_test[i] := l_sub_diag_items[i]*soln_vec_x[i-1] + d_diag_items[i]*soln_vec_x[i] + u_super_diag_items[i]*soln_vec_x[i+1];
    vec_b_test[n] := l_sub_diag_items[n]*soln_vec_x[n-1] + d_diag_items[n]*soln_vec_x[n];

    Write('Check b = A x:':25);
    for i:= 1 to n do
      Write(vec_b_test[i]:10:0);
    WriteLn;
  end
  else
    WriteLn('Error');

  // Pause Console
  WriteLn('Press enter to quit');
  ReadLn;
end.
```

### Overdetermined Systems (Least Squares)

Unlike other routines in the sle unit that require a square matrix $A$, `slegls` can solve systems with a rectangular matrix, where there are more equations than unknowns. These systems usually can't be solved exactly. However, an approximate solution can minimize the sum of squared residuals, i.e. the norm $\displaystyle{ \|\mathbf{b} - A \mathbf{x}\|_2 }$ is as small as possible. This method is commonly used for fitting equations to data (regression analysis).

```pascal
procedure slegls(var a: ArbFloat; m, n, rwidtha: ArbInt; var b, x: ArbFloat; var term: ArbInt);
```

**Parameters**

- `a` is the first element of an array of matrix $A$. The array won't be modified.
- `m` is the number of rows in matrix $A$ (i.e., the number of equations).
- `n` is the number of columns in matrix $A$ (i.e., the number of unknown variables). 
    - Note: $n$ must not exceed $m$.
- `rwidtha` specifies the allocated number of columns for matrix $A$, which can be larger than needed, with $n≤rwidth$.
- `b` is the first element of vector $b$, with a lenth matching $m$, though it can be allocated larger.
- `x` is the first element of the array for the solution vector $x$, with length matching $n$, though it can also be allocated larger.
- `term` returns an error code:
    - 1 - Success, solution $x$ is valid
    - 2 - No clear solution due to linearly dependent columns.
    - 3 - Input error: `n < 1`, or `n > m`.

The method uses Householder transformation to reduce $A$ to an upper triangular form.


**Example**

Find the least-squares solution for the system $A x = b$ of 4 equations and 3 unknowns.

$$
\displaystyle{  A=
 \left(
 \begin{array}{rrr}
  1 & 0 & 1 \\
  1 & 1 & 1 \\
  0 & 1 & 0 \\
  1 & 1 & 0
 \end{array}
 \right), \ \
 b=
 \left(
 \begin{array}{r}
 21 \\ 39 \\ 21 \\ 30
 \end{array}
 \right).
 }
$$

```pascal linenums="1" hl_lines="46"
program solve_leastsquares;

{$mode objfpc}{$H+}{$J-}

uses
  typ,
  sle,
  omv;

const
  m_row = 4;
  n_col = 3;

var
  mat_A: array[1..m_row, 1..n_col] of ArbFloat = (
     (1, 0, 1),
     (1, 1, 1),
     (0, 1, 0),
     (1, 1, 0));
  vec_b: array[1..m_row] of ArbFloat = (21, 39, 21, 30);
  soln_vec_x: array[1..n_col] of ArbFloat;
  term: ArbInt;
  i, j: integer;
  vec_b_test: array[1..m_row] of ArbFloat;
  sum: ArbFloat;

begin
  WriteLn('Solve A x = b with the least-squares method');
  WriteLn;

  // Display input data
  WriteLn('A = ');
  for i := 1 to m_row do
  begin
    for j := 1 to n_col do
      Write(mat_A[i, j]: 10: 0);
    WriteLn;
  end;
  WriteLn;
  WriteLn('b = ');
  for i := 1 to m_row do
    Write(vec_b[i]: 10: 0);
  WriteLn;

  // Calculate and show solution
  slegls(mat_A[1, 1], m_row, n_col, n_col, vec_b[1], soln_vec_x[1], term);

  WriteLn;
  WriteLn('Solution x = ');
  for j := 1 to n_col do
    Write(soln_vec_x[j]: 10: 0);
  WriteLn;

  // Calculate and display residuals
  WriteLn;
  WriteLn('Residuals A x - b = ');
  sum := 0;
  omvmmv(mat_A[1, 1], m_row, n_col, n_col, soln_vec_x[1], vec_b_test[1]);
  for i := 1 to m_row do
  begin
    Write((vec_b_test[i] - vec_b[i]): 10: 0);
    sum := sum + sqr(vec_b_test[i] - vec_b[i]);
  end;
  WriteLn;

  // Sum of squared residuals
  WriteLn;
  WriteLn('Sum of squared residuals');
  WriteLn(sum: 10: 0);

  WriteLn;
  WriteLn('----------------------------------------------------------------------------');
  WriteLn;

  // Modify solution to show that the sum of squared residuals increases';
  WriteLn('Modified solution x'' (to show that it has a larger sum of squared residuals)');
  soln_vec_x[1] := soln_vec_x[1] + 1;
  soln_vec_x[2] := soln_vec_x[2] - 1;
  WriteLn;
  for j := 1 to n_col do
    Write(soln_vec_x[j]: 10: 0);
  omvmmv(mat_A[1, 1], m_row, n_col, n_col, soln_vec_x[1], vec_b_test[1]);
  sum := 0;
  for i := 1 to m_row do
    sum := sum + sqr(vec_b_test[i] - vec_b[i]);
  WriteLn;
  WriteLn;
  WriteLn('Sum of squared residuals');
  WriteLn(sum: 5: 0);
  WriteLn;

  // Pause Console
  WriteLn('Press enter to quit');
  ReadLn;
end.
```

The output is:

```text
Solve A x = b with the least-squares method

A =
    1    0    1
    1    1    1
    0    1    0
    1    1    0

b =
   21   39   21   30

Solution x =
   10   20   10

Residuals A x - b =
   -1    1   -1   -0

Sum of squared residuals
    3

----------------------------------------------------------------------------

Modified solution x' (to show that it has a larger sum of squared residuals)

   11   19   10

Sum of squared residuals
    5
```

#### Why Calculate the Sum of Squared Residuals and Why Modify the Solution to Increase the Sum of Squared Residuals?


!!! Note "Why Calculate the Sum of Squared Residuals?"

    The sum of squared residuals is a measure of how well the computed solution fits the data. It tells us how far off the computed values (from $A × x$) are from the actual values in $b$.

    In a least-squares solution, this sum is minimised. By calculating it, the code verifies how close the solution is to the actual results. A smaller sum of squared residuals means a better fit.

    $$
    Sum\_of\_Squared\_Residuals=(-1)^{2} + (1)^{2} + (-1)^{2} + 0^{2}=3
    $$

    A smaller sum indicates a better fit. In this case, a sum of 3 is reasonable and shows that the solution is a close approximation.

!!! Note "What about the residual of $(-1, 1, -1, 0)$?"
    The residuals $A × x - b$ being $(-1, 1, -1, 0)$ indicate that the least-squares solution is a close **approximation**, but not perfect.

    However, in an over-determined system (more equations than unknowns, as here with 4 equations and 3 unknowns), an exact solution is usually not possible. The least-squares method tries to find a solution $x$ that minimizes the sum of the squared residuals.

    This means that, when solving the system, the predictions for some rows of $b$ are slightly off:

    - The first equation is off by -1 (under-predicted).
    - The second equation is off by 1 (over-predicted).
    - The third equation is off by -1 (under-predicted).
    - The fourth equation is off by 0 (almost perfect prediction).

!!! Note "Why Modify the Solution to Increase the Sum of Squared Residuals?"

    In the second part of the code, the solution vector $x$ is manually modified:

    ```pascal
    x[1] := x[1] + 1;
    x[2] := x[2] - 1;
    ```

    This shows what happens if the solution is **perturbed**. By changing $x$, the code     demonstrates that the sum of squared residuals increases:

    ```pascal
    sum := sum + sqr(b_test[i] - b[i]);
    WriteLn('Sum of squared residuals');
    WriteLn(sum:5:0);
    ```

    The point of this modification is to highlight the importance of the least-squares solution. The original solution minimizes the residuals, but altering the solution results in a worse fit (higher residuals).

    Deviating from the least-squares solution results in a higher sum of squared residuals, indicating a poorer fit.


## Unit `eig` - Eigenvalues and eigenvectors

An **eigenvector** is a special type of vector that keeps its direction even when a linear transformation (like multiplying by a matrix) is applied to it.When you multiply a square matrix $A$ by a non-zero vector $x$ and the result is the same vector multiplied by a number (called a **scalar**), then:
  - $x$ is the eigenvector
  - The number is called the **eigenvalue** (represented by $λ$).

$$
\displaystyle{  A \mathbf{x} = \lambda \mathbf{x}  \qquad \Rightarrow \qquad  A \mathbf{x} - \lambda \mathbf{x} = 0   }
$$

**NumLib** has tools to calculate eigenvalues and eigenvectors for different types of matrices. Each matrix type has two to four procedures with numbers 1 to 4 that indicate their complexity:

- **Procedures with "1":** Calculate all eigenvalues.
- **Procedures with "2":** Calculate specific eigenvalues in a certain range.
- **Procedures with "3":** Calculate all eigenvalues and eigenvectors.
- **Procedures with "4":** Calculate specific eigenvalues and eigenvectors in a certain range.

### Matrices with General Storage

The routines assume the $n \times n$ matrix is stored as a 2D (or 1D) array of **ArbFloat** values.

**For Generic Matrices**

- **eigge1:** Calculates all eigenvalues.
- **eigge3:** Calculates all eigenvalues and eigenvectors.

**For Generic Symmetric Matrices**

- **eiggs1:** Calculates all eigenvalues.
- **eiggs2:** Calculates some eigenvalues in a specific range.
- **eiggs3:** Calculates all eigenvalues and eigenvectors.
- **eiggs4:** Calculates some eigenvalues and eigenvectors in a specific range.

**For Symmetric Positive Definite Matrices**

- **eiggg1:** Calculates all eigenvalues.
- **eiggg2:** Calculates some eigenvalues in a specific range.

```pascal
// Generic matrix (without any special symmetries)
procedure eigge1(var a: ArbFloat; n, rwidth: ArbInt; var lam: complex; var term: ArbInt);                      // all eigenvalues
procedure eigge3(var a: ArbFloat; n, rwidtha: ArbInt; var lam, x: complex; rwidthx: ArbInt; var term: ArbInt); // all eigenvalues and eigenvectors

// Generic symmetric matrix
procedure eiggs1(var a: ArbFloat; n, rwidth: ArbInt; var lam: ArbFloat; var term: ArbInt);                     // all eigenvalues
procedure eiggs2(var a: ArbFloat; n, rwidth, k1, k2: ArbInt; var lam: ArbFloat; var term: ArbInt);             // some eigenvalues (index k1..k2)
procedure eiggs3(var a: ArbFloat; n, rwidtha: ArbInt; var lam, x: ArbFloat; var term: ArbInt);                 // all eigenvalues and eigenvectors
procedure eiggs4(var a: ArbFloat; n, rwidtha, k1, k2: ArbInt; var lam, x: ArbFloat; var term: ArbInt);         // some eigenvalues and eigenvectors (index k1..k2)

// Symmetric positive definite matrix
procedure eiggg1(var a: ArbFloat; n, rwidth: ArbInt; var lam: ArbFloat; var term: ArbInt);                     // all eigenvalues
procedure eiggg2(var a: ArbFloat; n, rwidth, k1, k2: ArbInt; var lam: ArbFloat; var term: ArbInt);             // some eigenvalues (index k1..k2)
procedure eiggg3(var a: ArbFloat; n, rwidtha: ArbInt; var lam, x: ArbFloat; var term: ArbInt);                 // all eigenvalues and eigenvectors
procedure eiggg4(var a: ArbFloat; n, rwidtha, k1, k2: ArbInt; var lam, x: ArbFloat; var term: ArbInt);         // some eigenvalues and eigenvectors (index k1..k2)
```

**Parameters**

- `a` is the first element of an array containing the matrix $A$ for which the eigenvalue/eigenvector has to be calculated. 
    - The array must be dimensioned to provide space for at least $n^{2}$ floating point values.
- `n` specifies the size of the matrix $A$, i.e. the number of rows or columns. Note that the input matrix must be square, i.e. the number of rows and columns is equal.
- `rwidth` is the allocated row length of the array a. It can be larger than n if the array is allocated larger than necessary, but normally `rwidth = n`.
- `lam` is the first element of an array receiving the calculated eigenvalues. 
    - In case of a generic matrix (`eigge1`, `eigge3`) the eigenvalues can be complex; therefore, the array must be dimensioned for values of type `complex` as declared in unit `typ`. 
    - In the other cases (`eiggs1..4` or `eiggg1..4`) the eigenvalues are real, and the array must be dimensioned for datatype `ArbFloat`. 
    - Since a $n \times n$ matrix has $n$ eigenvalues, the array must be allocated for at least `n` values, in case of the procedures with appended 2 or 4 only `k2-k1+1` values are sufficient (see below).
- `term` returns an error code:
    - 1 -- successful calculation
    - 2 -- calculation failed
    - 3 -- error in input data: n<1, k1<1, k1>k2, or k2>n.

Additionally, in case of `eigge3`:

- `x` is the first element of a matrix to receive the calculated eigenvectors. 
    - Again, the eigenvectors of a generic matrix can have complex components. Therefore, the matrix must be declared for the datatype `complex`, and it must be large enough to hold at least $n^{2}$ values. 
    - If the matrix is symmetric or positive definite, the eigenvectors are real, and the array must be declared for datatype `ArbFloat`. In any case, the eigenvectors are normalized to unit length and arranged in the columns of this matrix.
- `rwidthx` denotes the allocated row length of the matrix $x$. Thus it is possible to dimension the result matrix larger than actually needed.

Additionally, in case of procedures `eiggs2`, `eiggs4`, `eiggg2` and `eiggg4`:

- `k1` and `k2` define the interval of indexes `k` for which the eigenvalues ($\lambda k$) and eigenvalues ($c_k$) are to be calculated. They are integers and must be ordered such that $1<=k1<=k2<=n$.

**Example**

Calculate the eigenvalues and eigenvectors of the matrix.

$$
\displaystyle{  A=
             \begin{bmatrix}
                 8 & -1 & -5 \\
                -4 &  4 & -2 \\
                18 & -5 & -7
             \end{bmatrix}
   }
   $$


```pascal linenums="1" hl_lines="42"
program eig_general_matrix;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils, math, typ, eig;

const
  n = 3;         // Size of the matrix where m (row) = n (row)
  dec_place = 3; // No of decimal place

var
  // a is the input matrix
  mat_a: array[1..n, 1..n] of ArbFloat = (
    ( 8, -1, -5),
    (-4,  4, -2),
    (18, -5, -7)
  );
  eig_values_lambda: array[1..n] of complex;
  eig_vec_mat_x: array[1..n, 1..n] of complex;
  term: integer = 0;
  i, j: integer;

  function ComplexToStr(z: complex; decimals: integer): String;
  var
    sgn: array[boolean] of string = ('+', '-');
  begin
    Result := Format('%.*f %s %.*fi', [Decimals, z.Re, sgn[z.Im < 0], Decimals, abs(z.Im)]);
  end;

begin
  // write input matrix
  WriteLn('Matrix a = ');
  for i := 1 to n do begin
    for j := 1 to n do
      Write(mat_a[i, j]:10:dec_place);
    WriteLn;
  end;
  WriteLn;

  // Calculate eigenvalues/vectors
  eigge3(mat_a[1,1], n, n, eig_values_lambda[1], eig_vec_mat_x[1,1], n, term);

  // write eigenvalues
  WriteLn('Eigenvalues: lambda = ');
  for i := 1 to n do
    Write(ComplexToStr(eig_values_lambda[i], dec_place):25);
  WriteLn;
  WriteLn;

  // Write eigenvectors
  WriteLn('Eigenvectors (as columns): x = ');
  for i := 1 to n do begin
    for j := 1 to n do
      Write(ComplexToStr(eig_vec_mat_x[i, j], dec_place):25);
    WriteLn;
  end;

  // Pause console
  WriteLn('Press enter to quit');
  ReadLn;
end.
```


**Output**

```text
Matrix a =
     8.000    -1.000    -5.000
    -4.000     4.000    -2.000
    18.000    -5.000    -7.000

Eigenvalues: lambda =
           2.000 + 4.000i           2.000 - 4.000i           1.000 + 0.000i

Eigenvectors (as columns): x =
           0.316 + 0.316i           0.316 - 0.316i           0.408 + 0.000i
           0.000 + 0.632i           0.000 - 0.632i           0.816 + 0.000i
           0.632 + 0.000i           0.632 + 0.000i           0.408 + 0.000i
Press enter to quit
```

### Symmetric Band Matrices

NumLib provides four routines for calculating the eigenvalues and eigenvectors of **symmetric band matrices** (matrices with non-zero elements in a band around the main diagonal):

1. **eigbs1**: Calculates all eigenvalues.
   ```pascal
   procedure eigbs1(var a: ArbFloat; n, w: ArbInt; var lam: ArbFloat; var term: ArbInt);
   ```

2. **eigbs2**: Calculates some of the eigenvalues, within a specified range (from index `k1` to `k2`).
   ```pascal
   procedure eigbs2(var a: ArbFloat; n, w, k1, k2: ArbInt; var lam: ArbFloat; var term: ArbInt);
   ```

3. **eigbs3**: Calculates all eigenvalues and their corresponding eigenvectors.
   ```pascal
   procedure eigbs3(var a: ArbFloat; n, w: ArbInt; var lam, x: ArbFloat; rwidthx: ArbInt; var term: ArbInt);
   ```

4. **eigbs4**: Calculates some eigenvalues and their corresponding eigenvectors (within the range `k1` to `k2`).
   ```pascal
   procedure eigbs4(var a: ArbFloat; n, w, k1, k2: ArbInt; var lam, x: ArbFloat; rwidthx: ArbInt; var m2, term: ArbInt);
   ```

**Parameters**

- **a**: This is the first element of a 1D array that contains the matrix's diagonal and left band elements. The right band is ignored because the matrix is symmetric. The array needs to be large enough to hold at least `n*(w+1) - (w*(w+1)) div 2` elements.
  
- **n**: The number of rows and columns in the matrix (since the matrix is square).

- **w**: The **bandwidth**, or the number of diagonals the band extends on either side of the main diagonal (the left and right bandwidth must be the same since the matrix is symmetric).

- **lam**: The first element of an array where the computed eigenvalues will be stored. Since a matrix of size `n x n` has `n` eigenvalues, this array should have space for at least `n` values of type `ArbFloat`.

- **x**: (Used in `eigbs3` and `eigbs4`) The first element of a matrix that will store the computed eigenvectors. The matrix must be large enough to hold at least `n x n` values and will contain the eigenvectors as columns. These vectors are normalized (scaled to unit length).

- **rwidthx**: (Used in `eigbs3` and `eigbs4`) The allocated row length for the eigenvector matrix `x`. This allows the matrix to be larger than needed if desired.

- **k1, k2**: (Used in `eigbs2` and `eigbs4`) These define the range of eigenvalues and eigenvectors to compute. For example, setting `k1 = 1` and `k2 = 3` calculates the first three eigenvalues and their vectors. The values must satisfy `1 <= k1 <= k2 <= n`.

- **m2**: (Used in `eigbs4`) The index of the largest eigenvalue for which an eigenvector has been computed.

- **term**: A return code indicating the result:
   - **1**: Calculation was successful.
   - **2**: Calculation failed.
   - **3**: There was an error in the input parameters, such as `n < 1`, `w < 0`, `w >= n`, `k1 < 1`, `k1 > k2`, or `k2 > n`.

If the bandwidth `w` is larger than one-third of the matrix size (`n/3`), it is generally more efficient to compute all eigenvalues and eigenvectors, even if you don't need all of them.


**Example**

Calculate the eigenvalues and eigenvectors of the symmetric 7 x 7 matrix 

$$
\displaystyle{  A=
 \begin{bmatrix}
  5 & -4 & 1 &  &  &  & 0 \\
 -4 & 6  & -4 & 1 &  &  &  \\
  1 & -4 & 6  & -4 & 1 &  &  \\
   & 1 & -4 & 6 & -4 & 1 &  \\
   &  & 1 & -4 & 6 & -4 & 1 \\
   &  &  & 1 & -4 & 6 & -4 \\
 0 &  &  &  & 1 & -4 & 5
 \end{bmatrix}
 }
$$
 
 The eigenvalues are 
 
$$
\displaystyle{ \lambda_k = 16\sin^{4}\frac{k\pi}{16}, \qquad k=1,2,\ldots,7 }
$$

```pascal linenums="1" hl_lines="118"
program eig_symband_matrix;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils, math,
  typ, eig;

const
  n_mat_size = 7;    // Size of the matrix (n x n matrix)
  w_band_size = 2;   // Bandwidth of the matrix (how many diagonals on either side of the main diagonal)

  // Total number of elements in the 1D array representing the matrix's diagonals
  n_diag_elements = n_mat_size * (w_band_size + 1) - (w_band_size * (w_band_size + 1)) div 2;

  decimal_place = 3; // Number of decimal places for output

// Converts a matrix index (i, j) into the corresponding 1D array index for a band matrix
function MatrixIndexToArrayIndex(i, j, n, w: Integer): Integer;

  // Calculates the starting index for a given diagonal element
  function DiagElementIndex(i: Integer): Integer;
  var
    k: Integer;
  begin
    Result := 1;
    if i = 1 then
      exit;
    // For diagonals truncated at the left side of the matrix
    for k := 2 to w do begin
      Result := Result + k;
      if k = i then
        exit;
    end;
    // For full rows and diagonals truncated on the right side
    for k := w+1 to n do begin
      Result := Result + w + 1;
      if k = i then
        exit;
    end;
    Result := n;
  end;

var
  d: Integer;
begin
  // Ensures that we always access the upper diagonal by swapping (i, j) if necessary
  if j > i then begin
    Result := MatrixIndexToArrayIndex(j, i, n, w);
    exit;
  end;

  // Get the starting index of the diagonal element in the 1D array
  Result := DiagElementIndex(i);

  // If i = j, we are on the main diagonal
  if (i = j) then
    exit;

  // Calculate the offset for the element in the band
  d := i - j;
  if d > w then
    Result := -1 // Element is outside the bandwidth, return -1 to indicate zero
  else begin
    dec(Result, d); // Adjust the index for the diagonal element
    if (Result < 1) then
      Result := -1; // Invalid index (out of bounds)
  end;
end;

var
  // Array to hold the elements of the banded matrix (diagonal and left band)
  mat_a: array[1..n_diag_elements] of ArbFloat = (
     5,   // Main diagonal element (row 1, col 1)
    -4, 6,   // Row 2 (left band and main diagonal)
     1, -4, 6,   // Row 3 (two left band, main diagonal)
         1, -4, 6,   // Row 4, (two left band, main diagonal)
             1, -4, 6,  // Row 5, the same ...
                 1, -4, 6,
                     1, -4, 5 );

  // Array to store the calculated eigenvalues
  eig_values_lambda: array[1..n_mat_size] of ArbFloat;

  // Matrix to store the calculated eigenvectors
  eig_vectors_mat_x: array[1..n_mat_size, 1..n_mat_size] of ArbFloat;

  term: integer = 0; // Variable for capturing the result of the eigenvalue/eigenvector calculation
  i, j, k: integer;  // Loop counters

begin
  // Display matrix information
  WriteLn('n = ', n_mat_size);
  Writeln('(One-sided) band width w = ', w_band_size);

  // Print diagonal elements of the band matrix
  Write('Diagonal elements of A = ', mat_a[1]:0:0);
  for k := 2 to n_diag_elements do
    Write(mat_a[k]:3:0);
  WriteLn;
  WriteLn;

  // Reconstruct and display the full symmetric band matrix from the 1D array (for verification purposes)
  WriteLn('Reconstructed A = ');
  for i := 1 to n_mat_size do begin
    for j := 1 to n_mat_size do begin
      k := MatrixIndexToArrayIndex(i, j, n_mat_size, w_band_size); // Get the 1D array index for the element (i, j)
      if k = -1 then
        Write(0.0:3:0) // Print 0 if the element is outside the band
      else
        Write(mat_a[k]:3:0); // Print the matrix element
    end;
    WriteLn;
  end;
  WriteLn;

  // Call NumLib to calculate all eigenvalues and eigenvectors (eigbs3)
  eigbs3(mat_a[1], n_mat_size, w_band_size, eig_values_lambda[1], eig_vectors_mat_x[1,1], n_mat_size, term);

  // Check if the calculation was successful
  if term <> 1 then begin
    WriteLn('term = ', term, ' --> ERROR'); // Error message in case of failure
    halt;
  end;

  // Display expected eigenvalues for reference (example)
  WriteLn('Expected eigenvalues:');
  for i := 1 to n_mat_size do
    Write(16 * intpower(sin(i*pi/16), 4):15:decimal_place); // Compute and print expected eigenvalues
  WriteLn;
  WriteLn;

  // Print the calculated eigenvalues
  WriteLn('Calculated eigenvalues: lambda = ');
  for i := 1 to n_mat_size do
    Write(eig_values_lambda[i]:15:decimal_place); // Print each eigenvalue
  WriteLn;
  WriteLn;

  // Print the calculated eigenvectors (one per column)
  WriteLn('Eigenvectors (as columns): x = ');
  for i := 1 to n_mat_size do begin
    for j := 1 to n_mat_size do
      Write(eig_vectors_mat_x[i, j]:15:decimal_place); // Print each element of the eigenvector matrix
    WriteLn;
  end;

  // Pause to allow user to see results before exiting the program
  WriteLn('Press enter to quit');
  ReadLn;
end.
```

**Output**

```text
n = 7
(One-sided) band width w = 2
Diagonal elements of A = 5 -4  6  1 -4  6  1 -4  6  1 -4  6  1 -4  6  1 -4  5

Reconstructed A =
  5 -4  1  0  0  0  0
 -4  6 -4  1  0  0  0
  1 -4  6 -4  1  0  0
  0  1 -4  6 -4  1  0
  0  0  1 -4  6 -4  1
  0  0  0  1 -4  6 -4
  0  0  0  0  1 -4  5

Expected eigenvalues:
          0.023          0.343          1.524          4.000          7.647         11.657         14.805

Calculated eigenvalues: lambda =
          0.023          0.343          1.524          4.000          7.647         11.657         14.805

Eigenvectors (as columns): x =
         -0.191         -0.354          0.462          0.500          0.462          0.354          0.191
         -0.354         -0.500          0.354          0.000         -0.354         -0.500         -0.354
         -0.462         -0.354         -0.191         -0.500         -0.191          0.354          0.462
         -0.500         -0.000         -0.500          0.000          0.500          0.000         -0.500
         -0.462          0.354         -0.191          0.500         -0.191         -0.354          0.462
         -0.354          0.500          0.354         -0.000         -0.354          0.500         -0.354
         -0.191          0.354          0.462         -0.500          0.462         -0.354          0.191
Press enter to quit
```

### Symmetric Tridiagonal Matrices

Symmetric tridiagonal matrices can be handled using the following routines: `eigts1`, `eigts2`, `eigts3`, and `eigts4`.

1. **All Eigenvalues**  
   ```pascal
   procedure eigts1(var d, cd: ArbFloat; n: ArbInt; var lam: ArbFloat; var term: ArbInt);
   ```
   This procedure computes all eigenvalues of the matrix.

2. **Some Eigenvalues (with indices k1..k2)**  
   ```pascal
   procedure eigts2(var d, cd: ArbFloat; n, k1, k2: ArbInt; var lam: ArbFloat; var term: ArbInt);
   ```
   This computes the eigenvalues within the index range `k1` to `k2`.

3. **All Eigenvalues and Eigenvectors**  
   ```pascal
   procedure eigts3(var d, cd: ArbFloat; n: ArbInt; var lam, x: ArbFloat; rwidth: ArbInt; var term: ArbInt);
   ```
   This calculates all eigenvalues and their corresponding eigenvectors.

4. **Some Eigenvalues and Eigenvectors (with indices k1..k2)**  
   ```pascal
   procedure eigts4(var d, cd: ArbFloat; n, k1, k2: ArbInt; var lam, x: ArbFloat; rwidth: ArbInt; var m2, term: ArbInt);
   ```
   This computes eigenvalues and eigenvectors within the specified index range `k1` to `k2`.

---

**Parameters**

- **d**: A 1D array representing the main diagonal of the matrix. It must contain at least `n` elements.
- **cd**: A 1D array representing the subdiagonal of the matrix. It must have at least `n-1` elements.
- **n**: The size of the matrix (number of rows or columns), which must be square.
- **k1, k2**: The index range for the eigenvalues and eigenvectors to be calculated, where `1 <= k1 <= k2 <= n`.
- **lam**: A 1D array to store the calculated eigenvalues. For routines `eigts1` and `eigts3`, it must contain at least `n` elements. For `eigts2` and `eigts4`, it only needs to store `k2 - k1 + 1` elements.
- **x**: A 2D array to store the calculated eigenvectors. For `eigts3`, the array must have at least `n^2` elements. For `eigts4`, it must provide space for `n * (k2 - k1 + 1)` elements. Eigenvectors are normalized to unit length and stored in columns.
- **rwidth**: The row length of the matrix `x`. This allows the result matrix to be dimensioned larger than necessary.
- **term**: An error code returned after calculation:
  - `1`: Successful calculation.
  - `2`: Calculation failed.
  - `3`: Error in input data (e.g., `n < 1`, `k1 < 1`, `k1 > k2`, or `k2 > n`).

This explanation organizes the routines, clearly defines the parameters, and explains the error codes in a structured way for easier understanding.

**Example**

Calculate eigenvalues and eigenvectors of the matrix:

$$
\displaystyle{  A=
 \begin{bmatrix}
  1 & 1 & 0 & 0 \\
  1 & 1 & 2 & 0 \\
  0 & 2 & 1 & 1 \\
  0 & 0 & 1 & 1
 \end{bmatrix}
 }
$$

The expected eigenvalues are:

$$
\displaystyle{  -\sqrt{2},\ 2-\sqrt{2},\ \sqrt{2},\ 2+\sqrt{2} }
$$


```pascal linenums="1" hl_lines="1"
program eig_symtridiag_matrix;

{$mode objfpc}{$H+}{$J-}

uses
  typ, eig;

const
  n_mat_size = 4;

var
  // This var contains the elements of the main diagonal of the input matrix
  diag_elements: array[1..n_mat_size] of ArbFloat = (1, 1, 1, 1);

  // This var contains the elements of the subdiagonal
  subdiag_elements: array[2..n_mat_size] of ArbFloat = (1, 2, 1);

  // Array to store the calculated eigenvalues
  eig_values_lambda: array[1..n_mat_size] of ArbFloat;

  // Matrix to store the calculated eigenvectors
  eig_vectors_mat_x: array[1..n_mat_size, 1..n_mat_size] of ArbFloat;

  // Variable for capturing the result of the eigenvalue/eigenvector calculation
  term: integer = 0;

  // Variables for loops
  i, j, k: integer;

begin

  // Write elements of diagonal
  WriteLn('n = ', n_mat_size);
  Write('Elements of main diagonal = ', diag_elements[1]:0:0);
  for k := 2 to n_mat_size do
    Write(diag_elements[k]:3:0);
  WriteLn;

  // Write elements of sub-diagonal
  Write('Elements of subdiagonal   = ', ' ':3, subdiag_elements[2]:0:0);
  for k := 3 to n_mat_size do
    Write(subdiag_elements[k]:3:0);
  WriteLn;
  WriteLn;

  // Write reconstructed band input matrix (not needed for calculation)
  WriteLn('Reconstructed A = ');
  for i := 1 to n_mat_size do begin
    for j := 1 to n_mat_size do begin
      if j = i then
        Write(diag_elements[i]:3:0)
      else if (j = i-1) then
        Write(subdiag_elements[i]:3:0)
      else if (j = i+1) then
        Write(subdiag_elements[i+1]:3:0)
      else
        Write(0.0:3:0);
    end;
    WriteLn;
  end;
  WriteLn;

  // Calculate eigenvalues/vectors
  eigts3(diag_elements[1], subdiag_elements[2], n_mat_size, eig_values_lambda[1], eig_vectors_mat_x[1,1], n_mat_size, term);
  if term <> 1 then begin
    WriteLn('term = ', term, ' --> ERROR');
    halt;
  end;

  // Write expected results of eigenvalues
  WriteLn('Expected eigenvalues:');
  Write(-sqrt(2):15:3, 2-sqrt(2):15:3, sqrt(2):15:3, 2+sqrt(2):15:3);
  WriteLn;
  WriteLn;

  // write eigenvalues
  WriteLn('Calculated eigenvalues: lambda = ');
  for i := 1 to n_mat_size do
    Write(eig_values_lambda[i]:15:3);
  WriteLn;
  WriteLn;

  // Write eigenvectors
  WriteLn('Eigenvectors (as columns): x = ');
  for i := 1 to n_mat_size do begin
    for j := 1 to n_mat_size do
      Write(eig_vectors_mat_x[i, j]:15:3);
    WriteLn;
  end;

  // Pause to allow user to see results before exiting the program
  WriteLn('Press enter to quit');
  ReadLn;
end.
```

**Output**

```text
n = 4
Elements of main diagonal = 1  1  1  1
Elements of subdiagonal   =    1  2  1

Reconstructed A =
  1  1  0  0
  1  1  2  0
  0  2  1  1
  0  0  1  1

Expected eigenvalues:
         -1.414          0.586          1.414          3.414

Calculated eigenvalues: lambda =
         -1.414          0.586          1.414          3.414

Eigenvectors (as columns): x =
         -0.271         -0.653          0.653          0.271
          0.653          0.271          0.271          0.653
         -0.653          0.271         -0.271          0.653
          0.271         -0.653         -0.653          0.271
Press enter to quit
```


## Unit `roo` - Finding the roots of a function

### Roots of a Polynomial

A polynomial of degree `n` 

$$
\displaystyle{  z^n + a_1 z^{n-1} + a_2 z^{n-2} + ... + a_{n-1} z + a_n = 0 }
$$

always has `n` complex solutions, though they may not all be distinct. The datatype `complex` is described in the section on complex numbers. You can calculate the roots using the `roopol` function.

!!! Note

    The polynomial must be normalized, meaning the coefficient of the highest-degree term should be 1.

```pascal
procedure roopol(var a: ArbFloat; n: ArbInt; var z: complex; var k, term: ArbInt);
```

**Parameters**

- **a**: An array containing the polynomial coefficients, ordered from highest to lowest degree. 
    - The polynomial must be normalized, meaning the coefficient of the highest-degree term should be 1. This coefficient is **not** included in the array, so the array must have at least `n` elements. 
    - Since only real polynomials are handled, the array elements should be of type `ArbFloat`. Note: the data organization for this array is different from other polynomial routines in this library.
  
- **n**: The degree of the polynomial. It must be a positive integer.

- **z**: An array of complex values that will store the roots of the polynomial. The array must have at least `n` elements. The values returned are undefined if an error occurs (i.e., if `term <> 1`).

- **k**: The number of roots found. This value should always equal `n`; if it doesn't, an error has occurred.

- **term**: An error code returned by the function:
    - `1`: Successful completion, and the array `z` contains valid root data.
    - `2`: Not all roots were found (`k < n`).
    - `3`: Error in input data (`n < 1`).

**Example**

Calculate the roots of the polynomial $z^5 + 3 z^4 + 4 z^3 - 8 z^2$. The expected zero points are:

$$
\displaystyle{  z_1=0,\ z_2=0,\ z_3=1,\ z_4=-2+2i,\ z_5=-2-2i }
$$

```pascal linenums="1" hl_lines="28"
program solve_root_polynomials;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils, typ, roo;

const
  n = 5;

var
  a: array[1..n] of ArbFloat = (3, 4, -8, 0, 0);
  z: array[1..n] of complex;
  k: ArbInt;
  term: ArbInt;
  i: integer;
  c: complex;

  function ComplexToStr(z: complex; Decimals: integer): string;
  const
    SIGN: array[boolean] of string = ('+', '-');
  begin
    Result := Format('%.*f %s %.*f i', [Decimals, z.re, SIGN[z.im <0], Decimals, abs(z.im)]);
  end;

begin
  // Solve equation
  roopol(a[1], n, z[1], k, term);

  if term = 1 then begin
    // Display results
    WriteLn('Results of procedure roopol:');
    for i:=1 to n do
      WriteLn('  Solution #', i, ': ', ComplexToStr(z[i], 6):20);
    WriteLn;

    // Display expected results
    Writeln('Expected results:');
    c.Init(0, 0);  // z1 = 0
    WriteLn('  Solution #1: ', complexToStr(c, 6):20);
    c.Init(0, 0);  // z2 = 0
    WriteLn('  Solution #2: ', complexToStr(c, 6):20);
    c.Init(1, 0);  // z3 = 1
    WriteLn('  Solution #3: ', complexToStr(c, 6):20);
    c.Init(-2, +2);  // z4 = -2 + 2 i
    WriteLn('  Solution #4: ', complexToStr(c, 6):20);
    c.Init(-2, -2);  // z4 = -2 - 2 i
    WriteLn('  Solution #5: ', complexToStr(c, 6):20);
  end else
    WriteLn('ERROR');

  // Pause to allow user to see results before exiting the program
  WriteLn('Press enter to quit');
  ReadLn;
end.
```

### Roots of a Quadratic Equation

A quadratic equation is a polynomial of degree 2. It always has two complex roots, though they may not be distinct. You can find these roots using the `rooqua` procedure.

```pascal
procedure rooqua(p, q: ArbFloat; var z1, z2: complex);
```

**Parameters**

- **p**: The coefficient of the linear term in the quadratic equation.
- **q**: The constant term of the quadratic equation.
- **z1, z2**: Variables that will store the two complex roots of the equation. The type `complex` is declared in the unit `typ`.


`rooqua` assumes that the quadratic term has been normalized, meaning the coefficient of the quadratic term is set to 1.


**Example**

Determine the roots of the equation $z^2 + 2 z + 5 = 0$.

```pascal linenums="1" hl_lines="15"
program solve_root_quadratic;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils, typ, roo;

var
  z1, z2: complex;

const
  SIGN: array[boolean] of string = ('+', '-');

begin
  rooqua(2, 5, z1, z2);

  WriteLn(Format('1st solution: %g %s %g i', [z1.re, SIGN[z1.im < 0], abs(z1.im)]));
  WriteLn(Format('2nd solution: %g %s %g i', [z2.re, SIGN[z2.im < 0], abs(z2.im)]));

  // Pause to allow user to see results before exiting the program
  WriteLn('Press enter to quit');
  ReadLn;
end.
```

### Roots of the Binomial Equation

The binomial equation \( z^n = a \) is a special type of polynomial where all terms except for the highest- and lowest-order terms have zero coefficients. It has `n` complex solutions, which can be calculated using the `roobin` procedure.

```pascal
procedure roobin(n: ArbInt; a: complex; var z: complex; var term: ArbInt);
```

**Parameters**

- **n**: The exponent in the binomial equation. It must be a positive integer.
- **a**: The constant term on the right-hand side of the equation. This is expected to be a complex number (refer to the section on complex numbers).
- **z**: An array of type `complex` that will store the calculated roots. It must have space for at least `n` complex values.
- **term**: An error code returned by the procedure:
    - `1`: Successful termination.
    - `2`: Error in input data (`n < 1`).

**Example**

Calculate the roots of the equation:

$$
\displaystyle{ z^4 = -1 }
$$

The exact solutions are

$$
\displaystyle{ z_1 = \frac{1}{2} \sqrt{2} (1+i) ,\ z_2 = \frac{1}{2} \sqrt{2} (1-i) ,\ z_3= \frac{1}{2} \sqrt{2} (-1+i) ,\ z_4= \frac{1}{2} \sqrt{2} (-1-i) }
$$

```pascal linenums="1" hl_lines="30"
program solve_root_binomial;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils, typ, roo;

const
  n = 4;

var
  z: array[1..n] of complex;
  term: ArbInt;
  i: Integer;
  a: complex;
  c: complex;

  function ComplexToStr(z: complex; Decimals: Integer): string;
  const
    SIGN: array[boolean] of string = ('+', '-');
  begin
    Result := Format('%.*g %s %.*g i', [Decimals, z.re, SIGN[z.im < 0], Decimals, abs(z.im)]);
  end;

begin
  // Prepare constant term as a complex value
  a.Init(-1, 0);

  // Solve equation
  roobin(n, a, z[1], term);

  if term = 1 then begin
    // Display results
    WriteLn('Results of procedure roobin:');
    for i:=1 to n do
      WriteLn('  Solution #', i, ': ', ComplexToStr(z[i], 6):20);
    WriteLn;

    // Display expected results
    Writeln('Expected results:');
    c.Init(1, 1);
    c.Scale(0.5*sqrt(2));
    WriteLn('  Solution #1: ', complexToStr(c, 6):20);
    c.Init(1, -1);
    c.Scale(0.5*sqrt(2));
    WriteLn('  Solution #2: ', complexToStr(c, 6):20);
    c.Init(-1, 1);
    c.Scale(0.5*sqrt(2));
    WriteLn('  Solution #3: ', complexToStr(c, 6):20);
    c.Init(-1, -1);
    c.Scale(0.5*sqrt(2));
    WriteLn('  Solution #4: ', complexToStr(c, 6):20);
  end else
    WriteLn('ERROR');

  // Pause to allow user to see results before exiting the program
  WriteLn('Press enter to quit');
  ReadLn;
end.
```

**Output**

```text
Results of procedure roobin:
  Solution #1: 0.707107 + 0.707107 i
  Solution #2: 0.707107 - 0.707107 i
  Solution #3: -0.707107 + 0.707107 i
  Solution #4: -0.707107 - 0.707107 i

Expected results:
  Solution #1: 0.707107 + 0.707107 i
  Solution #2: 0.707107 - 0.707107 i
  Solution #3: -0.707107 + 0.707107 i
  Solution #4: -0.707107 - 0.707107 i
Press enter to quit
```


### Bisection Method

The bisection method is used to estimate the root of a function by identifying two values, `a` and `b`, such that the function has opposite signs at those points. The midpoint of the interval is calculated, and the subinterval where the function's signs differ is selected for the next iteration. This process continues until the desired precision (i.e., interval length) is achieved.

In **NumLib**, this method is implemented using the `roof1r` procedure:

```pascal
procedure roof1r(f: rfunc1r; a, b, ae, re: ArbFloat; var x: ArbFloat; var term: ArbInt);
```

**Parameters**

- **f**: The function for which the root is to be determined. It must take a single floating-point argument of type `ArbFloat`. The function type `rfunc1r` is declared in the `typ` unit.
- **a, b**: The endpoints of the interval. The root must lie between these two values, meaning the function values `f(a)` and `f(b)` should have opposite signs.
- **ae, re**: These determine the absolute (`ae`) and relative (`re`) precision of the root. `re` is relative to the maximum of `abs(a)` and `abs(b)`. Higher accuracy is achieved by setting `ae` to `MachEps` (from the `typ` unit). Both parameters must be non-negative.
- **x**: The variable that returns the found root.
- **term**: An error code indicating the result of the process:
    - `1`: Successful termination. A root has been found with the specified absolute or relative precision.
    - `2`: The required accuracy could not be achieved, but the value of `x` is the "best achievable" approximation.
    - `3`: Input error: `ae < 0` or `re < 0`, or the function values at `a` and `b` do not have opposite signs.

**Example**

The following program uses the bisection method to find the square root of 2. The root is the value of `x` where the function \( f(x) = x^2 - 2 \) equals zero. Since \( f(1) = -1 \) and \( f(2) = 2 \), the root lies between 1 and 2.

```pascal linenums="1" hl_lines="16"
program solve_root_bisection;

uses
  typ, roo;

function f(x: ArbFloat): ArbFloat;
begin
  Result := x*x - 2;
end;

var
  x: ArbFloat = 0.0;
  term: ArbInt;

begin
  roof1r(@f, 1.0, 2.0, 1e-9, 0, x, term);
  WriteLn('Bisection result: ', x);
  WriteLn('sqrt(2):          ', sqrt(2.0));
end.
```

**Output**

```text
Bisection result:  1.4142135621888698E+000
sqrt(2):           1.4142135623730951E+000
Press enter to quit
```

Here’s a tidied version of your text:

---

### Roots of a System of Nonlinear Equations

We want to find the roots of a system of nonlinear equations:

$$ f_{i}(x_1, x_2, \ldots, x_n) = 0, \; i=1,2,\ldots,n $$

The **`roofnr`** procedure can be used to find the roots of such a system:

```pascal
procedure roofnr(f: roofnrfunc; n: ArbInt; var x, residu: ArbFloat; ra: ArbFloat; var term: ArbInt);
```

**Parameters**

- **f**: The address of the procedure that calculates the function values \( f_i(x_1, x_2, \dots, x_n) \). The function type `roofnrfunc` is declared in the `typ` unit:
  ```pascal
  type
    roofnrfunc = procedure(var x, fx: ArbFloat; var deff: boolean);
  ```
  - **x**: An array of at least `n` `ArbFloat` values, providing the \( x_j \) values at which the functions are evaluated.
  - **fx**: An array to store the calculated values of the functions \( f_i \).
  - **deff**: A boolean flag that can be used to stop the root-finding process based on a condition.
  
For example, to solve the system of equations:

$$ 
\begin{array}{l}
f_1(x_1, x_2) = x_1^2 + x_2^2 - 2 = 0 \\
f_2(x_1, x_2) = -(x_1 - 1)^2 + x_2 = 0
\end{array}
$$

The function `f` can be written as:

```pascal
procedure func(var x1, f1x: real; var deff: boolean); 
var 
  x: array[1..2] of real absolute x1;
  f: array[1..2] of real absolute f1x;
begin
  f[1] := sqr(x[1]) + sqr(x[2]) - 2;
  f[2] := -sqr(x[1] - 1) + x[2];
end;
```

To stop the process when \( x_1 < 1 \), set `deff` to `false`:

```pascal
procedure func(var x1, f1x: real; var deff: boolean); far;
var 
  x: array[1..2] of real absolute x1;
  f: array[1..2] of real absolute f1x;
begin
  deff := x[1] >= 1;
  if deff then begin
    f[1] := sqr(x[1]) + sqr(x[2]) - 2;
    f[2] := -sqr(x[1] - 1) + x[2];
  end;
end;
```

**Parameters**

- **n**: The number of equations or variables ( \( x_i \) ) in the system.
- **x**: The first element of an array (with at least `n` values) used for both input and output. Initially, it should contain estimates of the roots. After computation, it holds the found roots.
- **residu**: The 2-norm of the vector of residuals in the solution:
  $$ \|f\|_2 = \sqrt{\sum_{i=1}^{n} f_i^2} $$
- **ra**: The relative accuracy for calculating the solution. Typical values are \( 10^{-3}, 10^{-5}, 10^{-8} \) depending on the precision (`single`, `real`, or `double`).
- **term**: An error code indicating the result:
    - `1`: Successful solution with desired accuracy.
    - `2`: The solution accuracy could not be achieved, but the returned `x` values are the best achievable.
    - `3`: Incorrect input data (e.g., `n < 0` or `re < 0`).
    - `4`: The calculation process was stopped due to exceeding function call limits.
    - `5`: Insufficient progress—no solution found or needs a different starting value.
    - `6`: The procedure attempted to compute a value outside the range defined by `deff`.


**Example**

Solve the system of equations:

$$ 
\begin{array}{l}
f_1(x_1, x_2) = x_1^2 + x_2^2 - 2 = 0 \\
f_2(x_1, x_2) = -(x_1 - 1)^2 + x_2 = 0
\end{array}
$$

```pascal linenums="1" hl_lines="35"
program solve_root_nonlinear;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils, typ, roo;

const
  n = 2;
  ra = 1e-10;

var
  x: array[1..n] of ArbFloat;
  f_check: array[1..n] of ArbFloat;
  residu: ArbFloat;
  i: integer;
  term: integer;
  deff: boolean;

  procedure funcs(var x0, fx: ArbFloat; var deff: boolean);
  var
    xloc: array[1..n] of ArbFloat absolute x0;
    f: array[1..n] of ArbFloat absolute fx;
  begin
    f[1] := sqr(xloc[1]) + sqr(xloc[2]) - 2;
    f[2] := -sqr(xloc[1] - 1) + xloc[2];
  end;

begin
  // Initial guess values
  x[1] := 0;
  x[2] := 0;

  // Solve the equation system
  roofnr(@funcs, n, x[1], residu, ra, term);

  WriteLn('term = ', term);
  WriteLn;

  if term in [1, 2] then begin
    WriteLn('Results found by procedure roofnr:');
    for i := 1 to n do
      WriteLn('Solution #' + IntToStr(i) + ': ', x[i]:0:6);
    WriteLn('Norm of residuals: ', residu:0:15);
  end else
    WriteLn('ERROR');

  // Pause to allow user to see results before exiting the program
  WriteLn('Press enter to quit');
  ReadLn;
end.
```

**Output**

```text
term = 1

Results found by procedure roofnr:
Solution #1: 1.404698
Solution #2: 0.163780
Norm of residuals: 0.000000000000000
Press enter to quit
```

## Unit `int` - Numerical integration of a function


The `int1fr` function in the `NumLib` library calculates the integral of a given function between limits `a` and `b`, with a specified absolute accuracy `ae`:

```pascal
procedure int1fr(f: rfunc1r; a, b, ae: ArbFloat; var integral, err: ArbFloat; var term: ArbInt);
```

**Parameters**

- **`f`**: A pointer to the function to be integrated. The function must take a single real variable of type `ArbFloat` and return a value of type `ArbFloat`. The function type is declared as `rfunc1r` in the `typ` unit.
  
- **`a, b`**: The integration limits. These can also be set to `+/-Infinity` to allow integration over infinite intervals. The order of `a` and `b` follows correct mathematical conventions.

- **`ae`**: The required absolute accuracy for the result.

- **`integral`**: The result of the integration. It is valid only if `term = 1`.

- **`err`**: The error in accuracy if the requested precision could not be achieved. In this case, `term = 2`.

- **`term`**: The termination status code:
    - `1`: Integration successful with absolute accuracy `ae`.
    - `2`: Requested accuracy not reached, but the result is approximated with error `err`.
    - `3`: Invalid input (e.g., `ae < 0`, or both `a` and `b` are infinite).
    - `4`: Integration failed due to divergence or slow convergence.



**Example**

This example demonstrates calculating the integral:

\[
\int_a^b \frac{1}{x^2} \, dx
\]

for various integration limits `a` and `b`. Since the function diverges at `x = 0`, the integration interval must not include this point. The expected analytic result is provided for comparison.

```pascal linenums="1" hl_lines="40"
program integrate;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils, Math, typ, int;

// Function representing 1/x^2
function recipx2(x: ArbFloat): ArbFloat;
begin
  Result := 1.0 / sqr(x);
end;

// Analytic result of the integral
function integral_recipx2(a, b: ArbFloat): Arbfloat;
begin
  if a = 0 then
    a := Infinity
  else if a = Infinity then
    a := 0.0
  else
    a := -1 / a;

  if b = 0 then
    b := Infinity
  else if b = Infinity then
    b := 0.0
  else
    b := -1 / b;

  Result := b - a;
end;

// Execute the integration and handle the result
procedure Execute(a, b: ArbFloat);
var
  err: ArbFloat = 0.0;
  term: ArbInt = 0;
  integral: ArbFloat = 0.0;
begin
  try
    int1fr(@recipx2, a, b, 1e-9, integral, err, term);
  except
    term := 4;
  end;

  Write('  The integral from ' + FloatToStr(a) + ' to ' + FloatToStr(b));

  case term of
    1: WriteLn(' is ', integral:0:9, ', expected: ', integral_recipx2(a, b):0:9);
    2: WriteLn(' is ', integral:0:9, ', error: ', err:0:9, ', expected: ', integral_recipx2(a, b):0:9);
    3: WriteLn(' cannot be calculated: Invalid input.');
    4: WriteLn(' cannot be calculated: Divergence or slow convergence.');
  end;
end;

begin
  WriteLn('Integral of f(x) = 1/x^2');
  Execute(1.0, 2.0);
  Execute(1.0, 1.0);
  Execute(2.0, 1.0);
  Execute(1.0, Infinity);
  Execute(-Infinity, -1.0);
  Execute(0.0, Infinity);
  // Note: The following case will raise an exception in some environments, but works outside the IDE.
  // Execute(-1.0, Infinity);

  // Pause to allow user to see results before exiting the program
  WriteLn('Press enter to quit');
  ReadLn;
end.
```

**Notes**

- **`recipx2`**: This function returns \( \frac{1}{x^2} \), which is the integrand.
  
- **`integral_recipx2`**: Computes the expected analytic result for the given integration limits.

- **`Execute`**: Runs the integration for the specified limits `a` and `b`, handles potential errors, and prints the result.

**Output**

```text
Integral of f(x) = 1/x^2
  The integral from 1 to 2 is 0.500000000, expected: 0.500000000
  The integral from 1 to 1 is 0.000000000, expected: 0.000000000
  The integral from 2 to 1 is -0.500000000, expected: -0.500000000
  The integral from 1 to +Inf is 1.000000000, expected: 1.000000000
  The integral from -Inf to -1 is 1.000000000, expected: 1.000000000
  The integral from 0 to +Inf cannot be calculated: Divergence or slow convergence.
Press enter to quit
```

## Unit `ode` - Ordinary differential equations


### Solving a Single First-Order Differential Equation

The procedure `odeiv1` solves an initial value problem for a first-order differential equation of the form:

\[
\begin{cases}
y' = f(x, y), \qquad x \in [a, b] \\
y(a) = \alpha
\end{cases}
\]

where `a`, `b`, `f`, and the initial condition `y(a) = α` are given.


```pascal
procedure odeiv1(f: rfunc2r; a, ya: ArbFloat; var b, yb: ArbFloat; ae: ArbFloat; var term: ArbInt);
```

**Parameters**

- `f`: The function to be solved, depending on two real variables and returning a real value. Defined as `type rfunc2r = function(x, y: ArbFloat): ArbFloat`.
- `a`: The starting `x` value of the interval.
- `ya`: The initial value `α` at `x = a`.
- `b`: The end `x` value of the interval. After the calculation, if `term = 2`, `b` contains the new endpoint with the required accuracy, `ae`.
- `yb`: Returns the computed value `y(b)` if `term < 3`. If `term = 3`, the result is undefined.
- `ae`: Specifies the absolute accuracy required for `y(b)`.
- `term`: Returns the following error codes:
    - `1`: Successful completion.
    - `2`: The solution could not reach `b` with the required accuracy. `yb` is an approximation at the delivered `b`.
    - `3`: Input error, `ae <= 0`.

This algorithm is based on a fifth-order adaptive Runge-Kutta method. It may not be accurate for stiff differential equations, and accuracy issues can arise in unstable problems where small variations in `y(a)` cause large variations in `y(b)`.

If you want to solve for multiple points, like from `x = 0` to `x = 1` with a step size of 0.1, avoid restarting at each step and "integrate" instead.

**Example**

Solve the equation $y'' = -10  (y - x^2)$ with initial condition $y(0) = 0$ and compare it to the exact solution $y(x) = -0.02  exp(-10  x) + x^2 - 0.2  x + 0.02$.

```pascal linenums="1"  hl_lines="52"
program solve_ode;

{$mode objfpc}{$H+}{$J-}

uses
  typ, // This unit contains common type definitions used in the program.
  ode; // This unit provides the ODE solver `odeiv1` used in the program.

// Define the function representing the differential equation: y' = f(x, y).
// In this case, f(x, y) = -10 * (y - x^2), a basic second-order ODE.
function f(x, y: ArbFloat): ArbFloat;
begin
  Result := -10 * (y - sqr(x)); // sqr(x) calculates x^2
end;

// Define the exact analytical solution for comparison purposes.
// The solution to the differential equation is given by y(x) = -0.02 * exp(-10 * x) + x^2 - 0.2 * x + 0.02.
function exact(x: real): real; far;
begin
  Result := -0.02 * exp(-10 * x) + sqr(x) - 0.2 * x + 0.02;
end;

const
  d = 0.5;  // The length of the interval over which to solve the ODE.
  ae = 1e-5; // The absolute error tolerance for the ODE solver.
  n = 10;    // The number of steps to take between the start and end of the interval.

var
  a, b, ya, yb: ArbFloat; // Variables representing the interval endpoints and function values.
  term: ArbInt;           // Variable to hold the error code returned by the ODE solver.
  i: ArbInt;              // Loop counter.

begin
  // Set initial conditions for the ODE solver.
  a := 0.0;      // Start at x = 0.
  b := a + d;    // End of the first step at x = 0.5 (the interval length).
  ya := 0.0;     // Initial condition: y(0) = 0.

  // Print table headers for output (x, y, exact solution, error code).
  WriteLn('x':12, 'y':12, 'exact':12, 'error code':17);
  WriteLn;

  // Output the initial condition at x = 0.
  WriteLn(a:12:5, ya:12:5, exact(a):12:5, '-':17); // Display the starting point.

  // Loop to solve the ODE in steps of 0.5 (controlled by 'd') until n steps are done.
  for i := 1 to n do
  begin
    // Call the ODE solver (odeiv1) to compute y(b) based on the function 'f'.
    // a: start of the interval, ya: initial value y(a), b: end of interval,
    // yb: result of y(b), ae: accuracy, term: error code.
    odeiv1(@f, a, ya, b, yb, ae, term);

    // Output the results after each step: x = b, y(b), exact solution, and error code.
    WriteLn(b:12:5, yb:12:5, exact(b):12:5, term:17);

    // Update for the next step: set a = b and ya = yb, and increment b by 'd' for the next interval.
    a := b;     // Move to the next interval.
    ya := yb;   // Use the computed value y(b) as the initial value for the next step.
    b := b + d; // Increment b for the next step.
  end;

  // Pause to allow user to see results before exiting the program
  WriteLn('Press enter to quit');
  ReadLn;
end.
```

**Output**

```text
           x           y       exact       error code

     0.00000     0.00000     0.00000                -
     0.50000     0.16986     0.16987                1
     1.00000     0.82000     0.82000                1
     1.50000     1.97000     1.97000                1
     2.00000     3.62000     3.62000                1
     2.50000     5.77000     5.77000                1
     3.00000     8.42000     8.42000                1
     3.50000    11.57000    11.57000                1
     4.00000    15.22000    15.22000                1
     4.50000    19.37000    19.37000                1
     5.00000    24.02000    24.02000                1
Press enter to quit
```

### Solving a System of First-Order Differential Equations

To solve a system of first-order differential equations:

\[
\begin{cases}
\mathbf{y}' = \mathbf{f}(x, \mathbf{y}), \qquad x \in [a, b] \\
\mathbf{y}(a) = \alpha
\end{cases}
\]

where:

- `y` is a vector $[y_1(x), y_2(x), ..., y_n(x)]$,
- `f(x, y)` is a vector function $[f_1(x, y), f_2(x, y), ..., f_n(x, y)]$,
- The initial conditions are given as $y(a) = [y_1(a), y_2(a), ..., y_n(a)]$.

This can be solved using the procedure `odeiv2`, which uses an adaptive fifth-order Runge-Kutta method with variable step size.


```pascal
procedure odeiv2(f: oderk1n; a: ArbFloat; var ya, b, yb: ArbFloat; n: ArbInt; ae: ArbFloat; var term: ArbInt);
```

**Parameters**

- `f`: The procedure calculating $f_i(x, y_i)$ values. It is of type `oderk1n = procedure(x: ArbFloat; var y, fxy: ArbFloat)`.
- `a`: Starting point of the calculation.
- `ya`: Initial values for the system, stored in an array.
- `b`: Endpoint of the interval. If `term = 2`, `b` will be updated.
- `yb`: Stores the results.
- `n`: Number of equations in the system.
- `ae`: Specifies absolute accuracy.
- `term`: Error codes:
  - `1`: Successful completion.
  - `2`: Solution couldn't reach `b` with desired accuracy. `yb` contains approximations.
  - `3`: Input error, `n < 1` or `ae <= 0`.

**Example**

Integrate the following system of ODEs between `x = 0` and `x = 1`:

\[
\begin{cases}
y'_1 = 2xy_1 + y_2 \\
y'_2 = -y_1 + 2xy_2 \\
y_1(0) = 0, \quad y_2(0) = 1
\end{cases}
\]

The exact solutions are:

\[
\begin{cases}
y_1(x) = \exp(x^2) \sin(x) \\
y_2(x) = \exp(x^2) \cos(x)
\end{cases}
\]

```pascal linenums="1" hl_lines="69"
program solve_ode_sys;

{$mode objfpc}{$H+}{$J-}

uses
  typ,  // Includes necessary types for operations with numlib.
  ode;  // Provides the ODE solver used in the program.

const
  ae = 1e-5;  // Absolute error tolerance for the ODE solver.

  // This procedure defines the system of differential equations to solve.
  // It takes the independent variable 'x' and the current values 'y' and computes the derivatives 'f(x,y)'.
  procedure f(x: ArbFloat; var y, fxy: ArbFloat);
  var
    // These arrays represent the system of equations.
    _y: array[1..2] of ArbFloat absolute y;
    // Current y values (dependent variables).
    _fxy: array[1..2] of ArbFloat absolute fxy;    // Corresponding derivatives (dy/dx).
  begin
    // First equation of the system: dy1/dx = 2*x*y1 + y2
    _fxy[1] := 2 * x * _y[1] + _y[2];
    // Second equation of the system: dy2/dx = -y1 + 2*x*y2
    _fxy[2] := -_y[1] + 2 * x * _y[2];
  end;

  // This function returns the exact solution for y1 at a given 'x'.
  function exact1(x: ArbFloat): ArbFloat;
  begin
    Result := exp(x * x) * sin(x);  // Exact solution for y1 = exp(x^2) * sin(x)
  end;

  // This function returns the exact solution for y2 at a given 'x'.
  function exact2(x: ArbFloat): ArbFloat;
  begin
    Result := exp(x * x) * cos(x);  // Exact solution for y2 = exp(x^2) * cos(x)
  end;

var
  a, b, d: ArbFloat; // 'a' and 'b' are the interval bounds, 'd' is the step size.
  ya, yb: array[1..2] of ArbFloat;  // Arrays to hold y values at 'a' and 'b'.
  term, i, n: ArbInt;
  // 'term' stores the error code, 'i' is the loop index, 'n' is the number of steps.

begin
  // Set initial values.
  a := 0.0;        // Start point for x.
  b := 0.1;        // Initial end point for x.
  d := b - a;      // Step size (difference between 'a' and 'b').
  ya[1] := 0.0;    // Initial condition for y1 (y1(0) = 0).
  ya[2] := 1.0;    // Initial condition for y2 (y2(0) = 1).
  n := 10;         // Number of steps.

  // Print table headers for output.
  WriteLn('x': 12, 'y[1]': 12, 'y[2]': 12, 'exact[1]': 12, 'exact[2]': 12, 'error code': 17);
  WriteLn;

  // Output the initial conditions at x = 0.
  WriteLn(a: 12: 5, ya[1]: 12: 5, ya[2]: 12: 5, exact1(a): 12: 5, exact2(a): 12: 5, '-': 17);

  // Loop through 'n' steps to solve the system over each interval.
  for i := 1 to n do
  begin
    // Call the ODE solver (odeiv2) to compute yb at x = b.
    // 'f' is the function defining the system of ODEs, 'a' is the start point,
    // 'ya[1]' are the initial y-values, 'b' is the end point,
    // 'yb[1]' will store the computed y-values at 'b',
    // '2' indicates the system size (two equations), 'ae' is the accuracy, and 'term' stores the error code.
    odeiv2(@f, a, ya[1], b, yb[1], 2, ae, term);

    // Output the results at each step: x = b, y1(b), y2(b), exact solutions, and error code.
    WriteLn(b: 12: 5, yb[1]: 12: 5, yb[2]: 12: 5, exact1(b): 12: 5, exact2(b): 12: 5, term: 17);

    // Update the values for the next iteration.
    a := b;         // Move to the next interval starting from the current 'b'.
    ya[1] := yb[1]; // Set ya to the computed yb for the next step.
    ya[2] := yb[2];
    b := b + d;     // Increment 'b' by the step size 'd'.
  end;

  // Pause to allow user to see results before exiting the program
  WriteLn('Press enter to quit');
  ReadLn;
end.
```

**Output**

```text
           x        y[1]        y[2]    exact[1]    exact[2]       error code

     0.00000     0.00000     1.00000     0.00000     1.00000                -
     0.10000     0.10084     1.00500     0.10084     1.00500                1
     0.20000     0.20678     1.02006     0.20678     1.02006                1
     0.30000     0.32335     1.04530     0.32335     1.04530                1
     0.40000     0.45699     1.08088     0.45699     1.08088                1
     0.50000     0.61559     1.12684     0.61559     1.12684                1
     0.60000     0.80932     1.18298     0.80932     1.18298                1
     0.70000     1.05157     1.24846     1.05157     1.24846                1
     0.80000     1.36045     1.32129     1.36045     1.32129                1
     0.90000     1.76085     1.39732     1.76085     1.39732                1
     1.00000     2.28736     1.46869     2.28736     1.46869                1
Press enter to quit
```

## Unit `ipf` - Interpolation and fitting

Coming soon.

## Unit `spe` - Special functions

Coming soon.
