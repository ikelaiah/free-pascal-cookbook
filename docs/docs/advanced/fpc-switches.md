## FPC Switches

The following Markdoien table was converted from the official [Reference chart with all compiler options and switches](http://downloads.freepascal.org/fpc/docs-pdf/chart.pdf).

## Local compiler switches

| **Cmd** | **Short** | **Long**         | **Explanation**                        |
|---------|-----------|------------------|----------------------------------------|
|         | $A        | $ALIGN           | Align Data.                            |
| A       |           | $ASMMODE         | Select assembler mode.                 |
|         | $B        | $BOOLEVAL        | Use complete boolean evaluation.       |
| Sa      | $C        | $ASSERTIONS      | Enable assertion support.              |
|         |           | $CALLING         | Set default calling convention         |
| d       |           | $DEFINE          | Define a symbol.                       |
|         |           | $ELSE            | Switch conditional compilation.        |
|         |           | $ENDIF           | End conditional compilation.           |
|         |           | $ERROR           | Generate error message.                |
|         | $F        |                  | Use far or near functions.             |
|         |           | $FATAL           | Generate fatal error message.          |
| Sg      |           | $GOTO            | SupportGoto andLabel.                  |
|         | $H        | $LONGSTRINGS     | Use AnsiStrings.                       |
|         |           | $HINT            | Generate hint message.                 |
| vh      |           | $HINTS           | Emit hints                             |
|         |           | $IF              | Start conditional compilation.         |
|         |           | $IFDEF           | Start conditional compilation.         |
|         |           | $IFNDEF          | Start conditional compilation.         |
|         |           | $IFOPT           | Start conditional compilation.         |
|         |           | $INFO            | Generate info message.                 |
| Si      |           | $INLINE          | Enable inline code support.            |
| Ci      | $I        | $IOCHECKS        | Include Input/Output checking.         |
|         | $I        | $INCLUDE         | Include file.                          |
|         | $I        | $INCLUDE         | Include compiler info.                 |
|         | $L        | $LINK            | Link object file.                      |
|         |           | $LINKLIB         | Link to a library.                     |
|         | $M        | $TYPEINFO        | Generate Run-Time type information.    |
| Sm      |           | $MACRO           | Enable macro support.                  |
|         |           | $MAXFPUREGISTER  | set maximum number of FPU registers    |
|         |           | $MESSAGE         | Generate info message.                 |
|         |           | $MMX             | Enable Intel MMX support.              |
|         |           | $NOTE            | Generate note message.                 |
| vn      |           | $NOTES           | Emit notes.                            |
| A       |           | $OUTPUTFORMAT    | Select compiler output format.         |
|         | $P        | $OPENSTRINGS     | Use open strings.                      |
|         |           | $PACKENUM        | Specify minimum enumeration type size. |
|         |           | $PACKRECORDS     | Specify Alignment of record elements.  |
| Co      | $Q        | $OVERFLOWCHECKS  | Use overflow checking.                 |
| Cr      | $R        | $RANGECHECKS     | Use range checking.                    |
|         |           | $SATURATION      | Enable saturation operations.          |
| XX      |           | $SMARTLINK       | Use smartlinking.                      |
| St      |           | $STATIC          | Enable use ofStatic keyword.           |
|         |           | $STOP            | Generate fatal error message.          |
|         | $T        | $TYPEDADDRESS    | Enable typed address operator.         |
| u       |           | $UNDEF           | Undefine a symbol.                     |
|         | $V        | $VARSTRINGCHECKS | Use strict var-string checking.        |
|         |           | $WAIT            | Wait for enter key press.              |
|         |           | $WARNING         | Generate warning message.              |
|         |           | $WARNINGS        | Emit warnings.                         |
|         | $X        | $EXTENDEDSYNTAX  | Enable use of extended syntax.         |



## Global compiler switches

| **Cmd** | **Short** | **Long**       | **Explanation**                            |
|---------|-----------|----------------|--------------------------------------------|
|         |           | $APPID         | Set application ID (PalmOS)                |
|         |           | $APPNAME       | Set application name (PalmOS)              |
|         |           | $APPTYPE       | Specify type of application (Windows only) |
| g       | $D        | $DEBUGINFO     | Include debugging symbols.                 |
|         |           | $DESCRIPTION   | Not supported.                             |
|         | $E        |                | Enable emulation of coprocessor.           |
|         | $G        |                | Generate 80286 code.                       |
| Fi      |           | $INCLUDEPATH   | Specify include file search path.          |
|         | $L        | $LOCALSYMBOLS  | Enable local symbol information.           |
| Fl      |           | $LIBRARYPATH   | Specify library search path.               |
|         | $M        | $MEMORY        | Specify memory sizes.                      |
| M       |           | $MODE          | Specify compiler compatibility mode.       |
|         | $N        |                | Enable numeric processing.                 |
|         | $O        |                | Enable overlay code generation.            |
| Fo      |           | $OBJECTPATH    | Specify object file search path.           |
| Ct      | $S        |                | Use stack checking                         |
| Fu      |           | $UNITPATH      | Specify unit file search path.             |
|         |           | $VERSION       | Set DLL version (Windows)                  |
|         | $W        | $STACKFRAMES   | Generate stackframes.                      |
| b       | $Y        | $REFERENCEINFO | Insert browser information.                |
