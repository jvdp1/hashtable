# Hash tables

Modern Fortran hash tables based on the function `hashword` in [lookup3.c](http://burtleburtle.net/bob/c/lookup3.c)

## Derived types

Five derived types are available:

Derived type       | Data to be stored
-------------------|--------------------------------------------
table_char_t        | Characters of a defined length. Trailing blanks are ignored.
table_int32_t       | Integer scalars of 32 bits
table_real32_t      | Real scalars of 32 bits
table_arrint32_t   | Integer arrays of 32 bits
table_arrreal32_t  | Real arrays of 32 bits

## Public derived type methods

Each derived type contains these 7 public methods.

Method         | Class      | Description
---------------|------------|----------------------------------
`add(element[, index, lnew])`   | Subroutine | Add the element `element` in the DT if not stored yet. The integer `index` (`intent(out), optional`) contains the index of the elements (between 1 and `getfilled()`. The logical `lnew` (`intent(out), optional`) is `.true.` if the element is not stored yet. Otherwise it is `.false.`.
`destroy()`            | Function   | Reset all scalars and arrays
`get(index)`           | Function   | Returns the element stored corresponding to the index `index`.
`getfilled()`          | Function   | Returns the number of elements stored in the DT
`getindex(element)`    | Function   | Returns the index (between 1 and `getfilled()`) of the element `element`
`getsize()`            | Function   | Returns the size of the arrays that contains the stored elements
`writetable(filename)` | Subroutine | Writes to a file called `filename` all elements and corresponding index stored in the DT.
