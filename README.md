# Hash tables

Modern Fortran hash tables based on the function `hashword` in [lookup3.c](http:://burtleburtle.net/bob/c/lookup3.c)

## Derived types

Five derived types are available:

Derived type       | Data to be stored
---------------------------------------------------------------
tablechar_t        | Characters of a defined length
tableint32_t       | Integer scalars of 32 bits
tablereal32_t      | Real scalars of 32 bits
table_arrint32_t   | Integer arrays of 32 bits
table_arrreal32_t  | Real arrays of 32 bits

## Public derived type methods

Each derived type contains these ... public methods

Method         | Class      | Description
-------------------------------------------------------------
`getfilled()`    | Function   | Returns the number of elements stored in the DT
`getsize()`      | Function   | Returns the size of the arrays that contains the stored elements
`add(element, index, lnew)`   | Subroutine | Add the element `element` in the DT if not stored yet. The integer `index` (`intent(out), optional`) contains the index of the elements (between 1 and `getfilled(). The logical `lnew` (`intent(out), optional`) is `.true.` if the element is not stored yet. Otherwise it is `.false.`.
`get(index)`           | Function   | Returns the element stored corresponding to the index `index`.
`getindex(element)`    | Function   | Returns the index of the element `element`
`writetable(filename)` | Subroutine | Writes to a file called `filename` all elements and corresponding index stored in the DT.
