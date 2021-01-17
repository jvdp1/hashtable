program test_table
 use modtable, only: tablechar_t, tableint32_t, tablereal32_t, table_arrint32_t
 implicit none
 type(tablechar_t) :: table
 type(tableint32_t) :: table32
 type(tablereal32_t) :: tabler32
 type(table_arrint32_t) :: tablearri32

 table = tablechar_t(12)

 call table%add('a')
 call table%add('ab')
 call table%add('abc')
 call table%add('abcdd')
 call table%add('1')
 call table%add('12')
 call table%add('123')
 call table%add('1234')
 call table%add('12345')
 call table%add('12346')
 call table%add('12347')
 call table%add('12348')
 call table%writetable('table0.dat')
 call table%add('12349')
 call table%add('12350')
 call table%add('12351')
 call table%add('12352')
 call table%add('12353')
 call table%add('12353')
 call table%add('12353')

 call table%writetable('table.dat')

 table32 = tableint32_t()

 call table32%add(1)
 call table32%add(2)
 call table32%add(5)
 call table32%writetable('tableint32.dat')

 tabler32 = tablereal32_t()

 call tabler32%add(1.)
 call tabler32%add(2.)
 call tabler32%add(5.)
 call tabler32%writetable('tablereal32.dat')


 tablearri32 = table_arrint32_t(3)

 call tablearri32%add([1,2,3])
 call tablearri32%add([3,2,3])
 call tablearri32%add([1,2,2])
 call tablearri32%writetable('tablearri32.dat')

 call tablearri32%add([1,2,2,3])
end program
