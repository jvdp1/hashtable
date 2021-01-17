program test_list
 use modlist, only: listchar_t, listint32_t,listreal32_t
 implicit none
 type(listchar_t) :: list
 type(listint32_t) :: list32
 type(listreal32_t) :: listr32

 list = listchar_t(12)

 call list%add('a')
 call list%add('ab')
 call list%add('abc')
 call list%add('abcdd')
 call list%add('1')
 call list%add('12')
 call list%add('123')
 call list%add('1234')
 call list%add('12345')
 call list%add('12346')
 call list%add('12347')
 call list%add('12348')
 call list%writetable('list0.dat')
 call list%add('12349')
 call list%add('12350')
 call list%add('12351')
 call list%add('12352')
 call list%add('12353')
 call list%add('12353')
 call list%add('12353')

 call list%writetable('list.dat')

 list32 = listint32_t()

 call list32%add(1)
 call list32%add(2)
 call list32%add(5)
 call list32%writetable('listint32.dat')

 listr32 = listreal32_t()

 call listr32%add(1.)
 call listr32%add(2.)
 call listr32%add(5.)
 call listr32%writetable('listreal32.dat')


end program
