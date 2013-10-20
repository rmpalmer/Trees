program main

    use binary_tree, only: binary_node => node, btree => tree, &
        binary_insert => insert, binary_search => search, binary_print => print, binary_init => init

    implicit none

    type(btree) :: b
    type(binary_node), pointer :: result
    integer :: i
    integer, dimension(10) :: vals
    real, dimension(10) :: r

    write (*,'(''Hello from Tree Test'')')
    call random_number(r)
    vals = nint(100.0 * r)
    do i=1,size(vals)
        write (*,'(F12.2,1X,I4)') r(i), vals(i)
    end do
    !
    call binary_init(b)
    do i=1,size(vals)
        result => binary_insert(vals(i), b)
        write (*,'(''inserted '',I4)') result%value
    end do
    do i=1,size(vals)
        result => binary_search(vals(i), b)
        if (associated(result)) then
            write (*,'(''found '',i4)') result%value
        else
            write (*,'(''lost '',i4)') vals(i)
        end if
    end do
    !
    call binary_print(b%head)
    !
    write (*,'(''Goodbye'')')

end program main
