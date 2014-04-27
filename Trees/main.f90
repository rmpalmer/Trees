program main

    use binary_tree, only: binary_node => node, btree => tree, &
        binary_insert => insert, binary_search => search, binary_print => print, binary_init => init

    implicit none

    type(btree) :: b
    type(binary_node), pointer :: result
    integer :: i
    integer, dimension(100) :: vals
    real, dimension(100) :: r

    write (*,'(''Hello from Tree Test'')')
    call random_number(r)
    vals = nint(10000.0 * r)
    do i=1,size(vals)
        write (*,'(F12.2,1X,I4)') r(i), vals(i)
    end do
    !
    call binary_init(b)
    do i=1,size(vals)
        result => binary_insert(vals(i), b)
    end do
!    do i=1,size(vals)
!        result => binary_search(vals(i), b)
!        if (associated(result)) then
!            write (*,'(''found '',i4)') result%value
!        else
!            write (*,'(''did not find '',i4)') vals(i)
!        end if
!    end do
    !
    call binary_print(b%head)
    !
    write (*,'(''Goodbye from Tree Test'')')

end program main
