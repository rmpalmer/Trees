program main

    use binary_tree, only: binary_node => node, btree => tree, &
        binary_insert => insert, binary_search => search, binary_print => print, binary_init => init

    use rb_tree, only: rb_node => node, rbtree => tree, &
        rb_insert => insert, rb_search => search, rb_print => print, rb_init => init

    implicit none

    type(btree) :: b
    type(binary_node), pointer :: bin_result

    type(rbtree) :: rb
    type(rb_node), pointer :: rb_result

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
        bin_result => binary_insert(vals(i), b)
    end do
!    do i=1,size(vals)
!        bin_result => binary_search(vals(i), b)
!        if (associated(bin_result)) then
!            write (*,'(''found '',i4)') bin_result%value
!        else
!            write (*,'(''did not find '',i4)') vals(i)
!        end if
!    end do
    !
    call binary_print(b%head)
    !
    call rb_init(rb)
    do i=1,size(vals)
        write(*,'(''insert '',i4,i8,'' into rb tree'')') i,vals(i)
        rb_result => rb_insert(vals(i), rb)
    end do
    call rb_print(rb)
    !
    write (*,'(''Goodbye from Tree Test'')')

end program main
