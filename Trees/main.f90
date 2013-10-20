program main

    use binary_tree, only: binary_node => node, &
        binary_insert => insert, binary_search => search, btree => tree, binary_init => init

    implicit none

    type(btree) :: b
    type(binary_node), pointer :: result
    integer :: value

    write (*,'(''Hello from Tree Test'')')
    value = 42
    !
    call binary_init(b)
    result => binary_insert(value, b%head)
    if (associated(result)) then
        write (*,'(''inserted '',i0)') result%value
    else
        write (*,'(''unable to insert '',i0)') value
    end if
    result => binary_search(value, b%head)
    if (associated(result)) then
        write (*,'(''found '',i0)') result%value
    else
        write (*,'(''unable to find '',i0)') value
    end if
    !
    write (*,'(''Goodbye'')')

end program main
