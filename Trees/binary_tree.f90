module binary_tree

    implicit none

    private

    type :: node
        integer :: value
        type(node), pointer :: left
        type(node), pointer :: right
    end type node

    type :: tree
        type(node), pointer :: head
        integer :: count
    end type tree

    public :: node, insert, search, tree, init, print

contains

subroutine init(t)
    !
    type(tree), intent(inout) :: t
    !
    nullify(t%head)
    t%count = 0
    !
end subroutine init

function insert(value, t) result(inserted_ptr)
    !
    integer, intent(in) :: value
    type(tree), intent(inout) :: t
    type(node), pointer :: inserted_ptr
    !
    type(node), pointer :: x
    !
    x => t%head
    inserted_ptr => insert_node(value, x)
    if (.not. associated(t%head)) t%head => x
    !
end function insert

function search(value, t) result(found_ptr)
    !
    integer, intent(in) :: value
    type(tree), intent(inout) :: t
    type(node), pointer :: found_ptr
    !
    type(node), pointer :: x
    !
    x => t%head
    found_ptr => search_node(value, x)
    !
end function search

function insert_node(value, x) result(inserted_ptr)
    !
    integer, intent(in) :: value
    type(node), pointer, intent(inout) :: x
    !
    !type(node), pointer :: x
    type(node), pointer :: f
    type(node), pointer :: inserted_ptr
    !
    !if (associated(start)) then
    !    x => start
    !else
    !    nullify(x)
    !end if
    nullify(f)
    write (*,*) 'before do loop'
    do while (associated(x))
        write (*,*) 'in do loop'
        f => x
        if (value < x%value) then
            x => x%left
        else
            x => x%right
        end if
    end do
    write (*,*) 'allocate x'
    allocate(x)
    nullify(x%left)
    nullify(x%right)
    write (*,*) 'set value'
    x%value = value
    write (*,*) 'set f pointers'
    if (associated(f)) then
        if (value < f%value) then
            f%left => x
        else
            f%right => x
        end if
    end if
    write (*,*) 'set return value'
    inserted_ptr => x
    !
end function insert_node

function search_node(value, start) result(found_ptr)
    !
    integer, intent(in) :: value
    type(node), pointer, intent(in) :: start
    type(node), pointer :: found_ptr
    !
    type(node), pointer :: x
    !
    x => start
    write (*,*) 'before do loop'
    do while (associated(x))
        write (*,*) ' *'
        if (value == x%value) then
            write (*,'(I4,'' is equal to '',I4)') value, x%value
            exit
        else if (value < x%value) then
            write (*,'(I4,'' is less than '',I4)') value, x%value
            x => x%left
        else
            write (*,'(I4,'' is greater than '',I4)') value, x%value
            x => x%right
        end if
    end do
    write (*,*) 'setting return value'
    found_ptr => x
    !
end function search_node

recursive subroutine print(x)
    !
    type(node), pointer, intent(in) :: x
    !
    if (associated(x)) then
        call print(x%left)
        write (*,'(I0)') x%value
        call print(x%right)
    end if
    !
end subroutine print

end module binary_tree
