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

    public :: node, insert, search, tree, init

contains

subroutine init(t)
    !
    type(tree), intent(inout) :: t
    !
    nullify(t%head)
    t%count = 0
    !
end subroutine init

function insert(value, start) result(inserted_ptr)
    !
    integer, intent(in) :: value
    type(node), pointer, intent(in) :: start
    !
    type(node), pointer :: x
    type(node), pointer :: f
    type(node), pointer :: inserted_ptr
    !
    if (associated(start)) then
        x => start
    else
        nullify(x)
    end if
    nullify(f)
    !write (*,*) 'before do loop'
    do while (associated(x))
        f => x
        if (value < x%value) then
            x => x%left
        else
            x => x%right
        end if
    end do
    !write (*,*) 'allocate x'
    allocate(x)
    nullify(x%left)
    nullify(x%right)
    !write (*,*) 'set value'
    x%value = value
    !write (*,*) 'set f pointers'
    if (associated(f)) then
        if (value < f%value) then
            f%left => x
        else
            f%right => x
        end if
    end if
    !write (*,*) 'set return value'
    inserted_ptr => x
    !
end function insert

function search(value, start) result(found_ptr)
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
            exit
        else if (value < x%value) then
            x => x%left
        else
            x => x%right
        end if
    end do
    write (*,*) 'setting return value'
    found_ptr => x
    !
end function search

end module binary_tree
