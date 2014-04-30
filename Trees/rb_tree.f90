module rb_tree

    !
    ! based on pascal language implementation from Sedgewick, Chapter 15
    !
    implicit none

    private

    type :: node
        integer :: value
        type(node), pointer :: left
        type(node), pointer :: right
        logical :: red
    contains
        procedure,nopass :: new_node
        procedure,pass   :: right_red
        procedure,pass   :: left_red
    end type node

    type :: tree
        type(node), pointer :: head
        type(node), pointer :: z
        integer :: count
    end type tree

    public :: node, insert, search, tree, init, print

contains

function new_node()
    type(node), pointer :: new_node
    allocate(new_node)
    new_node%red = .false.
    nullify(new_node%left)
    nullify(new_node%right)
end function new_node

function right_red(this)
    class(node), intent(in) :: this
    logical :: right_red
    right_red = .false.
    right_red = (associated(this%right) .and. this%right%red)
end function right_red

function left_red(this)
    class(node), intent(in) :: this
    logical :: left_red
    left_red = .false.
    left_red = (associated(this%left) .and. this%left%red)
end function left_red

subroutine init(t)
    !
    type(tree), intent(inout) :: t
    !
    t%head => new_node()
    t%z    => new_node()
    t%head%value = 0
    t%head%right => t%z
    t%head%left => t%z
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
    inserted_ptr => insert_node(t,value, x)
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

function insert_node(t, value, x) result(inserted_ptr)
    !
    type(tree), intent(in) :: t
    integer, intent(in) :: value
    type(node), pointer, intent(inout) :: x
    !
    type(node), pointer :: f, g, gg
    type(node), pointer :: inserted_ptr
    !
    nullify(f)
    nullify(g)
    nullify(gg)
    if (associated(x)) then
        f => x
        g => x
    end if
    do
        gg => g
        g => f
        f => x
        if (value < x%value) then
            x => x%left
        else
            x => x%right
        end if

        if (x%left_red() .and. x%right_red()) x => split(value, gg, g, f, x)
        if (associated(x, target=t%z)) exit
    end do

    allocate(x)
    x%left => t%z
    x%right => t%z
    x%value = value
    if (associated(f)) then
        if (value < f%value) then
            f%left => x
        else
            f%right => x
        end if
    end if
    inserted_ptr => x
    x => split(value, gg, g, f, x)
    !
end function insert_node

function split(value, gg, g, f, x) result(split_ptr)
    !
    integer, intent(in) :: value
    type(node), pointer, intent(inout) :: gg, g, f, x
    !
    type(node), pointer :: split_ptr
    !
    x%red = .true.
    x%left%red = .false.
    x%right%red = .false.
    if (f%red) then
        g%red = .true.
        if ((value<g%value) .neqv. (value<f%value)) f => rotate(value, g)
        x => rotate(value, gg)
        x%red = .false.
    end if
    split_ptr => x
    !
end function split

function rotate(value, y) result(rotate_ptr)
    !
    integer, intent(in) :: value
    type(node), pointer, intent(inout) :: y
    !
    type(node), pointer :: rotate_ptr
    !
    type(node), pointer :: s, gs
    !
    if (value < y%value) then
        s => y%left
    else
        s => y%right
    end if
    if (value < s%value) then
        gs => s%left
        s%left => gs%right
        gs%right => s
    else
        gs => s%right
        s%right => gs%left
        gs%left => s
    end if
    if (value < y%value) then
        y%left => gs
    else
        y%right => gs
    end if
    rotate_ptr => gs
    !
end function rotate

function search_node(value, start) result(found_ptr)
    !
    integer, intent(in) :: value
    type(node), pointer, intent(in) :: start
    type(node), pointer :: found_ptr
    !
    type(node), pointer :: x
    !
    x => start
    do while (associated(x))
        if (value == x%value) then
            exit
        else if (value < x%value) then
            x => x%left
        else
            x => x%right
        end if
    end do
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


end module rb_tree
