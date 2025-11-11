! Minimal RollerCoaster-Tycoon style prototype in Fortran 2003/2008
! Console-based; compile with gfortran -std=f2008 -O2

module types_mod
  implicit none
  integer, parameter :: RK = kind(1.0d0)
  integer, parameter :: MAXX = 20, MAXY = 10

  type :: cell_t
    character(len=1) :: c = ' '   ! ' ' empty, '.' path, 'E' entrance, 'T' track, 'G' guest
  end type

  type :: guest_t
    logical :: alive = .false.
    integer :: x = 1, y = 1
    integer :: ticks_on_ride = 0
    logical :: riding = .false.
  end type

end module types_mod

module park_mod
  use types_mod
  implicit none
  type(cell_t), allocatable :: grid(:,:)
  integer :: width=MAXX, height=MAXY
contains

  subroutine init_grid(w,h)
    integer, intent(in) :: w,h
    width = w; height = h
    allocate(grid(width,height))
    grid = cell_t()   ! default init (spaces)
  end subroutine

  function in_bounds(x,y) result(ok)
    integer, intent(in) :: x,y
    logical :: ok
    ok = x>=1 .and. x<=width .and. y>=1 .and. y<=height
  end function

  subroutine set_cell(x,y,ch)
    integer, intent(in) :: x,y
    character(len=1), intent(in) :: ch
    if (in_bounds(x,y)) then
      grid(x,y)%c = ch
    end if
  end subroutine

  function get_cell(x,y) result(ch)
    integer, intent(in) :: x,y
    character(len=1) :: ch
    if (in_bounds(x,y)) then
      ch = grid(x,y)%c
    else
      ch = '#'
    end if
  end function

  subroutine draw()
    integer :: i,j
    write(*,'(A)') repeat('-', width*2+3)
    do j=1,height
      write(*,'(A)', advance='no') "| "
      do i=1,width
        write(*,'(A)', advance='no') trim(adjustl(grid(i,j)%c)) // ' '
      end do
      write(*,'(A)') "|"
    end do
    write(*,'(A)') repeat('-', width*2+3)
  end subroutine

end module park_mod

module sim_mod
  use types_mod
  use park_mod
  implicit none
  integer, parameter :: MAXG = 200
  type(guest_t), allocatable :: guests(:)
  integer :: money = 1000
  integer :: tick = 0
contains

  subroutine init_sim()
    allocate(guests(MAXG))
    guests = guest_t()
    tick = 0
    money = 1000
  end subroutine

  ! spawn a guest at entrance (first E found)
  subroutine spawn_guest()
    integer :: i,j,k
    i = 0; j = 0
    do k=1,width*height
      if (grid((k-1) mod width +1, (k-1)/width +1)%c == 'E') then
        i = (k-1) mod width + 1
        j = (k-1)/width + 1
        exit
      end if
    end do
    if (i==0) return
    do k=1,MAXG
      if (.not. guests(k)%alive) then
        guests(k)%alive = .true.
        guests(k)%x = i
        guests(k)%y = j
        guests(k)%riding = .false.
        guests(k)%ticks_on_ride = 0
        exit
      end if
    end do
  end subroutine

  ! simple pathfinding: move randomly but prefer path/track cells
  subroutine update_guests()
    integer :: k, nx, ny, bestx, besty, tries(4,2)
    real :: r
    character(len=1) :: cell
    tries = reshape([1,0,-1,0,0,1,0,-1], shape(tries)) ! right,left,down,up
    do k=1,MAXG
      if (.not. guests(k)%alive) cycle
      if (guests(k)%riding) then
        guests(k)%ticks_on_ride = guests(k)%ticks_on_ride + 1
        if (guests(k)%ticks_on_ride >= 5) then
          ! finish ride, reward money and remove guest
          money = money + 25
          guests(k)%alive = .false.
        end if
        cycle
      end if

      ! check if adjacent to a track 'T' -> board ride
      do nx=1,4
        bestx = guests(k)%x + tries(nx,1)
        besty = guests(k)%y + tries(nx,2)
        if (in_bounds(bestx,besty)) then
          if (grid(bestx,besty)%c == 'T') then
            guests(k)%riding = .true.
            guests(k)%ticks_on_ride = 0
            exit
          end if
        end if
      end do

      ! otherwise move: prefer '.' and 'T' cells
      r = real(mod(tick+7*k,100)) / 100.0
      ! try to move to a neighboring path or track cell
      nx = 0; ny = 0
      do bestx=1,4
        nx = guests(k)%x + tries(bestx,1)
        ny = guests(k)%y + tries(bestx,2)
        if (.not. in_bounds(nx,ny)) cycle
        cell = grid(nx,ny)%c
        if (cell == '.' .or. cell == 'T' .or. cell == 'E') then
          guests(k)%x = nx; guests(k)%y = ny
          exit
        end if
      end do
      ! else random small move
      if (r < 0.25) then
        guests(k)%x = max(1, min(width, guests(k)%x + 1 - nint(2*r)))
      end if
    end do
  end subroutine

  subroutine place_random_guests(n)
    integer, intent(in) :: n
    integer :: i
    do i=1,n
      call spawn_guest()
    end do
  end subroutine

  subroutine tick_sim()
    tick = tick + 1
    ! spawn occasional guest
    if (mod(tick,3) == 0) call spawn_guest()
    call update_guests()
  end subroutine

  subroutine draw_with_guests()
    integer :: i,j,k
    type(cell_t) :: save
    ! overlay guests
    do k=1,MAXG
      if (.not. guests(k)%alive) cycle
      if (in_bounds(guests(k)%x, guests(k)%y)) then
        save = grid(guests(k)%x, guests(k)%y)
        grid(guests(k)%x, guests(k)%y)%c = 'G'
      end if
    end do

    call draw()

    ! restore cells
    do k=1,MAXG
      if (.not. guests(k)%alive) cycle
      if (in_bounds(guests(k)%x, guests(k)%y)) then
        ! find what underlying terrain should be: for simplicity, set to '.'
        grid(guests(k)%x, guests(k)%y)%c = '.'
      end if
    end do
  end subroutine

end module sim_mod

program rct_main
  use park_mod
  use sim_mod
  implicit none
  integer :: w,h
  integer :: cmd, x,y, i
  character(len=32) :: raws
  call init_grid(20,10)
  call init_sim()

  ! quick starter: set some default path/entrance/track
  call set_cell(2,5,'E')
  do i=3,10
    call set_cell(i,5,'.')
  end do
  call set_cell(11,5,'T')
  call set_cell(12,5,'T')
  call set_cell(13,4,'T')
  call set_cell(14,4,'T')
  call set_cell(15,4,'T')

  call place_random_guests(3)

  do
    write(*,*) "Tick:", tick, " Money: $", money
    call draw_with_guests()
    write(*,*) "Commands: 1 place path 2 place track 3 spawn guest 4 step 5 auto 0 quit"
    read(*,'(I1)', iostat=cmd) cmd
    if (cmd == 0) exit
    select case(cmd)
    case (1)
      write(*,'(A)', advance='no') "Place Path x y: "
      read(*,*) x,y
      if (in_bounds(x,y)) call set_cell(x,y,'.')
    case (2)
      write(*,'(A)', advance='no') "Place Track x y: "
      read(*,*) x,y
      if (in_bounds(x,y)) call set_cell(x,y,'T')
    case (3)
      call spawn_guest()
    case (4)
      call tick_sim()
    case (5)
      ! simple auto run for 20 ticks
      do i=1,20
        call tick_sim()
      end do
    case default
      write(*,*) "Unknown command."
    end select
  end do

  write(*,*) "Goodbye. Final money: $", money

end program rct_main
