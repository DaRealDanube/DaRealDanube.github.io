! navier2d.f90
! 2D incompressible Navier-Stokes (lid-driven cavity)
! Vorticity-Streamfunction formulation
! Single-file, no external libs. Fortran 90/95 style.
!
! Compile: gfortran -O3 navier2d.f90 -o navier2d
! Run: ./navier2d
!
program navier2d
  implicit none
  ! Parameters (change these)
  integer, parameter :: nx = 128       ! number of interior points in x
  integer, parameter :: ny = 128       ! number of interior points in y
  integer, parameter :: maxit = 20000  ! max time steps
  real(kind=8), parameter :: Lx = 1.0d0, Ly = 1.0d0
  real(kind=8), parameter :: Re = 100.0d0   ! Reynolds number
  real(kind=8), parameter :: dt = 0.001d0   ! time step
  real(kind=8), parameter :: tol_poisson = 1.0d-6
  integer, parameter :: max_sor = 20000
  real(kind=8), parameter :: omega_sor = 1.7d0  ! SOR relaxation factor

  ! Derived
  integer :: i, j, it
  real(kind=8) :: dx, dy, dx2, dy2, nu
  real(kind=8), parameter :: lid_velocity = 1.0d0

  ! Fields (including ghost boundaries: 0..nx+1, 0..ny+1)
  real(kind=8), allocatable :: psi(:,:), omega(:,:), u(:,:), v(:,:)
  real(kind=8), allocatable :: rhs(:,:)

  ! For SOR
  real(kind=8) :: resid
  integer :: sor_iter

  ! For time stepping
  real(kind=8), allocatable :: omega_new(:,:)

  ! Output control
  integer :: out_every
  out_every = 1000

  ! Initialize
  dx = Lx / real(nx)
  dy = Ly / real(ny)
  dx2 = dx*dx
  dy2 = dy*dy
  nu = 1.0d0 / Re

  allocate(psi(0:nx+1,0:ny+1))
  allocate(omega(0:nx+1,0:ny+1))
  allocate(u(0:nx+1,0:ny+1))
  allocate(v(0:nx+1,0:ny+1))
  allocate(rhs(0:nx+1,0:ny+1))
  allocate(omega_new(0:nx+1,0:ny+1))

  psi = 0.0d0
  omega = 0.0d0
  u = 0.0d0
  v = 0.0d0
  rhs = 0.0d0
  omega_new = 0.0d0

  call apply_boundary_conditions(omega, psi, u, v, dx, dy, lid_velocity)

  write(*,*) 'Starting time integration: nx=', nx, ' ny=', ny, ' dt=', dt, ' Re=', Re

  do it = 1, maxit
    ! Enforce boundary conditions on velocity and vorticity (compute vorticity there)
    call apply_boundary_conditions(omega, psi, u, v, dx, dy, lid_velocity)

    ! Solve Poisson: ∇²ψ = -ω  (SOR)
    rhs = -omega
    call poisson_sor(psi, rhs, dx2, dy2, tol_poisson, omega_sor, max_sor, sor_iter)

    ! Compute velocities from streamfunction (centered differences)
    do i = 1, nx
      do j = 1, ny
        u(i,j) =  (psi(i,j+1) - psi(i,j-1)) / (2.0d0*dy)    ! u = dψ/dy
        v(i,j) = -(psi(i+1,j) - psi(i-1,j)) / (2.0d0*dx)    ! v = -dψ/dx
      end do
    end do

    ! Time step vorticity: ω_t + u ω_x + v ω_y = ν ∇² ω
    ! Use 2nd order central differences for spatial derivatives, Euler explicit time stepping
    do i = 1, nx
      do j = 1, ny
        ! convection terms
        omega_new(i,j) = omega(i,j) - dt * ( &
             u(i,j) * (omega(i+1,j) - omega(i-1,j)) / (2.0d0*dx) + &
             v(i,j) * (omega(i,j+1) - omega(i,j-1)) / (2.0d0*dy) ) &
             + dt * nu * ( &
             (omega(i+1,j) - 2.0d0*omega(i,j) + omega(i-1,j)) / dx2 + &
             (omega(i,j+1) - 2.0d0*omega(i,j) + omega(i,j-1)) / dy2 )
      end do
    end do

    ! Update interior vorticity
    do i = 1, nx
      do j = 1, ny
        omega(i,j) = omega_new(i,j)
      end do
    end do

    ! Recompute boundary vorticity from velocities (no-slip)
    call apply_boundary_conditions(omega, psi, u, v, dx, dy, lid_velocity)

    if (mod(it, out_every) .eq. 0) then
      write(*,*) 'it=', it, ' sor_iter=', sor_iter
    end if

  end do

  ! Save final fields to CSV
  call write_csv('u.csv', u, nx, ny, dx, dy)
  call write_csv('v.csv', v, nx, ny, dx, dy)
  call write_csv('psi.csv', psi, nx, ny, dx, dy)
  call write_csv('omega.csv', omega, nx, ny, dx, dy)
  write(*,*) 'Done. Files: u.csv, v.csv, psi.csv, omega.csv'

contains

  subroutine apply_boundary_conditions(omega, psi, u, v, dx, dy, lid_vel)
    real(kind=8), intent(inout) :: omega(:,:), psi(:,:), u(:,:), v(:,:)
    real(kind=8), intent(in) :: dx, dy, lid_vel
    integer :: i, j

    ! Streamfunction boundaries: psi = 0 on walls (solid boundaries)
    psi(0,:) = 0.0d0
    psi(:,0) = 0.0d0
    psi(nx+1,:) = 0.0d0
    psi(:,ny+1) = 0.0d0

    ! Velocity boundaries:
    ! Bottom (y=0): u=v=0
    do i = 0, nx+1
      u(i,0) = 0.0d0
      v(i,0) = 0.0d0
    end do
    ! Top (y=Ly): u=lid_vel, v=0
    do i = 0, nx+1
      u(i,ny+1) = lid_vel
      v(i,ny+1) = 0.0d0
    end do
    ! Left and right walls: u=v=0
    do j = 0, ny+1
      u(0,j) = 0.0d0
      v(0,j) = 0.0d0
      u(nx+1,j) = 0.0d0
      v(nx+1,j) = 0.0d0
    end do

    ! Vorticity boundary values derived from streamfunction & wall velocities
    ! Using finite-difference approximation for wall vorticity:
    ! ω_wall = -2*(ψ_interior - ψ_wall)/d^2  + 2*U_wall/d  (for top lid)
    ! General approach: compute vorticity at ghost points using no-slip BC.
    ! Bottom wall (y=0)
    do i = 1, nx
      omega(i,0) = -2.0d0*(psi(i,1) - psi(i,0)) / (dy*dy)
    end do
    ! Top wall (y=ny+1) with lid velocity u=lid_vel
    do i = 1, nx
      omega(i,ny+1) = -2.0d0*(psi(i,ny) - psi(i,ny+1)) / (dy*dy) - 2.0d0*lid_vel/dy
    end do
    ! Left wall (x=0)
    do j = 1, ny
      omega(0,j) = -2.0d0*(psi(1,j) - psi(0,j)) / (dx*dx)
    end do
    ! Right wall (x=nx+1)
    do j = 1, ny
      omega(nx+1,j) = -2.0d0*(psi(nx,j) - psi(nx+1,j)) / (dx*dx)
    end do

    ! Corners (average)
    omega(0,0) = 0.5d0*(omega(1,0) + omega(0,1))
    omega(0,ny+1) = 0.5d0*(omega(1,ny+1) + omega(0,ny))
    omega(nx+1,0) = 0.5d0*(omega(nx,0) + omega(nx+1,1))
    omega(nx+1,ny+1) = 0.5d0*(omega(nx,ny+1) + omega(nx+1,ny))

  end subroutine apply_boundary_conditions

  subroutine poisson_sor(psi, rhs, dx2, dy2, tol, omega_relax, maxiter, iter_out)
    real(kind=8), intent(inout) :: psi(:,:)
    real(kind=8), intent(in) :: rhs(:,:)
    real(kind=8), intent(in) :: dx2, dy2
    real(kind=8), intent(in) :: tol, omega_relax
    integer, intent(in) :: maxiter
    integer, intent(out) :: iter_out

    integer :: i, j, iter
    integer :: nx_loc, ny_loc
    real(kind=8) :: denom, r, psi_old, acoef, bcoef

    nx_loc = size(psi,1) - 2
    ny_loc = size(psi,2) - 2

    denom = 2.0d0*(1.0d0/dx2 + 1.0d0/dy2)
    acoef = 1.0d0/dx2
    bcoef = 1.0d0/dy2

    iter = 0
    do
      iter = iter + 1
      r = 0.0d0
      do i = 1, nx_loc
        do j = 1, ny_loc
          psi_old = psi(i,j)
          psi(i,j) = (1.0d0 - omega_relax)*psi_old + &
               omega_relax*( (acoef*(psi(i+1,j)+psi(i-1,j)) + bcoef*(psi(i,j+1)+psi(i,j-1)) - rhs(i,j)) / denom )
          r = r + abs(psi(i,j) - psi_old)
        end do
      end do
      if (mod(iter,500) .eq. 0) then
        ! optional progress
      end if
      if (r < tol .or. iter >= maxiter) exit
    end do
    iter_out = iter
  end subroutine poisson_sor

  subroutine write_csv(filename, field, nx_loc, ny_loc, dx, dy)
    character(len=*), intent(in) :: filename
    real(kind=8), intent(in) :: field(:,:)
    integer, intent(in) :: nx_loc, ny_loc
    real(kind=8), intent(in) :: dx, dy
    integer :: i, j
    integer :: ios
    real(kind=8) :: x, y
    open(unit=99, file=filename, status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      write(*,*) 'Error opening', filename
      return
    end if
    write(99, '(A)') '# x,y,val'
    do j = 1, ny_loc
      y = (j-0.5d0)*dy
      do i = 1, nx_loc
        x = (i-0.5d0)*dx
        write(99,'(F10.6,1X,F10.6,1X,ES15.8)') x, y, field(i,j)
      end do
    end do
    close(99)
  end subroutine write_csv

end program navier2d
