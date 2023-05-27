module KesitModule
  use, intrinsic :: iso_fortran_env, only: wp => real64
  implicit none
  
  type :: Kesit
    real(wp) :: d, tw, r, h, bf, tf
    real(wp) :: area
  end type Kesit
  
  contains
  
  function calculate_area(self) result(area)
    class(Kesit), intent(inout) :: self
    real(wp) :: area
    real(wp) :: pi
    pi = acos(-1.0_wp)
    area = (2.0_wp * self%bf * self%tf) + (self%tw * (self%d - (2.0_wp * self%tf))) + (self%r ** 2.0_wp) * (4.0_wp - pi)
    area = round(area, -1)
  end function calculate_area
  
end module KesitModule


program Main
  use KesitModule
  implicit none
  
  type(Kesit) :: k, k1
  
  k%d = 96.0_wp
  k%tw = 5.0_wp
  k%r = 12.0_wp
  k%h = 56.0_wp
  k%bf = 100.0_wp
  k%tf = 8.0_wp
  
  k1%d = 114.0_wp
  k1%tw = 5.0_wp
  k1%r = 12.0_wp
  k1%h = 74.0_wp
  k1%bf = 120.0_wp
  k1%tf = 8.0_wp
  
  write(*,*) k%calculate_area()
  write(*,*) k1%calculate_area()
  
end program Main
