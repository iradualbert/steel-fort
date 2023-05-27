      program App_Steel
             real pi
c              real x, y, z
c              integer i
c              open(17, file="Steel Sections.csv", status='Unknown')
c              read(17, '()')
c              do 48 i=1,1
c              read(17, *)x,y,z
c              write(*,*)x,y,z
c 48            continue
c              close(17)


      I_area(d,tw,r,h,bf,tf)=2*bf*tf+(tw*(d-2*tf))+(r**2)*(4-pi)
c     Ix(d,tw,r,h,bf,tf)=2*((bf*tf**3)/12+(bf*tf*(d-tf)**2/4))+tw*(d-2*) ! Gave up on this formula as it was exceed the line length requirements
c      Iy(d,tw,r,h,bf,tf)=

      Ix(d,tw,r,h,bf,tf) = (bf*d**3 - (bf - tw)*(d - 2*tf)**3) / 12   ! formula for calculation the moment of inertia along the x - axis
      Iy(d,tw,r,h,bf,tf) = (2*tf*bf**3 + (d - 2*t)*tw**3) / 12
      
c     Radius of Gyration
      Rad_Gyr(I,A)= sqrt(I/A)
      
      

      pi = 4 * atan (1.0)
      area = I_area(96.0,5.0,12.0,56.0,100.0,8.0)
      moment_x = Ix(96.0,5.0,12.0,56.0,100.0,8.0)
      moment_y = Iy(96.0,5.0,12.0,56.0,100.0,8.0)
      rx = Rad_Gyr(moment_x,area)
      ry = Rad_Gyr(moment_y, area)
      write(*,*) moment_x, moment_y, rx, ry




      end program App_Steel
