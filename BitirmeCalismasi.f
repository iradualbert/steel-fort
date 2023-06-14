        program App_Steel
        real :: xcc,ycc,ixx,iyy
        real :: d, tw, r, h, b, tf , A1 , A2 , Ix1, Ix2, Iy1, Iy2, cy2
        real :: X, Z, Area, Rgx, Rgy, SMx, SMz, Cx, Cy, Pmx,Pmy !properties to be calculated
        logical :: isFound
        character(len=6)  c,l
        character(len=10) input, nameH, nameU,Name1,Name2
        character(len=10) name
        character(len=10) profileType
        character startAgain
        integer choice
        input = ""


        write(*,3)
3       format(//,15x,"************WELCOME To Steel Magic*************")
        write(*,4)
4       format(/,15x,"This program is designed to help you calculate")
        write(*,5)
5       format(20x,"the properties of steel profiles.")
        write(*,76)
76      format(//,15x,"Developed By:")
        write(*,77)
77      format(/, 15x, "Albert and Abdulrahman")
        write(*,78)
78      format(//,15x,"***********************************************")
        write(*,79)
79      format(//, "Press Enter to continue......")
        read(*,*) !Pause the program

        write(*,8)
8       format(/,16x, "List of Available Profiles")
        write(*,*)" "
        open(81, file="Steel Sections.csv", status="old", action="read")
        read(81,*) ! skip the header
        do i=1,14
           read(81, *)name
           write(*, '(A, A)', advance="no")"  ", name
        end do
        close(81)

100     write(*,6)
6       format(////,16x, "Please insert the name of the steel profile.")
        write(*,7)
7       format(/,16x, "Or type INPUT/COMPOUND to enter measurements")
        read(*,*)input

        if(input .eq. "INPUT" .or. input .eq. "input") then
           call get_measurement_from_the_user
        elseif(input .eq. "COMPOUND" .or. input .eq. "compound") then
        open(92, file="Steel Sections Area-Inertia.csv", status="old")
        read(92, *)  ! read and skip the header
        write(*,91)
91      format(/,16x, "Please choose the first compound section")
        read(*,*)nameH
        write(*,82)
82      format(/,16x, "Please choose the second compound section")
        read(*,*)nameU
         do i=1,7
           read(92, *)Name1,A1,Ixx1,Iyy1,b,d,Name2,A2,Ixx2,Iyy2,cy
c          profileType1 = Name1(1:1)
c          profileType2 = Name2(1:1)
           if(trim(nameH) .eq. trim(Name1)) then
           isFound = .true.
           exit ! exit the loop if the the profile is found
           end if
        end do
        close(92) ! always close the file
        else
        open(10, file="Steel Sections.csv", status="old")
        read(10, *)  ! read and skip the header
        isFound = .false.
        do i=1,14
           read(10, *)Name,d,b,tw,tf,r,h
           profileType = name(1:1)
           if(trim(Name) .eq. trim(input)) then
           isFound = .true.
           write(*,*)name, d,b,tw,tf,r,h
           exit ! exit the loop if the the profile is found
           end if
        end do
        close(10) ! always close the file
        if(.not. isFound) then
        write(*,*)"Profile NOT FOUND, Press Enter to Start Again......"
        read(*,*)
        go to 100
        end if
        end if

        if(profileType .eq. "H") then
        X = calculate_Ax(d,tw,r,h,b,tf)   !Ixx
        Z = calculate_Az(d,tw,r,h,b,tf)    !Izz
        Area = calculate_Area(d,tw,r,h,b,tf)  !Area
        Rgx = calculate_Rgx(d,tw,r,h,b,tf)  !Radius of Gyration X
        Rgy = calculate_Rgy(d,tw,r,h,b,tf)  !Radius of Gyration Y
        SMx = calculate_SMx(d,tw,r,h,b,tf)  !Section Modulus x
        SMz = calculate_SMz(d,tw,r,h,b,tf)  !Section Modulus y
        Cx = calculate_Cx(d,tw,r,h,b,tf)  !Centroid X
        Cy = calculate_Cy(d,tw,r,h,b,tf)  !Centroid y
        Pmx = calculate_Pmx(d,tw,r,h,b,tf)
        else if(profileType .eq. "U")  then
        X = calculate_Bx(d,tw,r,h,b,tf)   !Ixx
        Z = calculate_Bz(d,tw,r,h,b,tf)    !Izz
        Area = calculate_Ar2(d,tw,r,h,b,tf)  !Area
        Rgx = calculate_Rg2x(d,tw,r,h,b,tf)  !Radius of Gyration X
        Rgy = calculate_Rg2y(d,tw,r,h,b,tf)  !Radius of Gyration Y
        SMx = calculate_SMx2(d,tw,r,h,b,tf)  !Section Modulus x
        SMz = calculate_SMz2(d,tw,r,h,b,tf)  !Section Modulus y
        Cx = calculate_Cx2(d,tw,r,h,b,tf)  !Centroid X
        Cy = calculate_Cy2(d,tw,r,h,b,tf)  !Centroid y
       Pmx = calculate_Pmx2(d,tw,r,h,b,tf)
       elseif (input .eq. "COMPOUND" .or. input .eq. "compound") then
       xcc = calculate_xc(b)
       ycc = calculate_yc(A1,d,A2,cy)
       ixx = calculate_Ix(Ixx1,A1,Ixx2,cy,d)
       iyy = calculate_Iy(Iyy1,A1,Iyy2,A2)
       
       
        else
        write(*,*)"INVALID PROFILE, Press Enter to Start Again......"
        read(*,*)
        go to 100
        end if


        ! print the results
        write(*,40)
  40    format(/,"---------------Properties---------------")
        if(profileType .eq. "H" .or. profileType .eq. "U") then
        write(*,22) X
  22    format(/,"Ixx =",20x,f15.2,1x," mm^4")
        write(*,23) Z
  23    format(/,"Iyy =",20x,f15.2,1x," mm^4")
        write(*,25) Area
  25    format(/,"Area =",19x,f15.2,1x," mm^2")
        write(*,26) Rgx
  26    format(/,"Radius of Gyration X =",3x, f15.2,1x," mm")
        write(*,33) Rgy
  33    format(/,"Radius of Gyration Y =",3x, f15.2,1x," mm")
        write(*,27) SMx
  27    format(/,"Section Modulus x =",6x,f15.2,1x," mm^3")
        write(*,28) SMz
  28    format(/,"Section Modulus y =",6x,f15.2,1x," mm^3")
        write(*,29) Cx
  29    format(/,"Centroid X =",13x,f15.2,1x, " mm")
        write(*,30) Cy
  30    format(/,"Centroid Y =",13x,f15.2,1x, " mm")
        write(*,94) Pmx
  94    format(/,"Plastic Modulus =",8x,f15.2,1x, " mm^3")
        else
        write(*,95) xcc
  95    format(/,"Centroid X =",12x,f15.2,2x, " mm")
        write(*,96) ycc
  96    format(/,"Centroid Y =",12x,f15.2,2x, " mm")
        write(*,97) ixx
  97    format(/,"Ixx =",18x,f15.2,2x, " mm^4")
        write(*,98) iyy
  98    format(/,"Iyy =",18x,f15.2,2x, " mm^4")
        end if

  200   write(*,32)
  32    format(/,"Do you want to start again (Y/N):")
        read(*,*) startAgain
        if(startAgain.eq.'Y') then
        go to 100
        else if(startAgain.eq.'N') then
        stop !exit the program
        else
        write(*,*) "Invalid choice"
        go to 200
        end if



      contains

      !FUNCTIONS for I/H profiles
      function calculate_Ax(d,tw,r,h,b,tf) result(Ax)
      real :: Ax
      Ax =(b * d**3) / 12 - ((b - tw) * h**3) / 12   !Ixx
      end function calculate_Ax

      function calculate_Az(d,tw,r,h,b,tf) result(Az)   !Izz
      real :: Az
      Az = (h*tw**3)/12 + 2*((tf*b**3)/12.0)
      end function calculate_Az

      function calculate_Area(d,tw,r,h,b,tf) result(Ar)          !Area
      real :: Ar
      Ar = (2*b*tf) + (h*tw) + 4*((r**2) -(3.14*r**2)/4)
      end function calculate_Area

      function calculate_Rgx(d,tw,r,h,b,tf) result(Rgx)          !Radius of Gyration X  (there is a problem here)
      real :: Rgx
      Rgx =  sqrt(X/Area)
      end function calculate_Rgx

      function calculate_Rgy(d,tw,r,h,b,tf) result(Rgy)          !Radius of Gyration Y
      real :: Rgy
      Rgy =  sqrt(Z/Area)
      end function calculate_Rgy

      function calculate_SMx(d,tw,r,h,b,tf) result(SMx)          !Section Modulus x
      real :: SMx
      SMx = X/(b/2)
      end function calculate_SMx

      function calculate_SMz(d,tw,r,h,b,tf) result(SMz)          !Section Modulus z
      real :: SMz
      SMz = Z/(d/2)
      end function calculate_SMz

      function calculate_Cx(d,tw,r,h,b,tf) result(Cx)          !Centroid x
      real :: Cx
      Cx = b/2
      end function calculate_Cx

      function calculate_Cy(d,tw,r,h,b,tf) result(Cy)          !Centroid y
      real :: Cy
      Cy = d/2
      end function calculate_Cy
      
      function calculate_Pmx(d,tw,r,h,b,tf) result(Pmx)          !Plastic Modulus
      real :: Pmx
      Pmx =  (Area/2)*2*((b*tf)*0.5*(h+tf)+(tw*h**2)/8)/(b*tf + h*tw/2)
      end function calculate_Pmx

      !for C shaped sections
        function calculate_Bx(d,tw,r,h,b,tf) result(Bx)
        real :: Bx
        Bx = ((b*d**3) - (b-tw)*(d-2*tf)**3)/12
        end function calculate_Bx

        function calculate_Bz(d,tw,r,h,b,tf) result(Bz)
        real :: Bz
        Bz = (((d-2*tf)*tw**3)/3 + (2*tf*b**3)/3)/2
        end function calculate_Bz

        function calculate_Ar2(d,tw,r,h,b,tf) result(Ar2)
        real :: Ar2
        Ar2 = 2*b*tf + h*tw + ((r**2 - (3.1416*r**2)/4))*2
        end function calculate_Ar2

        function calculate_Rg2x(d,tw,r,h,b,tf) result(Rg2x)
        real :: Rg2x
        Rg2x = sqrt(X / Area)
        end function calculate_Rg2x
        
        function calculate_Rg2y(d,tw,r,h,b,tf) result(Rg2y)
        real :: Rg2y
        Rg2y = sqrt(Z / Area)
        end function calculate_Rg2y

        function calculate_SMx2(d,tw,r,h,b,tf) result(SMx2)
        real :: SMx2
        SMx2 = X/(b/2)
        end function calculate_SMx2

        function calculate_SMz2(d,tw,r,h,b,tf) result(SMz2)
        real :: SMz2
        SMz2 = Z/(d/2)
        end function calculate_SMz2

        function calculate_Cx2(d,tw,r,h,b,tf) result(Cx2)
        real :: Cx2
        Cx2 = ((2*tf*b**2)/2 + (h*tw**2)/2)/Area
        end function calculate_Cx2

        function calculate_Cy2(d,tw,r,h,b,tf) result(Cy2)
        real :: Cy2
        Cy2 = d/2
        end function calculate_Cy2
        
      function calculate_Pmx2(d,tw,r,h,b,tf) result(Pmx2)
      real :: Pmx2
      Pmx2 =(Area/2)*2*((b*tf)*0.5*(h+tf)+(tw*h**2)/8)/(b*tf + h*tw/2)
      end function calculate_Pmx2
      
      !! for composite shapes
      function calculate_xc(b) result(xc)
      real :: xc
      xc = b/2
      end function calculate_xc
      
      function calculate_yc(A1,d,A2,cy) result(yc)       !you need to fill out the J variable
      real :: yc
      yc = (A1*(d/2) + A2*(d+cy))/(A1+A2)
      end function calculate_yc
      
      function calculate_Ix(Ixx1,A1,Ixx2,cy,d) result(Ix)   !you need to fill the d variables
      real :: Ix
      Ix = (Ixx1 + (A1*ABS(ycc-d/2)**2)) + (Ixx2 + (A2*ABS(ycc-cy)**2))
      end function calculate_Ix
      
      function calculate_Iy(Iyy1,A1,Iyy2,A2) result(Iy)
      real :: Iy
      Iy = (Iyy1 + (A1)) + (Iyy2 + A2)
      end function calculate_Iy
      
      
      
        subroutine get_measurement_from_the_user()
        write(*,*) "Please insert profile type(H or U): "
        read(*,*) profileType
        write(*,*)"Please insert total height d in mm: "
        read(*,*) d
        write(*,*)"Please insert flange width b in mm: "
        read(*,*) b
        write(*,*)"Please insert web thickness tw in mm: "
        read(*,*) tw
        write(*,*)"Please insert flange thickness tf in mm: "
        read(*,*) tf
        write(*,*)"Please insert web radius r in mm: "
        read(*,*) r
        write(*,*)"Please insert web height h in mm: "
        read(*,*) h
        end subroutine

        end program



