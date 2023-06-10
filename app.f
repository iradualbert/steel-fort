        program App_Steel
        real :: pi
        real :: d, tw, r, h, b, tf
        real :: X, Z, Area, Rg, SMx, SMz, Cx, Cy !properties to be calculated
        character(len=10) name
        character(len=10) input
        character(len=20)  searchName
        logical :: isFound
        character(len=10) profileType
        


        write(*,3)
3       format(//,20x,"************WELCOME************")
        write(*,4)
4       format(/,15x,"This program is designed to help you calculate")
        write(*,5)
5       format(20x,"the properties of steel profiles.")
        write(*,8)
8       format(/,16x, "List of Profiles available:")
        write(*,9)
9       format(/,16x, "HE100A")
        write(*,10)
10      format(/,16x, "HE120A")
        write(*,11)
11      format(/,16x, "HE140A")
        write(*,12)
12      format(/,16x, "UPE80")
        write(*,13)
13      format(/,16x, "UPE100")
        write(*,14)
14      format(/,16x, "UPE120")
        write(*,6)
6       format(/,16x, "Please insert the name of the steel profile.")
        write(*,7)
7       format(/,16x, "Or type INSERT to enter measurements")
20      format(/,(20x,6(f6.2,2x)))
21       read(*,*)input
        
        profileType = input(1:1)

        if(profileType=="H")then
        ! open file
        open(10, file="HI_profiles.csv", status="old")
        ! ignore the header line
        read(10, *)
        ! Read and search for the profile entered by the user
        isFound = .false.
        do i=1, 6
           read(10, *)name,  d,b,tw,tf,r,h
           if(trim(name) == trim(input)) then
              isFound = .true.
              exit ! exit the loop if the the profile is found
           end if
        end do
        close(10)
        if(.not. isFound) then
             write(*,*) "Oops, that profile couldn't be found."
             go to 21
        end if
        X = calculate_Ax(d,tw,r,h,b,tf)   !Ixx
        Z = calculate_Az(d,tw,r,h,b,tf)    !Izz
        Area = calculate_Area(d,tw,r,h,b,tf)  !Area
        Rg = calculate_Rg(d,tw,r,h,b,tf)  !Radius of Gyration
        SMx = calculate_SMx(d,tw,r,h,b,tf)  !Section Modulus x
        SMz = calculate_SMz(d,tw,r,h,b,tf)  !Section Modulus y
        Cx = calculate_Cx(d,tw,r,h,b,tf)  !Centroid X
        Cy = calculate_Cy(d,tw,r,h,b,tf)  !Centroid y
        
        else if(profileType=="C") then
            write(*,*) "We are still working on C Profiles"
        else
            write(*,*) "Invalid Profile"
        endif






        
        write(*,22) X
  22    format(/,6(f20.3,2x))
        write(*,23) Z
  23    format(/,6(f20.3,2x))
        write(*,25) Area
  25    format(/,6(f20.3,2x))
        write(*,26) Rg
  26    format(/,6(f20.3,2x))
        write(*,27) SMx
  27    format(/,6(f20.3,2x))
        write(*,28) SMz
  28    format(/,6(f20.3,2x))
        write(*,29) Cx
  29    format(/,6(f20.3,2x))
        Cy = calculate_Cy(d,tw,r,h,b,tf)  !Centroid y
        write(*,30) Cy
  30    format(/,6(f20.3,2x))
  
        go to 21 ! start again

      contains

      !FUNCTIONS for I/H profiles
      function calculate_Ax(d,tw,r,h,b,tf) result(Ax)
      real :: Ax
      Ax =(tw * h**3)/12 + 2*((b * tf**3)/12+ ((tf*b*(h+tf)**2)/4))  !Ixx
      end function calculate_Ax
      
      function calculate_Az(d,tw,r,h,b,tf) result(Az)                !Izz
      real :: Az
      Az = (h*tw**3)/12 + 2*((tf*b**3)/12.0)
      end function calculate_Az
      
      function calculate_Area(d,tw,r,h,b,tf) result(Ar)          !Area
      real :: Ar
      Ar = (2*b*tf) + (h*tw)
      end function calculate_Area
      
      function calculate_Rg(d,tw,r,h,b,tf) result(Rg)          !Radius of Gyration
      real :: Rg
      Rg =  (x/Ar)**0.5
      end function calculate_Rg
      
      function calculate_SMx(d,tw,r,h,b,tf) result(SMx)          !Section Modulus x
      real :: SMx
      SMx = (2*x)/(h+ 2*tf)
      end function calculate_SMx
      
      function calculate_SMz(d,tw,r,h,b,tf) result(SMz)          !Section Modulus z
      real :: SMz
      SMz = (2*z)/b
      end function calculate_SMz
      
      function calculate_Cx(d,tw,r,h,b,tf) result(Cx)          !Centroid x
      real :: Cx
      Cx = b/2
      end function calculate_Cx
      
      function calculate_Cy(d,tw,r,h,b,tf) result(Cy)          !Centroid y
      real :: Cy
      Cy = h/2 + tf
      end function calculate_Cy
      
      end program
