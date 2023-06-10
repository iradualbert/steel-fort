        program App_Steel
        real :: pi
        real :: d, tw, r, h, b, tf, z
        logical :: isFound
        character(len=6)  c,l
        dimension a(20,6)  !For the geometric measurements
        character(len=10)  SteelNames(6) !for the steel sections's name
        character(len=10) input
        character(len=10) name
        character firstChar
        input = ""
       ! 1 = d, 2 = tw, 3 = r, 4 = h, 5 = bf, 6 = tf
        open(10, file="HI_profiles.csv", status="old", action="read")
        write(*,3)
3       format(//,20x,"************WELCOME************")
        write(*,4)
4       format(/,15x,"This program is designed to help you calculate")
        write(*,5)
5       format(20x,"the properties of steel profiles.")
        write(*,8)
8       format(/,16x, "List of Profiles available:")
        write(*,60)
60      format(/,16x, "HE100A")
        write(*,61)
61      format(/,16x, "HE120A")
        write(*,62)
62      format(/,16x, "HE140A")
        write(*,63)
63      format(/,16x, "HE160A")
        write(*,64)
64      format(/,16x, "HE180A")
        write(*,65)
65      format(/,16x, "HE200A")
        write(*,66)
66      format(/,16x, "HE220A")
        write(*,67)
67      format(/,16x, "UPE80")
        write(*,68)
68      format(/,16x, "UPE100")
        write(*,69)
69      format(/,16x, "UPE120")
        write(*,70)
70      format(/,16x, "UPE140")
        write(*,71)
71      format(/,16x, "UPE160")
        write(*,72)
72      format(/,16x, "UPE180")
        write(*,73)
73      format(/,16x, "UPE200")
        write(*,74)
74      format(/,16x, "UPE220")
31      write(*,6)
6       format(/,16x, "Please insert the name of the steel profile.")
        write(*,7)
7       format(/,16x, "Or type INSERT to enter measurements")
        read(*,*)input
        read(10, *)
        isFound = .false.
        do i=1,14
        read(10, *)name, d,b,tw,tf,r,h
        if(trim(name) .eq. trim(input)) then
        isFound = .true.
        write(*,*)name, d,b,tw,tf,r,h
        exit ! exit the loop if the the profile is found
        end if
        end do
        if(input .eq. "INPUT" .or. input .eq. "input") then
        write(*,*)"Please insert total height d in mm"
        read(*,*) d
        write(*,*)"Please insert flange width b in mm"
        read(*,*) b
        write(*,*)"Please insert web thickness tw in mm"
        read(*,*) tw
        write(*,*)"Please insert flange thickness tf in mm"
        read(*,*) tf
        write(*,*)"Please insert web height h in mm"
        read(*,*) h
        end if
        write(*,40)
        firstChar = input(1:1)
   40   format(/,"---------------Properties---------------")
        if(firstChar .eq. "H") then
        x = calculate_Ax(d,tw,r,h,b,tf)   !Ixx
        write(*,22) x
  22    format(/,"Ixx =",20x,f15.2,1x," mm^4")
        z = calculate_Az(d,tw,r,h,b,tf)    !Izz
        write(*,23) z
  23    format(/,"Iyy =",20x,f15.2,1x," mm^4")
        Ar = calculate_Area(d,tw,r,h,b,tf)  !Area
        write(*,25) Ar
  25    format(/,"Area =",19x,f15.2,1x," mm^2")
        Rg = calculate_Rg(d,tw,r,h,b,tf)  !Radius of Gyration
        write(*,26) Rg
  26    format(/,"Radius of Gyration =",5x, f15.2,1x," mm")
        SMx = calculate_SMx(d,tw,r,h,b,tf)  !Section Modulus x
        write(*,27) SMx
  27    format(/,"Section Modulus x =",6x,f15.2,1x," mm^3")
        SMz = calculate_SMz(d,tw,r,h,b,tf)  !Section Modulus y
        write(*,28) SMz
  28    format(/,"Section Modulus y =",6x,f15.2,1x," mm^3")
        Cx = calculate_Cx(d,tw,r,h,b,tf)  !Centroid X
        write(*,29) Cx
  29    format(/,"Centroid X =",13x,f15.2,1x, " mm")
        Cy = calculate_Cy(d,tw,r,h,b,tf)  !Centroid y
        write(*,30) Cy
  30    format(/,"Centroid Y =",13x,f15.2,1x, " mm")
        else
        x = calculate_Bx(d,tw,r,h,b,tf)   !Ixx
        write(*,50) x
  50    format(/,"Ixx =",20x,f15.2,1x," mm^4")
        z = calculate_Bz(d,tw,r,h,b,tf)    !Izz
        write(*,51) z
  51    format(/,"Iyy =",20x,f15.2,1x," mm^4")
        Ar = calculate_Ar2(d,tw,r,h,b,tf)  !Area
        write(*,52) Ar
  52    format(/,"Area =",19x,f15.2,1x," mm^2")
        Rg = calculate_Rg2(d,tw,r,h,b,tf)  !Radius of Gyration
        write(*,53) Rg
  53    format(/,"Radius of Gyration =",5x, f15.2,1x," mm")
        SMx = calculate_SMx2(d,tw,r,h,b,tf)  !Section Modulus x
        write(*,54) SMx
  54    format(/,"Section Modulus x =",6x,f15.2,1x," mm^3")
        SMz = calculate_SMz2(d,tw,r,h,b,tf)  !Section Modulus y
        write(*,55) SMz
  55    format(/,"Section Modulus y =",6x,f15.2,1x," mm^3")
        Cx = calculate_Cx2(d,tw,r,h,b,tf)  !Centroid X
        write(*,56) Cx
  56    format(/,"Centroid X =",13x,f15.2,1x, " mm")
        Cy = calculate_Cy2(d,tw,r,h,b,tf)  !Centroid y
        write(*,57) Cy
  57    format(/,"Centroid Y =",13x,f15.2,1x, " mm")
        end if
  
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
      
      !for C shaped sections
        function calculate_Bx(d,tw,r,h,b,tf) result(Bx)
        real :: Bx
        Bx = (b*tf**3)/3 + b*tf*(d - tf)**2+tw*d**3 / 12
        end function calculate_Bx

        function calculate_Bz(d,tw,r,h,b,tf) result(Bz)
        real :: Bz
        Bz = 2 * (tf * b**3 / 12)
        end function calculate_Bz

        function calculate_Ar2(d,tw,r,h,b,tf) result(Ar2)
        real :: Ar2
        Ar2 = b * tf + tw * d
        end function calculate_Ar2

        function calculate_Rg2(d,tw,r,h,b,tf) result(Rg2)
        real :: Rg2
        Rg2 = sqrt(Bx / Ar2)
        end function calculate_Rg2

        function calculate_SMx2(d,tw,r,h,b,tf) result(SMx2)
        real :: SMx2
        SMx2 = (b * tf**2/2) + (tw * d**2 / 6)
        end function calculate_SMx2

        function calculate_SMz2(d,tw,r,h,b,tf) result(SMz2)
        real :: SMz2
        SMz2 = b * tf**2 / 4
        end function calculate_SMz2

        function calculate_Cx2(d,tw,r,h,b,tf) result(Cx2)
        real :: Cx2
        Cx2 = b / 2
        end function calculate_Cx2

        function calculate_Cy2(d,tw,r,h,b,tf) result(Cy2)
        real :: Cy2
        Cy2 = (tf*(b**2 +4*d*tf)+tw*d**2)/(2*(2*b*tf +tw*d))
        end function calculate_Cy2
      
        end program
      


