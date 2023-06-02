        program App_Steel
        real :: pi
        real :: d, tw, r, h, b, tf, z
        dimension a(20,6)  !For the geometric measurements
        character(len=10)  SteelNames(6) !for the steel sections's name
        character(len=10) input
        SteelNames(1) = "HE100A"
        SteelNames(2) = "HE120A"
        SteelNames(3) = "HE140A"
        SteelNames(4) = "UPE80"
        SteelNames(5) = "UPE100"
        SteelNames(6) = "UPE120"
       ! 1 = d, 2 = tw, 3 = r, 4 = h, 5 = bf, 6 = tf
       
        open(17, file="SteelSections.txt", status='old')
        do 15 i = 1,6
        do 15 j = 1,6
        a(i,j) = 0.0
15      continue

        read(17,*)((a(i,j), j= 1,6),i= 1,6)
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
        write(*,20)((a(i,j), j= 1,6), i= 1,6)
20      format(/,(20x,6(f6.2,2x)))
        read(*,*)input
        do 21 i= 1,6
        do 21 j= 1,1
        if (input == SteelNames(i)) then   !checking which name has been input
        d = a(i,j)             !inserting the properties of the input steel
        b = a(i,j+1)
        tw = a(i,j+2)
        tf = a(i,j+3)
        r = a(i,j+4)
        h = a(i,j+5)
        end if
  21    continue
        x = calculate_Ax(d,tw,r,h,b,tf)   !Ixx
        write(*,22) x
  22    format(/,6(f20.3,2x))
        z = calculate_Az(d,tw,r,h,b,tf)    !Izz
        write(*,23) z
  23    format(/,6(f20.3,2x))
        Ar = calculate_Area(d,tw,r,h,b,tf)  !Area
        write(*,25) Ar
  25    format(/,6(f20.3,2x))
        Rg = calculate_Rg(d,tw,r,h,b,tf)  !Radius of Gyration
        write(*,26) Rg
  26    format(/,6(f20.3,2x))
        SMx = calculate_SMx(d,tw,r,h,b,tf)  !Section Modulus x
        write(*,27) SMx
  27    format(/,6(f20.3,2x))
        SMz = calculate_SMz(d,tw,r,h,b,tf)  !Section Modulus y
        write(*,28) SMz
  28    format(/,6(f20.3,2x))
        Cx = calculate_Cx(d,tw,r,h,b,tf)  !Centroid X
        write(*,29) Cx
  29    format(/,6(f20.3,2x))
        Cy = calculate_Cy(d,tw,r,h,b,tf)  !Centroid y
        write(*,30) Cy
  30    format(/,6(f20.3,2x))
  
        !The functions for I and H sections
c     Ax(d,tw,r,h,b,tf)=(tw*h**3)/12+2((b*tw**3)+3*(tw*b*(h+tf)**2))/12) !moment area strong axis
c     Az(d,tw,r,h,b,tf)=(h*tw**3)/12 + 2((tf*b**3)/12) !moment area weak axis


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
      
c     Radius of Gyration
c     Rad_Gyr(I,A)= sqrt(I/A)

