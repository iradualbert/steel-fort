        program App_Steel
        real pi
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
        write(*,*)d,b,tw,tf,r,h

        !The functions for I and H sections
c     Ax(d,tw,r,h,b,tf)=(tw*h**3)/12+2((b*tw**3)+3*(tw*b*(h+tf)**2))/12) !moment area strong axis
c     Az(d,tw,r,h,b,tf)=(h*tw**3)/12 + 2((tf*b**3)/12) !moment area weak axis
c     Iy(d,tw,r,h,b,tf)=(b*d**3)/12 - ((b-tw)*(d-(2*tf))**3)/12  ! Strong Axis for I/H sections
c     Iz(d,tw,r,h,b,tf)=((d-2*tf)*tw**3)/12 + (2*tf*b**3)/12     !Weak Axis for I/H sections


c     Radius of Gyration
c     Rad_Gyr(I,A)= sqrt(I/A)

      end program App_Steel
