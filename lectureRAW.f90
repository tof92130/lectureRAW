module mesParametres

  integer, parameter    :: Geo2D  = 2
  integer, parameter    :: GeoAx  = 4
  integer, parameter    :: GeoAxi = 4
  integer, parameter    :: Geo3D  = 3
  integer, parameter    :: none         =  0
  integer, parameter    :: line         =  3  !> bar2      (VTK_LINE                )
  integer, parameter    :: triangle     =  5  !> tria3     (VTK_TRIANGLE            )
  integer, parameter    :: quad         =  9  !> quad4     (VTK_QUAD                )
  integer, parameter    :: tetra        = 10  !> tetra4    (VTK_TETRA               )
  integer, parameter    :: hexahedron   = 12  !> hexa8     (VTK_HEXAHEDRON          )
  integer, parameter    :: wedge        = 13  !> penta6    (VTK_WEDGE               ) 06 nodes pentahedron
  integer, parameter    :: pyramid      = 14  !> pyramid5  (VTK_PYRAMID             )  
  integer, parameter    :: line2        = 21  !> bar3      (VTK_QUADRATIC_EDGE      )
  integer, parameter    :: triangle2    = 22  !> tria6     (VTK_QUADRATIC_TRIANGLE  )
  integer, parameter    :: quad2        = 23  !> quad8     (VTK_QUADRATIC_QUAD      )
  integer, parameter    :: tetra2       = 24  !> tetra10   (VTK_QUADRATIC_TETRA     )
  integer, parameter    :: hexahedron2  = 25  !> hexa20    (VTK_QUADRATIC_HEXAHEDRON) 
  integer, parameter    :: wedge2       = 26  !> penta15   (VTK_QUADRATIC_WEDGE     ) 15 nodes pentahedron
  integer, parameter    :: pyramid2     = 27  !> pyramid13 (VTK_QUADRATIC_PYRAMID   ) 13 nodes pyramides

  integer, parameter    :: hexahedron3  =101
  integer, parameter    :: wedge3       =102
  integer, parameter    :: pyramid3     =103
  integer, parameter    :: tetra3       =104
  integer, parameter    :: quad3        =105
  integer, parameter    :: triangle3    =106
  integer, parameter    :: line3        =107 
  
  integer, parameter    :: hexahedron4  =201
  integer, parameter    :: wedge4       =202
  integer, parameter    :: pyramid4     =203
  integer, parameter    :: tetra4       =204
  integer, parameter    :: quad4        =205
  integer, parameter    :: triangle4    =206
  integer, parameter    :: line4        =207 
  
  integer, parameter    :: hexahedron5  =301
  integer, parameter    :: wedge5       =302
  integer, parameter    :: pyramid5     =303
  integer, parameter    :: tetra5       =304
  integer, parameter    :: quad5        =305
  integer, parameter    :: triangle5    =306
  integer, parameter    :: line5        =307
  
  integer, parameter    :: EqnLEE=4  !> Linearized Euler Equations
  integer, parameter    :: EqnEUL=5  !> Euler Equations

end module mesParametres



module mesProcedures
  use mesParametres
  implicit none
  
 !interface operator(.equal.)       ; module procedure equal                         ; end interface
  interface operator(==)            ; module procedure equal                         ; end interface
  
  type :: mesDonnees
    character(256)        :: file
    integer               :: geometry
    integer               :: equation
    integer               :: iRework
    integer               :: iter
    integer               :: iCell,nCell
    integer               :: iVar,iDeg
    integer               :: ker=5
    integer               :: count0
    integer               :: count1
    integer               :: siz
    integer               :: version
    logical               :: solution
    real(8)               :: time
    real(8) , allocatable :: sol(:,:)
    integer , allocatable :: deg(:)
    integer , allocatable :: ord(:)
    integer , allocatable :: cellType(:)
  end type mesDonnees

contains 

subroutine delete(ob)
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  type(mesDonnees) :: ob
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if( allocated(ob%sol) )deallocate(ob%sol)
  if( allocated(ob%deg) )deallocate(ob%deg)
  if( allocated(ob%ord) )deallocate(ob%ord)
  if( allocated(ob%cellType) )deallocate(ob%cellType)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  return
end subroutine delete


subroutine readRaw(ob)
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  type(mesDonnees) :: ob
  !>
  integer          :: iVar,iDeg,iCell
  character(80)    :: buffer  
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  print '(/"Reading: ",a)',trim(ob%file)
  
  open(unit=250            ,&
  &    file=trim(ob%file)  ,&
  &    form='unformatted'  ,&
  &    recordtype='stream' ,&
  &    action='read'       ,&
  &    status='old'         )
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Header
  read(unit=250)buffer ; write(*,'(a)')buffer
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Version
  read(unit=250)buffer ; write(*,'(a)')trim(buffer)
  select case( trim(buffer) )
  case("Raw format version 1") ; ob%version=1
  case("Raw format version 2") ; ob%version=2
  case("Raw format version 3") ; ob%version=3
  end select
  print '("Space Raw Format Version: ",i1)',ob%version
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Position/Solution  
  ob%solution=.true.   !> par defaut
  read(unit=250)buffer ; write(*,'(a)')trim(buffer)
  select case( trim(buffer) )
  case("Solutions") ; ob%solution=.true.
  case("Positions") ; ob%solution=.false.
  end select
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Comments
  read(unit=250)buffer ! write(*,'(a)')trim(buffer)
  read(unit=250)buffer ! write(*,'(a)')trim(buffer)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Geometry
  if( ob%version>=3 )then
    read(unit=250)buffer ; write(*,'(a)')buffer
    select case( trim(buffer) )
    case("Geometry2D" ) ; ob%geometry=Geo2D
    case("GeometryAxi") ; ob%geometry=GeoAx
    case("Geometry3D" ) ; ob%geometry=Geo3D
    case default
      write(*,'(/"Choice geometry not possible: ",a)')trim(buffer)
      stop
    end select
    !print '("geometry: ",i1)',ob%geometry
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Equation
  read(unit=250)buffer ! write(*,'(a)')buffer
  print '("equation: ",a)',trim(buffer)
  select case( trim(buffer) )
  case("LEE") ; ob%equation=EqnLEE
  case("EUL") ; ob%equation=EqnEUL
  case default
    write(*,'(/"Choice equation not possible: ",a)')trim(buffer)
  end select
  !print '("equation: ",i1)',ob%equation
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if( ob%solution )then
    if( ob%version>=3 )then
      if    ( trim(buffer)=="LEE" .and. ob%geometry==Geo2D )then ; ob%ker=3
      elseif( trim(buffer)=="LEE" .and. ob%geometry==GeoAx )then ; ob%ker=4
      elseif( trim(buffer)=="LEE" .and. ob%geometry==Geo3D )then ; ob%ker=4
      elseif( trim(buffer)=="EUL" .and. ob%geometry==Geo2D )then ; ob%ker=4
      elseif( trim(buffer)=="EUL" .and. ob%geometry==Geo3D )then ; ob%ker=5
      else ; stop"Bad configuration"
      endif
    endif
  else
    ob%ker=3
  endif
  print '("ker: ",i1)',ob%ker
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  read(unit=250)ob%iRework ; write(*,'("iRework=",i10  )')ob%iRework
  read(unit=250)ob%iter    ; write(*,'("iter=   ",i10  )')ob%iter
  read(unit=250)ob%count0  ; write(*,'("count0= ",i10  )')ob%count0
  read(unit=250)ob%count1  ; write(*,'("count1= ",i10  )')ob%count1
  read(unit=250)ob%time    ; write(*,'("time=   ",f10.4)')ob%time
  read(unit=250)ob%nCell   ; write(*,'("nCell=  ",i10  )')ob%nCell
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Order
  allocate(ob%ord(1:ob%nCell))
  read(unit=250)(ob%ord(iCell), iCell=1,ob%nCell)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> CellType
  if( ob%version>=3 )then
    allocate(ob%cellType(1:ob%nCell))
    read(unit=250)(ob%cellType(iCell), iCell=1,ob%nCell)
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  read(unit=250)ob%siz  ; write(*,'("siz=    ",i10  )')ob%siz
  ob%siz=ob%siz/ob%ker
  allocate(ob%sol(1:ob%ker,1:ob%siz))
  read(unit=250) ((ob%sol(iVar,iDeg),iVar=1,ob%ker),iDeg=1,ob%siz)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  close(unit=250)
  
  if( ob%version>=3 )then
    allocate(ob%deg(1:ob%nCell+1)) ; ob%deg(1)=1
    do iCell=1,ob%nCell
      select case(ob%cellType(iCell))
      case(hexahedron,hexahedron2)                 ; ob%deg(iCell+1)=ob%deg(iCell)+(ob%ord(iCell)+1)*(ob%ord(iCell)+1)*(ob%ord(iCell)+1)
      case(wedge                 )                 ; ob%deg(iCell+1)=ob%deg(iCell)+(ob%ord(iCell)+1)*(ob%ord(iCell)+1)*(ob%ord(iCell)+2)/2
      case(pyramid               )                 ; ob%deg(iCell+1)=ob%deg(iCell)+(ob%ord(iCell)+1)*(ob%ord(iCell)+2)*(2*ob%ord(iCell)+3)/6
      case(tetra,tetra2          )                 ; ob%deg(iCell+1)=ob%deg(iCell)+(ob%ord(iCell)+1)*(ob%ord(iCell)+2)*(ob%ord(iCell)+3)/6
      case(quad,quad2,quad3,quad4)                 ; ob%deg(iCell+1)=ob%deg(iCell)+(ob%ord(iCell)+1)*(ob%ord(iCell)+1)
      case(triangle,triangle2,triangle3,triangle4) ; ob%deg(iCell+1)=ob%deg(iCell)+(ob%ord(iCell)+1)*(ob%ord(iCell)+2)/2
      end select
    enddo
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
end subroutine readRaw

subroutine display(ob)
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  type(mesDonnees), intent(in) :: ob
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  print '(/"ob%ker     =",i6    )',ob%ker
  print '( "ob%siz     =",i6    )',ob%siz
  print '( "ob%nCell   =",i6    )',ob%nCell
  print '( "ob%iRework =",i6    )',ob%iRework
  print '( "ob%equation=",i6    )',ob%equation
  print '( "ob%count0  =",i6    )',ob%count0
  print '( "ob%count1  =",i6    )',ob%count1
  print '( "ob%time    =",e22.15)',ob%time
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  return
end subroutine display

subroutine displaySol(ob)
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  type(mesDonnees), intent(in) :: ob
  !>
  integer                      :: iCell,iDeg
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  iCell=1
  do iDeg=1,ob%siz
    if( iDeg>ob%deg(iCell+1)-1 )iCell=iCell+1
    print '(3x,"iCell=",i10," iDeg=",i10," xyz=",5(e22.15,1x))',iCell,iDeg,ob%sol(1:ob%ker,iDeg)
  enddo
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  return
end subroutine displaySol


function equal(ob1,ob2)
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  type(mesDonnees), intent(in) :: ob1
  type(mesDonnees), intent(in) :: ob2
  logical                      :: equal
  !>
  integer                      :: iCell
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if(      ob1%ker     ==ob2%ker      &
  &  .and. ob1%siz     ==ob2%siz      &
  &  .and. ob1%nCell   ==ob2%nCell    &
  &  .and. ob1%iRework ==ob2%iRework  &
  &  .and. ob1%equation==ob2%equation &
  &  .and. ob1%count0  ==ob2%count0   &
  &  .and. ob1%count1  ==ob2%count1   &
  &  .and. ob1%time    ==ob2%time     )then
    equal=.true.
  else
    equal=.false.
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  do iCell=1,ob1%nCell
    if( .not.ob1%ord(iCell)==ob2%ord(iCell) )then
      equal=.false.
      print '("iCell=",i10," ord1=",i3," ord2=",i3)',iCell,ob1%ord(iCell),ob2%ord(iCell)
    endif
  enddo
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if( ob1%version>=3 .and.  ob2%version>=3 )then
    do iCell=1,ob1%nCell
      if( .not.ob1%cellType(iCell)==ob2%cellType(iCell) )then
        equal=.false.
        print '("iCell=",i10," cellType1=",i3," cellType2=",i3)',iCell,ob1%cellType(iCell),ob2%cellType(iCell)
      endif
    enddo
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  return
end function equal


end module mesProcedures


subroutine compare()
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  use mesParametres
  use mesProcedures
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  implicit none
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  type :: clock
    character(8)   :: date
    character(10)  :: time
    integer        :: year,mounth,day
    integer        :: hour,minute,second
    character(5)   :: zone
  end type clock
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  type(mesDonnees)    :: ob
  type(mesDonnees)    :: ob1
  type(mesDonnees)    :: ob2
  integer             :: iCell,iCellMax
  integer             :: iVar
  integer             :: iDeg,iDegMax
  real(8)             :: dSol(1:5),dSolMax(1:5),d,dMax,dAve
  real(8) , parameter :: eps=1d-12
  integer             :: cpt
  integer             :: verbose
  character(80)       :: buffer
  integer             :: iErr
  integer             :: values(8)
  type(clock)         :: clock0
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call date_and_time( &
  &    date=clock0%date  ,&
  &    time=clock0%time  ,&
  &    zone=clock0%zone  ,&
  &    values=values  )
  
  clock0%year  =values(1)
  clock0%mounth=values(2)
  clock0%day   =values(3)
  clock0%hour  =values(5)
  clock0%minute=values(6)
  clock0%second=values(7)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
  verbose=0
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  print '(/"usage: lecture <file1> <file2>"/)'
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if( command_argument_count()>=2 )then
    call get_command_argument(number=1,value =ob1%file   )
    call get_command_argument(number=2,value =ob2%file   )
  elseif( command_argument_count()==1 )then
    call get_command_argument(number=1,value =ob1%file   )
    write(*,'( "file2: ")',advance='no') ; read(*,'(a)')ob2%file
  else
    write(*,'(/"file1: ")',advance='no') ; read(*,'(a)')ob1%file
    write(*,'( "file2: ")',advance='no') ; read(*,'(a)')ob2%file
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call readRAW(ob=ob1)
  call readRAW(ob=ob2)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if( .not.ob1==ob2 )then
    call display(ob=ob1)
    call display(ob=ob2)
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  print '(/"Starting Analysis")'
  if( ob1==ob2 )then
    
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    print '(3x,"Files are compatible")'
    ob%ker    =ob1%ker
    ob%nCell  =ob1%nCell
    ob%siz    =ob1%siz
    ob%iRework=ob1%iRework
    ob%count0 =ob1%count0
    ob%count1 =ob1%count1
    
    do iDeg=1,ob1%siz
      d=norm2(ob1%sol(1:ob1%ker,iDeg))
      dAve=dAve+d
    enddo
    dAve=dAve/ob1%siz
    print '(3x,"Computing     ",21x,        "          Average=",e22.15)',dAve
    
    dMax=-1d10
    cpt=0
    iCell=1
    do iDeg=1,ob%siz ! print '("iDeg=",i10,"/",i10)',iDeg,ob%siz
    
      if( ob1%version>2 )then
        if( iDeg>ob1%deg(iCell+1)-1 )iCell=iCell+1
      endif
      
      dSol(1:ob%ker)=ob2%sol(1:ob2%ker,iDeg)-ob1%sol(1:ob1%ker,iDeg)
      d=norm2(dSol(1:ob%ker))/dAve
      if( dMax<d )then
        iDegMax=iDeg
        iCellMax=iCell
        dSolMax(1:ob%ker)=dSol(1:ob%ker)
        dMax=d
      endif
      if( d>eps )cpt=cpt+1
    enddo
    print '(3x,"Computing cpt=",i10,"/",i10," deltaMax/Average=",e22.15)',cpt,ob%siz,dMax
    
    
    if    ( 0d0<dMax .and. dMax<eps )then
      print '(/3x,"Solutions are found to be in perfect agreement")'
    elseif( eps<dMax )then
      print '(/3x,"Solutions are not found to be in perfect agreement")'
      
      if( ob1%version>2 )then
        select case(ob%ker)
        case(3)
          print '(/3x,"iCell=",i10," iDeg=",i10," sol2-sol1=",3(e22.15,1x)," d=",e22.15)',iCellMax,iDegMax,dSolMax(1:ob%ker),dMax
          print '( 3x,"iCell=",i10," iDeg=",i10," sol1     =",3(e22.15,1x)             )',iCellMax,iDegMax,ob1%sol(1:ob1%ker,iDegMax)
          print '( 3x,"iCell=",i10," iDeg=",i10," sol2     =",3(e22.15,1x)             )',iCellMax,iDegMax,ob2%sol(1:ob2%ker,iDegMax)
        case(4)
          print '(/3x,"iCell=",i10," iDeg=",i10," sol2-sol1=",4(e22.15,1x)," d=",e22.15)',iCellMax,iDegMax,dSolMax(1:ob%ker),dMax
          print '( 3x,"iCell=",i10," iDeg=",i10," sol1     =",4(e22.15,1x)             )',iCellMax,iDegMax,ob1%sol(1:ob1%ker,iDegMax)
          print '( 3x,"iCell=",i10," iDeg=",i10," sol2     =",4(e22.15,1x)             )',iCellMax,iDegMax,ob2%sol(1:ob2%ker,iDegMax)
        case(5)
          print '(/3x,"iCell=",i10," iDeg=",i10," sol2-sol1=",5(e22.15,1x)," d=",e22.15)',iCellMax,iDegMax,dSolMax(1:ob%ker),dMax
          print '( 3x,"iCell=",i10," iDeg=",i10," sol1     =",5(e22.15,1x)             )',iCellMax,iDegMax,ob1%sol(1:ob1%ker,iDegMax)
          print '( 3x,"iCell=",i10," iDeg=",i10," sol2     =",5(e22.15,1x)             )',iCellMax,iDegMax,ob2%sol(1:ob2%ker,iDegMax)
        end select
      else
        select case(ob%ker)
        case(3)
          print '(/3x,"iDeg=",i10," sol2-sol1=",3(e22.15,1x)," d=",e22.15)',iDegMax,dSolMax(1:ob%ker),dMax
          print '( 3x,"iDeg=",i10," sol1     =",3(e22.15,1x)             )',iDegMax,ob1%sol(1:ob1%ker,iDegMax)
          print '( 3x,"iDeg=",i10," sol2     =",3(e22.15,1x)             )',iDegMax,ob2%sol(1:ob2%ker,iDegMax)
        case(4)
          print '(/3x,"iDeg=",i10," sol2-sol1=",4(e22.15,1x)," d=",e22.15)',iDegMax,dSolMax(1:ob%ker),dMax
          print '( 3x,"iDeg=",i10," sol1     =",4(e22.15,1x)             )',iDegMax,ob1%sol(1:ob1%ker,iDegMax)
          print '( 3x,"iDeg=",i10," sol2     =",4(e22.15,1x)             )',iDegMax,ob2%sol(1:ob2%ker,iDegMax)
        case(5)
          print '(/3x,"iDeg=",i10," sol2-sol1=",5(e22.15,1x)," d=",e22.15)',iDegMax,dSolMax(1:ob%ker),dMax
          print '( 3x,"iDeg=",i10," sol1     =",5(e22.15,1x)             )',iDegMax,ob1%sol(1:ob1%ker,iDegMax)
          print '( 3x,"iDeg=",i10," sol2     =",5(e22.15,1x)             )',iDegMax,ob2%sol(1:ob2%ker,iDegMax)
        end select
      endif
    endif
    print '()'
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    open(unit=100,file='regression.log',position='append',action='write', iostat=iErr)
    write(100,'(a,2x,a,3x,2(a,2x),"cpt=",i10,"/",i10,2x,"dltMax/Aver=",e22.15)')clock0%date,clock0%time(1:6),trim(ob1%file),trim(ob2%file),cpt,ob%siz,dMax
    close(100)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    if( .not.cpt==0 )then
      
      print '(/"Starting Analysis2 =")'
      cpt=0
      iCell=1
      if( ob1%version>2 )then
        do iDeg=1,ob%siz ! print '("iDeg=",i10,"/",i10)',iDeg,ob%siz
          if( iDeg>ob1%deg(iCell+1)-1 )iCell=iCell+1
          dSol(1:ob%ker)=ob2%sol(1:ob2%ker,iDeg)-ob1%sol(1:ob1%ker,iDeg)
          d=norm2(dSol(1:ob%ker))/dAve
          if( d>0.80*dMax )then
            if( verbose==1 )then
              select case(ob%ker)
              case(3) ; print '(3x,"iCell=",i10,"  iDeg=",i10," sol2-sol1=",3(e22.15,1x)," d=",e22.15)',iCell,iDeg,dSol(1:ob%ker),d
              case(4) ; print '(3x,"iCell=",i10,"  iDeg=",i10," sol2-sol1=",4(e22.15,1x)," d=",e22.15)',iCell,iDeg,dSol(1:ob%ker),d
              case(5) ; print '(3x,"iCell=",i10,"  iDeg=",i10," sol2-sol1=",5(e22.15,1x)," d=",e22.15)',iCell,iDeg,dSol(1:ob%ker),d
              end select
            endif
            cpt=cpt+1
          endif
        enddo
      else
        do iDeg=1,ob%siz ! print '("iDeg=",i10,"/",i10)',iDeg,ob%siz
          dSol(1:ob%ker)=ob2%sol(1:ob2%ker,iDeg)-ob1%sol(1:ob1%ker,iDeg)
          d=norm2(dSol(1:ob%ker))/dAve
          if( d>0.80*dMax )then
            if( verbose==1 )then
              select case(ob%ker)
              case(3) ; print '(3x,"iDeg=",i10," sol2-sol1=",3(e22.15,1x)," d=",e22.15)',iDeg,dSol(1:ob%ker),d
              case(4) ; print '(3x,"iDeg=",i10," sol2-sol1=",4(e22.15,1x)," d=",e22.15)',iDeg,dSol(1:ob%ker),d
              case(5) ; print '(3x,"iDeg=",i10," sol2-sol1=",5(e22.15,1x)," d=",e22.15)',iDeg,dSol(1:ob%ker),d
              end select
            endif
            cpt=cpt+1
          endif
        enddo
      endif
      
      print '(/3x,"cpt=",i10,"/",i10," dMax=",e22.15)',cpt,ob%siz,dMax
    endif
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if( verbose>0 .and. ob1==ob2 .and. .not.cpt==0 .and. dMax>eps )then
    
    print '(/"Writing file: spaceDelta.dat")'
    ob%file="./spaceDelta.dat"
    open(unit=250            ,&
    &    file=trim(ob%file)  ,&
    &    form='unformatted'  ,&
    &    recordtype='stream' ,&
    &    action='write'      ,&
    &    status='unknown'     )
    
    buffer="Space DG Solver: Data Compare" ;  write(250)buffer
    
    !> Version
    if( ob1%version>=3 )then      
      buffer="Raw format version 3" ;  write(250)buffer
    else
      buffer="Raw format version 2" ;  write(250)buffer
    endif
    
    !> Comments
    buffer="" ; write(unit=250)buffer ! write(*,'(a)')buffer
    buffer="" ; write(unit=250)buffer ! write(*,'(a)')buffer
    buffer="" ; write(unit=250)buffer ! write(*,'(a)')buffer
    
    !> geometry
    if( ob1%version>=3 )then      
      select case( ob1%geometry )
      case(Geo2D) ; write(buffer,'("Geometry2D" )') ; write(250)buffer
      case(GeoAx) ; write(buffer,'("GeometryAxi")') ; write(250)buffer
      case(Geo3D) ; write(buffer,'("Geometry3D" )') ; write(250)buffer
      end select
    endif
    
    !> Equation
    select case(ob1%equation)
    case(EqnLEE) ; buffer="LEE" ; write(unit=250)buffer ! write(*,'(a)')buffer
    case(EqnEUL) ; buffer="EUL" ; write(unit=250)buffer ! write(*,'(a)')buffer
    end select
    
    write(unit=250)ob1%iRework ; write(*,'("iRework=",i10  )')ob%iRework
    write(unit=250)ob1%iter    ! write(*,'("iter=   ",i10  )')ob%iter
    write(unit=250)ob1%count0  ! write(*,'("count0= ",i10  )')ob%count0
    write(unit=250)ob1%count1  ! write(*,'("count1= ",i10  )')ob%count1
    write(unit=250)ob1%time    ! write(*,'("time=   ",f10.4)')ob%time
    write(unit=250)ob1%nCell   ! write(*,'("nCell=  ",i10  )')ob%nCell
    
    write(unit=250)(ob1%ord(iCell), iCell=1,ob1%nCell)
    
    if( ob1%version>=3 )then
      write(unit=250)(ob1%cellType(iCell), iCell=1,ob1%nCell)
    endif
    
    write(unit=250) ob1%ker*ob1%siz  ! write(*,'("siz=   ",i10  )')ob%siz
    write(unit=250) ((ob1%sol(iVar,iDeg)-ob2%sol(iVar,iDeg),iVar=1,ob1%ker),iDeg=1,ob1%siz)
    
    close(unit=250)
    
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call delete(ob=ob )
  call delete(ob=ob1)
  call delete(ob=ob2)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  return
end subroutine compare


subroutine readPosition()
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  use mesParametres
  use mesProcedures
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  implicit none
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  type :: clock
    character(8)   :: date
    character(10)  :: time
    integer        :: year,mounth,day
    integer        :: hour,minute,second
    character(5)   :: zone
  end type clock
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  type(mesDonnees)    :: ob
  integer             :: iCell,iCellMax
  integer             :: iVar
  integer             :: iDeg,iDegMax
  real(8)             :: dSol(1:5),dSolMax(1:5),d,dMax,dAve
  real(8) , parameter :: eps=1d-12
  integer             :: cpt
  integer             :: verbose
  character(80)       :: buffer
  integer             :: iErr
  integer             :: values(8)
  type(clock)         :: clock0
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call date_and_time(     &
  &    date=clock0%date  ,&
  &    time=clock0%time  ,&
  &    zone=clock0%zone  ,&
  &    values=values      )
  
  clock0%year  =values(1)
  clock0%mounth=values(2)
  clock0%day   =values(3)
  clock0%hour  =values(5)
  clock0%minute=values(6)
  clock0%second=values(7)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
  verbose=1
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  print '(/"usage: lecture <file>"/)'
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if( command_argument_count()>=1 )then
    call get_command_argument(number=1,value =ob%file   )
  else
    write(*,'(/"file: ")',advance='no') ; read(*,'(a)')ob%file
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call readRAW(ob=ob)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call displaySol(ob=ob)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call delete(ob=ob )
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  return
end subroutine readPosition


program main
  !call compare()
  call readPosition()
end program main



