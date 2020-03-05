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


module myData
  implicit none ! private
  type, public :: monType
    !private
    character(256)        :: file
    integer               :: geometry
    integer               :: equation
    integer               :: iRework
    integer               :: iter
    integer               :: iCell,nCell
    integer               :: iVar,iDeg
    integer               :: ker
    integer               :: count0
    integer               :: count1
    integer               :: nDeg
    integer               :: version
    logical               :: solution
    real(8)               :: time
    logical               :: solutionIsReal
    real(8)    , pointer  :: dsol(:,:)
    complex(8) , pointer  :: zsol(:,:)
    integer    , pointer  :: deg(:)
    integer    , pointer  :: ord(:)
    integer    , pointer  :: cellType(:)
    integer               :: meshOrder
    integer               :: nH6
    integer               :: nW5
    integer               :: nP5
    integer               :: nT4
    integer               :: nQ4
    integer               :: nT3
    real(8) , pointer     :: H6uvw(:,:)
    real(8) , pointer     :: W5uvw(:,:)
    real(8) , pointer     :: P5uvw(:,:)
    real(8) , pointer     :: T4uvw(:,:)
    real(8) , pointer     :: Q4uvw(:,:)
    real(8) , pointer     :: T3uvw(:,:)
  contains
    procedure, pass :: delete     => delete
    procedure, pass :: display    => display
    procedure, pass :: displaySol => displaySol
    procedure, pass :: isoOrder   => isoOrderRaw
    procedure, pass :: readRaw    => readRaw
    procedure, pass :: writeInria => writeInriaHOBinary
    !generic :: write(unformatted) => writeRAW
  end type monType

  interface
    module subroutine delete(ob)
      class(monType) :: ob
    end subroutine delete
    module function equal(ob1,ob2)
      class(monType), intent(in) :: ob1
      class(monType), intent(in) :: ob2
      logical                   :: equal
    end function equal
    module subroutine readRaw(ob)
      class(monType) :: ob
    end subroutine readRaw
    module subroutine isoOrderRaw(ob,ord)
      class(monType)       :: ob
      integer, intent(in)  :: ord
    end subroutine isoOrderRaw
    module subroutine writeRaw(ob)
      class(monType) :: ob
    end subroutine writeRaw
    module subroutine writeInriaHO(ob)
      class(monType) :: ob
    end subroutine writeInriaHO
    module subroutine writeInriaHOBinary(ob)
      class(monType) :: ob
    end subroutine writeInriaHOBinary
    module subroutine display(ob)
      class(monType), intent(in) :: ob
    end subroutine display
    module subroutine displaySol(ob)
      class(monType), intent(in) :: ob
    end subroutine displaySol
  end interface
  
  interface operator(==)
    module procedure equal
  end interface
  
  interface afficheSol
    procedure :: displaySol
  end interface

end module myData

submodule(myData) myProcedures
use mesParametres
contains
module procedure delete
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  print '(/">>> Delete object")'
  if( associated(ob%deg )     )deallocate(ob%deg)
  if( associated(ob%ord )     )deallocate(ob%ord)
  if( associated(ob%cellType) )deallocate(ob%cellType)
  if( associated(ob%H6uvw)    )deallocate(ob%H6uvw)
  if( associated(ob%W5uvw)    )deallocate(ob%W5uvw)
  if( associated(ob%P5uvw)    )deallocate(ob%P5uvw)
  if( associated(ob%T4uvw)    )deallocate(ob%T4uvw)
  if( associated(ob%Q4uvw)    )deallocate(ob%Q4uvw)
  if( associated(ob%T3uvw)    )deallocate(ob%T3uvw)
  if( associated(ob%dsol)     )deallocate(ob%dsol)
  if( associated(ob%zsol)     )deallocate(ob%zsol)
  print '("<<< Delete object")'
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  return
end procedure delete

module procedure equal
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  integer :: iCell
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if(      ob1%ker     ==ob2%ker      &
  &  .and. ob1%nDeg    ==ob2%nDeg     &
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
end procedure equal

module procedure readRaw
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  integer          :: iVar,iDeg,iCell
  integer          :: iOrd,nNod,Strd
  real(8),pointer  :: uvw(:,:)
  character(80)    :: buffer  
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  print '(/">>> Reading: ",a)',trim(ob%file)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!  open(unit=250            ,&
!  &    file=trim(ob%file)  ,&
!  &    form='unformatted'  ,&
!  &    recordtype='stream' ,&
!  &    action='read'       ,&
!  &    status='old'         )
  open(unit=250            ,&
  &    file=trim(ob%file)  ,&
  &    form='unformatted'  ,&
  &    access='stream'     ,&
  &    action='read'       ,&
  &    status='old'         )
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Header
  read(unit=250)buffer ; write(*,'(4x,a)')buffer
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Version
  read(unit=250)buffer ! write(*,'(a)')trim(buffer)
  select case( trim(buffer) )
  case("Raw format version 1") ; ob%version=1
  case("Raw format version 2") ; ob%version=2
  case("Raw format version 3") ; ob%version=3
  case("Raw format version 4") ; ob%version=4
  case default
    write(*,'(/"Choice Raw format version not possible: ",a)')trim(buffer)
    stop
  end select
  print '(4x,"Space Raw Format Version: ",i1)',ob%version
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Position/Solution  
  ob%solution=.true.   !> par defaut
  read(unit=250)buffer ! write(*,'(a)')trim(buffer)
  select case( trim(buffer) )
  case("Solutions") ; ob%solution=.true.
  case("Positions") ; ob%solution=.false.
  end select
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Real or Complex Solution
  read(unit=250)buffer ! write(*,'(a)')trim(buffer)
  select case(trim(buffer))
  case("Real")    ; ob%solutionIsReal=.true. 
  case("Complex") ; ob%solutionIsReal=.false.
  case default
    write(*,'(/"Choice Real/Complex not possible: ",a)')trim(buffer)
    stop
  end select
  print '(4x,"Solution is ",a)',trim(buffer)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Mesh Order
  read(unit=250)buffer ! write(*,'(a)')trim(buffer)  
  read(buffer,'(11x,i2)')ob%meshOrder
  print '(4x,"meshOrder: ",i2)',ob%meshOrder
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Geometry
  if( ob%version>=3 )then
    read(unit=250)buffer ; write(*,'(4x,a)')trim(buffer)
    select case( trim(buffer) )
    case("Geometry2D" ) ; ob%geometry=Geo2D
    case("GeometryAxi") ; ob%geometry=GeoAx
    case("Geometry3D" ) ; ob%geometry=Geo3D
    case default
      write(*,'(/"Choice geometry not possible: ",a)')trim(buffer)
      stop
    end select
    !print '(4x,"geometry: ",i1)',ob%geometry
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Equation
  read(unit=250)buffer ! write(*,'(4x,a)')buffer
  print '(4x,"equation: ",a)',trim(buffer)
  select case( trim(buffer) )
  case("LEE") ; ob%equation=EqnLEE
  case("EUL") ; ob%equation=EqnEUL
  case default
    write(*,'(/"Choice equation not possible: ",a)')trim(buffer)
  end select
  !print '(4x,"equation: ",i1)',ob%equation
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if( ob%solution )then
    if( ob%version>=3 )then
      if    ( trim(buffer)=="LEE" .and. ob%geometry==Geo2D )then ; ob%ker=3
      elseif( trim(buffer)=="LEE" .and. ob%geometry==GeoAx )then ; ob%ker=4
      elseif( trim(buffer)=="LEE" .and. ob%geometry==Geo3D )then ; ob%ker=4
      elseif( trim(buffer)=="EUL" .and. ob%geometry==Geo2D )then ; ob%ker=4
      elseif( trim(buffer)=="EUL" .and. ob%geometry==Geo3D )then ; ob%ker=5
      else ; stop "Bad configuration"
      endif
    endif
  else
    ob%ker=3
  endif
  print '(4x,"=> ker: ",i1)',ob%ker
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  read(unit=250)ob%iRework ; write(*,'(4x,"iRework=",i10  )')ob%iRework
  read(unit=250)ob%iter    ; write(*,'(4x,"iter=   ",i10  )')ob%iter
  read(unit=250)ob%count0  ; write(*,'(4x,"count0= ",i10  )')ob%count0
  read(unit=250)ob%count1  ; write(*,'(4x,"count1= ",i10  )')ob%count1
  read(unit=250)ob%time    ; write(*,'(4x,"time=   ",f10.4)')ob%time
  read(unit=250)ob%nCell   ; write(*,'(4x,"nCell=  ",i10  )')ob%nCell
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
  read(unit=250)ob%nDeg ; ob%nDeg=ob%nDeg/ob%ker
  write(*,'(4x,"nDeg=   ",i10  )')ob%nDeg
  if( ob%solutionIsReal )then
     allocate(ob%dsol(1:ob%ker,1:ob%nDeg))
    read(unit=250) ((ob%dsol(iVar,iDeg),iVar=1,ob%ker),iDeg=1,ob%nDeg)
    write(*,'(4x,"Readed ob%dsol size(ob%dsol)=",i0,"x",i0)')size(ob%dsol,1),size(ob%dsol,2)
  else
    allocate(ob%dsol(1:ob%ker,1:ob%nDeg))
    allocate(ob%zsol(1:ob%ker,1:ob%nDeg))
    read(unit=250) ((ob%zsol(iVar,iDeg),iVar=1,ob%ker),iDeg=1,ob%nDeg)
    write(*,'(4x,"Readed ob%zsol size(ob%zsol)=",i0,"x",i0)')size(ob%zsol,1),size(ob%zsol,2)
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Reading uvw
  if( ob%version>=4 )then
    if( ob%solution )then
      readingNodesPositions : do
        read(unit=250)buffer ; write(*,'(/4x,a)')trim(buffer) ; if( trim(buffer)=="End" )exit readingNodesPositions
        read(unit=250)iOrd
        read(unit=250)Strd
        read(unit=250)nNod
        write(*,'(4x,"iOrd=",i0," Strd=",i0," nNod=",i0)')iOrd,Strd,nNod
        
        select case(trim(buffer))
        case("HexahedraQ1NodesPositions"     ,"HexahedraQ2NodesPositions"     ,"HexahedraQ3NodesPositions"     ) ! ob%H6uvw=>uvw
          allocate(ob%H6uvw(1:strd,1:nNod))
          read(250)ob%H6uvw(1:strd,1:nNod)
          uvw=>ob%H6uvw
        case("PrismsP1NodesPositions"        ,"PrismsP2NodesPositions"        ,"PrismsP3NodesPositions"        ) ! ob%W5uvw=>uvw
          allocate(ob%W5uvw(1:strd,1:nNod))
          read(250)ob%W5uvw(1:strd,1:nNod)
          uvw=>ob%W5uvw
        case("PyramidsP1NodesPositions"      ,"PyramidsP2NodesPositions"      ,"PyramidsP3NodesPositions"      ) ! ob%P5uvw=>uvw
        case("TetrahedraP1NodesPositions"    ,"TetrahedraP2NodesPositions"    ,"TetrahedraP3NodesPositions"    )
          allocate(ob%T4uvw(1:strd,1:nNod))
          read(250)ob%T4uvw(1:strd,1:nNod)
          uvw=>ob%T4uvw
        case("QuadrilateralsQ1NodesPositions","QuadrilateralsQ2NodesPositions","QuadrilateralsQ3NodesPositions") ! ob%Q4uvw=>uvw 
          allocate(ob%Q4uvw(1:strd,1:nNod))
          read(250)ob%Q4uvw(1:strd,1:nNod)
          uvw=>ob%H6uvw
        case("TrianglesP1NodesPositions"     ,"TrianglesP2NodesPositions"     ,"TrianglesP3NodesPositions"     ) ! ob%T3uvw=>uvw
          allocate(ob%T3uvw(1:strd,1:nNod))
          read(250)ob%T3uvw(1:strd,1:nNod)
          uvw=>ob%T3uvw
        case default
          write(*,'(/"Choice Elemement not possible: ",a)')trim(buffer)
          stop      
        end select
        
        !do iNod=1,nNod
        !  print '("uvw=",*(f12.5,1x))',uvw(1:strd,iNod)
        !enddo
        uvw=>null()
        
      enddo readingNodesPositions
    endif
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  close(unit=250)
  print '(4x,"Closing file: ",a)',trim(ob%file)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if( ob%version>=3 )then
    allocate(ob%deg(1:ob%nCell+1)) ; ob%deg(1)=1
    do iCell=1,ob%nCell
      select case(ob%cellType(iCell))
      case(hexahedron,hexahedron2)                 ; ob%deg(iCell+1)=ob%deg(iCell)+(ob%ord(iCell)+1)*(ob%ord(iCell)+1)*(ob%ord(iCell)+1)
      case(wedge                 )                 ; ob%deg(iCell+1)=ob%deg(iCell)+(ob%ord(iCell)+1)*(ob%ord(iCell)+1)*(ob%ord(iCell)+2)/2
      case(pyramid               )                 ; ob%deg(iCell+1)=ob%deg(iCell)+(ob%ord(iCell)+1)*(ob%ord(iCell)+2)*(2*ob%ord(iCell)+3)/6
      case(tetra,tetra2,tetra3   )                 ; ob%deg(iCell+1)=ob%deg(iCell)+(ob%ord(iCell)+1)*(ob%ord(iCell)+2)*(ob%ord(iCell)+3)/6
      case(quad,quad2,quad3,quad4)                 ; ob%deg(iCell+1)=ob%deg(iCell)+(ob%ord(iCell)+1)*(ob%ord(iCell)+1)
      case(triangle,triangle2,triangle3,triangle4) ; ob%deg(iCell+1)=ob%deg(iCell)+(ob%ord(iCell)+1)*(ob%ord(iCell)+2)/2
      end select
    enddo
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  ob%nH6=0
  ob%nW5=0
  ob%nP5=0
  ob%nT4=0
  ob%nQ4=0
  ob%nT3=0
  do iCell=1,ob%nCell
    select case(ob%cellType(iCell))
    case(hexahedron,hexahedron2)                 ; ob%nH6=ob%nH6+1
    case(wedge                 )                 ; ob%nW5=ob%nW5+1
    case(pyramid               )                 ; ob%nP5=ob%nP5+1
    case(tetra,tetra2,tetra3   )                 ; ob%nT4=ob%nT4+1
    case(quad,quad2,quad3,quad4)                 ; ob%nQ4=ob%nQ4+1
    case(triangle,triangle2,triangle3,triangle4) ; ob%nT3=ob%nT3+1
    end select
  enddo
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
  print '("<<< End Reading: ",a)',trim(ob%file)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  return
end procedure readRaw

module procedure isoOrderRaw
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  use modDeterminant
  use baseSimplex3D
  use baseSimplex2D
  use baseSimplex1D
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  type lagrange
    real(8), pointer :: ai(:,:)
  end type lagrange
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  integer                 :: ordMin,ordMax,iOrd
  integer                 :: iCell0,iCell,nCell
  integer                 :: nDeg
  integer                 :: iDeg0,nDeg0
  integer                 :: iDeg1,nDeg1
  logical, pointer        :: orderIsPresent(:)
  real(8), pointer        :: uvw0(:,:),a0(:),b0(:),c0(:)
  real(8), pointer        ::           a (:),b (:),c (:)
  real(8), pointer        :: vand(:,:)
  real(8), pointer        :: ai  (:,:)
  type(lagrange), pointer :: base(:)
  real(8)   , pointer     :: dsol0(:,:),dsol1(:,:),dSol(:,:)
  complex(8), pointer     :: zsol0(:,:),zsol1(:,:),zSol(:,:)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  ordMin=min(minval(ob%ord),ord)
  ordMax=max(maxval(ob%ord),ord)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  NotIsoOrder: if( .not.(ordMin==ordMin .and. ord==ordMin) )then
    print '(/">>> Building isoOrder solution ord=",i0)',ord
    !print '(4x,"min/max(order)=",i0,"/",i0)',ordMin,ordMax
    
    !>>> Initialisation
    iCell0=0
    iDeg0=0
    iDeg1=0
    !<<<
    
    !>>> Allocation
    nDeg=0
    if( .not.ob%nH6==0 )nDeg=nDeg+ob%nH6*(ord+1)*(ord+1)*(ord+1)
    if( .not.ob%nT4==0 )nDeg=nDeg+ob%nT4*(ord+1)*(ord+2)*(ord+3)/6
    if( .not.ob%nQ4==0 )nDeg=nDeg+ob%nQ4*(ord+1)*(ord+1)
    if( .not.ob%nT3==0 )nDeg=nDeg+ob%nT3*(ord+1)*(ord+2)/2
    print '(4x,"Intial/Final nDeg=",i0," -> ",i0)',ob%nDeg,nDeg
    
    if( ob%solutionIsReal )then
      allocate(dSol(1:ob%ker,1:nDeg))
      dSol(1:ob%ker,1:nDeg)=0d0
    else
      allocate(zSol(1:ob%ker,1:nDeg))
      zSol(1:ob%ker,1:nDeg)=(0d0,0d0)
    endif
    
    allocate(orderIsPresent(ordMin:ordMax)) ; orderIsPresent(ordMin:ordMax)=.false.
    allocate(base          (ordMin:ordMax))    
    !<<<

    !>>> Hexa
    !<<<

    !>>> Wedges
    !<<<
    
    !>>> Pyramids
    !<<<
    
    !>>> Tetra
    nCell=ob%nT4
    if( .not.nCell==0 )then
      
      !> Points d'interpollation a ord
      deallocate(ob%T4uvw)
      call nodesT4   (ord=ord,uvw=ob%T4uvw,display=.false.)
      call nodesT4opt(ord=ord,uvw=ob%T4uvw,display=.false.)
      call nodesT4uvw2abc(uvw=ob%T4uvw,a=a,b=b,c=c,display=.false.)
      nDeg1=size(a) !> = nNod
      
      orderIsPresent(ordMin:ordMax)=.false.
      do iCell=iCell0+1,iCell0+nCell
        orderIsPresent(ob%ord(iCell))=.true.
      enddo
      
      do iOrd=ordMin,ordMax
        if( orderIsPresent(iOrd) .and. .not.iOrd==ord )then
          print '(4x,"Passing from order=",i0," to order ",i0,t70,"nCell=",i0,"/",i0)',iOrd,ord,count(ob%ord(iCell0+1:iCell0+nCell)==iOrd),nCell
          
          call nodesT4   (ord=iOrd,uvw=uvw0,display=.false.)
          call nodesT4opt(ord=iOrd,uvw=uvw0,display=.false.)
          call nodesT4uvw2abc(uvw=uvw0,a=a0,b=b0,c=c0,display=.false.)
          call vandermondeT4(ord=iOrd,a=a0,b=b0,c=c0,vand=vand)
          nDeg0=size(a0) !> = nMod
          deallocate(uvw0,a0,b0,c0)
          
          allocate(base(iOrd)%ai(1:nDeg0,1:nDeg1)) ! print '(4x,"size(ai) order=",i0,": nMod x nNod= ",i0,"x",i0)',iOrd,nDeg0,nDeg1
          ai=>base(iOrd)%ai(1:nDeg0,1:nDeg1)
          call lagrangeT4(ord=iOrd,vand=vand,a=a,b=b,c=c,lx=ai,transpose=.true.) ! (nMod x nNod) => transpose=.true.
          !do ad=1,nDeg1 ; print '(6x,"ai(",i2,")=",*(f12.5,2x))',ad,ai(:,ad) ;enddo
          deallocate(vand)
        endif
      enddo
      deallocate(a,b,c)
      
      !> sol(1:ker,1:nDeg1)=sol(1:ker,1:nDeg0) x ai(1:nDeg0,1:nDeg1)
      do iCell=iCell0+1,iCell0+nCell ! print '("iCell=",i10,"/",i10,2x,"ord:",i0," -> ",i0)',iCell,nCell,ob%ord(iCell),ord
        if( ob%ord(iCell)==ord )then
          nDeg0=nDeg1
          if( ob%solutionIsReal )then
            dSol(1:ob%ker,iDeg1+1:iDeg1+nDeg1)=ob%dSol(1:ob%ker,iDeg0+1:iDeg0+nDeg0)
          else
            zSol(1:ob%ker,iDeg1+1:iDeg1+nDeg1)=ob%zSol(1:ob%ker,iDeg0+1:iDeg0+nDeg0)
          endif
        else
          ai=>base(ob%ord(iCell))%ai(:,:)
          nDeg0=size(ai,1)
          if( ob%solutionIsReal )then
            dSol0=>ob%dSol(1:ob%ker,iDeg0+1:iDeg0+nDeg0)
            dSol1=>   dSol(1:ob%ker,iDeg1+1:iDeg1+nDeg1)
            dSol1(1:ob%ker,1:nDeg1)=matmul(dSol0(1:ob%ker,1:nDeg0),ai(1:nDeg0,1:nDeg1))
          else
            zSol0=>ob%zSol(1:ob%ker,iDeg0+1:iDeg0+nDeg0)
            zSol1=>   zSol(1:ob%ker,iDeg1+1:iDeg1+nDeg1)
            zSol1(1:ob%ker,1:nDeg1)=matmul(zSol0(1:ob%ker,1:nDeg0),ai(1:nDeg0,1:nDeg1))
          endif
          !print '("iCell=",i10," ob%ord=",i10," -> ",i10)',iCell,ob%ord(iCell),ord
          ob%ord(iCell)=ord
        endif
        
        iDeg0=iDeg0+nDeg0
        iDeg1=iDeg1+nDeg1
      enddo

      do iCell=iCell0+2,iCell0+nCell ! print '("iCell=",i10,"/",i10,2x,"ord:",i0," -> ",i0)',iCell,nCell,ob%ord(iCell),ord
       !print '("iCell=",i10," ob%deg=",i10," -> ",i10)',iCell,ob%deg(iCell),ob%deg(iCell-1)+nDeg1
        ob%deg(iCell)=ob%deg(iCell-1)+nDeg1
      enddo
      
      !> Nettoyage de la mémoire
      do iOrd=ordMin,ordMax
        if( orderIsPresent(iOrd) .and. .not.iOrd==ord )then
          deallocate(base(iOrd)%ai)
        endif
      enddo
      
      iCell0=iCell0+nCell
    endif
    !<<< Tetra

    !>>> Quads
    nCell=ob%nQ4
    if( .not.nCell==0 )then

    endif
    !<<< Quads
    
    !>>> Triangles
    nCell=ob%nT3
    if( .not.nCell==0 )then
      
      !> Points d'interpollation a ord
      deallocate(ob%T3uvw)
      call nodesT3   (ord=ord,uvw=ob%T3uvw,display=.false.)
      call nodesT3opt(ord=ord,uvw=ob%T3uvw,display=.false.)
      call nodesT3uv2ab(uv=ob%T3uvw,a=a,b=b,display=.false.)
      nDeg1=size(a) !> = nNod
      
      orderIsPresent(ordMin:ordMax)=.false.
      do iCell=iCell0+1,iCell0+nCell
        orderIsPresent(ob%ord(iCell))=.true.
      enddo
      
      do iOrd=ordMin,ordMax
        if( orderIsPresent(iOrd) .and. .not.iOrd==ord )then
          print '(4x,"Passing from order=",i0," to order ",i0,t70,"nCell=",i0,"/",i0)',iOrd,ord,count(ob%ord(iCell0+1:iCell0+nCell)==iOrd),nCell
          
          call nodesT3   (ord=iOrd,uvw=uvw0,display=.false.)
          call nodesT3Opt(ord=iOrd,uvw=uvw0,display=.false.)
          call nodesT3uv2ab(uv=uvw0,a=a0,b=b0,display=.false.)
          call vandermondeT3(ord=iOrd,a=a0,b=b0,vand=vand)
          nDeg0=size(a0) !> = nMod
          deallocate(uvw0,a0,b0)
          
          allocate(base(iOrd)%ai(1:nDeg0,1:nDeg1)) ! print '(4x,"size(ai) order=",i0,": nMod x nNod= ",i0,"x",i0)',iOrd,nDeg0,nDeg1
          ai=>base(iOrd)%ai(1:nDeg0,1:nDeg1)
          call lagrangeT3(ord=iOrd,vand=vand,a=a,b=b,lx=ai,transpose=.true.) ! (nMod x nNod) => transpose=.true.
          !do ad=1,nDeg1 ; print '(6x,"ai(",i2,")=",*(f12.5,2x))',ad,ai(:,ad) ;enddo
          deallocate(vand)
        endif
      enddo
      deallocate(a,b)
      
      !> sol(1:ker,1:nDeg1)=sol(1:ker,1:nDeg0) x ai(1:nDeg0,1:nDeg1)
      do iCell=iCell0+1,iCell0+nCell ! print '("iCell=",i10,"/",i10,2x,"ord:",i0," -> ",i0)',iCell,nCell,ob%ord(iCell),ord
        if( ob%ord(iCell)==ord )then
          nDeg0=nDeg1
          if( ob%solutionIsReal )then
            dSol(1:ob%ker,iDeg1+1:iDeg1+nDeg1)=ob%dSol(1:ob%ker,iDeg0+1:iDeg0+nDeg0)
          else
            zSol(1:ob%ker,iDeg1+1:iDeg1+nDeg1)=ob%zSol(1:ob%ker,iDeg0+1:iDeg0+nDeg0)
          endif
        else
          ai=>base(ob%ord(iCell))%ai(:,:)
          nDeg0=size(ai,1)
          if( ob%solutionIsReal )then
            dSol0=>ob%dSol(1:ob%ker,iDeg0+1:iDeg0+nDeg0)
            dSol1=>   dSol(1:ob%ker,iDeg1+1:iDeg1+nDeg1)
            dSol1(1:ob%ker,1:nDeg1)=matmul(dSol0(1:ob%ker,1:nDeg0),ai(1:nDeg0,1:nDeg1))
          else
            zSol0=>ob%zSol(1:ob%ker,iDeg0+1:iDeg0+nDeg0)
            zSol1=>   zSol(1:ob%ker,iDeg1+1:iDeg1+nDeg1)
            zSol1(1:ob%ker,1:nDeg1)=matmul(zSol0(1:ob%ker,1:nDeg0),ai(1:nDeg0,1:nDeg1))
          endif
          !print '("iCell=",i10," ob%ord=",i10," -> ",i10)',iCell,ob%ord(iCell),ord
          ob%ord(iCell)=ord
        endif
        
        iDeg0=iDeg0+nDeg0
        iDeg1=iDeg1+nDeg1
      enddo

      do iCell=iCell0+2,iCell0+nCell ! print '("iCell=",i10,"/",i10,2x,"ord:",i0," -> ",i0)',iCell,nCell,ob%ord(iCell),ord
       !print '("iCell=",i10," ob%deg=",i10," -> ",i10)',iCell,ob%deg(iCell),ob%deg(iCell-1)+nDeg1
        ob%deg(iCell)=ob%deg(iCell-1)+nDeg1
      enddo
      
      !> Nettoyage de la mémoire
      do iOrd=ordMin,ordMax
        if( orderIsPresent(iOrd) .and. .not.iOrd==ord )then
          deallocate(base(iOrd)%ai)
        endif
      enddo
      
      iCell0=iCell0+nCell
    endif
    !<<< Triangles
    
    !>>> Affectation
    if( .not.iDeg0==ob%nDeg )stop '("Probleme isoOrderRaw iDeg0!=ob%nDeg")'
    if( .not.iDeg1==   nDeg )stop '("Probleme isoOrderRaw iDeg1!=   nDeg")'
    
    print '(4x,"iDeg0=",i0,"/",i0)',iDeg0,ob%nDeg
    print '(4x,"iDeg1=",i0,"/",i0)',iDeg1,   nDeg
    
    if( ob%solutionIsReal )then
      ob%nDeg=nDeg
      deallocate(ob%dSol)
      ob%dSol=>dSol(1:ob%ker,1:ob%nDeg)
      dSol=>null()
    else
      ob%nDeg=nDeg
      deallocate(ob%zSol)
      ob%zSol=>zSol(1:ob%ker,1:ob%nDeg)
      zSol=>null()
    endif
    !<<<

    !>>>
    deallocate(orderIsPresent)
    deallocate(base          )
    !<<<

    print '("<<< Building isoOrder solution ord=",i0)',ord
  endif NotIsoOrder
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

  return
end procedure isoOrderRaw

module procedure  writeRaw
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  integer          :: iVar,iDeg,iCell
  character(80)    :: buffer
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  print '(/"Writing: ",a)',trim(ob%file)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !open(unit=250            ,&
  !&    file=trim(ob%file)  ,&
  !&    recordtype='stream' ,&
  !&    form='unformatted'  ,&
  !&    action='write'      ,&
  !&    status='unknown'     )
  open(unit=250            ,&
  &    file=trim(ob%file)  ,&
  &    access='stream'     ,&
  &    form='unformatted'  ,&
  &    action='write'      ,&
  &    status='unknown'     )
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Header (Version 1:)
  buffer="Space DG ReplaceRaw"  ;  write(250)buffer ; write(*,'(a)')trim(buffer)
  buffer="Raw format version 3" ;  write(250)buffer ; write(*,'(a)')trim(buffer)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Comments (Version 1:)
  buffer="Solutions"            ;  write(250)buffer ; write(*,'(a)')trim(buffer)
  buffer="Real"                 ;  write(250)buffer ; write(*,'(a)')trim(buffer)
  buffer=""                     ;  write(250)buffer
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Geometry (version 3:)
  select case( ob%geometry )
  case(Geo2D) ; write(buffer,'("Geometry2D" )') ; write(250)buffer
  case(GeoAx) ; write(buffer,'("GeometryAxi")') ; write(250)buffer
  case(Geo3D) ; write(buffer,'("Geometry3D" )') ; write(250)buffer
  case default
    write(*,'(/"Choice geometry not possible: ",a)')trim(buffer)
    stop
  end select
  write(*,'(a)')trim(buffer)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Equation
 !print '("equation: ",i1)',ob%equation
  select case( ob%equation )
  case(EqnLEE) ; write(buffer,'("LEE" )') ; write(250)buffer
  case(EqnEUL) ; write(buffer,'("EUL" )') ; write(250)buffer
  case default
    write(*,'(/"Choice equation not possible: ",a)')trim(buffer)
  end select
  print '("equation: ",a)',trim(buffer)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> iRework Version 1:
  write(250)ob%iRework    ; write(*,'("iRework=",i10  )')ob%iRework
  write(250)ob%iter       ; write(*,'("iter=   ",i10  )')ob%iter
  write(250)ob%count0     ; write(*,'("count0= ",i10  )')ob%count0
  write(250)ob%count1     ; write(*,'("count1= ",i10  )')ob%count1
  write(250)ob%time       ; write(*,'("time=   ",f10.4)')ob%time
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> nCellGlob Version 1:
  write(250)ob%nCell      ; write(*,'("nCell=  ",i10  )')ob%nCell
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Order (Version 1:)
  write(250)(ob%ord(iCell),iCell=1,ob%nCell)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> cellType (version 3:)
  write(250)(ob%cellType(iCell),iCell=1,ob%nCell)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> sol Version 1:
  write(unit=250)ob%nDeg*ob%ker
  write(*,'("nDeg=   ",i10  )')ob%nDeg
  write(unit=250) ((ob%dsol(iVar,iDeg),iVar=1,ob%ker),iDeg=1,ob%nDeg)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  close(unit=250)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  print '(/"End Writing: ",a)',trim(ob%file)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  return
end procedure writeRaw

module procedure writeInriaHO
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  integer          :: l
  integer          :: inriaSol
  integer          :: iCell,nCell,iType
  integer          :: iNod,nNod
  integer          :: deg0,nDeg
  real(8), pointer :: uvw(:,:)
  character(256)   :: file
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  l=index(ob%file,'.',.true.)-1 ; file=ob%file(1:l)//'.sol'
  print '(/"Writing Inria HO solution: ",a)',trim(file)
  
  open(newunit=inriaSol,file=trim(file),action='write')  
  write(inriaSol,'("MeshVersionFormatted 2"/)')
  select case(ob%geometry)
  case(Geo2D,GeoAx) ; write(inriaSol,'("Dimension 2"/)')
  case(Geo3D)       ; write(inriaSol,'("Dimension 3"/)')
  end select
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> HO NodesPositions
  
  if( .not.ob%nH6==0 )then
    write(inriaSol,'(/"HOSolAtHexahedraQ",i1,"NodesPositions")')ob%meshOrder
    uvw=>ob%H6uvw
    nNod=size(uvw,2)
    write(inriaSol,'(i0)')nNod
    do iNod=1,nNod
      write(inriaSol,'(*(g0,1x))') (1d0+uvw(:,iNod))*5d-1                                           !> \in [0,1]^3
    enddo
  endif
  
  if( .not.ob%nT4==0 )then
    write(inriaSol,'(/"HOSolAtTetrahedraP",i1,"NodesPositions")')ob%meshOrder
    uvw=>ob%T4uvw
    nNod=size(uvw,2)
    write(inriaSol,'(i0)')nNod
    do iNod=1,nNod
      write(inriaSol,'(*(g0,1x))') uvw(4,iNod),uvw(1:3,iNod)                                        !> Barycentric Coordinates
     !write(inriaSol,'(*(g0,1x))') uvw(1:4,iNod)                                                    !> Nodal Coordinates
    enddo
    uvw=>null()
  endif
  
  if( .not.ob%nQ4==0 )then
    write(inriaSol,'(/"HOSolAtQuadrilateralsQ",i1,"NodesPositions")')ob%meshOrder
    uvw=>ob%Q4uvw
    nNod=size(uvw,2)
    write(inriaSol,'(i0)')nNod
    do iNod=1,nNod
      write(inriaSol,'(*(g0,1x))') (1d0+uvw(:,iNod))*5d-1                                           !> \in [0,1]^2
    enddo
    uvw=>null()
  endif
  
  if( .not.ob%nT3==0 )then
    write(inriaSol,'(/"HOSolAtTrianglesP",i1,"NodesPositions")')ob%meshOrder
    uvw=>ob%T3uvw
    nNod=size(uvw,2)
    write(inriaSol,'(i0)')nNod
    do iNod=1,nNod
      write(inriaSol,'(*(g0,1x))') uvw(3,iNod),uvw(1:2,iNod)                                        !> Barycentric Coordinates
     !write(inriaSol,'(*(g0,1x))') uvw(1:3,iNod)                                                    !> Nodal Coordinates
    enddo
    uvw=>null()
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> HO Solutions
  
  !> Hexahedra
  nCell=ob%nH6
  if( .not.nCell==0 )then
    nNod=size(ob%H6uvw,2)
    !>
    select case(ob%meshOrder)
    case(1) ; iType=hexahedron
    case(2) ; iType=hexahedron2
    case(3) ; iType=hexahedron3
    case default
      write(*,'(/"Mesh Order not possible with Hexahedra")')
      stop
    end select
    !>
    write(inriaSol,'(/"HOSolAtHexahedraQ",i1)')ob%meshOrder
    write(inriaSol,'(i0)')nCell
    select case(ob%equation)
    case(EqnLEE) ; write(inriaSol,'("2 2 1")')    ! {u1, v1, w1 }, x1=rh1*a0/rho0
    case(EqnEUL) ; write(inriaSol,'("3 1 2 1")')  ! rho, {rho u, rho v, rho w}, rho E
    case default
      write(*,'(/"Choice equation not possible: ",i0)')ob%equation
    end select
    write(inriaSol,'(i0,1x,i0)')ob%ord(1),nNod  ! on met ob%ord(1) car iso ordre
    call writeBlock()
  endif
  
  !> Tetrahedra
  nCell=ob%nT4
  if( .not.nCell==0 )then
    nNod=size(ob%T4uvw,2)
    !>
    select case(ob%meshOrder)
    case(1) ; iType=tetra
    case(2) ; iType=tetra2
    case(3) ; iType=tetra3
    case default
      write(*,'(/"Mesh Order not possible with Tetrahedra meshOrder=",i0)')ob%meshOrder
      stop
    end select
    !>
    write(inriaSol,'(/"HOSolAtTetrahedraP",i1)')ob%meshOrder
    write(inriaSol,'(i0)')nCell
    select case(ob%equation)
    case(EqnLEE) ; write(inriaSol,'("2 2 1")')    ! {u1, v1, w1 }, x1=rh1*a0/rho0
    case(EqnEUL) ; write(inriaSol,'("3 1 2 1")')  ! rho, {rho u, rho v, rho w}, rho E
    case default
      write(*,'(/"Choice equation not possible: ",i0)')ob%equation
    end select
    write(inriaSol,'(i0,1x,i0)')ob%ord(1),nNod  ! on met ob%ord(1) car iso ordre
    call writeBlock()
  endif  
  
  !> Quadrilaterals
  nCell=ob%nQ4
  if( .not.nCell==0 )then
    nNod=size(ob%Q4uvw,2)
    print '("nQ4=",i0)',ob%nQ4
    print '("nNod=",i0)',nNod
    !>
    select case(ob%meshOrder)
    case(1) ; iType=quad
    case(2) ; iType=quad2
    case(3) ; iType=quad3
    case(4) ; iType=quad4
    case default
      write(*,'(/"Mesh Order not possible with Quadrilaterals")')
      stop
    end select
    !>
    write(inriaSol,'(/"HOSolAtQuadrilateralsQ",i1)')ob%meshOrder
    write(inriaSol,'(i0)')nCell
    select case(ob%equation)
    case(EqnLEE) ; write(inriaSol,'("2 2 1")')    ! {u1, v1}, x1=rh1*a0/rho0
    case(EqnEUL) ; write(inriaSol,'("3 1 2 1")')  ! rho, {rho u, rho v}, rho E
    case default
      write(*,'(/"Choice equation not possible: ",i0)')ob%equation
    end select
    write(inriaSol,'(i0,1x,i0)')ob%ord(1),nNod  ! on met ob%ord(1) car iso ordre
    call writeBlock()
  endif
  
  !> Triangles
  nCell=ob%nT3
  if( .not.nCell==0 )then
    nNod=size(ob%T3uvw,2)
    print '("nT3=",i0)',ob%nT3
    print '("nNod=",i0)',nNod
    !>
    select case(ob%meshOrder)
    case(1) ; iType=triangle
    case(2) ; iType=triangle2
    case(3) ; iType=triangle3
    case(4) ; iType=triangle4
    case default
      write(*,'(/"Mesh Order not possible with Triangles")')
      stop
    end select
    !>
    write(inriaSol,'(/"HOSolAtTrianglesP",i1)')ob%meshOrder
    write(inriaSol,'(i0)')nCell
    select case(ob%equation)
!    case(EqnLEE) ; write(inriaSol,'("1 1")')    !  x1=rh1*a0/rho0, {u1,v1}
    case(EqnLEE) ; write(inriaSol,'("2 2 1")')    ! {u1,v1}, x1=rh1*a0/rho0
    case(EqnEUL) ; write(inriaSol,'("3 1 2 1")')  ! rho, {rho u, rho v}, rho E
    case default
      write(*,'(/"Choice equation not possible: ",i0)')ob%equation
    end select
    write(inriaSol,'(i0,1x,i0)')ob%ord(1),nNod  ! on met ob%ord(1) car iso ordre
    call writeBlock()
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  write(inriaSol,'(/"End")')
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  close(inriaSol)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  print '(/"End Writing Inria HO solution: ",a)',trim(file)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  return
contains
  
  subroutine writeBlock()
    if( ob%solutionIsReal )then
      do iCell=1,ob%nCell
        if( ob%cellType(iCell)==iType )then
          deg0=ob%deg(iCell)-1 !> juste avant
          nDeg=ob%deg(iCell+1)-ob%deg(iCell)
         !write(inriaSol,'(*(g0,1x))')ob%dsol(3,deg0+1:deg0+nDeg)
          write(inriaSol,'(*(g0,1x))')ob%dsol(:,deg0+1:deg0+nDeg)
        endif        
      enddo    
    else
      do iCell=1,ob%nCell
        if( ob%cellType(iCell)==iType )then
          deg0=ob%deg(iCell)-1 !> juste avant
          nDeg=ob%deg(iCell+1)-ob%deg(iCell)
          write(inriaSol,'(*(g0,1x))')abs(ob%zsol(:,deg0+1:deg0+nDeg))
        endif        
      enddo    
    endif
    
    return
  end subroutine writeBlock
    
end procedure writeInriaHO


module procedure writeInriaHOBinary
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  use iso_c_binding , only: c_loc,c_f_pointer
  use libMeshb7
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  integer          :: l
  integer(8)       :: inriaSol
  integer          :: nCell,iCell0
  integer          :: iNod,nNod
  integer          :: iDeg,nDeg
  real(8), pointer :: uvw(:,:)
  character(256)   :: file
  integer          :: iErr
  integer          :: GmfKey
  integer          :: nFld,kind(1:20)
  real(8)          :: x
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  l=index(ob%file,'.',.true.)-1 ; file=ob%file(1:l)//'.solb'
  print '(/">>> Writing Inria HO solution: ",a)',trim(file)
  
  select case(ob%geometry)
  case(Geo2D,GeoAx) ; inriaSol=GmfOpenMesh(trim(file), GmfWrite, GmfDouble, 2)
  case(Geo3D)       ; inriaSol=GmfOpenMesh(trim(file), GmfWrite, GmfDouble, 3)
  end select
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> HO NodesPositions
  
  HexahedraNodesPositions: if( .not.ob%nH6==0 )then
    print '(4x,"HexahedraNodesPositions {u,v,w}",t70,"(order=",i0,")")',ob%ord(1)
    uvw=>ob%H6uvw
    nNod=size(uvw,2)
    uvw(:,:)=(uvw(:,:)+1d0)*5d-1                                                                    !> \in [0,1]^3 INRIA
    
    select case(ob%meshOrder)
    case(1) ; GmfKey=GmfHOSolAtHexahedraQ1NodesPositions
    case(2) ; GmfKey=GmfHOSolAtHexahedraQ2NodesPositions
    case(3) ; GmfKey=GmfHOSolAtHexahedraQ3NodesPositions
    case default ; stop "HOSolAtHexahedra ob%meshOrder>3 not implemented"
    end select
    
    call writeNodeBlock()
    
    uvw(:,:)=2d0*uvw(:,:)-1d0                                                                       !> \in [-1,+1]^3  Space
    uvw=>null()
  endif HexahedraNodesPositions
  !<<<
  
  !>>>
  TetrahedraNodesPositions: if( .not.ob%nT4==0 )then
    print '(4x,"TetrahedraNodesPositions {1-u-v-w,u,v,w}",t70,"(order=",i0,")")',ob%ord(1)
    uvw=>ob%T4uvw
    nNod=size(uvw,2)
    
    do iNod=1,nNod                                                                                  !> {1-u-v-w,u,v,w} INRIA
      x=uvw(4,iNod)
      uvw(4,iNod)=uvw(3,iNod)
      uvw(3,iNod)=uvw(2,iNod)
      uvw(2,iNod)=uvw(1,iNod)
      uvw(1,iNod)=x
      print '(6x,*(e12.5,1x))',uvw(1:4,iNod)
    enddo
    
    select case(ob%meshOrder)
    case(1) ; GmfKey=GmfHOSolAtTetrahedraP1NodesPositions
    case(2) ; GmfKey=GmfHOSolAtTetrahedraP2NodesPositions
    case(3) ; GmfKey=GmfHOSolAtTetrahedraP3NodesPositions
    case default ; stop "HOSolAtTetrahedra ob%meshOrder>3 not implemented"
    end select
    
    call writeNodeBlock()
    
    do iNod=1,nNod                                                                                  !> {u,v,w,1-u-v-w} Space
      x=uvw(1,iNod)
      uvw(1,iNod)=uvw(2,iNod)
      uvw(2,iNod)=uvw(3,iNod)
      uvw(3,iNod)=uvw(4,iNod)
      uvw(4,iNod)=x
    enddo
    
    uvw=>null()
  endif TetrahedraNodesPositions
  !<<<
  
  !>>>
  QuadrilateralsNodesPositions: if( .not.ob%nQ4==0 )then
    print '(4x,"QuadrilateralsNodesPositions {u,v}",t70,"(order=",i0,")")',ob%ord(1)
    uvw=>ob%Q4uvw
    uvw(:,:)=(uvw(:,:)+1d0)*5d-1                                                                    !> \in [0,1]^2 INRIA
    nNod=size(uvw,2)
    
    do iNod=1,nNod
      print '(6x,*(e22.15,1x))',uvw(:,iNod)
     !print '(6x,*(e22.15,1x))',(1d0-uvw(1,iNod))*(1d0-uvw(2,iNod)),&
     !&                         (    uvw(1,iNod))*(1d0-uvw(2,iNod)),&
     !&                         (    uvw(1,iNod))*(    uvw(2,iNod)),&
     !&                         (1d0-uvw(1,iNod))*(    uvw(2,iNod))  
    enddo
    
    select case(ob%meshOrder)
    case(1) ; GmfKey=GmfHOSolAtQuadrilateralsQ1NodesPositions
    case(2) ; GmfKey=GmfHOSolAtQuadrilateralsQ2NodesPositions
    case(3) ; GmfKey=GmfHOSolAtQuadrilateralsQ3NodesPositions
    case default ; stop "HOSolAtQuadrilaterals ob%meshOrder>3 not implemented"
    end select
    
    call writeNodeBlock()
    
    
    uvw(:,:)=2d0*uvw(:,:)-1d0                                                                       !> \in [-1,1]^2 Space
    
    uvw=>null()
  endif QuadrilateralsNodesPositions
  !<<<
  
  !>>>
  TrianglesNodesPositions: if( .not.ob%nT3==0 )then
    print '(4x,"TrianglesNodesPositions {1-u-v,u,v}",t70,"(order=",i0,")")',ob%ord(1)
    uvw=>ob%T3uvw
    nNod=size(uvw,2)
    
    do iNod=1,nNod                                                                                  !> {1-u-v,u,v} INRIA
      x=uvw(3,iNod)
      uvw(3,iNod)=uvw(2,iNod)
      uvw(2,iNod)=uvw(1,iNod)
      uvw(1,iNod)=x
      print '(6x,*(e22.15,1x))',uvw(1:3,iNod)
    enddo
    
    select case(ob%meshOrder)
    case(1) ; GmfKey=GmfHOSolAtTrianglesP1NodesPositions
    case(2) ; GmfKey=GmfHOSolAtTrianglesP2NodesPositions
    case(3) ; GmfKey=GmfHOSolAtTrianglesP3NodesPositions
    case default ; stop "HOSolAtTetrahedra ob%meshOrder>3 not implemented"
    end select
    
    call writeNodeBlock()
    
    do iNod=1,nNod                                                                                  !> {u,v,1-u-v} Space
      x=uvw(1,iNod)
      uvw(1,iNod)=uvw(2,iNod)
      uvw(2,iNod)=uvw(3,iNod)
      uvw(3,iNod)=x
    enddo
    
    uvw=>null()
  endif TrianglesNodesPositions
  !<<<
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> HO Solutions
  
  print '(4x,"HOSol Initialisation")'
  
  !>>> nFld and kind
  select case(ob%equation)
  case(EqnLEE) ; nFld=2 
    select case(ob%geometry)
    case(Geo2D,Geo3D) ; kind(1:nFld)=[GmfVec  ,GmfSca]   ! {u1, v1, w1 }, x1=rh1*a0/rho0
    case(GeoAx)       ; kind(1:nFld)=[GmfVec+1,GmfSca]   ! {u1, v1, w1 }, x1=rh1*a0/rho0
    end select    
  case(EqnEUL) ; nFld=3 ; kind(1:nFld)=[GmfSca,GmfVec,GmfSca]  ! rho, {rho u, rho v, rho w}, rho E
  case default
    write(*,'(/"Choice equation not possible: ",i0)')ob%equation
  end select  
  !<<< nFld and kind
  
  !>>> Intialisation
  iCell0=1
  iDeg=0
  !<<<
  
  !>>> HOSolAtHexahedra
  nCell=ob%nH6
  if( .not.nCell==0 )then
    print '(4x,"HOSolAtHexahedra")'
    nNod=size(ob%H6uvw,2) ; nDeg=nCell*nNod
    
    select case(ob%meshOrder)
    case(1) ; GmfKey=GmfHOSolAtHexahedraQ1
    case(2) ; GmfKey=GmfHOSolAtHexahedraQ2
    case(3) ; GmfKey=GmfHOSolAtHexahedraQ3
    case default ; stop "ob%meshOrder>3 not implemented"
    end select
    
    call writeSoluBlock()
    
    iDeg=iDeg+nDeg
    iCell0=iCell0+nCell
  endif
  !<<< HOSolAtHexahedra
  
  !>>> HOSolAtTetrahedra
  nCell=ob%nT4
  if( .not.nCell==0 )then
    print '(4x,"HOSolAtTetrahedra")'
    nNod=size(ob%T4uvw,2) ; nDeg=nCell*nNod
    
    select case(ob%meshOrder)
    case(1) ; GmfKey=GmfHOSolAtTetrahedraP1
    case(2) ; GmfKey=GmfHOSolAtTetrahedraP2
    case(3) ; GmfKey=GmfHOSolAtTetrahedraP3
    case default ; stop "ob%meshOrder>3 not implemented"
    end select
    
    call writeSoluBlock()
    
    iDeg=iDeg+nDeg
    iCell0=iCell0+nCell
  endif
  !<<< HOSolAtTetrahedra
  
  !>>> HOSolAtQuadrilaterals
  nCell=ob%nQ4
  if( .not.nCell==0 )then
    print '(4x,"HOSolAtQuadrilaterals")'
    nNod=size(ob%Q4uvw,2) ; nDeg=nCell*nNod
    
    select case(ob%meshOrder)
    case(1) ; GmfKey=GmfHOSolAtQuadrilateralsQ1
    case(2) ; GmfKey=GmfHOSolAtQuadrilateralsQ2
    case(3) ; GmfKey=GmfHOSolAtQuadrilateralsQ3
    case default ; stop "ob%meshOrder>3 not implemented"
    end select
    
    call writeSoluBlock()
    
    iDeg=iDeg+nDeg
    iCell0=iCell0+nCell
  endif
  !<<<
  
  !>>> HOSolAtTriangles
  nCell=ob%nT3
  if( .not.nCell==0 )then
    print '(4x,"HOSolAtTriangles")'
    nNod=size(ob%T3uvw,2) ; nDeg=nCell*nNod
    
    select case(ob%meshOrder)
    case(1) ; GmfKey=GmfHOSolAtTrianglesP1
    case(2) ; GmfKey=GmfHOSolAtTrianglesP2
    case(3) ; GmfKey=GmfHOSolAtTrianglesP3
    case default ; stop "ob%meshOrder>3 not implemented"
    end select
    
    call writeSoluBlock()
    
    iDeg=iDeg+nDeg
    iCell0=iCell0+nCell
  endif
  !<<< HOSolAtTriangles  
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  iErr=GmfCloseMesh(inriaSol)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  print '("<<< Writing Inria HO solution: ",a)',trim(file)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  return
contains
  
  subroutine writeNodeBlock()
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    iErr=GmfSetKwd(inriaSol,GmfKey,nNod)
    iErr=GmfSetBlock(                                    &
    &    inriaSol                                       ,&
    &    GmfKey                                         ,&
    &    int(   1,kind=8)                               ,&
    &    int(nNod,kind=8)                               ,&
    &    0, %val(0), %val(0)                            ,&
    &    GmfDoubleVec,size(uvw,1), uvw(1,1), uvw(1,nNod) )
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    return
  end subroutine writeNodeBlock
  
  subroutine writeSoluBlock()
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    real(8), pointer :: solu0(:,:)
    real(8), pointer :: solu1(:,:)  
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    print '(4x,"writeSoluBlock")'
    print '(6x,"ord  ="      ,i0 )',ob%ord(iCell0)
    print '(6x,"strd ="      ,i0 )',ob%ker
    print '(6x,"nCell      =",i10)',nCell
    print '(6x,"nNod       =",i10)',nNod
    print '(6x,"nCell*nNod =",i10)',nCell*nNod
    print '(6x,"nDeg       =",i10)',nDeg
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>       
    !> solu0(1:ob%ker,1:nNod*nCell)
    
    if( ob%solutionIsReal )then
      
      print '(6x,"nDeg       =",i10)',nDeg
      print '(6x,"size(dsol )=",i2,"x",i10)',size(ob%dsol,1),size(ob%dsol,2)
      solu0=>ob%dsol(1:ob%ker,iDeg+1:iDeg+nDeg)
      print '(6x,"size(solu0)=",i10,"x",i10,"=",i10)',size(solu0,1),size(solu0,2),size(solu0)
      
    else
      
      block
      integer :: jDeg
      print '(6x,"size(zsol )=",i3,"x",i0)',size(ob%zsol,1),size(ob%zsol,2)
      allocate(solu0(1:ob%ker,iDeg+1:iDeg+nDeg))
      
      do jDeg=iDeg+1,iDeg+nDeg
        solu0(1:ob%ker,jDeg)=real(ob%zsol(1:ob%ker,jDeg),kind=8)
      enddo
      print '(6x,"nDeg       =",i10)',nDeg
      print '(6x,"size(solu0)=",i10,"=",i10,"x",i10)',size(solu0),size(solu0,1),size(solu0,2)
      end block
      
    endif
    
    !> solu1(1:ob%ker*nNod,1:nCell)
    call c_f_pointer(cptr=c_loc(solu0), fptr=solu1, shape=[ob%ker*nNod,nCell])
    print '(6x,"size(solu1)=",i10,"=",i10,"x",i10)',size(solu1),size(solu1,1),size(solu1,2)
    
    iErr=GmfSetKwd(inriaSol,GmfKey,nCell,nFld,kind(1),ob%ord(iCell0),nNod)                        !> ob%ord(iCell0) car iso ordre
    print '(6x,"nFld=",i10,2x,"kind=",*(i2,1x))',nFld,kind(1:nFld)
    
    iErr=GmfSetBlock(                                          &
    &    inriaSol                                             ,&
    &    GmfKey                                               ,&
    &    int(    1,kind=8)                                    ,&
    &    int(nCell,kind=8)                                    ,&
    &    0, %val(0), %val(0)                                  ,&
    &    GmfDoubleVec,ob%ker*nNod, solu1(1,1), solu1(1,nCell)  )
    
    if( ob%solutionIsReal )then
      solu0=>null()
      solu1=>null()
    else
      deallocate(solu0)
      solu1=>null()
    endif
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    print '(4x,"writeSoluBlock end")'
    return
  end subroutine writeSoluBlock
  
end procedure writeInriaHOBinary

module procedure display
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  integer                    :: iKer
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if( ob%solution )then
    write(*,'(/"Solutions")')
  else
    write(*,'(/"Positions")')
  endif
  print '(/"ob%ker     =",i6    )',ob%ker
  print '( "ob%nCell   =",i6    )',ob%nCell
  print '( "ob%nDeg    =",i6    )',ob%nDeg
  if( ob%solution )then
    print '( "ob%iRework =",i6    )',ob%iRework
    print '( "ob%equation=",i6    )',ob%equation
    print '( "ob%count0  =",i6    )',ob%count0
    print '( "ob%count1  =",i6    )',ob%count1
    print '( "ob%time    =",e22.15)',ob%time
    
    if( ob%solutionIsReal )then
      do iKer=1,ob%ker
        print '("iCmp=",i0," min/max=",2(e22.15,1x))',iKer,minval(ob%dSol(iKer,:)),maxval(ob%dSol(iKer,:))
      enddo
    else
      do iKer=1,ob%ker
        print '("iCmp=",i0," min/max=",2(e22.15,1x))',iKer,minval(abs(ob%zSol(iKer,:))),maxval(abs(ob%zSol(iKer,:)))
      enddo
    endif
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  return
end procedure display

module procedure displaySol
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  integer                   :: iKer
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  print '(/">>> displaySol")'
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if( ob%solutionIsReal )then
    do iKer=1,ob%ker
      print '(4x,"comp ",i1,": min/max=",e22.15,"/",e22.15)',iKer,minval(ob%dsol(iKer,:)),maxval(ob%dsol(iKer,:)) 
    enddo
  else
    do iKer=1,ob%ker
      print '(4x,"comp ",i1,": min/max=",e22.15,"/",e22.15)',iKer,minval(abs(ob%zsol(iKer,:))),maxval(abs(ob%zsol(iKer,:))) 
    enddo
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !block
  !integer                   :: iCell,iDeg
  !iCell=1
  !do iDeg=1,ob%nDeg
  !  if( iDeg>ob%deg(iCell+1)-1 )iCell=iCell+1
  !  print '(4x,"iCell=",i10," iDeg=",i10," xyz=",5(e22.15,1x))',iCell,iDeg,ob%dsol(1:ob%ker,iDeg)
  !enddo
  !end block
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  print '("<<< displaySol")'
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  return
end procedure displaySol

end submodule myProcedures


subroutine compareRAW()
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  use mesParametres
  use myData
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
  type(monType)       :: ob
  type(monType)       :: ob1
  type(monType)       :: ob2
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
  logical             :: test
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
  call ob1%readRAW
  call ob2%readRAW
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  test = ob1==ob2

  if( .not.test )then
    call ob1%display
    call ob2%display
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  print '(/"Starting Analysis")'
  if( test )then
    
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    print '(4x,"Files are compatible")'
    ob%ker    =ob1%ker
    ob%nCell  =ob1%nCell
    ob%nDeg   =ob1%nDeg
    ob%iRework=ob1%iRework
    ob%count0 =ob1%count0
    ob%count1 =ob1%count1
    
    do iDeg=1,ob1%nDeg
      d=norm2(ob1%dsol(1:ob1%ker,iDeg))
      dAve=dAve+d
    enddo
    dAve=dAve/ob1%nDeg
    print '(3x,"Computing     ",21x,        "          Average=",e22.15)',dAve
    
    dMax=-1d10
    cpt=0
    iCell=1
    do iDeg=1,ob%nDeg ! print '("iDeg=",i10,"/",i10)',iDeg,ob%nDeg
    
      if( ob1%version>2 )then
        if( iDeg>ob1%deg(iCell+1)-1 )iCell=iCell+1
      endif
      
      dSol(1:ob%ker)=ob2%dsol(1:ob2%ker,iDeg)-ob1%dsol(1:ob1%ker,iDeg)
      d=norm2(dSol(1:ob%ker))/dAve
      if( dMax<d )then
        iDegMax=iDeg
        iCellMax=iCell
        dSolMax(1:ob%ker)=dSol(1:ob%ker)
        dMax=d
      endif
      if( d>eps )cpt=cpt+1
    enddo
    print '(3x,"Computing cpt=",i10,"/",i10," deltaMax/Average=",e22.15)',cpt,ob%nDeg,dMax
    
    
    if    ( 0d0<dMax .and. dMax<eps )then
      print '(/3x,"Solutions are found to be in perfect agreement")'
    elseif( eps<dMax )then
      print '(/3x,"Solutions are not found to be in perfect agreement")'
      
      if( ob1%version>2 )then
        select case(ob%ker)
        case(3)
          print '(/3x,"iCell=",i10," iDeg=",i10," sol2-sol1=",3(e22.15,1x)," d=",e22.15)',iCellMax,iDegMax,dSolMax(1:ob%ker),dMax
          print '( 3x,"iCell=",i10," iDeg=",i10," sol1     =",3(e22.15,1x)             )',iCellMax,iDegMax,ob1%dsol(1:ob1%ker,iDegMax)
          print '( 3x,"iCell=",i10," iDeg=",i10," sol2     =",3(e22.15,1x)             )',iCellMax,iDegMax,ob2%dsol(1:ob2%ker,iDegMax)
        case(4)
          print '(/3x,"iCell=",i10," iDeg=",i10," sol2-sol1=",4(e22.15,1x)," d=",e22.15)',iCellMax,iDegMax,dSolMax(1:ob%ker),dMax
          print '( 3x,"iCell=",i10," iDeg=",i10," sol1     =",4(e22.15,1x)             )',iCellMax,iDegMax,ob1%dsol(1:ob1%ker,iDegMax)
          print '( 3x,"iCell=",i10," iDeg=",i10," sol2     =",4(e22.15,1x)             )',iCellMax,iDegMax,ob2%dsol(1:ob2%ker,iDegMax)
        case(5)
          print '(/3x,"iCell=",i10," iDeg=",i10," sol2-sol1=",5(e22.15,1x)," d=",e22.15)',iCellMax,iDegMax,dSolMax(1:ob%ker),dMax
          print '( 3x,"iCell=",i10," iDeg=",i10," sol1     =",5(e22.15,1x)             )',iCellMax,iDegMax,ob1%dsol(1:ob1%ker,iDegMax)
          print '( 3x,"iCell=",i10," iDeg=",i10," sol2     =",5(e22.15,1x)             )',iCellMax,iDegMax,ob2%dsol(1:ob2%ker,iDegMax)
        end select
      else
        select case(ob%ker)
        case(3)
          print '(/3x,"iDeg=",i10," sol2-sol1=",3(e22.15,1x)," d=",e22.15)',iDegMax,dSolMax(1:ob%ker),dMax
          print '( 3x,"iDeg=",i10," sol1     =",3(e22.15,1x)             )',iDegMax,ob1%dsol(1:ob1%ker,iDegMax)
          print '( 3x,"iDeg=",i10," sol2     =",3(e22.15,1x)             )',iDegMax,ob2%dsol(1:ob2%ker,iDegMax)
        case(4)
          print '(/3x,"iDeg=",i10," sol2-sol1=",4(e22.15,1x)," d=",e22.15)',iDegMax,dSolMax(1:ob%ker),dMax
          print '( 3x,"iDeg=",i10," sol1     =",4(e22.15,1x)             )',iDegMax,ob1%dsol(1:ob1%ker,iDegMax)
          print '( 3x,"iDeg=",i10," sol2     =",4(e22.15,1x)             )',iDegMax,ob2%dsol(1:ob2%ker,iDegMax)
        case(5)
          print '(/3x,"iDeg=",i10," sol2-sol1=",5(e22.15,1x)," d=",e22.15)',iDegMax,dSolMax(1:ob%ker),dMax
          print '( 3x,"iDeg=",i10," sol1     =",5(e22.15,1x)             )',iDegMax,ob1%dsol(1:ob1%ker,iDegMax)
          print '( 3x,"iDeg=",i10," sol2     =",5(e22.15,1x)             )',iDegMax,ob2%dsol(1:ob2%ker,iDegMax)
        end select
      endif
    endif
    print '()'
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    open(unit=100,file='regression.log',position='append',action='write', iostat=iErr)
    write(100,'(a,2x,a,3x,2(a,2x),"cpt=",i10,"/",i10,2x,"dltMax/Aver=",e22.15)')clock0%date,clock0%time(1:6),trim(ob1%file),trim(ob2%file),cpt,ob%nDeg,dMax
    close(100)
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    
    if( .not.cpt==0 )then
      
      print '(/"Starting Analysis2 =")'
      cpt=0
      iCell=1
      if( ob1%version>2 )then
        do iDeg=1,ob%nDeg ! print '("iDeg=",i10,"/",i10)',iDeg,ob%nDeg
          if( iDeg>ob1%deg(iCell+1)-1 )iCell=iCell+1
          dSol(1:ob%ker)=ob2%dsol(1:ob2%ker,iDeg)-ob1%dsol(1:ob1%ker,iDeg)
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
        do iDeg=1,ob%nDeg ! print '("iDeg=",i10,"/",i10)',iDeg,ob%nDeg
          dSol(1:ob%ker)=ob2%dsol(1:ob2%ker,iDeg)-ob1%dsol(1:ob1%ker,iDeg)
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
      
      print '(/3x,"cpt=",i10,"/",i10," dMax=",e22.15)',cpt,ob%nDeg,dMax
    endif
    !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if( verbose>0 .and. test .and. .not.cpt==0 .and. dMax>eps )then
    
    print '(/"Writing file: spaceDelta.dat")'
    ob%file="./spaceDelta.dat"
    !open(unit=250            ,&
    !&    file=trim(ob%file)  ,&
    !&    form='unformatted'  ,&
    !&    recordtype='stream' ,&
    !&    action='write'      ,&
    !&    status='unknown'     )
    open(unit=250            ,&
    &    file=trim(ob%file)  ,&
    &    form='unformatted'  ,&
    &    access='stream'     ,&
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
    
    write(unit=250) ob1%ker*ob1%nDeg  ! write(*,'("nDeg=   ",i10  )')ob%nDeg
    write(unit=250) ((ob1%dsol(iVar,iDeg)-ob2%dsol(iVar,iDeg),iVar=1,ob1%ker),iDeg=1,ob1%nDeg)
    
    close(unit=250)
    
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call ob%delete
  call ob1%delete
  call ob2%delete
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  return
end subroutine compareRAW

subroutine exportInriaHO()
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  use myData
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  implicit none
  type(monType) :: ob
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  print '(/"usage: lecture <file.dat>")'
  print '(4x,"file: solutions")'
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if( command_argument_count()>=1 )then
    call get_command_argument(number=1,value =ob%file   )
  else
    write(*,'(/"file: ")',advance='no') ; read(*,'(a)')ob%file
  endif
  call ob%readRaw
  call ob%displaySol
  call ob%isoOrder       (ord=max(maxval(ob%ord),1))
  call ob%displaySol
  call ob%writeInria
  call ob%delete
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  return
end subroutine exportInriaHO


subroutine replaceRAW()
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  use mesParametres
  use myData
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
  type(monType)       :: ob1,ob2
  integer             :: i
  integer             :: iDeg
  real(8) , parameter :: eps=1d-12
  integer             :: verbose
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
  print '(/"usage: lecture <file1> <file2>")'
  print '(4x,"file1: positions")'
  print '(4x,"file2: solutions")'
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if( command_argument_count()>=1 )then
    call get_command_argument(number=1,value =ob1%file   )
    call get_command_argument(number=2,value =ob2%file   )
  else
    write(*,'(/"file1: ")',advance='no') ; read(*,'(a)')ob1%file
    write(*,'( "file2: ")',advance='no') ; read(*,'(a)')ob2%file
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  print '(/"Starting Replacing Solution")'
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call readRAW(ob=ob1)
  call readRAW(ob=ob2)
  
 !call display(ob=ob1)
 !call display(ob=ob2)
  if( .not.ob1%nDeg==ob2%nDeg )then
    print '( "ob1%nDeg=",i6    )',ob1%nDeg
    print '( "ob2%nDeg=",i6    )',ob2%nDeg
    print '("Stop @ replaceRAW ob1%nDeg/=ob2%nDeg")'
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Replacing Values
  block
  real(8)             :: x,y
  real(8)             :: x0,y0,z0
  !real(8)             :: T,p,r,u,v,w,ru,rv,rw,re,p0,r0,ru0,rv0,rw0,u0,v0,w0,re0,vRef
  real(8)             :: T,p,r,u,v,ru,rv,re,p0,r0,ru0,rv0,u0,v0,re0,vRef
  real(8)             :: amp,sigma
  real(8)             :: coef
  real(8) , parameter :: ln2=0.693147180559945d0
  real(8) , parameter :: gamma=1.4d0
  
  !>>>>>>
  amp=1d0
  sigma=3e-2
  
  x0=5d-1
  y0=5d-1
  z0=5d-1
  !<<<<<<
  
  select case(ob2%geometry)
  case(Geo2D)
    
    select case(ob2%equation)
    case(EqnEUL)
      do iDeg=1,ob2%nDeg
        x=ob1%dsol(1,iDeg)
        y=ob1%dsol(2,iDeg)
        coef=amp*exp(-ln2*((x-x0)**2+(y-y0)**2 )/(sigma*sigma))
        
        !> Non-dimensional Values of T
        T=1d0+(Gamma-1)/(2d0*gamma)*coef
        
        !> Non-dimensional Values of u,v,r,p
        u=0d0
        v=0d0
        p=T**(Gamma/(gamma-1d0))
        r=T**(  1d0/(gamma-1d0))
       !print '("T=",f12.5,1x,"r=",f12.5,1x,"p=",f12.5)',T,r,p
        
        !> Physical Values of u,v,r,p
        r0 =ob2%dsol(1,iDeg)
        ru0=ob2%dsol(2,iDeg) ; u0=ru0/r0
        rv0=ob2%dsol(3,iDeg) ; v0=rv0/r0
        re0=ob2%dsol(4,iDeg)
        p0=(gamma-1d0)*re0+5d-1*r0*(u0*u0+v0*v0)
        
        
        vRef=sqrt(p0/r0)
        u=vRef*u
        v=vRef*v
        r=r0*r
        p=p0*p
        
        !> Advection
        u=u0 !+u
        v=v0 !+v
        
        !> Conservative Variables
        ru=r*u
        rv=r*v
        re=p/(gamma-1d0)+5d-1*r*(u*u+v*v)
        
        !> Variations correspondantes
        r =r -r0
        ru=ru-ru0
        rv=rv-rv0
        re=re-re0
        
        ob2%dsol(1:4,iDeg)=ob2%dsol(1:4,iDeg)+[r,ru,rv,re] ! print '("sol=",4(e22.15,1x))',sol(1:4,iVert)
        
      enddo
    case default
      print '("Stop @ replaceRAW Equation not Implemented")'
    end select
    
  case(Geo3D)
    
    select case(ob2%equation)
    case default
      print '("Stop @ replaceRAW Equation not Implemented")'
    end select
    
  case default
    print '("Stop @ replaceRAW Geometry not Implemented")'
  end select
  
  end block
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  i=index(ob2%file,'.',.true.)-1 ; ob2%file=ob2%file(1:i)//'_new.dat'
 !print '("New Raw: ",a)',trim(ob2%file)
  call writeRAW(ob=ob2)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call delete(ob=ob1)
  call delete(ob=ob2)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  return
end subroutine replaceRAW


subroutine readPosition()
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  use mesParametres
  use myData
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
  type(monType)       :: ob
  real(8) , parameter :: eps=1d-12
  integer             :: verbose
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
  
  !call compareRAW()
  !call readPosition()
  !call replaceRAW()
  
  call exportInriaHO()
    
end program main
