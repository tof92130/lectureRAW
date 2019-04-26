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
  end type mesDonnees

contains 

subroutine delete(ob)
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  type(mesDonnees) :: ob
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if( associated(ob%dsol) )deallocate(ob%dsol)
  if( associated(ob%zsol) )deallocate(ob%zsol)
  if( associated(ob%deg ) )deallocate(ob%deg)
  if( associated(ob%ord ) )deallocate(ob%ord)
  if( associated(ob%cellType) )deallocate(ob%cellType)
  if( associated(ob%H6uvw) )deallocate(ob%H6uvw)
  if( associated(ob%W5uvw) )deallocate(ob%W5uvw)
  if( associated(ob%P5uvw) )deallocate(ob%P5uvw)
  if( associated(ob%T4uvw) )deallocate(ob%T4uvw)
  if( associated(ob%Q4uvw) )deallocate(ob%Q4uvw)
  if( associated(ob%T3uvw) )deallocate(ob%T3uvw)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  return
end subroutine delete

subroutine readRaw(ob)
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  type(mesDonnees) :: ob
  !>
  integer          :: iVar,iDeg,iCell
  integer          :: iOrd,iNod,nNod,Strd
  real(8),pointer  :: uvw(:,:)
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
  print '("Space Raw Format Version: ",i1)',ob%version
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
  print '("Solution is ",a)',trim(buffer)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Mesh Order
  read(unit=250)buffer ! write(*,'(a)')trim(buffer)  
  read(buffer,'(11x,i2)')ob%meshOrder
  print '("meshOrder: ",i2)',ob%meshOrder
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Geometry
  if( ob%version>=3 )then
    read(unit=250)buffer ; write(*,'(a)')trim(buffer)
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
  read(unit=250)ob%nDeg ; ob%nDeg=ob%nDeg/ob%ker
  write(*,'("nDeg=   ",i10  )')ob%nDeg
  if( ob%solutionIsReal )then
    allocate(ob%dsol(1:ob%ker,1:ob%nDeg))
    read(unit=250) ((ob%dsol(iVar,iDeg),iVar=1,ob%ker),iDeg=1,ob%nDeg)
  else
    allocate(ob%zsol(1:ob%ker,1:ob%nDeg))
    read(unit=250) ((ob%zsol(iVar,iDeg),iVar=1,ob%ker),iDeg=1,ob%nDeg)
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !> Reading uvw
  if( ob%version>=4 )then
    if( ob%solution )then
      readingNodesPositions : do
        read(unit=250)buffer ; write(*,'(/a)')trim(buffer) ; if( trim(buffer)=="End" )exit readingNodesPositions
        read(unit=250)iOrd 
        read(unit=250)Strd
        read(unit=250)nNod
        write(*,'("iOrd=",i0," Strd=",i0," nNod=",i0)')iOrd,Strd,nNod
        
        select case(trim(buffer))
        case("HexahedraQ1NodesPositions"     ,"HexahedraQ2NodesPositions"     ,"HexahedraQ3NodesPositions"     ) ; ob%H6uvw=>uvw
          allocate(ob%H6uvw(1:strd,1:nNod))
          read(250)ob%H6uvw(1:strd,1:nNod)
        case("PrismsP1NodesPositions"        ,"PrismsP2NodesPositions"        ,"PrismsP3NodesPositions"        ) ; ob%W5uvw=>uvw
          allocate(ob%W5uvw(1:strd,1:nNod))
          read(250)ob%W5uvw(1:strd,1:nNod)
        case("PyramidsP1NodesPositions"      ,"PyramidsP2NodesPositions"      ,"PyramidsP3NodesPositions"      ) ; ob%P5uvw=>uvw
        case("TetrahedraP1NodesPositions"    ,"TetrahedraP2NodesPositions"    ,"TetrahedraP3NodesPositions"    )
          allocate(ob%T4uvw(1:strd,1:nNod))
          read(250)ob%T4uvw(1:strd,1:nNod)
        case("QuadrilateralsQ1NodesPositions","QuadrilateralsQ2NodesPositions","QuadrilateralsQ3NodesPositions") ; ob%Q4uvw=>uvw 
          allocate(ob%Q4uvw(1:strd,1:nNod))
          read(250)ob%Q4uvw(1:strd,1:nNod)
        case("TrianglesP1NodesPositions"     ,"TrianglesP2NodesPositions"     ,"TrianglesP3NodesPositions"     ) ; ob%T3uvw=>uvw
          allocate(ob%T3uvw(1:strd,1:nNod))
          read(250)ob%T3uvw(1:strd,1:nNod)
        case default
          write(*,'(/"Choice geometry not possible: ",a)')trim(buffer)
          stop      
        end select
        
        !read(250)uvw(1:strd,1:nNod)
        !do iNod=1,nNod
        !  print '("uvw=",*(f12.5,1x))',uvw(1:strd,iNod)
        !enddo
        !uvw=>null()
        
      enddo readingNodesPositions
    endif
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  close(unit=250)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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
    case(tetra,tetra2          )                 ; ob%nT4=ob%nT4+1
    case(quad,quad2,quad3,quad4)                 ; ob%nQ4=ob%nQ4+1
    case(triangle,triangle2,triangle3,triangle4) ; ob%nT3=ob%nT3+1
    end select
  enddo
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  return
end subroutine readRaw

subroutine writeRaw(ob)
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  type(mesDonnees) :: ob
  !>
  integer          :: iVar,iDeg,iCell
  character(80)    :: buffer
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  print '(/"Writing: ",a)',trim(ob%file)
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  open(unit=250            ,&
  &    file=trim(ob%file)  ,&
  &    recordtype='stream' ,&
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
  
  return
end subroutine writeRaw

subroutine writeInriaHO(ob)
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  type(mesDonnees) :: ob
  !>
  integer          :: l
  integer          :: inriaSol
  integer          :: iCell,nCell,iType
  integer          :: iNod,nNod
  integer          :: deg0,iDeg,nDeg
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
    do iCell=1,ob%nCell
      if( ob%cellType(iCell)==iType )then
        deg0=ob%deg(iCell)-1 !> juste avant
        nDeg=ob%deg(iCell+1)-ob%deg(iCell)
        write(inriaSol,'(*(g0,1x))')ob%dsol(:,deg0+1:deg0+nDeg)
      endif
    enddo
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
      write(*,'(/"Mesh Order not possible with Tetrahedra")')
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
    do iCell=1,ob%nCell
      if( ob%cellType(iCell)==iType )then
        deg0=ob%deg(iCell)-1 !> juste avant
        nDeg=ob%deg(iCell+1)-ob%deg(iCell)
        write(inriaSol,'(*(g0,1x))')ob%dsol(:,deg0+1:deg0+nDeg)
      endif        
    enddo
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
    do iCell=1,ob%nCell
      if( ob%cellType(iCell)==iType )then
        deg0=ob%deg(iCell)-1 !> juste avant
        nDeg=ob%deg(iCell+1)-ob%deg(iCell)
        write(inriaSol,'(*(g0,1x))')ob%dsol(:,deg0+1:deg0+nDeg)
      endif        
    enddo    
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
    case(EqnLEE) ; write(inriaSol,'("2 2 1")')    ! {u1, v1}, x1=rh1*a0/rho0
    case(EqnEUL) ; write(inriaSol,'("3 1 2 1")')  ! rho, {rho u, rho v}, rho E
    case default
      write(*,'(/"Choice equation not possible: ",i0)')ob%equation
    end select
    write(inriaSol,'(i0,1x,i0)')ob%ord(1),nNod  ! on met ob%ord(1) car iso ordre
    do iCell=1,ob%nCell
      if( ob%cellType(iCell)==iType )then
        deg0=ob%deg(iCell)-1 !> juste avant
        nDeg=ob%deg(iCell+1)-ob%deg(iCell)
        write(inriaSol,'(*(g0,1x))')ob%dsol(:,deg0+1:deg0+nDeg)
      endif        
    enddo    
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  write(inriaSol,'(/"End")')
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  close(inriaSol)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  return
end subroutine writeInriaHO


subroutine display(ob)
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  type(mesDonnees), intent(in) :: ob
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
  endif
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
  do iDeg=1,ob%nDeg
    if( iDeg>ob%deg(iCell+1)-1 )iCell=iCell+1
    print '(3x,"iCell=",i10," iDeg=",i10," xyz=",5(e22.15,1x))',iCell,iDeg,ob%dsol(1:ob%ker,iDeg)
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
end function equal


end module mesProcedures


subroutine compareRAW()
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
    
    write(unit=250) ob1%ker*ob1%nDeg  ! write(*,'("nDeg=   ",i10  )')ob%nDeg
    write(unit=250) ((ob1%dsol(iVar,iDeg)-ob2%dsol(iVar,iDeg),iVar=1,ob1%ker),iDeg=1,ob1%nDeg)
    
    close(unit=250)
    
  endif
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  call delete(ob=ob )
  call delete(ob=ob1)
  call delete(ob=ob2)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  return
end subroutine compareRAW


subroutine replaceRAW()
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
  type(mesDonnees)    :: ob1,ob2
  integer             :: i
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
  real(8)             :: x,y,z
  real(8)             :: x0,y0,z0
  real(8)             :: rho0,rhou0,rhov0,rhow0,rhoE0,T,p,r,u,v,w,ru,rv,rw,re,p0,r0,ru0,rv0,rw0,u0,v0,w0,re0,vRef
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
  use mesProcedures
  
  !call compareRAW()
  !call readPosition()
  !call replaceRAW()
  
  
  block
  type(mesDonnees) :: ob
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
  call readRaw     (ob=ob)
  call writeInriaHO(ob=ob)
  end block
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
end program main



