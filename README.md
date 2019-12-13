# lectureRAW

lecture des sorties binaires et space
comparaisons de deux sorties binaires solutions de space



ifort lectureRAW.f90 ${LIBMESH_DIR}/lib/libMeshbf.7.a  -I${LIBMESH_DIR}/include  ${SPACE_BASIS_DIR}/lib/libspaceBasis.a -I${SPACE_BASIS_DIR}/include  -mkl -o lectureRaw


ifort lectureRAW.f90 ${LIBMESH_DIR}/lib/libMeshbf.7.a  -I${LIBMESH_DIR}/include  ${SPACE_BASIS_DIR}/lib/libspaceBasis.a -I${SPACE_BASIS_DIR}/include  -mkl -g  -traceback -check -o lectureRaw


Version GNU


gfortran-9 lectureRAW.f90 ${LIBMESH_DIR}/lib/libMeshbf.7.a  -I${LIBMESH_DIR}/include  ${SPACE_BASIS_DIR}/lib/libspaceBasis.a -I${SPACE_BASIS_DIR}/include  -g  -ffree-line-length-256 -framework Accelerate -o lectureRaw


cp lectureRAW /Users/peyret/Applications/bin//lectureRaw


rsync --delete -av ~/Developer/Space/lectureRAW peyret@spiro-daaa:~/Developer/lectureRAW/

