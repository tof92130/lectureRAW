# lectureRAW

lecture des sorties binaires et space
comparaisons de deux sorties binaires solutions de space

ifort lectureRAW.f90 ${LIBMESH_DIR}/lib/libMeshbf.7.a -I${LIBMESH_DIR}/include -o lectureRaw

ifort lectureRAW.f90 ${LIBMESH_DIR}/lib/libMeshbf.7.a -I${LIBMESH_DIR}/include -o lectureRaw -g  -traceback -check


cp lectureRAW /Users/peyret/Applications/bin//lectureRaw


rsync --delete -av ~/Developer/Space/lectureRAW peyret@spiro-daaa:~/Developer/lectureRAW/

