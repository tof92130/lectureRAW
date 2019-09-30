# lectureRAW

lecture des sorties binaires et space
comparaisons de deux sorties binaires solutions de space

ifort lectureRAW.f90 ${LIBMESH_DIR}/lib/libMeshbf.7.a -I${LIBMESH_DIR}/include -o lectureRAW

ifort lectureRAW.f90 ${LIBMESH_DIR}/lib/libMeshbf.7.a -I${LIBMESH_DIR}/include -o lectureRAW -g  -traceback -check


cp lectureRAW /Users/peyret/Applications/bin//lectureRaw

