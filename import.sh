#!/bin/bash -xe

POLY="$1"
test -z "$POLY" && POLY="europe"
POLY="$POLY.poly"

X="$2"
test -z "$X" && X="test"

OUTDIR="$3"
test -z "$OUTDIR" && OUTDIR=$PWD/build/data

#URL="http://planet.openstreetmap.org/planet/planet-latest.osm.bz2"
#URL="http://download.geofabrik.de/openstreetmap/europe.osm.bz2"
URL="http://download.geofabrik.de/europe/germany-latest.osm.bz2"
#URL="http://download.geofabrik.de/openstreetmap/europe/germany/baden-wuerttemberg.osm.bz2"
#URL="http://download.geofabrik.de/openstreetmap/europe/germany/baden-wuerttemberg/tuebingen-regbez.osm.bz2"
PROFILES="foot bicycle mtb"

#renice 20 -p $$
#ionice -c 3 -p $$

test -d $OUTDIR || mkdir -p $OUTDIR
rm -vf ${OUTDIR}/$X.osm.pbf
for P in $PROFILES; do
    rm -vf ${OUTDIR}/$P/$X.*
done

test -f ${OUTDIR}/$X.osm.bz2 || { wget -O ${OUTDIR}/$X.osm.bz2 "$URL" && rm -vf ${OUTDIR}/$X.cut.osm.bz2 ; }

# download poly files
test -f ${OUTDIR}/europe.poly || wget -O ${OUTDIR}/europe.poly "https://raw.github.com/MaZderMind/osm-history-splitter/master/clipbounds/europe.poly"
test -f ${OUTDIR}/germany.poly || wget -O ${OUTDIR}/germany.poly "http://download.geofabrik.de/europe/germany.poly"
test -f ${OUTDIR}/bw.poly || wget -O ${OUTDIR}/bw.poly "https://raw.github.com/MaZderMind/osm-history-splitter/master/clipbounds/europe/germany/baden-wuerttemberg.poly"
# create poly file
{ cat <<EOF
none
1
    9.04293199 48.52349435
    9.04292543 48.51572149
    9.07109099 48.51570754
    9.07110187 48.52348040
END
END
EOF
} > ${OUTDIR}/tue.poly

# cut out poly using osmosis
test -f ${OUTDIR}/$X.cut.osm.bz2 || pv ${OUTDIR}/$X.osm.bz2|pbzip2 -d|osmosis --read-xml file=/dev/stdin --bounding-polygon file=${OUTDIR}/$POLY completeWays=yes --write-xml /dev/stdout|pbzip2 > ${OUTDIR}/$X.cut.osm.bz2

pushd waysplit
# note: $X.split.osm.bz2 only for debugging
time ./waysplit.sh ${OUTDIR}/$X.cut.osm.bz2 ${OUTDIR}/${X}.ways.dbm ${OUTDIR}/${X}.sql.bz2 |tee >(pbzip2 > ${OUTDIR}/$X.split.osm.bz2)|osmosis --read-xml - --write-pbf ${OUTDIR}/$X.osm.pbf omitmetadata=true
popd

for P in $PROFILES; do
    PROFILE="profiles/$P.lua"
    mkdir -p ${OUTDIR}/$P
    cp -vl ${OUTDIR}/$X.osm.pbf ${OUTDIR}/$P/$X.osm.pbf
    time ./build/osrm-extract ${OUTDIR}/$P/$X.osm.pbf $PROFILE
    time ./build/osrm-prepare ${OUTDIR}/$P/$X.osrm ${OUTDIR}/$P/$X.osrm.restrictions $PROFILE
    rm -v ${OUTDIR}/$P/$X.osm.pbf
done
