#!/bin/bash -xe

function timed() {
    /usr/bin/time -v "$@"
}

POLY="$1"
test -z "$POLY" && POLY="europe"
POLY="$POLY.poly"

X="$2"
test -z "$X" && X="test"

OUTDIR="$3"
test -z "$OUTDIR" && OUTDIR=$PWD/build/data

#URL="http://planet.openstreetmap.org/planet/planet-latest.osm.bz2"
URL="http://download.geofabrik.de/openstreetmap/europe.osm.bz2"
#URL="http://download.geofabrik.de/europe/germany-latest.osm.bz2"
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

test -f ${OUTDIR}/$X.osm.bz2 || { wget -O ${OUTDIR}/$X.osm.bz2 "$URL" && rm -vf ${OUTDIR}/$X.cut.*.osm.bz2 ; }

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
} > ${OUTDIR}/tuemini.poly

{ cat <<EOF
none
1
    8.77 48.59
    8.77 48.44
    9.33 48.44
    9.33 48.59
END
END
EOF
} > ${OUTDIR}/tue.poly

{ cat <<EOF
none
1
    10.65 45.9
    10.65 45.82
    10.89 45.82
    10.89 45.9
END
END
EOF
} > ${OUTDIR}/gardasee.poly

{ cat <<EOF
none
1
    10.65 45.9
    10.65 45.82
    10.89 45.82
    10.89 45.9
END
2
    10 47.62
    10 47.47
    10.4 47.47
    10.4 47.62
END
3
    8.77 48.59
    8.77 48.44
    9.33 48.44
    9.33 48.59
END
END
EOF
} > ${OUTDIR}/tests.poly

# cut out poly using osmosis
CUTOSM="${OUTDIR}/$X.cut.${POLY%*.poly}.osm.bz2"
test -f "$CUTOSM" || pv ${OUTDIR}/$X.osm.bz2|pbzip2 -d|osmosis --read-xml file=/dev/stdin --bounding-polygon file=${OUTDIR}/$POLY completeWays=yes --write-xml /dev/stdout|pbzip2 > "$CUTOSM"

pushd waysplit
# note: $X.split.osm.bz2 only for debugging
timed ./waysplit.sh "$CUTOSM" ${OUTDIR}/${X}.ways.dbm ${OUTDIR}/${X}.sql.bz2 |tee >(pbzip2 > ${OUTDIR}/$X.split.osm.bz2)|osmosis --read-xml - --write-pbf ${OUTDIR}/$X.osm.pbf omitmetadata=true
popd

for P in $PROFILES; do
    PROFILE="profiles/$P.lua"
    mkdir -p ${OUTDIR}/$P
    cp -vl ${OUTDIR}/$X.osm.pbf ${OUTDIR}/$P/$X.osm.pbf
    timed ./build/osrm-extract ${OUTDIR}/$P/$X.osm.pbf $PROFILE
    time ./build/osrm-prepare ${OUTDIR}/$P/$X.osrm ${OUTDIR}/$P/$X.osrm.restrictions $PROFILE
    rm -v ${OUTDIR}/$P/$X.osm.pbf
done
