#!/bin/bash -xe
X="$1"
test -z "$X" && X="unstable"
#renice 20 -p $$
#ionice -c 3 -p $$
rm -vf ~/osm/$X.osm.bz2 ~/osm/$X.osm.pbf ~/osm/$X.osrm.*
URL="http://planet.openstreetmap.org/planet/planet-latest.osm.bz2"
#URL="http://download.geofabrik.de/openstreetmap/europe.osm.bz2"
#URL="http://download.geofabrik.de/openstreetmap/europe/germany/baden-wuerttemberg.osm.bz2"
#URL="http://download.geofabrik.de/openstreetmap/europe/germany/baden-wuerttemberg/tuebingen-regbez.osm.bz2"
test -f ~/osm/$X.osm.bz2 || wget -O ~/osm/$X.osm.bz2 "$URL"

# cut out poly using osmosis
test -f ~/osm/europe.poly || wget -O ~/osm/europe.poly "https://raw.github.com/MaZderMind/osm-history-splitter/master/clipbounds/europe.poly"
test -f ~/osm/bw.poly || wget -O ~/osm/bw.poly "https://raw.github.com/MaZderMind/osm-history-splitter/master/clipbounds/europe/germany/baden-wuerttemberg.poly"

pv ~/osm/$X.osm.bz2|pbzip2 -d|osmosis --read-xml file=/dev/stdin --bounding-polygon file=~/osm/bw.poly completeWays=yes --write-xml /dev/stdout|pbzip2 > ~/osm/$X.2.osm.bz2
mv -v ~/osm/$X.osm.bz2 ~/osm/$X.1.osm.bz2
mv -v ~/osm/$X.2.osm.bz2 ~/osm/$X.osm.bz2


pushd waysplit
time ./waysplit.sh ~/osm/$X.osm.bz2 ~/osm/${X}_ways.dbm |osmosis --read-xml - --write-pbf ~/osm/$X.osm.pbf omitmetadata=true
popd

PROFILE="profiles/bicycle.lua"
time ./build/osrm-extract ~/osm/$X.osm.pbf $PROFILE
time ./build/osrm-prepare ~/osm/$X.osrm ~/osm/$X.osrm.restrictions $PROFILE
