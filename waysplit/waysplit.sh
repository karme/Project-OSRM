#!/bin/bash -e
# split osm ways at intersections (and do some other stuff):
# - drop unused nodes
# - add elevation profile to ways
# - denormalize relations
#
# Copyright (C) 2013 Jens Thiele <karme@karme.de>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

set -o pipefail

OSM_IN="$1"
test -z "$OSM_IN" && OSM_IN="test.osm.bz2"
test -e "$OSM_IN"

OSM_OUT="$2"
test -z "$OSM_OUT" && OSM_OUT="${OSM_IN%*.osm.bz2}_out.osm.pbf"
test ! -e "$OSM_OUT" || {
    echo "$OSM_OUT exists"
    exit 1
}

echo Processing "$OSM_IN" to "$OSM_OUT"

function infilter()
{
    pv|bzcat
    # note: doesn't help if file was not compressed with pbzip2!
    # pv|pbzip2 -d
}

function outfilter()
{
    #pbzip2 > "$OSM_OUT"
    osmosis --read-xml - --write-pbf "$OSM_OUT" omitmetadata=true
}

# store a sparse set/bitmap to a dbm file
function store-sparse-bitmap-dbm()
{
    local DBM="$1"
    if test -f "$DBM" && file "$DBM"|grep -q dbm; then
	rm -v "$DBM" 1>&2
    fi
    ./store-sparse-bitmap-dbm.scm "$DBM" 1>&2
}

function max-id()
{
    # builds dom :(
    # xmlstarlet sel --text -t -v "math:max(//@id)"
    ./min-max-id.c|cut -f 2 -d " "
}

function cpus()
{
    grep ^proc /proc/cpuinfo |wc -l
}

echo "A) extract used nodes, real nodes, max id and transform xml to sxml"
< "$OSM_IN" infilter|tee \
    >(./fastxml2sxml.scm|grep '^(\(node\|way.*k "highway"\|relation\)'|tee >(grep '^(way'|./used-nodes.scm|sort -n|tee >(uniq -d|store-sparse-bitmap-dbm real-nodes.dbm)|uniq|store-sparse-bitmap-dbm used-nodes.dbm) > osm_as_sxml.scm) \
    |max-id > max-id.out

echo "B) store used nodes positions (depends on A)"
rm -vf node-pos.dbm
pv osm_as_sxml.scm|grep '^(node'|./used-nodes-pos.scm used-nodes.dbm|./store-node-pos.scm node-pos.dbm "$(cat max-id.out)"
# note: don't need used-nodes.dbm anymore
#rm -v used-nodes.dbm

echo "C) calculate way splits (depends on A, todo: could be merged with B)"
rm -vf way-splits.dbm
pv osm_as_sxml.scm|grep '^(way'|./parallel-pipe.scm $(cpus) read-line print-line read-line print ./way-splits.scm real-nodes.dbm |./store-way-splits.scm "$(cat max-id.out)" way-splits.dbm

echo "D) parse relations (todo: could be pipelined)"
rm -vf way-relation.dbm relation.dbm
pv osm_as_sxml.scm|grep '^(relation'|./relations.scm

echo "E) drop unused nodes, apply way splits, profile ways, denormalize relations, fix way references in restriction relations, transform sxml to xml"
PSPLITS=2
if [ $(cpus) -gt 2 ]; then
    PSPLITS=$[$(cpus)/2]
fi
pv osm_as_sxml.scm| { ./parallel-pipe.scm $PSPLITS read-line print-line read-blob write-blob ./apply-way-splits.scm way-splits.dbm parallel-pipe|./fastsxml2xml.scm|outfilter ; } |& tee restrictions.log|grep ' WARNING!\| INFO:'
# serial version
#pv osm_as_sxml.scm| { ./apply-way-splits.scm way-splits.dbm write-lines|./fastsxml2xml.scm|outfilter ; } |& tee restrictions.log|grep ' WARNING!\| INFO:'

# remove temp files
# rm -v real-nodes.dbm max-id.out osm_as_sxml.scm way-splits.dbm restrictions.log
