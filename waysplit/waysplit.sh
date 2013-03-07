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

function msg()
{
    echo "$@" >&2
}

function atexit()
{
    local R=$?
    test 0 -ne $R && msg ERROR: $R maybe see also $MYTMPDIR/waysplit.log || true
}

function infilter()
{
    pv|bzcat
    # note: doesn't help if file was not compressed with pbzip2!
    # pv|pbzip2 -d
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

trap atexit EXIT 

OSM_IN="$1"
test -z "$OSM_IN" && OSM_IN="test.osm.bz2"
test -e "$OSM_IN"

MYTMPDIR="$(mktemp -d)"
{
msg Processing "$OSM_IN" to stdout using temporary directory $MYTMPDIR

msg "A) extract used nodes, real nodes, max id and transform xml to sxml"
< "$OSM_IN" infilter|tee \
    >(./fastxml2sxml.scm|grep '^(\(node\|way.*k "highway"\|relation\)'|tee >(grep '^(way'|./used-nodes.scm|sort -n|tee >(uniq -d|store-sparse-bitmap-dbm $MYTMPDIR/real-nodes.dbm)|uniq|store-sparse-bitmap-dbm $MYTMPDIR/used-nodes.dbm) > $MYTMPDIR/osm_as_sxml.scm) \
    |max-id > $MYTMPDIR/max-id.out

msg "B) store used nodes positions (depends on A)"
pv $MYTMPDIR/osm_as_sxml.scm|grep '^(node'|./used-nodes-pos.scm $MYTMPDIR/used-nodes.dbm|./store-node-pos.scm $MYTMPDIR/node-pos.dbm "$(cat $MYTMPDIR/max-id.out)"
# note: don't need used-nodes.dbm anymore
#rm -v $MYTMPDIR/used-nodes.dbm

msg "C) calculate way splits (depends on A, todo: could be merged with B)"
pv $MYTMPDIR/osm_as_sxml.scm|grep '^(way'|./parallel-pipe.scm $(cpus) read-line print-line read-line print ./way-splits.scm $MYTMPDIR/real-nodes.dbm |./store-way-splits.scm "$(cat $MYTMPDIR/max-id.out)" $MYTMPDIR/way-splits.dbm

msg "D) parse relations (todo: could be pipelined)"
pv $MYTMPDIR/osm_as_sxml.scm|grep '^(relation'|./relations.scm $MYTMPDIR/way-relation.dbm $MYTMPDIR/relation.dbm
} >&2

msg "E) drop unused nodes, apply way splits, profile ways, denormalize relations, fix way references in restriction relations, transform sxml to xml"
PSPLITS=2
if [ $(cpus) -gt 2 ]; then
    PSPLITS=$[$(cpus)/2]
fi
pv $MYTMPDIR/osm_as_sxml.scm| { ./parallel-pipe.scm $PSPLITS read-line print-line read-blob write-blob ./apply-way-splits.scm parallel-pipe $MYTMPDIR/way-splits.dbm $MYTMPDIR/node-pos.dbm $MYTMPDIR/way-relation.dbm $MYTMPDIR/relation.dbm|./fastsxml2xml.scm ; } 2> $MYTMPDIR/waysplit.log
# serial version
#pv $MYTMPDIR/osm_as_sxml.scm| { ./apply-way-splits.scm write-lines $MYTMPDIR/way-splits.dbm $MYTMPDIR/node-pos.dbm $MYTMPDIR/way-relation.dbm $MYTMPDIR/relation.dbm|./fastsxml2xml.scm ; } 2> $MYTMPDIR/waysplit.log
grep ' WARNING!\| INFO:' $MYTMPDIR/waysplit.log >&2

{
# remove temp files
rm -v $MYTMPDIR/real-nodes.dbm $MYTMPDIR/osm_as_sxml.scm $MYTMPDIR/max-id.out $MYTMPDIR/node-pos.dbm $MYTMPDIR/way-splits.dbm $MYTMPDIR/way-relation.dbm $MYTMPDIR/relation.dbm 
rm -v $MYTMPDIR/used-nodes.dbm
rm -v $MYTMPDIR/waysplit.log
rmdir $MYTMPDIR
} >&2
