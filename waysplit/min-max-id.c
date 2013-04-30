/*BINFMTC: -Wall -I/usr/include/libxml2 -lxml2
 */

/*
  extract minimum and maximum id

  Copyright (c) 2013 Jens Thiele <karme@karme.de>

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.

  3. Neither the name of the authors nor the names of its contributors
     may be used to endorse or promote products derived from this
     software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdio.h>
#include <libxml/xmlreader.h>
#include <stdint.h>
#include <limits.h>
#include <assert.h>

typedef unsigned long long ID;
ID cmin=ULLONG_MAX;
ID cmax=0;

static void
processNode(xmlTextReaderPtr reader) {
  const xmlChar *name;

  name = xmlTextReaderConstName(reader);
  if (name == NULL)
    name = BAD_CAST "--";

  xmlChar *id=xmlTextReaderGetAttribute(reader,(const xmlChar *)"id");
  if ((id)&&(id[0]!=0)) {
    char *res;
    ID nid=strtoull((char *)id, &res, 10);
    assert(res[0]==0);
    if (nid<cmin) cmin=nid;
    if (nid>cmax) cmax=nid;
    xmlFree(id);
  }
}

static void
streamFile() {
  xmlTextReaderPtr reader;
  int ret;

  reader = xmlReaderForFd(0, NULL, NULL, 0);
  if (reader != NULL) {
    ret = xmlTextReaderRead(reader);
    while (ret == 1) {
      processNode(reader);
      ret = xmlTextReaderRead(reader);
    }
    xmlFreeTextReader(reader);
    if (ret != 0) {
      fprintf(stderr, "failed to parse\n");
    }
  } else {
    fprintf(stderr, "Unable to open\n");
  }
}

int main(int argc, char **argv) {
  LIBXML_TEST_VERSION

  streamFile();
  xmlCleanupParser();
  /*
   * this is to debug memory for regression tests
   */
  xmlMemoryDump();
  printf("%llu %llu\n", cmin, cmax);
  return(0);
}
