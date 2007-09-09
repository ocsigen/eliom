Miniwiki
---------

Miniwiki is a simple wiki written for Ocsigen by Janne Hellsten
(jjhellst@gmail.com).  It's primary purpose is to server as a code
example for Ocsigen module developers.

Compiling & running
-------------------

Miniwiki should be compiled and install with Ocsigen.

The default config file usually contains miniwiki preconfigured.

If not, adapt files/miniwiki.conf and start Ocsigen with Miniwiki by running

   ocsigen -c examples/miniwiki/files/miniwiki.conf

4. Point your browser to localhost:9999/

Implementation
--------------

Wiki page storage

Each wiki page is a file in the wiki storage directory
(/var/lib/ocsigen/miniwiki/wikidata/ is the default).  A wiki page "Foo"
corresponds to a file called "Foo.wiki" in the "wikidata" directory.

Character encoding

Wiki pages are stored as UTF-8 text files.  If the site is properly
configured, the wiki properly allows the use of UTF-8 content in wiki
pages.  Since wiki page names map directly to filenames on disk, page
names containing non 7-bit ASCII might not work well.
