#!/bin/sh
# Generate wiki API documentation using ocamldoc + wikidoc plugin.
# Replaces the old ocamlbuild-based doc generation.
# Usage: ./build/gen_wikidoc.sh [server|client|all]

set -e

WIKIDOC_DIR=$(ocamlfind query wikidoc)

gen_doc() {
  SIDE="$1"

  case "$SIDE" in
    server)
      CMI_DIR=_build/default/src/lib/server/.eliom_server.objs/byte
      MLI_DIR=_build/default/src/lib/server
      DUNE_DIR=src/lib/server
      ODOCL=src/lib/server/api.odocl
      INTRO=doc/server.indexdoc
      SUBPROJECT=server
      ;;
    client)
      CMI_DIR=_build/default/src/lib/client/.eliom_client.objs/byte
      MLI_DIR=_build/default/src/lib/client
      DUNE_DIR=src/lib/client
      ODOCL=src/lib/client/api.odocl
      INTRO=doc/client.indexdoc
      SUBPROJECT=client
      ;;
    *)
      echo "Unknown side: $SIDE"; exit 1
      ;;
  esac

  # Ensure dune build is up to date
  dune build @check 2>/dev/null || true

  # Get include paths from dune's merlin config
  INCLUDES=$(dune ocaml dump-dot-merlin "$DUNE_DIR" 2>/dev/null \
    | grep "^B " | sed 's/^B /-I /' | tr '\n' ' ')
  # Add js_of_ocaml (needed for type references in .mli files)
  JSOO=$(ocamlfind query js_of_ocaml 2>/dev/null || true)
  [ -n "$JSOO" ] && INCLUDES="$INCLUDES -I $JSOO"

  # Create temp directory with short-name .cmi aliases
  TMPDIR=$(mktemp -d)

  echo "Creating module aliases for ocamldoc..."
  for cmi in "$CMI_DIR"/eliom__*.cmi; do
    base=$(basename "$cmi" .cmi)                   # eliom__Bus
    short=$(echo "$base" | sed 's/^eliom__//')     # Bus
    # Module name: capitalize first letter (eliom__Bus -> Eliom__Bus)
    modname=$(echo "${base}" | sed 's/^./\U&/')
    echo "include ${modname}" > "$TMPDIR/${short}.ml"
  done
  # Compile in multiple passes to handle inter-dependencies
  for pass in 1 2 3; do
    compiled=0
    for ml in "$TMPDIR"/*.ml; do
      short=$(basename "$ml" .ml)
      if [ ! -f "$TMPDIR/${short}.cmi" ]; then
        if eval ocamlfind ocamlc -c -I "$CMI_DIR" -I "$TMPDIR" $INCLUDES \
          -o "$TMPDIR/${short}.cmo" "$ml" 2>/dev/null; then
          compiled=$((compiled + 1))
        fi
      fi
    done
    echo "  pass $pass: compiled $compiled new modules"
    [ "$compiled" -eq 0 ] && break
  done
  echo "  total: $(ls "$TMPDIR"/*.cmi 2>/dev/null | wc -l) cmi files"

  # Collect .mli files from odocl
  MLIFILES=""
  while IFS= read -r mod; do
    mod=$(echo "$mod" | tr -d '[:space:]')
    [ -z "$mod" ] && continue
    mli="$MLI_DIR/$mod.mli"
    if [ -f "$mli" ]; then
      MLIFILES="$MLIFILES $mli"
    else
      echo "  Warning: $mli not found, skipping"
    fi
  done < "$ODOCL"

  # Output directory
  OUTDIR=_build/doc/dev/api/$SUBPROJECT
  rm -rf "$OUTDIR"
  mkdir -p "$OUTDIR"

  echo "Generating $SIDE wiki documentation..."
  eval ocamldoc \
    -colorize-code \
    -I "$TMPDIR" \
    -I "$CMI_DIR" \
    $INCLUDES \
    -intro "$INTRO" \
    -g "$WIKIDOC_DIR/odoc_wiki.cma" \
    -d "$OUTDIR" \
    -subproject "$SUBPROJECT" \
    $MLIFILES

  NFILES=$(ls "$OUTDIR"/*.wiki 2>/dev/null | grep -cv "^.*index\|menu\|type_" || true)
  echo "Done: $OUTDIR/"

  rm -rf "$TMPDIR"
}

case "${1:-all}" in
  server|client) gen_doc "$1" ;;
  all) gen_doc server; gen_doc client ;;
  *) echo "Usage: $0 [server|client|all]"; exit 1 ;;
esac
