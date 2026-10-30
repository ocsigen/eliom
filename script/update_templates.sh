#!/usr/bin/env bash

# Regenerate the distillery template app from the eliom_template_exe
# repository.
#
#   script/update_templates.sh [source]
#
# source is a git URL or a local clone; it defaults to the
# eliom_template_exe repository on GitHub. A local clone gives the
# committed state of its current branch. The files tracked by git are
# copied with the layout eliom-distillery expects: "/" becomes "!" in
# file names, project_name becomes PROJECT_NAME in file names and
# %%%PROJECT_NAME%%% in file contents, Project_name becomes
# %%%MODULE_NAME%%%. The files listed in .eliomverbatim are copied
# unchanged.

set -eo pipefail

source=${1:-https://github.com/ocsigen/eliom_template_exe}
# Resolve a local path before leaving the current directory
if [[ -d $source ]]; then source=$(cd "$source" && pwd); fi

cd "$(dirname "$0")/.."

# Turn "foo/project_name" into "foo!PROJECT_NAME".
encode_fname ()
{
  local f=${1//\//!}
  printf '%s\n' "${f//project_name/PROJECT_NAME}"
}

# Replace markers inside file contents
subst_file ()
{
  LC_ALL=C sed \
    -e "s/project_name/%%%PROJECT_NAME%%%/g" \
    -e "s/Project_name/%%%MODULE_NAME%%%/g"
}

# encode_dir input output
encode_dir ()
{
  local src=$1 dest=$2 src_f dest_f
  # The source is a throwaway clone: an empty list if it has none
  touch "$src/.eliomverbatim"
  mkdir "$dest"
  git -C "$src" ls-files -z | while IFS= read -r -d '' src_f; do
    dest_f=$(encode_fname "$src_f")
    if grep -qxF -- "$dest_f" "$src/.eliomverbatim"; then
      cp "$src/$src_f" "$dest/$dest_f"
    else
      subst_file < "$src/$src_f" > "$dest/$dest_f"
    fi
    if [[ -x $src/$src_f ]]; then chmod +x "$dest/$dest_f"; fi
  done
}

tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT
git clone "$source" "$tmpdir/src"
encode_dir "$tmpdir/src" "$tmpdir/out"
rm -r pkg/distillery/templates/app
mv "$tmpdir/out" pkg/distillery/templates/app
