#!/usr/bin/env bash

set -e

# Turn "foo/project_name" into "foo!PROJECT_NAME".
encode_fname ()
{
  local f=${1//\//!}
  echo ${f/project_name/PROJECT_NAME}
}

# Replace markers inside file names
subst_file ()
{
  sed \
    -e "s/project_name/%%%PROJECT_NAME%%%/g" \
    -e "s/Project_name/%%%MODULE_NAME%%%/g"
}

# encode_dir input output
encode_dir ()
{
  local src=$1 dest=$2
  mkdir -p "$dest"
  git -C "$src" ls-files | while read src_f; do
    if ! [[ $src_f = rename ]]; then
      subst_file < "$src/$src_f" > "$dest/$(encode_fname "$src_f")"
    fi
  done
}

update_template ()
{
  local name=$1 url=$2
  local tmpdir=$(mktemp -d) template_dir=pkg/distillery/templates/$name
  git clone "$url" "$tmpdir"
  rm -r "$template_dir"
  encode_dir "$tmpdir" "$template_dir"
  rm -rf "$tmpdir"
}

update_template "app.exe" "https://github.com/ocsigen/eliom_template_exe"
