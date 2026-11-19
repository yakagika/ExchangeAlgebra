#!/usr/bin/env bash
# Compile and run the manual examples against the pinned Hackage package.
set -euo pipefail
repo_root=$(cd "$(dirname "$0")/.." && pwd)
cd "$repo_root"
build_root="$repo_root/site/book/code-build"
mkdir -p "$build_root"

# Fail before compiling if the selected environment resolves another release.
package_version=$(stack --stack-yaml examples/stack.yaml exec --package exchangealgebra -- \
    ghc-pkg field exchangealgebra version --simple-output)
if [[ "$package_version" != "0.6.0.1" ]]; then
    echo "Expected exchangealgebra 0.6.0.1, got: $package_version" >&2
    exit 1
fi

count=0
for source in site/code/*.hs; do
    name=$(basename "$source" .hs)
    output_dir="$build_root/$name"
    mkdir -p "$output_dir"
    stack --stack-yaml examples/stack.yaml exec --package exchangealgebra -- \
        ghc -Wall -Werror -fforce-recomp -hide-all-packages \
        -package base -package exchangealgebra \
        -outputdir "$output_dir" -o "$output_dir/program" "$source"
    (cd site/code && "$output_dir/program") > "$output_dir/actual"
    diff -u "site/code/$name.expected" "$output_dir/actual"
    echo "PASS $name (compile and output)"
    count=$((count + 1))
done
echo "PASS $count programs against exchangealgebra-$package_version"
