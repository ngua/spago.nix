# shellcheck disable=SC2148
# NOTE
# This script is meant to be run with `nix run .#generate-package-sets`
# It is intentionally lacking a shebang -- this will be set by Nix. It's not
# written inline in the flake in order to avoid nasty escaping

expr='builtins.concatStringsSep " " (import ./lib/upstreams.nix)'
IFS=' ' read -r -a upstreams <<< "$(nix eval --raw --impure --expr "$expr")"

mkdir -p package-sets

len="${#upstreams[@]}"

for i in "${!upstreams[@]}"; do
    upstream="${upstreams[$i]}"
    path="package-sets/$upstream"
    idx=$((1 + i))
    if [[ ! -d "$path" ]]; then
        mkdir "$path"
        dhallpath="$path/packages.dhall"
        wget -qO- \
            "https://github.com/purescript/package-sets/releases/download/$upstream/packages.dhall" \
            >"$dhallpath"
        nixpath="$path/default.nix"
        echo -n "[$idx / $len] Creating $path..."
        pure-spago-nix generate "$dhallpath" >"$nixpath"
        echo " done"
    else
        echo "[$idx / $len] $upstream already exists, skipping ..."
    fi
done
