# shellcheck disable=SC2148
# NOTE
# This script is meant to be run with `nix run .#generate-package-sets`
# It is intentionally lacking a shebang -- this will be set by Nix. It's not
# written inline in the flake in order to avoid nasty escaping

# TODO
# Enable the rest of these after more testing
declare upstreams=(
    psc-0.15.4-20220808
    psc-0.15.4-20220805
    psc-0.15.4-20220725
    psc-0.15.4-20220723
    psc-0.15.4-20220722
    psc-0.15.4-20220718
    psc-0.15.4
    psc-0.15.3-20220712
    psc-0.15.3
    psc-0.15.2-20220706
    psc-0.15.2-20220630
    psc-0.15.2-20220624
    psc-0.15.2-20220621
    psc-0.15.2-20220620
    psc-0.15.2-20220615
    psc-0.15.2-20220613
    psc-0.15.2-20220612
    psc-0.15.2-20220611
    psc-0.15.2-20220610
    psc-0.15.2-20220609
    psc-0.15.2-20220531
    psc-0.15.2-20220530
    psc-0.15.2
    psc-0.15.0-20220527
    psc-0.15.0-20220526
    psc-0.15.0-20220525
    psc-0.15.0-20220523
    psc-0.15.0-20220522
    psc-0.15.0-20220516
    psc-0.15.0-20220515
    psc-0.15.0-20220513
    psc-0.15.0-20220510
    psc-0.15.0-20220509
    psc-0.15.0-20220507
    psc-0.15.0-20220506
    psc-0.15.0-20220505
    psc-0.15.0-20220504
    psc-0.15.0-20220503
    psc-0.15.0-20220502
    psc-0.15.0-20220429
    psc-0.15.0-20220428
    psc-0.15.0
    psc-0.14.7-20220418
    psc-0.14.7-20220404
    psc-0.14.7-20220321
    psc-0.14.7-20220320
    psc-0.14.7-20220315
    psc-0.14.7-20220303
    psc-0.14.7-20220228
    psc-0.14.7
    psc-0.14.6-20220228
    psc-0.14.6
    psc-0.14.5-20220224
    psc-0.14.5-20220216
    psc-0.14.5-20220203
    psc-0.14.5-20220202
    psc-0.14.5-20220201
    psc-0.14.5-20220127
    psc-0.14.5-20220110
    psc-0.14.5-20220103
    psc-0.14.5-20220102
    psc-0.14.5-20211116
    psc-0.14.5-20211111
    psc-0.14.5
    psc-0.14.4-20211109
    psc-0.14.4-20211030
    psc-0.14.4-20211028
    psc-0.14.4-20211026
    psc-0.14.4-20211005
    psc-0.14.4-20210919
    psc-0.14.4-20210905
    psc-0.14.4-20210826
    psc-0.14.4
    psc-0.14.3-20210825
    psc-0.14.3-20210823
    psc-0.14.3-20210811
    psc-0.14.3-20210808
    psc-0.14.3-20210722
    psc-0.14.3-20210716
    psc-0.14.3
    psc-0.14.2-20210713
    psc-0.14.2-20210629
    psc-0.14.2-20210623
    psc-0.14.2-20210622
    psc-0.14.2-20210613
    psc-0.14.2
    psc-0.14.1-20210613
    psc-0.14.1-20210516
    psc-0.14.1-20210506
    psc-0.14.1-20210429
    psc-0.14.1-20210427
    psc-0.14.1-20210419
    psc-0.14.0-20210409
    psc-0.14.0-20210406
    psc-0.14.0-20210405
    psc-0.14.0-20210402
    psc-0.14.0-20210401
    psc-0.14.0-20210331
    psc-0.14.0-20210329
    psc-0.14.0-20210324
    psc-0.14.0-20210318
    psc-0.14.0-20210317
    psc-0.14.0-20210315
    psc-0.14.0-20210313
    psc-0.14.0-20210311
    psc-0.14.0-20210309
    psc-0.14.0-20210308
    psc-0.14.0-20210307
    psc-0.14.0-20210304
    psc-0.14.0-20210302
    psc-0.14.0
)

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
        echo "[$idx / $len] $path already exists, skipping ..."
    fi
done
