default:
    @just --list

clean *args:
    dune clean

build *args:
    env OCAMLRUNPARAM=b dune build @ocaml-index {{args}}

run *args:
    dune exec sabacc {{args}}

test *args:
    dune test {{args}}
