# jippity

A simple CLI wrapper for GPT.

## (Rough) Installation steps

0. Ensure that you have [Opam](https://opam.ocaml.org/), [OCaml v5+](https://ocaml.org/docs/installing-ocaml), and [Dune](https://dune.build/) installed.
1. Clone the repo.
2. Install the necessary packages using `opam`.
3. Run `dune build`.
4. Run `install.sh` (only works on Unix).
5. Start using `jippity`!

## Usage

```sh
$ jippity [-c] [-s "<system_message>"] "<my_prompt_that_i_urgently_hope_my_ai_overlords_can_answer_satisfactorily>"
```
