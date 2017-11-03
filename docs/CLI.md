## Jelly VM CLI

`jellyvm` is a minimal command-line runner for Jelly bytecode modules.

### Exit codes

- `0`: executed successfully
- `1`: VM trapped (runtime error); prints `trap: code=... msg=...`
- `2`: usage / IO error / loader error

### Output

For now the CLI prints the entry return value in a minimal way:

- `null`, `true`/`false`, `i32` are printed as text
- atoms print as `atom(<id>)`
- pointers print as `ptr(<hex>)`

