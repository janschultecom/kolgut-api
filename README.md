# kolgut-api

## Requirements

Idris has to be installed using the `ffi`. See https://www.idris-lang.org/download/ for more information.

You need [lightyear](https://github.com/ziman/lightyear) installed:
```bash
git clone https://github.com/ziman/lightyear.git
cd lightyear 
make
```

Kolgut currently uses a Rust http backend. You can install Rust from the official page: https://www.rust-lang.org

Now build the rust backend:
```bash
cd Rust
cargo build
```

### Running
```bash
# Typecheck
idris -p lightyear -p effects -p contrib Main.idr

# Run
:exec
```
Then open a browser on `http://localhost:3000`

## Building
```bash
idris --build kolgut.ipkg
```
