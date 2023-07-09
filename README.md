# Hoot
Opinionated haskell package builder (based on cabal)

### Create a new project
```sh
hoot new hello

cd hello
hoot run
```

### Add dependencies
```sh
hoot add QuickCheck
# Added QuickCheck v2.14.3
```

### Package manifest
Package manifests are stored in `Hoot.toml`
```toml
[package]
name = "example"

[dependencies]
quickcheck = "v2.14.3"
```

