<h1 align="center">
    <div>
        <img alt="postmap" width="320" src="https://github.com/vst/postmap/assets/374793/4dea3f1f-f192-4335-ab15-a0e9b34cdd8e" />
    </div>
    <sub>PostgreSQL Database Introspection, Mapping and Code Generation Tool</sub>
    <p></p>
    <div>
        <img alt="GitHub Release" src="https://img.shields.io/github/v/release/vst/postmap?display_name=tag&style=for-the-badge">
        <img alt="GitHub Issues or Pull Requests" src="https://img.shields.io/github/issues/vst/postmap?style=for-the-badge">
        <img alt="GitHub Issues or Pull Requests" src="https://img.shields.io/github/issues-pr/vst/postmap?style=for-the-badge">
    </div>
</h1>

> [!WARNING]
>
> This is an experimental project that is in its early stages of
> development. Both the functionality and API are subject to change at
> any time.
>
> It is not recommended to use this in production or any other
> critical environment. Use at your own risk.

## Development

Provision Nix shell via `direnv`:

```sh
direnv allow
```

Big, long build command for the impatient:

```sh
hpack &&
    direnv reload &&
    fourmolu -i app/ src/ test/ &&
    prettier --write . &&
    find . -iname "*.nix" -not -path "*/nix/sources.nix" -print0 | xargs --null nixpkgs-fmt &&
    hlint app/ src/ test/ &&
    cabal build -O0 &&
    cabal run -O0 postmap -- --version &&
    cabal v1-test &&
    cabal haddock -O0
```

Use following command to lint, build, test and haddock the codebase:

```sh
dev-test-build
```

## License

Copyright &copy; 2024 Vehbi Sinan Tunalioglu. This work is licensed
under [MIT License].

<!-- REFERENCES -->

[MIT License]: https://opensource.org/license/mit
