```yaml
name: publish

permissions:
  contents: write

on:
  push:
    branches:
      - main

jobs:
  publish:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - uses: sol/haskell-autotag@v1
        id: autotag

      - run: cabal sdist

      - uses: haskell-actions/hackage-publish@v1.1
        with:
          hackageToken: ${{ secrets.HACKAGE_AUTH_TOKEN }}
          publish: true
        if: steps.autotag.outputs.created
```
