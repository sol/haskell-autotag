# A GitHub action that creates git tags on `.cabal` version bumps

This can be used to automate the release process of a Haskell package.

## Setup

To use this action to automate the release process of your Haskell package you
will need to:

1. Generate a Hackage Auth Token
2. Add the generated Hackage Auth Token to your GitHub repository
3. Create a GitHub Actions workflow

By following these three steps you will end up with a GitHub Actions workflow
that automates releases.

Specifically, whenever the workflow detects a new version without a
corresponding git tag it:

 - creates a git tag
 - publishes your package to [Hackage](https://hackage.haskell.org)

### Step 1: Generate a Hackage Auth Token
1. Go to https://hackage.haskell.org/users/account-management
2. Sign in with your Hackage credentials
3. Generate a new token under **Authentication Tokens** -> **Register new token**

### Step 2: Add the generated Hackage Auth Token to your GitHub repository
1. Go to **Settings** -> **Secrets and variables** -> **Actions** -> **Repository secrets** -> **New repository secret**
2. Use`HACKAGE_AUTH_TOKEN` as **Name** and the Auth token value as **Secret**
3. Click **Add secret**

### Step 3: Create a GitHub Actions workflow
```yaml
# file: .github/workflows/publish.yml
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
