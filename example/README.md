# Preconditions

## stack
install [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
Then setup ghc
```
stack setup
```

## node
install [node](https://nodejs.org)

## ghcjs
install [ghcjs](https://github.com/louis/ghcjs)

```
git clone git@github.com:louispan/ghcjs.git # this fork contains install fixes
cd ghcjs
git submodule update --init --recursive
```

Create a stack.yaml without dependencies based on original ghcjs/stack.yaml
This allows usages of `stack exec` to bring ghc into scope.
```
grep '^\s*resolver:' stack.yaml > ghcjs.yaml
```

./utils/makePackages.sh requirements
```
stack --stack-yaml=ghcjs.yaml install cabal-install happy alex
cabal v1-update
```

Now you can run `./utils/makePackages.sh`
```
stack --stack-yaml=ghcjs.yaml exec --no-ghc-package-path ./utils/makePackages.sh
```

Compile ghcjs. This will take a long while and copy bins to `~/.local/bin/`
```
stack install
```

Finalize ghcjs so it can compile regular haskell and packages. This will also take some time.
```
stack --stack-yaml=ghcjs.yaml exec ghcjs-boot
```

# build

```
git clone git@github.com:louispan/javascript-extras.git
cd javascript-extras/example
./build.sh
```

# Dev

Run webpack-dev-server. See `package.json:scripts.start`
```
npm start
```

# Prod
Create static website. See `package.json:scripts.build`
```
npm run build
```

You can use http-server to serve:
```
npm install -g http-server
http-server dist -c-1 # -c-1 disables caching if debugging
```
