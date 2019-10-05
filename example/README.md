# Preconditions
install [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
install [node](https://nodejs.org)
install [ghcjs](https://github.com/ghcjs/ghcjs)

# build
```
cd test
npm install
./build.sh
```

# run dev
```
npm start
```

# prod depedencies
```
npm install -g http-server
```

# run prod
```
npm run build
http-server dist -c-1 # -c-1 disables caching if debugging
```
