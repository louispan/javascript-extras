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
