## LolChat ##
Erlang beadandó
![Chitchat screenshot](https://dl.dropboxusercontent.com/u/10389667/lolchat_screen.png "Logo Title Text 1")
## Letöltés ##
```
$ git clone git://github.com/akoskovacs/LolChat.git
```

## Az Erlang szerver ##

### Fordítás ###
```
$ cd server
$ make
```
### Indítás ###
```
$ make 
```

## C++/Qt kliens ##

### Letölthető binárisok
[Aktuális verzió](https://github.com/akoskovacs/LolChat/releases/tag/v0.1)

 * Windows x64 a szükséges `DLL`-ekkel `Qt 5.1.1`
 * Linux x64 `Qt 5.2.0` az osztott könyvtárak nélkül

### Fordítás ###
Legalább Qt 5.0 szükséges.
A `qmake`nek pedig a `PATH`-ben kell lennie.
```
$ cd client
$ ./build.sh
```

### Indítás ###
```
$ ./ChatClient`
```
