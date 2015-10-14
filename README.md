#eidolon

##Introduction

A image gallery service written in Haskell as a yesod webapp.

Visit the test instance at [eidolon.nek0.eu][eidolon]

##Installation

###Dependencies

A working Haskell capable environment. For that you will need `ghc` and `cabal-install`.

update your package list with `cabal update` and upgrade `cabal-install`, if necessary.

Additionally to Haskell and its dependencies you will need the following software and libraries:

* alex
* happy
* libmagick++-dev

```bash
sudo aptitude install libmagick++-dev
cabal install alex happy
```

###Building

get the source with

```bash
git clone https://github.com/nek0/eidolon.git
```

It's generally a good idea to create a sandbox for compiling. To do so `cd` into the project directory and invoke

```bash
cabal sandbox init
```

Then install all dependencies with the following command. This may lead you into dependency hell, as I am working with very new libraries.

```bash
cabal install --only-dependencies
```

After installing all dependencies you can configure and build the software with

```bash
cabal configure
cabal build
```

##Deploying

After compiling you will find an executable called `eidolon` in `dist/build/eidolon/`. Copy or link it anywhere you want. The executable needs to be accompanied by the folders `config` and `static` and their contents. It's best to copy them to your desired destination.

Also check `config/settings.yml` and set the values there accrding to your configuration.

It may also be necessery to create a reverse proxy to your gallery. I would recommend using [nginx](http://nginx.org/).

##Customizing

Unfortunately the gallery is not highly customizable, but you can change most of its appearance by changing the files `static/css/static-*.css`. Especially the default background image can be changed by replacing `static/css/img/bg.jpg`.

##Starting

You can start the gallery now by running the executable. You need to provide a settings file, normally found in `config/settings.yml`

Since eidolon will block your console, I recommend wrapping a init-script around it. how you can do that is written in my [blog](http://nek0.eu/posts/2014-10-23-Daemonize-a-yesod-app.html).

##Migrations

###0.0.3-0.0.4

* do not start or restart your eidolon service before migration
* run migration script from your run location (where your `static` folder with all the images is located)
	* if you are building in a sandbox run `runghc -package-db/full/path/to/sandbox/XXX-ghc-version-packages.conf.d /path/to/eidolon/Migrations/0.0.3-0.0.4/Migration.hs`
		* Note: No space between the option `-package-db` and its argument
	* without sandbox: `runghc /path/to/eidolon/Migrations/0.0.3-0.0.4/Migration.hs`
* start or restart your eidolon service

##Acknowledgements:

* This software uses the web Framework "Yesod" by Michael Snoyman. See more at: <http://www.yesodweb.com/>

* This software uses parts of the template "Parallelism" by n33co. see more at: <http://html5up.net/>

[eidolon]: http://eidolon.nek0.eu
