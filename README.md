#eidolon

##Introduction

A image gallery service written in Haskell as a yesod webapp.

Visit the test instance at [eidolon.nek0.eu][eidolon]

##Installation

###Dependencies

####Build dependencies

A working Haskell capable environment. For that you will need `haskell-stack` and `cabal-install`, which you can install with:

```bash
sudo apt-get install haskell-stack cabal-install
```

Shouldn't stack be available through your package repositories you can get it [here][stack].

Now you can set up your stack with `stack setup` and follow its instructions. This will install the latest GHC Haskell compiler on your system.

update your package list with

```bash
cabal update
```

and upgrade `cabal-install`, if necessary with the command

```bash
cabal install cabal-install
```

Now you should add `~/.cabal/bin` to your `PATH` environment variable.

Additionally to Haskell and its dependencies you will need the following software and libraries:

* alex
* happy
* libmagick++-dev

which can be installed through

```bash
cabal install alex happy
sudo apt-get install libmagick++-dev
```

####Elasticsearch dependencies

Since version 0.0.5 there is an Elasticsearch integration for Eidolon. To Be able to run eidolon , you need to install `elasticsearch` additionally with:

```bash
sudo apt-get install elasticsearch
```
On how to configure your elasticsearch server, look into the [elasticsearch documentation][elasticdocu].

###Building

get the source with

```bash
git clone https://github.com/nek0/eidolon.git
```

It's generally a good idea to create a sandbox for compiling. To do so `cd` into the project directory and invoke

```bash
cabal sandbox init
```

If the sandbox generation fails, please make sure, you are using the latest version of `cabal istall`.

Then install all dependencies with the following command. This may lead you into dependency hell, as I am working with very new libraries. I am trying my best to avoid this.

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

Also check `config/settings.yml` and set the values there accrding to your configuration. Especially the settings for elasticsearch are vital.

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

###0.0.4-0.0.5

* run migration script from your run location (where your `static` folder with all the images is located)
	* if you are building in a sandbox run `runghc -package-db/full/path/to/sandbox/XXX-ghc-version-packages.conf.d /path/to/eidolon/Migrations/0.0.4-0.0.5/Migration.hs`
		* Note: No space between the option `-package-db` and its argument
	* without sandbox: `runghc /path/to/eidolon/Migrations/0.0.4-0.0.5/Migration.hs`
* start or restart your eidolon service

##Acknowledgements:

* This software uses the web Framework "Yesod" by Michael Snoyman. See more at: <http://www.yesodweb.com/>

* This software uses parts of the template "Parallelism" by n33co. see more at: <http://html5up.net/>

[eidolon]: http://eidolon.nek0.eu
[stack]: https://github.com/commercialhaskell/stack/releases
[elasticdocu]: https://www.elastic.co/guide/en/elasticsearch/reference/current/setup-configuration.html
