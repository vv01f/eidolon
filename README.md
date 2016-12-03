#eidolon

##Introduction

A image gallery service written in Haskell as a yesod webapp.

Visit the test instance at [eidolon.nek0.eu][eidolon]

##Installation

###Dependencies

####Build dependencies

A working Haskell capable environment. For that you will need `cabal-install`,
which you can install with:

```bash
sudo apt-get install cabal-install
```

Now you can set up your cabal with `cabal update` and follow its instructions.
with this you will have also installed the latest GHC Haskell compiler on your
system.

Additionally to Haskell and its dependencies you will need the following
software and libraries:

* alex
* happy
* libpq-dev
* postgresql
* libfftw3-dev

which can be installed through

```bash
sudo apt-get install alex happy libpq-dev postgresql
```

for migrations also install:

* libmagic-dev

####Elasticsearch dependencies

**WARNING: THIS SECTION IS ONLY VALID FOR VERSIONS >= 0.0.5 and < 0.1.0.0**

Since version 0.0.5 there is an Elasticsearch integration for Eidolon. To Be
able to run eidolon , you need to install `elasticsearch` additionally with:

```bash
sudo apt-get install elasticsearch
```
On how to configure your elasticsearch server, look into the
[elasticsearch documentation][elasticdocu].

###Building

get the source with

```bash
git clone https://github.com/nek0/eidolon.git
```

build your sandbox with

```bash
cabal sandbox init
```

build the source with

```bash
cabal build
```

Everything should work automagically from there.

##Deploying

After compiling you will find an executable called `eidolon` in
`dist/build/eidolon/eidolon`. Copy or link it anywhere you
want. The executable needs to be accompanied by the folders `config` and
`static` and their contents. It's best to copy them to your desired destination.

Also check `config/settings.yml` and set the values there accrding to your
configuration. The field `contactEmail` is optional, you can comment it out, but
be aware. In certain jurisdictions you are required to give some contact
information.

It may also be necessery to create a reverse proxy to your gallery. I would
recommend using [nginx](http://nginx.org/).

##Customizing

Unfortunately the gallery is not highly customizable, but you can change most of
its appearance by changing the files `static/css/main.css`. Especially the
default background image can be changed by replacing `static/css/img/bg.jpg`.

##Starting

You can start the gallery now by running the executable. You need to provide a
settings file as argument, normally found in `config/settings.yml`

Since eidolon will block your console, I recommend wrapping a init-script around
it. how you can do that is written in my
[blog](http://nek0.eu/posts/2014-10-23-Daemonize-a-yesod-app.html).

##Migrations

###0.0.3-0.0.4

* do not start or restart your eidolon service before migration
* run migration script from your run location (where your `static` folder with
all the images is located)
	* if you are building in a sandbox run
`runghc -package-db/full/path/to/sandbox/XXX-ghc-version-packages.conf.d
/path/to/eidolon/Migrations/0.0.3-0.0.4/Migration.hs`
		* Note: No space between the option `-package-db` and its argument
	* without sandbox: `runghc /path/to/eidolon/Migrations/0.0.3-0.0.4/Migration.hs`
* start or restart your eidolon service

###0.0.4-0.0.5

* run migration script from your run location (where your `static` folder with
all the images is located)
	* if you are building in a sandbox run `runghc
-package-db/full/path/to/sandbox/XXX-ghc-version-packages.conf.d
/path/to/eidolon/Migrations/0.0.4-0.0.5/Migration.hs`
		* Note: No space between the option `-package-db` and its argument
	* without sandbox: `runghc /path/to/eidolon/Migrations/0.0.4-0.0.5/Migration.hs`
* start or restart your eidolon service

###0.0.7-0.1.0.0

You have two options to accomplish the migration:
1. Build the migration binary with `cabal exec -- ghc --make
Migrations/0.0.7-0.1.0.0/Migration.hs` and run the executable in your run
location.
2. Link the `static/data/` folder from your link location in the same location
in the project directory and run `cabal exec -- runghc
Migrations/0.0.7-0.1.0.0/Migration.hs`

###0.1.2.4-0.1.3.0
* Stop Eidolon
* Log into your database and issue this command:
`ALTER TABLE "comment" DROP COLUMN "author_slug";`
* Start Eidolon

###0.1.3.3-0.1.4.0
* Stop eidolon
* Compile migration script with `cabal exec -- ghc --make Migrations/0.1.3.3-0.1.4.0/Migration.hs`
* Copy or move compiled script to run location and execute
* Follow the prompts of the script
* Start eidolon

###0.1.4.0-0.1.5.0
* Stop eidolon
* Compile migration script with `cabal exec -- ghc --make Migrations/0.1.4.0-0.1.5.0/Migration.hs`
* Copy or move compiled script to run location and execute
* Follow the prompts of the script
* Start eidolon

##Acknowledgements:

* This software uses the web Framework "Yesod" by Michael Snoyman. See more at:
<http://www.yesodweb.com/>

* This software uses parts of the template "Parallelism" by n33co. see more at: <http://html5up.net/>

[eidolon]: http://eidolon.nek0.eu
[stack]: https://github.com/commercialhaskell/stack/releases
[elasticdocu]: https://www.elastic.co/guide/en/elasticsearch/reference/current/setup-configuration.html
