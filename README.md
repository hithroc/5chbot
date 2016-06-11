# 5chbot

A message broadcasting bot for [/r/5cardhearthstone](https://www.reddit.com/r/5cardhearthstone) subreddit.

## Building

First of all, you need to download `stack` from [haskellstack.org](http://docs.haskellstack.org/en/stable/README/).
Then, go to the root directory of the project and setup the environment and build the project:

    $ stack setup
    $ stack build

`stack` does everything for you: it downloads the needed version of GHC and downloads and builds all the dependencies.

## Running

To run the project you need to copy the example config file and fill the login information there:

    $ cp config.json.example config.json

And run the project:

    $ stack exec 5chbot
