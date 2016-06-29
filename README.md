# 5chbot

A message broadcasting bot for [/r/5cardhearthstone](https://www.reddit.com/r/5cardhearthstone) subreddit.

## Building

First of all, you need to download `stack` from [haskellstack.org](http://docs.haskellstack.org/en/stable/README/).
Then, go to the root directory of the project and setup the environment and build the project:

    $ stack setup
    $ stack build

`stack` does everything for you: it downloads the needed version of GHC and downloads and builds all the dependencies.

## Runtime Dependenices

The project uses Python script as a wrapper for Google Spreadsheet API. You need Python 3 installed and [gspread](https://pypi.python.org/pypi/gspread) and [oauth2client](https://pypi.python.org/pypi/oauth2client) Python packages. You can install them using PiPy:

    $ pip3 install oauth2client
    $ pip3 install gspread

## Running

To run the project you need to copy the example config file and fill the login information there and mailing list spreadsheet ID there:

    $ cp config.json.example config.json

You also need to create a new project in [Google Developers Console](https://console.developers.google.com/project), enable Drive API for it. Then create a new service account key and download it as JSON file. Save it as "google.json" in the root directory of the project. Then you should share the mailing list spreadsheet with the service account email in the JSON file.

And then run the project:

    $ stack exec 5chbot
