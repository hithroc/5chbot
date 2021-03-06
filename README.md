# 5chbot

A message broadcasting bot for [/r/5cardhearthstone](https://www.reddit.com/r/5cardhearthstone) subreddit.

## Building

First of all, you need to download `stack` from [haskellstack.org](http://docs.haskellstack.org/en/stable/README/).
Then, go to the root directory of the project and setup the environment and
build the project:

    $ stack setup
    $ stack build

`stack` does everything for you: it downloads the needed version of GHC and
downloads and builds all the dependencies.

## Runtime Dependenices

The project uses a python script as a wrapper for Google Spreadsheet API. You
need Python 3+, [gspread](https://pypi.python.org/pypi/gspread)
and [oauth2client](https://pypi.python.org/pypi/oauth2client) Python packages.
You can install them using PyPy:

    $ pip3 install oauth2client
    $ pip3 install gspread

## Running

To run the bot you need to copy the example config file, fill the login
information and mailing list spreadsheet ID there:

    $ cp config.json.example config.json

You also need to create a new project in [Google Developers Console](https://console.developers.google.com/project),
enable Drive API for it. Then create a new service account key and download it
as JSON file. Save it as "google.json" in the root directory of the bot.
Then you should share the mailing list spreadsheet with the service account
email in the JSON file.

Now you can just run it:

    $ stack exec 5chbot

Or look at the list of available command-line arguments

    $ stack exec 5chbot -- -h

Keep in mind, that all command-line should go after `stack exec 5chbot --`.

## Using the bot

The bot is mostly used by sending a reddit PM to it with the command
you want to execute. All commands start with `!` and followed with the name of
the command. For example `!unsubscribe`.

Here's a list of all the available commands. Commands that are marked with `[M]`
can only be executed by a moderator (moderator users are specified by config
file). If someone tries to execute such command, and they're not a moderator,
the command will do nothing, and [the incident will be reported](https://xkcd.com/838/).

### [M] Broadcast

This command fetches the Google Spreadsheet with all the users, that are subscribed
to recieve broadcast messages. Then it sends a private message to everyone in
the list. The title of the broadcasting message will be the title of your
message with the request, and the body of the message will be everything you put
after `!broadcast` in you request message. For example:

Message title: `Hello world!`

Message body:

    !broadcast Hiya

Broadcast title: `Hello world!`

Broadcast body:

    Hiya

If something bad happens and the broadcast fails (for example, the bot failed
to fetch mailing list for some reason), the error message will be sent as a
reply to your request.

### Echo

Echo command is very simple. It's just replies to you with the message you've
sent. For example:

Message title: `Echoes!`

Message body:

    !echo Wooooo!

Reply body:

    Wooooo!

This command will be probably be removed at some point, it's main purpose is
to check if the bot is working or not.

### Version

Replies with the current version of the bot. For example:

Message title: `Any title`
Message body:

    !version

Reply body:

    5chbot ver 1.0.1

### Unsubscribe

Unsubscribes you from the mailing list.

Message title: `Any title`
Message body:

    !unsubscribe

Reply body if the operation succeedes:

    You have been unsubscribed!

Reply body if something bad happens:

    Failed to unsubscribe. Try again later or contact subreddit moderators!
