# metoffice-datahub-lib
Emacs package to query, display, and cache information from the [Metoffice Datahub service](https://www.metoffice.gov.uk/services/data/met-office-weather-datahub)

This is composed of two main packages `metoffice-datahub-lib` which handles the querying, caching, and parsing of data from the new (as of 2025) Metoffice Datahub API, this also offers a `metoffice-datahub-lib-forecast` which pops up a buffer of forecasted data from the most recent onwards.  This library queries the Metoffice's "Site-Specific forecast" service for a specific place.

The second is `metoffice-datahub-mode` which is a small package designed to put either some text or a couple of emoji in your modeline so that you can see from a glance what the predicted weather over the next increment is, and also mouse-over it for a more detailed description.


# metoffice-datahub-lib

The first thing you need to do for this is visit the [Datahub pages](https://datahub.metoffice.gov.uk/) to apply for an account, you won't need more than the free account as long as you're not going to try and run this on a hundred machines with the same API key and refresh them all at once.

Once you have the account then this explains inserting API keys from the "subscription" page into the auth: [API Documentation AUTHENTICATION](https://datahub.metoffice.gov.uk/docs/f/category/site-specific/type/site-specific/api-documentation#auth)

As of 2025-07 you get 360 free calls a day, by default this library will make a call to the three-hour weather forecast only if the cached data is over 30m old  (see `metoffice-datahub-lib-data-cache-maxage`) so you should have plenty of calls, as a long running emacs session will only use about 48 a day.

```
(use-package metoffice-datahub-lib
  :config
  (setq metoffice-datahub-lib-apikey "thisisareallylongstringokabcdef0123456789"
        metoffice-datahub-lib-long-lat '("x.y" "x.y"))

  ;; You can set this to "hourly", "three-hourly", or "daily" to get different types of forecast.
  ;; (setq metoffice-datahub-lib-query-type "three-hourly")

  (metoffice-datahub-refresh))
```

If you want to use it with `pass(1)` then something like this to insert your key from the command line: 

```
pass edit datahub.metoffice.gov.uk
```

Then something like this in your emacs config to get it back out

```
(setq metoffice-datahub-lib-apikey (auth-source-pick-first-password :host "datahub.metoffice.gov.uk"))
```

(before that works you'll need to do something like this to get that going:

```
(use-package auth-source
  :config
  (push 'password-store auth-sources)
  (setq auth-source-debug 'trivia))
```

Once you've gotten the authentication configured and the `metoffice-datahub-refresh` function has run you can find forecasted information in the following variables:

* `metoffice-datahub-lib-long-description` (a long description of the current weather)
* `metoffice-datahub-lib-description` (just the short textual description of the significant Weather Code)
* `metoffice-datahub-lib-symbol` (an emoji indicating the current weather code)
* `metoffice-datahub-lib-current-weather` (a hash-table of the most currently applying forecast data)
* `metoffice-datahub-lib-cached-response` (a parsed block of data from the JSON response)

The API returns JSON data, which is parsed into Emacs data.  Broadly the most interesting parts are its a hash table, which has a key called "features" which is a vector, the first element of which is the data we mostly want to work with as a hash table, that has a key called "properties" which is a hash table whic h has a key called "timeSeries" which is a vector of hash-tables of actual forecast data.

It sounds a lot but functions like `metoffice-datahub-lib--decode-three-hourly-info` should make it pretty clear whats going on.

There's quite a lot of customisable variables, all prefixed with `metoffice-datahub-lib-*` but the defaults should make sense, as long as your API key and lat/long are set then you should be ok.

If you want to hook something into this there is a `metoffice-datahub-lib-after-fetch-hook` function that runs after every fresh fetch of data, not just a re-read from cache.

You can also run `metoffice-datahub-lib-forecast` to get a `*forecast*` buffer describing all known future weather events, or change the `metoffice-datahub-lib-forecast-max-events` variable to limit it to the next N events, something like 3 or 5 is generally enough to get an idea without having a hugely noisy buffer.

If you want that to be more legible then try `metoffice-datahub-lib-forecast-table` to get a `*forecast*` buffer describing some features of the upcoming forecast formatted as an org-mode table.


# metoffice-datahub-mode

This is a smaller mode that puts a couple of small emoji into the modeline, offers a mouse-over of that to get a fuller forecast, and also runs timers to periodically refresh the forecast, which will either go from cache or from the API.

It inserts its modeline string into `mode-line-misc-info` and calls the `metoffice-datahub-lib-refresh` function every `metoffice-datahub-mode-data-update-in-seconds`, by default every hour.

```
(use-package metoffice-datahub-mode
 :after metoffice-datahub-lib
 :config
 (metoffice-datahub-mode 1))
```

If you have a terminal that doesn't like wide composed characters then you can either redefine the `metoffice-datahub-lib-significant-weather-codes-to-symbols` variable or this may help:

```
  (unless window-system
    (setq metoffice-datahub-mode-use-symbol nil
          metoffice-datahub-mode-indicate-temp t
          metoffice-datahub-mode-temp-symbols '(" Cold" "" " Hot")))
```

Or alternatively if you want temperature numbers in your modeline use:

```
(setq metoffice-datahub-mode-temp-symbols
        '(" TEMPERATURE" " TEMPERATURE" " TEMPERATURE"))
```


# Screenshots

Of course

Full mouse-over forecast: 
![Image showing a rainy face emoji and a catface emoji with a description of the rain in a long popup from the modeline](https://github.com/twitchy-ears/metoffice-datahub-lib/blob/assets/metoffice-datahub-lib-mouseover.png)

Just the emojis, notice I have customised the `metoffice-datahub-mode-temp-symbols` variable to make the OK temperature a catface: 
![Image showing a rainy face emoji and a catface emoji](https://github.com/twitchy-ears/metoffice-datahub-lib/blob/assets/metoffice-datahub-lib-just-emoji.png)

Showing the pure text config, its Heavy rain at the minute and feels like 13 degrees C
![Image showing the text "Heavy rain 13"](https://github.com/twitchy-ears/metoffice-datahub-lib/blob/assets/metoffice-datahub-lib-just-text.png)

Showing the `metoffice-datahub-lib-forecast-table` function in its default output
![Image showing a table with the headings time, temp, rain, snow, type - then data for 15:00 onwards on a day](https://github.com/twitchy-ears/metoffice-datahub-lib/blob/assets/metoffice-datahub-lib-forecast-table.png)



# Bugs

Almost certainly some exciting parsing failures somewhere.

If you want to investigate the response dumps then they're horrible to attempt to pick through, you probably want something like [describe-hash](https://github.com/Junker/describe-hash) or to make something similar yourself.
