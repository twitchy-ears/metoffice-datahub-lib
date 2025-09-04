;;; metoffice-datahub-lib.el --- Queries weather forecasts from the UK Metoffice Datahub API -*- lexical-binding: t -*-

;; Copyright 2025 - Twitchy Ears

;; Author: Twitchy Ears https://github.com/twitchy-ears/
;; URL: https://github.com/twitchy-ears/metoffice-datahub-lib
;; Version: 0.1
;; Package-Requires ((emacs "30.1") cl-lib url json parse-time)
;; Keywords: weather api

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; History
;;
;; 2025-07-15 Initial version.

;;; Commentary:
;;
;; This specifically is designed around using the Metoffice Datahub
;; site-specific point API.
;;
;; You will need to visit https://datahub.metoffice.gov.uk/ to apply
;; for an account
;;
;; Then this explains inserting API keys from the "subscription" page
;; into the auth:
;; 
;; https://datahub.metoffice.gov.uk/docs/f/category/site-specific/type/site-specific/api-documentation#auth
;;
;; As of 2025-07 you get 360 free calls a day, by default this library
;; will make a call to the three-hour weather forecast only if the
;; cached data is over 30m old so you should have plenty of calls (as
;; a long running Emacs session will only use 48 a day)
;;
;; Informative documentation lingers around here:
;; https://www.metoffice.gov.uk/services/data/met-office-weather-datahub
;;
;; (use-package metoffice-datahub-lib
;;   :config
;;   (setq metoffice-datahub-lib-apikey "thisisareallylongstringokabcdef0123456789"
;;         metoffice-datahub-lib-long-lat '("x.y" "x.y"))
;; 
;;   (metoffice-datahub-refresh))
;;
;; After it runs you should find interesting smaller and more
;; managable data subsets in these variables:
;;
;; metoffice-datahub-lib-long-description
;; metoffice-datahub-lib-description
;; metoffice-datahub-lib-current-weather
;;
;; And the full API response in this variable:
;; metoffice-datahub-lib-cached-response
;;
;; The metoffice-datahub-lib--decode-info function will help you decode that.
;;
;; There is a metoffice-datahub-lib-after-fetch-hook hook that runs
;; after every time the weather is fetched from the API (and not
;; pulled from cache) for if you want to update your own things off
;; the back of that.
;;
;; If you want to see a fuller forecast then try the
;; metoffice-datahub-lib-forecast function which will create one in a
;; popup buffer.
;;
;; You can also use metoffice-datahub-lib-forecast-table to get a buffer
;; with the upcoming forecast formatted as an org-mode table.

;;; Code:

(require 'json)
(require 'url)
(require 'parse-time)
(require 'cl-lib)
(require 'org-table)

(defvar metoffice-datahub-lib-verbose-mode nil "If set to t then functions may output various progress and debugging messages.")

(defvar metoffice-datahub-lib-forecast-buffer
  "*forecast*"
  "Buffer name for outputting future weather forecasts.
Used by metoffice-datahub-lib-forecast function.")

(defvar metoffice-datahub-lib-forecast-max-events
  nil
  "How many events included in forecast buffer, nil means all of them.")

(defvar metoffice-datahub-lib-forecast-spacer
  "\n\n"
  "String appended to each line of forecast, by default two newlines to ensure a gap between them")

(defvar metoffice-datahub-lib-apikey nil
  "The API key for your metoffice datahub account.
See https://datahub.metoffice.gov.uk/docs/f/category/site-specific/type/site-specific/api-documentation#auth for setting it.")

(defvar metoffice-datahub-lib-base-url
  "https://data.hub.api.metoffice.gov.uk/sitespecific/v0/point/"
  "Base URL for metoffice API, you shouldn't need to update this.")

(defvar metoffice-datahub-lib-query-type
  "three-hourly"
  "Choice of time specific API either `hourly', `three-hourly', or `daily'.")

(defvar metoffice-datahub-lib-long-lat '(nil nil) "List, the first element is longitude, the second is latitude.")

(defvar metoffice-datahub-lib-cached-response nil "Contains the latest response cached, a JSON data parsed into elisp.")

(defvar metoffice-datahub-lib-long-description
  nil
  "String describing the current weather conditions from the last update.")

(defvar metoffice-datahub-lib-description
  nil
  "String describing the current weather conditions from the last update.")

(defvar metoffice-datahub-lib-symbol
  nil
  "A unicode emoji describing the current weather conditions.")

(defvar metoffice-datahub-lib-current-weather nil "Contains a hashtable of the current weather window.")

(defvar metoffice-datahub-lib-decoder
  nil
  "Function used to parse the response and set all the internal variables.

If set to nil then will guess based on metoffice-datahub-lib-query-type.")

(defvar metoffice-datahub-lib-data-cache
  nil
  "File that stores the cached responses from querying the API.

Cached according to the query-type, if nil will use
metoffice-datahub-lib--generate-data-cacheto pick the location.")

(defvar metoffice-datahub-lib-data-cache-maxage (* 30 60) "Maximum age in seconds of the cache before loading it again.")

(defvar metoffice-datahub-lib-significant-weather-codes
  '((-1 . "Trace rain")
    (0 . "Clear night")
    (1 . "Sunny day")
    (2 . "Partly cloudy (night)")
    (3	. "Partly cloudy (day)")
    (4 . "Not used")
    (5 . "Mist")
    (6 . "Fog")
    (7 . "Cloudy")
    (8 . "Overcast")
    (9 . "Light rain shower (night)")
    (10 . "Light rain shower (day)")
    (11 . "Drizzle")
    (12	. "Light rain")
    (13	. "Heavy rain shower (night)")
    (14	. "Heavy rain shower (day)")
    (15	. "Heavy rain")
    (16	. "Sleet shower (night)")
    (17	. "Sleet shower (day)")
    (18	. "Sleet")
    (19	. "Hail shower (night)")
    (20	. "Hail shower (day)")
    (21	. "Hail")
    (22	. "Light snow shower (night)")
    (23	. "Light snow shower (day)")
    (24	. "Light snow")
    (25	. "Heavy snow shower (night)")
    (26	. "Heavy snow shower (day)")
    (27	. "Heavy snow")
    (28	. "Thunder shower (night)")
    (29	. "Thunder shower (day)")
    (30	. "Thunder"))
  "An alist of weather codes to strings they describe.

See https://www.metoffice.gov.uk/services/data/datapoint/code-definitions.")

(defvar metoffice-datahub-lib-significant-weather-codes-to-symbols
  '((-1 . "🌧️") ;; Trace rain
    (0 . "🌌") ;; Clear night
    (1 . "☀️") ;; Sunny day
    (2 . "☁️") ;; Partly cloudy (night)
    (3	. "⛅") ;; Partly cloudy (day)
    (4 . "?") ;; Not used
    (5 . "🌫️") ;; Mist
    (6 . "🌫️") ;; Fog
    (7 . "☁️") ;; Cloudy
    (8 . "☁️") ;; Overcast
    (9 . "🌧️") ;; Light rain shower (night)
    (10 . "🌦️") ;; Light rain shower (day)
    (11 . "🌧️") ;; Drizzle
    (12	. "🌧️") ;; Light rain
    (13	. "🌧️") ;; Heavy rain shower (night)
    (14	. "🌦️") ;; Heavy rain shower (day)
    (15	. "🌧️") ;; Heavy rain
    (16	. "❄️") ;; Sleet shower (night)
    (17	. "❄️") ;; Sleet shower (day)
    (18	. "❄️") ;; Sleet
    (19	. "❄️") ;; Hail shower (night)
    (20	. "❄️") ;; Hail shower (day)
    (21	. "❄️") ;; Hail
    (22	. "❄️") ;; Light snow shower (night)
    (23	. "❄️") ;; Light snow shower (day)
    (24	. "❄️") ;; Light snow
    (25	. "❄️") ;; Heavy snow shower (night)
    (26	. "❄️") ;; Heavy snow shower (day)
    (27	. "❄️") ;; Heavy snow
    (28	. "🌩️") ;; Thunder shower (night)
    (29	. "🌩️") ;; Thunder shower (day)
    (30	. "🌩️")) ;; Thunder
  "An alist of weather codes to emojis they describe.

See https://www.metoffice.gov.uk/services/data/datapoint/code-definitions")

(defvar metoffice-datahub-lib-response-to-string-time-format
  "%F %H:%M:%S %Z"
  "String used by functions to convert timeSeries data into a string.

See metoffice-datahub-lib--hour-based-weather-to-string and 
 metoffice-datahub-lib--day-based-weather-to-string")

(defvar metoffice-datahub-lib-hourly-response-to-string-output-format
  "%l (%t) %d - Feels Like: %f°C (%m-%M°C), Rain: %r, Wind: %w m/s, UV: %u, Humidity: %h%, Snow: %s"
  "Final format used to convert hourly timeSeries data into a long string.

See metoffice-datahub-lib--hour-based-weather-to-string

The formatting options are:
%l = current location
%t = current time (see metoffice-datahub-lib-response-to-string-time-format)
%q = query type
%d = description of significant weather
%x = unicode character describing significant weather
%f = feels like temperature in C
%m = minimum temperature in C
%M = maximum temperature in C
%r = rain chance / amount
%w = wind speed in metres/sec
%u = UV index
%h = humidity in percentage
%s = snow chance / amount")

(defvar metoffice-datahub-lib-hourly-response-to-csv-output-format
  "%t, %f°C, %r, %s, %d"
  "Used when outputting tables of forecasts

Used by metoffice-datahub-lib--hour-based-weather-to-csv when called from
metoffice-datahub-lib-forecast-table.

Formatting codes from
metoffice-datahub-lib-hourly-response-to-string-output-format")


(defvar metoffice-datahub-lib-daily-response-to-string-output-format
  "%l (%t) %d / %D - Temp Feels Like: %f°C/%F°C, Rain: %p%/%P%, Wind: %w/%W m/s, UV: %u, Humidity: %h%/%H%, Snow: %s%/%S%"
  "Final format used to convert daily timeSeries data into a long string.

See metoffice-datahub-lib--day-based-weather-to-string

The formatting options are:
%l = current location
%t = current time (see metoffice-datahub-lib-response-to-string-time-format)
%q = query type
%d = description of significant weather for day
%D = description of significant weather for night
%x = unicode character describing significant weather for day
%X = unicode character describing significant weather for night
%f = feels like temperature in C for day
%F = feels like temperature in C for night
%p = chance of precipitation for day
%P = chance of precipitation for night
%w = wind speed in metres/sec for day
%W = wind speed in metres/sec for night
%h = humidity in percentage for day
%H = humidity in percentage for night
%s = snow chance / amount for day
%S = snow chance / amount for night")

(defvar metoffice-datahub-lib-daily-response-to-csv-output-format
  "%t, %f/%F°C, %p%/%P%, %s%/%S%, %x/%X"
  "Used when outputting tables of forecasts

Used by metoffice-datahub-lib--day-based-weather-to-csv when called from
metoffice-datahub-lib-forecast-table.

Formatting codes from
metoffice-datahub-lib-daily-response-to-string-output-format")



(defvar metoffice-datahub-lib-after-fetch-hook
  nil
  "Hook that runs after data is fetched fresh from the API (and not from cache).")



;; Functions

(defun metoffice-datahub-lib--debug-message (&rest args)
  "Outputs debugging messages if verbose-mode on.

`metoffice-datahub-lib-verbose-mode' controls if ARGS are printed,
joined as a single string."
  (when metoffice-datahub-lib-verbose-mode
    (message "%s" (string-join args " "))))

(defun metoffice-datahub-lib--file-mtime (filename)
  "Extracts the mtime of a file FILENAME."
  (string-to-number
   (format-time-string
    "%s"
    (file-attribute-modification-time
     (file-attributes
      (file-truename (expand-file-name filename)))))))

(defun metoffice-datahub-lib--file-age-check-p (filename &optional maxage)
  "Check to see if a file is older the the maximum age allowed.

Returns t if the file is fresh enough to use, and nil if the file is
stale/too old to use.

Takes an argument of `FILENAME' which should be the full path to a file

Second optional argument is `MAXAGE' which is a time in seconds

If maxage isn't set uses metoffice-datahub-lib-data-cache-maxage"
  (unless maxage
    (setq maxage metoffice-datahub-lib-data-cache-maxage))
  
  (unless (integerp maxage)
    (error
     (format
      "Maxage not integerp instead '%s'"
      maxage)))

  (unless (file-exists-p (expand-file-name filename))
    (error (format "Filename '%s' doesn't exist" filename)))

  (let ((maxage-ts (- (string-to-number (format-time-string "%s")) maxage))
        (fileage (metoffice-datahub-lib--file-mtime filename)))
    ;; (message "(>= %s %s) = '%s'" fileage maxage-ts (>= fileage maxage-ts))
    (>= fileage maxage-ts)))

(defun metoffice-datahub-lib--generate-data-cache ()
  "Generate a location for the JSON cache file.

By default stores a file in the `user-emacs-directory' called
metoffice-datahub-%s-cache.json filling in the blank with
`metoffice-datahub-lib-query-type'"
  (expand-file-name
   (format "%s/%s"
           user-emacs-directory
           (format "metoffice-datahub-%s-cache.json"
                   metoffice-datahub-lib-query-type))))
  

(defun metoffice-datahub-lib--save-json (json-data &optional filename)
  "Save JSON data into a file for caching the response.

Also sets it into a cache variable called
`metoffice-datahub-lib-cached-response' for other functions to retrieve.

Takes 2 arguments, first is the JSON-DATA and the second is the FILENAME.

If the filename is nil or missing it uses
`metoffice-datahub-lib--generate-data-cache' to generate one."
  (setq metoffice-datahub-lib-cached-response json-data)

  (unless filename
    (setq filename (metoffice-datahub-lib--generate-data-cache)))
  
  (with-temp-file filename
    (let ((data (json-serialize json-data)))
      (when data
        (metoffice-datahub-lib--debug-message
         (format "metoffice-datahub-lib-save-json: Writing JSON data to %s"
                 filename))
        (insert (format "%s" data))))))

(defun metoffice-datahub-lib--load-json (&optional filename)
  "Load JSON data out of FILENAME and into a cache variable.

If the filename isn't specified uses
`metoffice-datahub-lib--generate-data-cache' to generate it."
  (unless filename
    (setq filename (metoffice-datahub-lib--generate-data-cache)))
  
  (if (file-exists-p filename)
      ;; json-read-file and json-parse-string or json-parse-buffer do
      ;; different things and you get an elisp structure from
      ;; read-file that you don't get with parse-buffer
      (with-temp-buffer
        (insert-file-contents-literally filename)
        (let ((jd (json-parse-buffer)))
          (when jd
            (metoffice-datahub-lib--debug-message
             (format "metoffice-datahub-lib-load-json: Restored JSON data from '%s'"
                     filename))
            (setq metoffice-datahub-lib-cached-response jd)
            jd)))))

(defun metoffice-datahub-lib-process (&optional status cbargs)
  "The default callback function run from `metoffice-datahub-lib-fetch'.

Takes the arguments STATUS and CBARGS which are ignored, instead checks
the response buffer for being a 200 OK response, then attempts to extract
the JSON data from it, if it runs
`metoffice-datahub-lib--save-json' to save it to disk and set the
`metoffice-datahub-lib-cached-response' variable.

Finally it calls the `metoffice-datahub-lib--decode-info' function
to decode the output into multiple useful variables for end user
consumption."
  (cl-block moproc
    (goto-char (point-min))
    
    (unless (string-match "200 OK\n" (thing-at-point 'line))
      ;; (message "FAILURE TO RETRIEVE DATA")
      ;; (message "DATA: %s" (buffer-substring-no-properties (point-min) (point-max)))
      (cl-return-from moproc nil))

    ;; Skip beyond the HTTP headers that are in the buffer
    (re-search-forward "^[[:space:]]*$")

    ;; Rest of the buffer should be JSON data response.
    (let ((jd (json-parse-buffer)))
      (when jd
        ;; (message "json-data: '%s'" jd)
        (metoffice-datahub-lib--save-json jd metoffice-datahub-lib-data-cache)
        (metoffice-datahub-lib--decode-info)
        (cl-return-from moproc jd)))

    (message "metoffice-datahub-lib-process: Failure to parse JSON data")
    nil))


(defun metoffice-datahub-lib-build-url (url args)
  "Construct a URL from a base and some args.

URL should be the basic address, which has `?' on the end followed by
a string of A=B options from ARGS, which should be specified as a list
of cons cells."
  ;; This is essentially stolen from the example of pepita--build-querystring from https://github.com/sebasmonia/pepita/blob/489ddc2675906f9dd27bd4ec69ef140e52194952/pepita.el#L119-L164 thanks Sebastian"
  (format "%s?%s"
          url
          (mapconcat (lambda (data)
                       (format "%s=%s" (car data)
                               (url-hexify-string (cdr data))))
                     args
                     "&")))

(cl-defun metoffice-datahub-lib-fetch (&optional &key mo-url callback long-lat)
  "Actually fetches the fresh data and caches it.

Takes 3 keyword arguments, all optional:

`MO-URL' - specify the URL to append queries too

`CALLBACK' - specify the function to send data too when fetched

`LONG-LAT' - a 2 element list, the first of which is logitude, the
second latitude both as strings.

Relies on `metoffice-datahub-lib-apikey' being set, constructs a URL
by default using the `metoffice-datahub-lib-base-url' and
`metoffice-datahub-lib-query-type' variables, and makes a call to
the metoffice API, passes that response to the callback function.
By default this is `metoffice-datahub-lib-process' that caches the data
to disk then parses it out into useful variables for later."
  (interactive)

  (unless mo-url
    (setq mo-url (format "%s%s" metoffice-datahub-lib-base-url
                         metoffice-datahub-lib-query-type)))

  (unless callback
    (setq callback 'metoffice-datahub-lib-process))

  (unless long-lat
    (setq long-lat metoffice-datahub-lib-long-lat))

  
  (let* ((headers (list (cons "apikey" metoffice-datahub-lib-apikey)))
         (args (list (cons "excludeParameterMetadata" "false")
                     (cons "includeLocationName" "true")
                     (cons "longitude" (nth 0 long-lat))
                     (cons "latitude" (nth 1 long-lat))))
         (url (metoffice-datahub-lib-build-url
               mo-url
               args))
         (url-request-extra-headers headers)
         (url-request-method "GET")
         (url-request-data nil))
    ;; (url-request-data (encode-coding-string data 'utf-8)))
    (metoffice-datahub-lib--debug-message
     (format "metoffice-datahub-lib-fetch: Retrieving '%s'" url))
    (url-retrieve url
                  callback
                  nil
                  (not metoffice-datahub-lib-verbose-mode)) ;; invert to silence
    (run-hooks 'metoffice-datahub-lib-after-fetch-hook)))
  

(defun metoffice-datahub-lib-refresh (&optional force)
  "Either refresh the data and variables from cache, or pull from the API.

Intended as the way the user or another elisp system on a timer should
refresh the data.

It checks for the existence of a cache file from
 `metoffice-datahub-lib-data-cache' and if it exists uses
 `metoffice-datahub-lib--file-age-check-p' to work out if its fresh
 enough.  If it is it re-reads the file and freshens all the variables
 and other things parsed from it to make sure they are all consistent.

Otherwise it calls `metoffice-datahub-lib-fetch' to fetch fresh data
 from the API and that then caches and parses the output.

If called with a prefix argument will delete the cache on both disk and
 variable to force a refresh."

  (interactive "P")

  ;; If we don't have it but the file exists, then either load from
  ;; cache or fetch
  (let ((cache-file (if (not metoffice-datahub-lib-data-cache)
                        (metoffice-datahub-lib--generate-data-cache)
                      metoffice-datahub-lib-data-cache)))

  (when force
    (when (file-exists-p cache-file)
      (delete-file cache-file))
    (setq metoffice-datahub-lib-cached-response nil))
    
    (if (and (file-exists-p cache-file)
             (metoffice-datahub-lib--file-age-check-p cache-file))
        
        (progn
          (metoffice-datahub-lib--load-json cache-file)
          (metoffice-datahub-lib--decode-info))
      
      ;; This calls (metoffice-datahub-lib--decode-info) itself in the
      ;; callback function because the url-retrieve is async and
      ;; otherwise you'll get an error
      (metoffice-datahub-lib-fetch)))
    
    (when (called-interactively-p 'any)
      (message "%s" metoffice-datahub-lib-long-description)))
    

(defun metoffice-datahub-lib--sig-weather-code-to-string (code)
  "Take an integer CODE and return the string that it describes."
  ;; https://www.metoffice.gov.uk/services/data/datapoint/code-definitions
  (alist-get code metoffice-datahub-lib-significant-weather-codes))

(defun metoffice-datahub-lib--sig-weather-code-to-symbol (code)
  "Take an integer CODE and return the emoji unicode symbol that it describes."
  ;; https://www.metoffice.gov.uk/services/data/datapoint/code-definitions
  (alist-get code metoffice-datahub-lib-significant-weather-codes-to-symbols))

(defun metoffice-datahub-lib--decode-info (&optional data)
  "Parse the JSON data response from the API at the top level.

If DATA not nil then runs the user selected `metoffice-datahub-lib-decoder'
function across the provided data, and expects it to set a lot of variables,
if that variable is nil it guesses based on `metoffice-datahub-lib-query-type'."
  (if metoffice-datahub-lib-decoder

      ;; User supplied function?  Call it
      (funcall metoffice-datahub-lib-decoder data)

    ;; Otherwise make a good guess based on the query type
    (cond ((cl-equalp metoffice-datahub-lib-query-type "hourly")
           (metoffice-datahub-lib--decode-hourly-info data))
          
          ((cl-equalp metoffice-datahub-lib-query-type "three-hourly")
           (metoffice-datahub-lib--decode-three-hourly-info data))
          
          ((cl-equalp metoffice-datahub-lib-query-type "daily")
           (metoffice-datahub-lib--decode-daily-info data))
          
          (t (error
              "Error: metoffice-datahub-lib--decode-info - no function supplied and metoffice-datahub-lib-query-type doesn't match known values")))))

(defun metoffice-datahub-lib--extract-current-timeseries (timeseries)
  "Take a TIMESERIES vector and return the most date appropriate entry.

Walks along the vector checking until it finds one not in the past.

Returns a list that is (current-weather weather-index) where
current-weather is a hash-table of the weather information and
weather-index is its position in timeseries, this is used for
collecting up future output for longer scale forecasts"

    (unless (vectorp timeseries)
      (error "Error: metoffice-datahub-lib--extract-current-timeseries timeseries not a vector instead '%s'" timeseries))

    ;; all the response data is in UTC so we have to fetch for UTC as well
    (let ((runtime (current-time))
          (current-weather nil)
          (weather-index nil)
          (vlen (length timeseries)))

      ;; Run through the vector looking for a time thats in the future
      (catch 'done
        (dotimes (i vlen)
          (let* ((ts (aref timeseries i))
                 (series-time (gethash "time" ts))
                 (parsed-time (parse-time-string series-time))
                 (encoded-time (encode-time parsed-time)))
            
            ;; (message "considering %s [item: %s vs runtime: %s]"
            ;;           series-time
            ;;           (format-time-string "%s" encoded-time)
            ;;           (format-time-string "%s" runtime))

            (when (not (time-less-p encoded-time runtime))
              (metoffice-datahub-lib--debug-message
               (format "setting current-weather to %s" series-time))
              (setq current-weather ts)
              (setq weather-index i)
              (throw 'done ts)))))
      
      ;; Return either result or first slot as a backup
      (if (and current-weather
               weather-index)

          (list current-weather weather-index)
        
        (progn
          (metoffice-datahub-lib--debug-message "metoffice-datahub-lib--extract-current-timeseries failed to extract a timeseries event, falling back to first")
          (list (aref timeseries 0) 0)))))

(defun metoffice-datahub-lib--hour-based-weather-to-string (location-name current-weather)
  "Parse timeSeries type hash-tables into string representations.

Takes 2 arguments, first being a string LOCATION-NAME, second being the
CURRENT-WEATHER which should be an hash-table entry from the timeSeries
vector in your results.

Returns a string describing the weather.

This version expects the format of the hourly/three-hourly queries."
  
  (unless location-name
    (error "Error: metoffice-datahub-lib--hour-based-weather-to-string location-name not defined"))
    
  (unless (hash-table-p current-weather)
    (error "Error: metoffice-datahub-lib--hour-based-weather-to-string current-weather not a hash-table is '%s'" current-weather))
  
  (let* ((curr-time-data (encode-time
                          (parse-time-string
                           (gethash "time" current-weather))))
         (curr-time (format-time-string
                     metoffice-datahub-lib-response-to-string-time-format
                     curr-time-data))
         (curr-pop (gethash "probOfPrecipitation" current-weather))
         (curr-precip-amount (gethash "totalPrecipAmount" current-weather))
         (curr-humidity (gethash "screenRelativeHumidity" current-weather))
         (curr-snow (gethash "probOfSnow" current-weather))
         (curr-snow-amount (gethash "totalSnowAmount" current-weather))
         (curr-uv (gethash "uvIndex" current-weather))

         ;; the "three-hourly" uses feelsLikeTemp but the "hourly"
         ;; uses feelsLikeTemperature so make a guess.
         ;;
         ;; Round into the nearest int
         (curr-flt (format
                    "%d"
                    (fround
                     (if (gethash "feelsLikeTemp" current-weather)
                         (gethash "feelsLikeTemp" current-weather)
                       (gethash "feelsLikeTemperature" current-weather)))))
         
         (curr-min-temp (format
                         "%d"
                         (fround (gethash "minScreenAirTemp" current-weather))))
         (curr-max-temp (format
                         "%d"
                         (fround (gethash "maxScreenAirTemp" current-weather))))
         
         (curr-ws (gethash "windSpeed10m" current-weather))
         (curr-desc (metoffice-datahub-lib--sig-weather-code-to-string
                     (gethash "significantWeatherCode" current-weather)))
         (curr-symbol (metoffice-datahub-lib--sig-weather-code-to-symbol
                       (gethash "significantWeatherCode" current-weather)))

         ;; Include the amount of precipitation if we have it
         (rain-str (if (> curr-precip-amount 0)
                       (format "%s%% (%smm)"
                               curr-pop
                               curr-precip-amount)
                     (format "%s%%" curr-pop)))
         
         (snow-str (cond

                    ;; "three-hourly" includes probOfSnow even if its
                    ;; 0, "hourly" only includes the amount
                    ((numberp curr-snow)
                     (if (> curr-snow-amount 0)
                         (format "%s%% (%smm)"
                                 curr-snow
                                 curr-snow-amount)
                       (format "%s%%" curr-snow)))

                    ;; This should catch the "hourly" when we have an
                    ;; amount but not a probability.
                    ((numberp curr-snow-amount)
                     (format "%smm" curr-snow-amount))))

         (out-fmt-string (list (cons ?l location-name)
                               (cons ?t curr-time)
                               (cons ?q metoffice-datahub-lib-query-type)
                               (cons ?d curr-desc)
                               (cons ?x curr-symbol)
                               (cons ?f curr-flt)
                               (cons ?m curr-min-temp)
                               (cons ?M curr-max-temp)
                               (cons ?r rain-str)
                               (cons ?w curr-ws)
                               (cons ?u curr-uv)
                               (cons ?h curr-humidity)
                               (cons ?s snow-str)))

         (desc (format-spec
                metoffice-datahub-lib-hourly-response-to-string-output-format
                out-fmt-string t)))
           
    desc))

(defun metoffice-datahub-lib--day-based-weather-to-string (location-name current-weather)
  "Parse timeSeries type hash-tables into string representations.

Takes 2 arguments, first being a string LOCATION-NAME, second being the
CURRENT-WEATHER which should be an hash-table entry from the timeSeries
vector in your results.

Returns a string describing the weather.

This version expects the format of the daily query."
 
  (unless location-name
    (error "Error: metoffice-datahub-lib--day-based-weather-to-string location-name not defined"))
    
  (unless (hash-table-p current-weather)
    (error "Error: metoffice-datahub-lib--day-based-weather-to-string current-weather not a hash-table is '%s'" current-weather))
  
  (let* ((curr-time-data (encode-time
                          (parse-time-string
                           (gethash "time" current-weather))))
         (curr-time (format-time-string
                     metoffice-datahub-lib-response-to-string-time-format
                     curr-time-data))

         (day-ws (gethash "midday10MWindSpeed" current-weather))
         (night-ws (gethash "midnight10MWindSpeed" current-weather))

         (day-pop (gethash "dayProbabilityOfPrecipitation" current-weather))
         (night-pop (gethash "nightProbabilityOfPrecipitation" current-weather))

         (day-max-flt (gethash "dayMaxFeelsLikeTemp" current-weather))
         (night-min-flt (gethash "nightMinFeelsLikeTemp" current-weather))

         (day-snow (gethash "dayProbabilityOfSnow" current-weather))
         (night-snow (gethash "nightProbabilityOfSnow" current-weather))

         (day-max-uv (gethash "maxUvIndex" current-weather))

         (day-humidity (gethash "middayRelativeHumidity" current-weather))
         (night-humidity (gethash "midnightRelativeHumidity" current-weather))

         (day-desc (metoffice-datahub-lib--sig-weather-code-to-string
                    (gethash "daySignificantWeatherCode" current-weather)))

         (night-desc (metoffice-datahub-lib--sig-weather-code-to-string
                      (gethash "nightSignificantWeatherCode" current-weather)))

         (day-symbol (metoffice-datahub-lib--sig-weather-code-to-symbol
                      (gethash "daySignificantWeatherCode" current-weather)))

         (night-symbol (metoffice-datahub-lib--sig-weather-code-to-symbol
                      (gethash "nightSignificantWeatherCode" current-weather)))


         (out-fmt-string (list (cons ?l location-name)
                               (cons ?t curr-time)
                               (cons ?q metoffice-datahub-lib-query-type)
                               
                               (cons ?d day-desc)
                               (cons ?D night-desc)

                               (cons ?x day-symbol)
                               (cons ?X night-symbol)

                               (cons ?f day-max-flt)
                               (cons ?F night-min-flt)

                               (cons ?p day-pop)
                               (cons ?P night-pop)

                               (cons ?w day-ws)
                               (cons ?W night-ws)

                               (cons ?u day-max-uv)

                               (cons ?h day-humidity)
                               (cons ?H night-humidity)

                               (cons ?s day-snow)
                               (cons ?S night-snow)))

         (desc (format-spec
                metoffice-datahub-lib-daily-response-to-string-output-format
                out-fmt-string t)))
    
    ;; (desc (format "%s (%s) %s / %s - Temp Feels Like: %s°C/%s°C, Rain: %s%%/%s%%, Wind: %s/%s m/s, UV: %s, Humidity: %s%%/%s%%, Snow: %s%%/%s%%"
    ;;               location-name
    ;;               curr-time
    ;;               ;; metoffice-datahub-lib-query-type
    
    ;;               day-desc
    ;;               night-desc
    
    ;;               day-max-flt
    ;;               night-min-flt
    
    ;;               day-pop
    ;;               night-pop
    
    ;;               day-ws
    ;;               night-ws
    
    ;;               day-max-uv
    
    ;;               day-humidity
    ;;               night-humidity
    
    ;;               day-snow
    ;;               night-snow)))
    
    desc))


(defun metoffice-datahub-lib--decode-hourly-info (&optional data)
  "Decodes API responses from the `hourly' query type.

Passes the DATA over to `metoffice-datahub-lib--decode-three-hourly-info'."
  (unless data
    (setq data metoffice-datahub-lib-cached-response))
  
  (unless (hash-table-p data)
    (error "Error: metoffice-datahub-lib--decode-info data not a hashtable instead '%s'" data))

  ;; Works well enough to get the next hours data actually
  ;;
  ;; FIXME: Should read out the next few hours and build a string with all their data in
  (metoffice-datahub-lib--decode-three-hourly-info data))

(defun metoffice-datahub-lib--decode-three-hourly-info (&optional data)
  "Take the JSON data from the API and extracts current weather.

Takes one argument which should be the DATA from the API and should be
 from either the hourly or three-hourly query, otherwise it uses
the cached response in `metoffice-datahub-lib-cached-response'.

At the end it sets the following variables:
`metoffice-datahub-lib-current-weather' current weather hash-table
`metoffice-datahub-lib-long-description' long parsed string description
of the most current forecast
`metoffice-datahub-lib-description' The short string description of the
most current forecast
`metoffice-datahub-lib-symbol' a symbol or string from the most current
forecast

It also returns the long parsed string description."

  (unless data
    (setq data metoffice-datahub-lib-cached-response))

  (unless (hash-table-p data)
    (error "Error: metoffice-datahub-lib--decode-info data not a hashtable instead '%s'" data))
  
  ;; The hashtable has a "features" vector, the first element of which
  ;; is a hashtable which contains the actual information we want.
  ;;
  ;; properties is a hashtable containing a few useful variables and
  ;; another vector of the data (timeSeries) each of which is a
  ;; hashtable of useful facts.
  (let* ((features (aref (gethash "features" data) 0))
         ;; (geometry (gethash "geometry" features))
         (properties (gethash "properties" features))
         (location-name (gethash "name" (gethash "location" properties)))
         ;; (modelrundate (gethash "modelRunDate" properties))
         (timeseries (gethash "timeSeries" properties))
         (weather-data (metoffice-datahub-lib--extract-current-timeseries
                        timeseries))
         (current-weather (car weather-data))

         ;; Decode current weather
         (desc (metoffice-datahub-lib--hour-based-weather-to-string
                location-name
                current-weather))
         (curr-desc (metoffice-datahub-lib--sig-weather-code-to-string
                     (gethash "significantWeatherCode" current-weather)))
         (curr-symbol (metoffice-datahub-lib--sig-weather-code-to-symbol
                       (gethash "significantWeatherCode" current-weather))))

    ;; Save variables for later
    (setq metoffice-datahub-lib-current-weather current-weather
          metoffice-datahub-lib-long-description desc
          metoffice-datahub-lib-description curr-desc
          metoffice-datahub-lib-symbol curr-symbol)
    desc))

(defun metoffice-datahub-lib--decode-daily-info (&optional data)
  "Take the JSON data from the API and extracts current weather.

Takes one argument which should be the DATA from the API and should be
from the daily query, otherwise it uses the cached response in
`metoffice-datahub-lib-cached-response'.

At the end it sets the following variables:
`metoffice-datahub-lib-current-weather' current weather hash-table
`metoffice-datahub-lib-long-description' long parsed string description
of the most current forecast
`metoffice-datahub-lib-description' The short string description of the
most current forecast
`metoffice-datahub-lib-symbol' a symbol or string from the most current
forecast

It also returns the long parsed string description."
  
  (unless data
    (setq data metoffice-datahub-lib-cached-response))

  (unless (hash-table-p data)
    (error "Error: metoffice-datahub-lib--decode-info data not a hashtable instead '%s'" data))
  
  ;; The hashtable has a "features" vector, the first element of which
  ;; is a hashtable which contains the actual information we want.
  ;;
  ;; properties is a hashtable containing a few useful variables and
  ;; another vector of the data (timeSeries) each of which is a
  ;; hashtable of useful facts.
  (let* ((features (aref (gethash "features" data) 0))
         ;; (geometry (gethash "geometry" features))
         (properties (gethash "properties" features))
         (location-name (gethash "name" (gethash "location" properties)))
         ;; (modelrundate (gethash "modelRunDate" properties))
         (timeseries (gethash "timeSeries" properties))
         (weather-data (metoffice-datahub-lib--extract-current-timeseries
                        timeseries))
         (current-weather (car weather-data))

         ;; Decode current weather
         (desc (metoffice-datahub-lib--day-based-weather-to-string
                location-name
                current-weather))
         (curr-desc (format "%s / %s"
                            (metoffice-datahub-lib--sig-weather-code-to-string
                             (gethash "daySignificantWeatherCode" current-weather))
                            (metoffice-datahub-lib--sig-weather-code-to-string
                             (gethash "nightSignificantWeatherCode" current-weather))))
         (curr-symbol (format "%s/%s"
                              (metoffice-datahub-lib--sig-weather-code-to-symbol
                               (gethash "daySignificantWeatherCode" current-weather))
                              (metoffice-datahub-lib--sig-weather-code-to-symbol
                               (gethash "nightSignificantWeatherCode" current-weather)))))

    ;; Save variables for later
    (setq metoffice-datahub-lib-current-weather current-weather
          metoffice-datahub-lib-long-description desc
          metoffice-datahub-lib-description curr-desc
          metoffice-datahub-lib-symbol curr-symbol)
      desc))


(defun metoffice-datahub-lib--extract-timeseries (&optional data)
  "Take an API DATA response, return the vector timeseries.

Returns a vector of hash-tables.  Checks the cached response if no
data provided."

  (unless data
    (setq data metoffice-datahub-lib-cached-response))
  
  (unless (hash-table-p data)
    (error "Error: metoffice-datahub-lib-forecast data not a hashtable instead '%s'" data))

  ;; Its a vector data, the first element is hash-table, we extract
  ;; the features, from there the hashtable of properties, from there
  ;; the timeSeries hashtable, and return that.
  (gethash "timeSeries" (gethash "properties" (aref (gethash "features" data) 0))))


(defun metoffice-datahub-lib--hour-based-weather-to-csv (location current-weather)
  "Output some parts of forecast as CSV

time, temperature, rain-chance, snow, description"
  (let ((metoffice-datahub-lib-hourly-response-to-string-output-format
         metoffice-datahub-lib-hourly-response-to-csv-output-format))
    (metoffice-datahub-lib--hour-based-weather-to-string location current-weather)))

(defun metoffice-datahub-lib--day-based-weather-to-csv (location current-weather)
  "Output some parts of forecast as CSV

time, temperature, rain-chance, snow, description"
  (let ((metoffice-datahub-lib-daily-response-to-string-output-format
         metoffice-datahub-lib-daily-response-to-csv-output-format))
    (metoffice-datahub-lib--day-based-weather-to-string location current-weather)))

(defun metoffice-datahub-lib-forecast-table (&optional max-events data)
  "Create a popup buffer of a weather forecast as an org-mode table"
  (interactive)

  (let ((metoffice-datahub-lib-forecast-spacer "\n")
        (func (cond ((cl-equalp metoffice-datahub-lib-query-type
                                "hourly")
                     #'metoffice-datahub-lib--hour-based-weather-to-csv)
                    ((cl-equalp metoffice-datahub-lib-query-type
                                "three-hourly")
                     #'metoffice-datahub-lib--hour-based-weather-to-csv)
                    ((cl-equalp metoffice-datahub-lib-query-type
                                "daily")
                     #'metoffice-datahub-lib--day-based-weather-to-csv))))
    
    (metoffice-datahub-lib-forecast max-events
                                    func
                                    data)
    
    (let ((buf (get-buffer-create metoffice-datahub-lib-forecast-buffer)))
      (with-current-buffer buf
        (let ((buffer-read-only nil))
          (goto-char (point-min))
          (forward-line) ;; after the heading
          (insert "\ntime, temp, rain, snow, type\n")
          (forward-line -1) ;; before the heading
          (beginning-of-line)
          (org-table-convert-region (point) (point-max) nil)
          (goto-char (org-table-begin))
          (org-table-insert-hline))))))
  

(defun metoffice-datahub-lib-forecast (&optional max-events string-func data)
  "Create a popup buffer of a weather forecast.

Takes three arguments:

`MAX-EVENTS' which is an integer defining how many after the current
event should be parsed out, how far to look in the future.  If missing
this uses `metoffice-datahub-lib-forecast-max-events' and if nil parses
all events.

`STRING-FUNC' is the function used to parse the timeSeries of events
 into strings to insert into the buffer, by default it guesses based on
 query-type.

`DATA' is the data you want it to parse, if missing uses
the cached response in `metoffice-datahub-lib-cached-response'

Output buffer named by `metoffice-datahub-lib-forecast-buffer' variable."

  (interactive)

  (unless data
    (setq data metoffice-datahub-lib-cached-response))
  
  (unless (hash-table-p data)
    (error "Error: metoffice-datahub-lib-forecast data not a hashtable instead '%s'" data))

  (when (and max-events
             (not (integerp max-events)))
    (error "Error: metoffice-datahub-lib-forecast max-events is not an int '%s'" max-events))

  (unless max-events
    (setq max-events metoffice-datahub-lib-forecast-max-events))

  ;; Delete the previous buffer
  (let ((existing-buf (get-buffer metoffice-datahub-lib-forecast-buffer)))
    (when existing-buf
      (kill-buffer existing-buf)))
  
  (let* ((features (aref (gethash "features" data) 0))
         ;; (geometry (gethash "geometry" features))
         (properties (gethash "properties" features))
         (location-name (gethash "name" (gethash "location" properties)))
         ;; (modelrundate (gethash "modelRunDate" properties))
         (timeseries (gethash "timeSeries" properties))
         
         (weather-data (metoffice-datahub-lib--extract-current-timeseries
                        timeseries))
         (weather-index (cadr weather-data))
         (vlen (length timeseries))

         ;; If we're defining it then check its under the limit
         (last-event (if max-events
                         (if (<= (+ weather-index max-events) vlen)
                             (+ weather-index max-events)
                           vlen)
                       vlen))
         
         (buf (get-buffer-create metoffice-datahub-lib-forecast-buffer)))
    
    (with-current-buffer buf
      (erase-buffer)
      (goto-char (point-min))
      
      (insert (format "Weather forecast output at %s\n"
                      (current-time-string)))
      
      (while (< weather-index last-event)
        ;; (message "looking at timeseries index %s, query-type %s"
        ;;          weather-index
        ;;          metoffice-datahub-lib-query-type)
        (let ((current-weather (aref timeseries weather-index)))
          (cond ((functionp string-func)
                 (insert (funcall string-func location-name current-weather))
                 (insert metoffice-datahub-lib-forecast-spacer))

                ((cl-equalp metoffice-datahub-lib-query-type
                            "hourly")
                 (insert (metoffice-datahub-lib--hour-based-weather-to-string
                          location-name
                          current-weather))
                 (insert metoffice-datahub-lib-forecast-spacer))
                
                
                ((cl-equalp metoffice-datahub-lib-query-type
                            "three-hourly")
                 (insert (metoffice-datahub-lib--hour-based-weather-to-string
                          location-name
                          current-weather))
                 (insert metoffice-datahub-lib-forecast-spacer))
                
                ((cl-equalp metoffice-datahub-lib-query-type
                            "daily")
                 (insert (metoffice-datahub-lib--day-based-weather-to-string
                          location-name
                          current-weather))
                 (insert metoffice-datahub-lib-forecast-spacer))
                
                (t "Couldn't understand query type '%s' to parse:\n%s"
                   metoffice-datahub-lib-query-type
                   current-weather))
        
          (cl-incf weather-index)))

      (metoffice-datahub-forecast-mode)
      (visual-line-mode t)
      (pop-to-buffer metoffice-datahub-lib-forecast-buffer)
      (goto-char (point-min)))))


(define-derived-mode metoffice-datahub-forecast-mode
  special-mode
  "Metoffice-Forecast"
  "Major mode for viewing metoffice forecasts.")


(provide 'metoffice-datahub-lib)
;;; metoffice-datahub-lib.el ends here
