;;; metoffice-datahub-mode.el --- A minor mode that puts a current weather symbol into your modeline using metoffice-datahub-lib -*- lexical-binding: t -*-

;; Copyright 2025 - Twitchy Ears

;; Author: Twitchy Ears https://github.com/twitchy-ears/
;; URL: https://github.com/twitchy-ears/metoffice-datahub-lib
;; Version: 0.1
;; Package-Requires ((emacs "30.1") metoffice-datahub-lib)
;; Keywords: weather api modeline

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
;; First get metoffice-datahub-lib configured then use: 
;;
;; (use-package metoffice-datahub-mode
;;  :after metoffice-datahub-lib
;;  :config
;;  (metoffice-datahub-mode 1))
;;
;; If you have a terminal that doesn't like wide composed characters
;; then you can either redefine the
;; metoffice-datahub-lib-significant-weather-codes-to-symbols variable
;; or this may help:
;;
;;   (unless window-system
;;     (setq metoffice-datahub-mode-use-symbol nil
;;           metoffice-datahub-mode-indicate-temp t
;;           metoffice-datahub-mode-temp-symbols '(" Cold" "" " Hot")))
;;
;; Or alternatively if you want temperature numbers in your modeline use:
;; 
;; (setq metoffice-datahub-mode-temp-symbols
;;         '(" TEMPERATURE" " TEMPERATURE" " TEMPERATURE"))

(require 'metoffice-datahub-lib)

(defvar metoffice-datahub-mode-current-weather-format "[%s]" "The format string which will contain the current weather on the modeline")

(defvar metoffice-datahub-mode-current-weather-short "[W]"
  "The short version of the weather shown in the modeline, formatted according to the metoffice-datahub-mode-current-weather-format")

(defvar metoffice-datahub-mode-data-update-in-seconds (* 60 60)
  "Seconds between calling refreshes of the metoffice data from the API, should make sense with your metoffice-datahub-lib-data-cache-maxage setting")

(defvar metoffice-datahub-mode-modeline-update-in-seconds (* 5 60)
  "Seconds between updating the string displayed on the modeline")

(defvar metoffice-datahub-mode-data-update-timer nil
  "Holds the timer for refreshing the weather")

(defvar metoffice-datahub-mode-modeline-update-timer nil
  "Holds the timer for refreshing the modeline string")

(defvar metoffice-datahub-mode-use-symbol t "Use the symbol in the modeline and not the short textual description")

(defvar metoffice-datahub-mode-indicate-temp t "Add a second symbol/string to indicate how hot/cold the temperature is at a glance")

(defvar metoffice-datahub-mode-temp-thresholds '(5 19)
  "A list of two temperatures, anything under the first element will be cold, anything over the second will be hot, anything in between will be neutral")

(defvar metoffice-datahub-mode-temp-symbols '("🥶" "🆗" "🥵") "A list of three elements used to display temperature in combination with metoffice-datahub-mode-temp-thresholds broadly the list is '(cold regular hot) if an element is a blank string then nothing will be displayed in that temperature range, if it contains the string TEMPERATURE then the actual temperature will be put there.")

(defvar metoffice-datahub-mode-after-hook nil "Hook that runs after the mode is turned on or off")

(defun metoffice-datahub-mode--temp-symbol (temp)
  "Takes a temperature which is expected to be a float, then looks up the correct symbol or string to return from metoffice-datahub-mode-temp-symbols based on metoffice-datahub-mode-temp-thresholds"
  (when (and metoffice-datahub-mode-indicate-temp
             (numberp temp)
             (= 2 (length metoffice-datahub-mode-temp-thresholds))
             (= 3 (length metoffice-datahub-mode-temp-symbols)))

    (let* ((out-elem (cond
                     ;; Hot
                     ((>= temp (nth 1 metoffice-datahub-mode-temp-thresholds))
                      2)
    
                     ;; Cold
                     ((<= temp (nth 0 metoffice-datahub-mode-temp-thresholds))
                      0)

                     ;; Between
                     (t 1))))
           (string-replace "TEMPERATURE"
                           (number-to-string (truncate temp))
                           (nth out-elem metoffice-datahub-mode-temp-symbols)))))
      

(defun metoffice-datahub-mode-update-modeline ()
  "Creates a modeline string by looking at the metoffice-datahub-lib-*
variables for the current weather.  If metoffice-datahub-mode-use-symbol
is true then it will use the metoffice-datahub-lib-symbol instead of the
textual description.  Ifmetoffice-datahub-mode-indicate-temp is true it
will append a second symbol/string as per
metoffice-datahub-mode--temp-symbol based on the current `feelsLikeTemp'
value from the current weather.

Sets the current metoffice-datahub-lib-long-description to be a
'help-echo tooltip on the string and formats it into
metoffice-datahub-mode-current-weather-format which should be a format
string with a single %s where you want the output to go.

This is set into metoffice-datahub-mode-current-weather-short and also
returned from the function."

  (interactive)
  (when (and metoffice-datahub-lib-long-description
             metoffice-datahub-lib-description
             metoffice-datahub-lib-symbol)
    
    ;; Set the string to the current useful thing
    (let* ((data (if metoffice-datahub-mode-use-symbol
                     metoffice-datahub-lib-symbol
                   metoffice-datahub-lib-description))

           ;; FIXME this only works for hour based data not daily based data
           (curr-flt (gethash "feelsLikeTemp" metoffice-datahub-lib-current-weather)))

      ;; Append the temperature face string if needs be
      (when metoffice-datahub-mode-indicate-temp
        (setq data (concat data
                           (metoffice-datahub-mode--temp-symbol curr-flt))))

      ;; Then set and return it with a mouseover-tooltip-help function
      ;; of the details
      (setq metoffice-datahub-mode-current-weather-short
            (propertize
             (format metoffice-datahub-mode-current-weather-format data)
             'help-echo
             metoffice-datahub-lib-long-description))

      ;; (message "metoffice-datahub-mode-update-modeline now '%s'" metoffice-datahub-mode-current-weather-short)

      metoffice-datahub-mode-current-weather-short)))

(defun metoffice-datahub-mode-output-modeline ()
  (interactive)
  (if metoffice-datahub-mode-current-weather-short
      metoffice-datahub-mode-current-weather-short
    ""))

(defun metoffice-datahub-mode--already-in-modeline-p ()
  (let ((results (cl-search
                     "(:eval (metoffice-datahub-mode-output-modeline))"
                     (format "%s" mode-line-misc-info))))
    (if results
        t
      nil)))

(define-minor-mode metoffice-datahub-mode ()
  "Shows your weather in the modeline"
  :init-value nil
  :global nil
  :lighter ""
  :after-hook metoffice-datahub-mode-after-hook

  (if metoffice-datahub-mode

      ;; Turn on
      (progn

        ;; Kill existing timers
        (when metoffice-datahub-mode-data-update-timer
          (cancel-timer metoffice-datahub-mode-data-update-timer))

        (when metoffice-datahub-mode-modeline-update-timer
          (cancel-timer metoffice-datahub-mode-modeline-update-timer))


        ;; Setup new one
        (setq metoffice-datahub-mode-data-update-timer
              (run-with-timer metoffice-datahub-mode-data-update-in-seconds
                              metoffice-datahub-mode-data-update-in-seconds
                              (lambda ()
                                (metoffice-datahub-lib-refresh))))

        (setq metoffice-datahub-mode-modeline-update-timer
              (run-with-timer 10
                              metoffice-datahub-mode-modeline-update-in-seconds
                              (lambda ()
                                (metoffice-datahub-mode-update-modeline))))

        ;; Trigger a full refresh
        (metoffice-datahub-lib-refresh)
        (metoffice-datahub-mode-update-modeline)

        ;; Add into the modeline
        (unless (metoffice-datahub-mode--already-in-modeline-p)
          (setq mode-line-misc-info
                (cons '(:eval (metoffice-datahub-mode-output-modeline))
                      mode-line-misc-info))))

    
    ;; Turn off
    (progn

      ;; Remove string from modeline
      (setq mode-line-misc-info
            (cl-remove-if
             (lambda (k)
               (cl-search "(:eval (metoffice-datahub-mode-output-modeline))"
                          (format "%s" k)))
             mode-line-misc-info))

      ;; Kill timers
      (when metoffice-datahub-mode-data-update-timer
          (cancel-timer metoffice-datahub-mode-data-update-timer)
          (setq metoffice-datahub-mode-data-update-timer nil))

      (when metoffice-datahub-mode-modeline-update-timer
          (cancel-timer metoffice-datahub-mode-modeline-update-timer)
          (setq metoffice-datahub-mode-modeline-update-timer nil))

      )))



(provide 'metoffice-datahub-mode)
