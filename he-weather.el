;;; he-weather.el --- Displays weather information in mode-line  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2015 DarkSun <lujun9972@gmail.com>.

;; Author: DarkSun <lujun9972@gmail.com>
;; URL: https://github.com/lujun9972/he-weather-mode
;; Package-Version: 20160426.529
;; Package-Requires: ((emacs "24"))
;; Keywords: weather, mode-line
;; Created: 2015-12-28
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; customize the `he-weather-location' which supports chinese characters and then
;;
;;   M-x he-weather-mode
;;
;; default it will only display the weather and temperature.
;; you can specify `he-weather-format' to display more information

;;; Code:

(require 'url)
(require 'json)

(defgroup he-weather nil
  "He-Weather minor mode"
  :group 'emacs)

(defcustom he-weather-location "上海"
  "location"
  :type 'string
  :group 'he-weather)

(defcustom he-weather-key "e94238f0442c4d2bab47ca5e5b31970b"
  "key"
  :type 'string
  :group 'he-weather)

(defcustom he-weather-guess-location-function nil
  "Function to set `he-weather-location'"
  :type 'function
  :group 'he-weather)

(defcustom he-weather-format "[%(weather) %(temperature)°C]"
  "how to display the weather information.

%(weather) %(temperature) %(wind-chill) %(wind-direction) %(wind-direction) %(wind-speed)
%(atmosphere-humidity) %(atmosphere-pressure) %(atmosphere-rising) %(atmosphere-visibility)
%(sunrise-time) %(sunset-time) will be replaced by the real value"
  :type 'string
  :group 'he-weather)

(defcustom he-weather-use-F nil
  "Set t to use Fahrenheit"
  :type 'boolean
  :group 'he-weather)

(defcustom he-weather-temperture-format "%.2f"
  "Precision to display string format.  Use '%d' for integer or '%.2f' for two decimals."
  :type 'string
  :group 'he-weather)

(defcustom he-weather-update-interval 3600
  "Seconds after which the weather information will be updated."
  :type 'integer
  :group 'he-weather)

(defvar he-weather-info)

(defun he-weather-get-query-url (location key)
  "generate url that used to fetch weather information"

  (let* ((he_query (format "location=%s&key=%s&lang=en" location key))
         (url (format "https://free-api.heweather.net/s6/weather?%s" he_query)))
    (message "+++ url: %s" url)
    url))

(defun he-weather--extract-first-object (json-object)
  "get first he weather object from JSON-OBJECT"
  (let* ((first-object (aref (he-weather--extract-from-json-object json-object '(HeWeather6)) 0)))
    first-object))

(defun he-weather--extract-daily-from-heweather-object (weather-object)
  "get first daily-forecast object from he weather object"
  (let* ((daily-forecast-object (aref (he-weather--extract-from-json-object weather-object '(daily_forecast)) 0)))
    daily-forecast-object))

(defun he-weather--extract-from-json-object (json-object extract-place-list)
  "extract data from JSON-OBJECT which responsed by he weather"
  (let* ((place (car extract-place-list))
         (extract-place-list (cdr extract-place-list))
         (json-object (cdr (assoc place json-object))))
    (if extract-place-list
        (he-weather--extract-from-json-object json-object extract-place-list)
      json-object)))

(defun he-weather-info-format (json-object format-string)
  (let* ((he-weather-object (he-weather--extract-first-object json-object))
         (temperature (string-to-number (he-weather--extract-from-json-object he-weather-object '(now tmp))))
         (text (he-weather--extract-from-json-object he-weather-object '(now cond_txt)))
         (wind-chill (he-weather--extract-from-json-object he-weather-object '(now fl)))
         (wind-direction (he-weather--extract-from-json-object he-weather-object '(now wind_dir)))
         (wind-speed (he-weather--extract-from-json-object he-weather-object '(now wind_sep)))
         (atmosphere-humidity (he-weather--extract-from-json-object he-weather-object '(now hum)))
         (atmosphere-pressure (he-weather--extract-from-json-object he-weather-object '(now pres)))
         (atmosphere-rising (he-weather--extract-from-json-object he-weather-object '(now cloud)))
         (atmosphere-visibility (he-weather--extract-from-json-object he-weather-object '(now vis)))
         (daily-forecast-object (he-weather--extract-daily-from-heweather-object he-weather-object))
         (sunrise-time (he-weather--extract-from-json-object daily-forecast-object '(sr)))
         (sunset-time (he-weather--extract-from-json-object daily-forecast-object '(ss))))
    (setq format-string (replace-regexp-in-string "%(weather)" text format-string t))
    (if he-weather-use-F
        (setq format-string (replace-regexp-in-string "%(temperature)" (format he-weather-temperture-format temperature-F) format-string t))
      (setq format-string (replace-regexp-in-string "%(temperature)" (format he-weather-temperture-format temperature) format-string t)))
    (setq format-string (replace-regexp-in-string "%(wind-chill)" wind-chill format-string t))
    (setq format-string (replace-regexp-in-string "%(wind-direction)" wind-direction format-string t))
    (setq format-string (replace-regexp-in-string "%(wind-speed)" wind-speed format-string t))
    (setq format-string (replace-regexp-in-string "%(atmosphere-humidity)" atmosphere-humidity format-string t))
    (setq format-string (replace-regexp-in-string "%(atmosphere-pressure)" atmosphere-pressure format-string t))
    (setq format-string (replace-regexp-in-string "%(atmosphere-rising)" atmosphere-rising format-string t))
    (setq format-string (replace-regexp-in-string "%(atmosphere-visibility)" atmosphere-visibility format-string t))
    (setq format-string (replace-regexp-in-string "%(sunrise-time)" sunrise-time format-string t))
    (setq format-string (replace-regexp-in-string "%(sunset-time)" sunset-time format-string t))
    format-string))

(defun he-weather-update-info-cb (status &rest cbargs)
  (let (content)
    (goto-char (point-min))
    (when (search-forward-regexp "^$" nil t)
      (setq content (buffer-substring-no-properties (+ (point) 1) (point-max))))
    (kill-buffer)
    (force-mode-line-update t)
    (setq he-weather-info (json-read-from-string content))))

(defun he-weather--ipinfo-parse (loc-data)
  "convert LOC-DATA to string"
  (concat (alist-get 'city loc-data)
          ", "
          (alist-get 'country loc-data)))

(defun he-weather-update-location-cb (status &rest _)
  (let (content)
    (goto-char (point-min))
    (when (search-forward-regexp "^$" nil t)
      (setq content (buffer-substring-no-properties (+ (point) 1) (point-max))))
    (kill-buffer)
    (force-mode-line-update t)
    (setq he-weather-location (he-weather--ipinfo-parse (json-read-from-string content)))))

(defvar he-weather--ipinfo-url "https://ipinfo.io/json")

(defun he-weather-ipinfo ()
  "update location information"
  (interactive)
  (url-retrieve he-weather--ipinfo-url #'he-weather-update-location-cb nil t))

(defun he-weather-update-info ()
  "update weather information"
  (interactive)
  (when he-weather-guess-location-function
    (funcall he-weather-guess-location-function))
  (let ((url (he-weather-get-query-url he-weather-location he-weather-key)))
    (url-retrieve url #'he-weather-update-info-cb nil t)))

(defun he-weather--f_to_c (temp)
  "convert fahrenheit to celsius"
  (/ (* (- temp 32.0) 5.0) 9.0))


;;; Glboal Minor-mode

(defcustom he-weather-mode-line
  '(:eval (he-weather-info-format he-weather-info he-weather-format))
  "Mode line lighter for he-weather-mode."
  :type 'sexp
  :risky t
  :group 'he-weather)

(defvar he-weather-update-info-timer nil)

;;;###autoload
(define-minor-mode he-weather-mode
  "Toggle weather information display in mode line (he-weather information mode).
With a prefix argument ARG, enable he-weather mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :global t :group 'he-weather
  (unless global-mode-string
    (setq global-mode-string '("")))
  (when (timerp he-weather-update-info-timer)
    (cancel-timer he-weather-update-info-timer))
  (if (not he-weather-mode)
      (setq global-mode-string
            (delq 'he-weather-mode-line global-mode-string))
    (setq he-weather-update-info-timer (run-at-time nil he-weather-update-interval #'he-weather-update-info))
    (add-to-list 'global-mode-string 'he-weather-mode-line t)
    (he-weather-update-info)))

(provide 'he-weather)
;;; he-weather.el ends here
