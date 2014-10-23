;-*- coding: utf-8 -*-
;; lookup-word-on-internet.el -- helpful commands for looking up the internet

;; Copyright © 2011 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )

;; You can redistribute this program and/or modify it. Please give credit and link. Thanks.

;;; DESCRIPTION

;; this package provides convenient commands for looking up the web.
;; The commands are:

;; lookup-word-on-internet
;; lookup-google
;; lookup-wikipedia
;; lookup-word-dict-org
;; lookup-word-definition
;; lookup-answers.com
;; lookup-wiktionary

;;; REQUIREMENT

;; none

;;; INSTALL

;; To install, place this file 〔lookup-word-on-internet.el〕 in the directory 〔~/.emacs.d/lisp/〕.

;; Then, place the following code in your emacs init file

;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (autoload 'lookup-google "lookup-word-on-internet" "Lookup in browser" t)
;; (autoload 'lookup-wikipedia "lookup-word-on-internet" "Lookup in browser" t)
;; (autoload 'lookup-word-dict-org "lookup-word-on-internet" "Lookup in browser" t)
;; (autoload 'lookup-word-definition "lookup-word-on-internet" "Lookup in browser" t)
;; (autoload 'lookup-wiktionary "lookup-word-on-internet" "Lookup word in browser" t)

;; ;; Suggested keys
;; (global-set-key (kbd "<f1> 7") 'lookup-google)
;; (global-set-key (kbd "<f1> 8") 'lookup-wikipedia)
;; (global-set-key (kbd "<f1> 9") 'lookup-word-definition)
;; …

;;; DOCUMENTATION

;; Just some simple useful commands
;; For detail, see http://ergoemacs.org/emacs/emacs_lookup_ref.html

;; like it? buy my emacs tutorial

;; Buy Xah Emacs Tutorial
;; http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html

;;; HISTORY

;; 2014-10-20 changes are no longer logged here. Just github. version goes by date.
;; version 1.5, 2013-04-21 removed lookup-php-ref. Doesn't belong here.
;; version 1.4, 2013-03-23 added 2 more dict to the all-dictionaries. Good for vocabulary researchers
;; version 1.3, 2012-05-11 added “lookup-all-dictionaries”.
;; version 1.2, 2012-05-10 added “lookup-answers.com”. Improved inline docs.
;; version 1.1, 2012-05-09 changed the input from 「'symbol」 to 「'word」. Changed the English dictionary used from 「http://www.answers.com/main/ntquery?s=�」 to 「http://www.thefreedictionary.com/�」.
;; version 1.0, 2011-11-14 First released to public.

;;; Code:




(defvar all-dictionaries nil "A vector of dictionaries. Used by `lookup-all-dictionaries'. http://wordyenglish.com/words/dictionary_tools.html ")
(setq all-dictionaries [
"http://www.dict.org/bin/Dict?Form=Dict2&Database=*&Query=�" ; 1913 Webster, WordNet
"http://www.thefreedictionary.com/�"                         ; AHD
"http://www.answers.com/main/ntquery?s=�"                    ; AHD
"http://en.wiktionary.org/wiki/�"
"http://www.google.com/search?q=define:+�" ; google
"http://www.etymonline.com/index.php?search=�" ; etymology
] )

(defun xah-asciify-region (&optional φfrom φto)
  "Change some Unicode characters into equivalent ASCII ones.
For example, “passé” becomes “passe”.

This function works on chars in European languages, and does not transcode arbitrary Unicode chars (such as Greek, math symbols).  Un-transformed unicode char remains in the string.

When called interactively, work on text selection or current line.
Version 2014-10-20"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((ξinputStr (buffer-substring-no-properties φfrom φto))
        (ξcharChangeMap [
                         ["á\\|à\\|â\\|ä\\|ã\\|å" "a"]
                         ["é\\|è\\|ê\\|ë" "e"]
                         ["í\\|ì\\|î\\|ï" "i"]
                         ["ó\\|ò\\|ô\\|ö\\|õ\\|ø" "o"]
                         ["ú\\|ù\\|û\\|ü"     "u"]
                         ["Ý\\|ý\\|ÿ"     "y"]
                         ["ñ" "n"]
                         ["ç" "c"]
                         ["ð" "d"]
                         ["þ" "th"]
                         ["ß" "ss"]
                         ["æ" "ae"]
                         ]))
    (let ((case-fold-search t))
        (save-restriction
          (narrow-to-region φfrom φto)
          (mapc
           (lambda (ξcurrentPair)
             (goto-char (point-min))
             (while (search-forward-regexp (elt ξcurrentPair 0) (point-max) t)
               (replace-match (elt ξcurrentPair 1))))
           ξcharChangeMap)))))

(defun xah-asciify-string (φstring)
  "Change some Unicode characters into equivalent ASCII ones.
For example, “passé” becomes “passe”.
See `xah-asciify-region'
Version 2014-10-20"
  (with-temp-buffer 
      (insert φstring)
      (xah-asciify-region (point-min) (point-max))
      (buffer-string)))

(defun lookup-word-on-internet (&optional φinput-word φsite-to-use)
  "Look up current word or text selection in a online reference site.
This command launches/switches you to default browser.

Optional argument φinput-word and φsite-to-use can be given.
φsite-to-use a is URL string in this form: 「http://en.wiktionary.org/wiki/�」.
the 「�」 is a placeholder for the query string.

If ΦSITE-TO-USE is nil, Google Search is used.

For a list of online reference sites, see:
 URL `http://ergoemacs.org/emacs/emacs_lookup_ref.html'"
  (interactive)
  (let (ξword ξrefUrl ξmyUrl)
    (setq ξword
          (if φinput-word
              φinput-word
            (if (region-active-p)
                (buffer-substring-no-properties (region-beginning) (region-end))
              (thing-at-point 'symbol))))

    (setq ξword (replace-regexp-in-string " " "%20" (xah-asciify-string ξword)))

    (setq ξrefUrl
          (if φsite-to-use
              φsite-to-use
            "http://www.google.com/search?q=�" ))

    (setq ξmyUrl (replace-regexp-in-string "�" ξword ξrefUrl t t))
    (cond
     ((string-equal system-type "windows-nt") ; any flavor of Windows
      (browse-url-default-windows-browser ξmyUrl))
     ((string-equal system-type "gnu/linux")
      (eww ξmyUrl))
     ((string-equal system-type "darwin") ; Mac
      (browse-url ξmyUrl)))))

(defun lookup-google (&optional φinput-word)
  "Lookup current word or text selection in Google Search.
See also `lookup-word-on-internet'."
  (interactive)
  (let ((ξdictUrl "http://www.google.com/search?q=�" ))
    (lookup-word-on-internet φinput-word ξdictUrl) ) )

(defun lookup-wikipedia (&optional φinput-word)
  "Lookup current word or text selection in Wikipedia.
See also `lookup-word-on-internet'."
  (interactive)
  (let ((ξdictUrl "http://en.wikipedia.org/wiki/�" ))
    (lookup-word-on-internet φinput-word ξdictUrl) ) )

(defun lookup-word-dict-org (&optional φinput-word)
  "Lookup definition of current word or text selection in URL `http://dict.org/'.
See also `lookup-word-on-internet'."
  (interactive)
  (let ((ξdictUrl "http://www.dict.org/bin/Dict?Form=Dict2&Database=*&Query=�" ))
    (lookup-word-on-internet φinput-word ξdictUrl)
    ) )

(defun lookup-word-definition (&optional φinput-word)
  "Lookup definition of current word or text selection in URL `http://thefreedictionary.com/'.
See also `lookup-word-on-internet'."
  (interactive)
  (let ((ξdictUrl "http://www.thefreedictionary.com/�") )
    (lookup-word-on-internet φinput-word ξdictUrl) ) )

(defun lookup-answers.com (&optional φinput-word)
  "Lookup current word or text selection in URL `http://answers.com/'.
See also `lookup-word-on-internet'."
  (interactive)
  (let ((ξdictUrl "http://www.answers.com/main/ntquery?s=�"
) )
    (lookup-word-on-internet φinput-word ξdictUrl) ) )

(defun lookup-wiktionary (&optional φinput-word)
  "Lookup definition of current word or text selection in URL `http://en.wiktionary.org/'
See also `lookup-word-on-internet'."
  (interactive)
  (let ((ξdictUrl "http://en.wiktionary.org/wiki/�" ))
    (lookup-word-on-internet φinput-word ξdictUrl) ) )

(defun lookup-all-dictionaries (&optional φinput-word)
  "Lookup definition in many dictionaries.
Current word or text selection is used as input.
The dictionaries used are in `all-dictionaries'.

See also `lookup-word-on-internet'."
  (interactive)
  (mapc (lambda (ξdictUrl) (lookup-word-on-internet φinput-word ξdictUrl)) all-dictionaries) )

(provide 'lookup-word-on-internet)
