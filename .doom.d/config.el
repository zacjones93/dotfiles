;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Zac Jones"
      user-mail-address "zacjones93@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-spacegrey)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/03.Resources/org-roam")


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq
 org_notes "~/Documents/03.Resources/org-roam"
 org-directory org_notes
 deft-directory org_notes
 org-roam-directory org_notes
 zot_bib "~/Documents/03.Resources/org-roam/masterLib.bib"
 +biblio-pdf-library-dir "~/Documents/03.Resources/org-roam/pdfs/"
 +biblio-default-bibliography-files '("~/Documents/03.Resources/org-roam/masterLib.bib")
 +biblio-notes-path "~/Documents/03.Resources/org-roam/"
 )
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
;;
;
;; Allows you to refile into different files - specifically to
;; create new 'parent' headings
(setq org-refile-use-outline-path 'file)
;; makes org-refile outline working with helm/ivy
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(defun +org/opened-buffer-files ()
  "Return the list of files currently opened in emacs"
  (delq nil
        (mapcar (lambda (x)
                  (if (and (buffer-file-name x)
                           (string-match "\\.org$"
                                         (buffer-file-name x)))
                      (buffer-file-name x)))
                (buffer-list))))

(setq org-refile-targets '((+org/opened-buffer-files :maxlevel . 9)))
;;


(setq org-roam-directory "~/Documents/03.Resources/org-roam")


(after! org-roam
  (setq org-roam-graph-viewer "/usr/bin/open")
  (setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+TITLE: ${title}
#+ROAM_KEY: ${ref}
- source :: ${ref}"
           :unnarrowed t)))
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t))))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-auto-save-interval -1.0)
  (deft-directory "~/Documents/03.Resources/org-roam"))

(use-package org-journal
      :bind
      ("C-c n j" . org-journal-new-entry)
      :custom
      (org-journal-dir "~/Documents/03.Resources/org-roam")
      (org-journal-date-prefix "#+TITLE: ")
      (org-journal-file-format "%Y-%m-%d.org")
      (org-journal-date-format "%A, %d %B %Y"))
    (setq org-journal-enable-agenda-integration t)

(after! org
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'orge-metaup))

;; provides some interactive functions which allows users to transpose windows arrangement in currently selected frame.
;; https://www.emacswiki.org/emacs/TransposeFrame
(require 'transpose-frame)

;;(require 'company-org-roam)
;;    (use-package company-org-roam
;;      :when (featurep! :completion company)
;;      :after org-roam
;;      :config
;;      (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))

(after! org-roam
        (map! :leader
            :prefix "n"
            :desc "org-roam" "l" #'org-roam
            :desc "org-roam-insert" "i" #'org-roam-insert
            :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
            :desc "org-roam-find-file" "f" #'org-roam-find-file
            :desc "org-roam-show-graph" "g" #'org-roam-show-graph
            :desc "org-roam-insert" "i" #'org-roam-insert
            :desc "org-roam-capture" "c" #'org-roam-capture))

(use-package org-roam-server
  :ensure t)

;;
;; org noter
(use-package! org-noter
  :after (:any org pdf-view)
  :config
    (setq
    ;; The WM can handle splits
     org-noter-notes-window-location 'horizontal-split
     ;; Please stop opening frames
     org-noter-always-create-frame nil
     ;; I want to see the whole file
     org-noter-hide-other nil
     ;; Everything is relative to the main notes file
     org-noter-notes-search-path (list org_notes)
   )
    )

(use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions#'org-noter-pdftools-jump-to-note)))

(sp-with-modes '(org-mode)
    (sp-local-pair "=" "="))

(use-package org-pomodoro
  :ensure t)

;;My Roam Capture Templates
  (setq org-roam-capture-templates
    '(
      ("d" "default" plain (function org-roam--capture-get-point)
        "%?"
        :file-name "${slug}"
        :head "#+title: ${title}\n#+created: %u\n#+last_modified: %U\n#+roam_alias: \n#+roam_tags: \n"
        :unnarrowed t)

       ("b" "book" plain (function org-roam--capture-get-point)
        "%?"
        :file-name "${slug}"
        :head "#+title: ${title}\n#+roam_alias: \n#+roam_tags:\n:author:\n:medium: [[file:Books.org][Book]]\n:GENRE:"
        :unnarrowed t)

       ("l" "lesson" plain (function org-roam--capture-get-point)
        "%?"
        :file-name "egghead lesson: ${slug}"
        :head "#+title: egghead lesson: ${title}\n#+roam_alias: \n#+roam_tags:\"lesson\"\n:instructor:\n:medium: [[file:Lesson.org][Lesson]]\n:link:\n\n* General\n* Screen Setup\n* Lesson edits"
        :unnarrowed t)

        ("m" "meeting" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "#+title: ${title}\n:participants:\n* %<%m-%d-%Y  %H:%M>"
         :unnarrowed t
         :immediate-finish t))
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#21242b" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(custom-safe-themes
   (quote
    ("2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" default)))
 '(deft-auto-save-interval -1.0)
 '(deft-default-extension "org" t)
 '(deft-directory "~/Documents/03.Resources/org-roam")
 '(deft-recursive t)
 '(deft-use-filter-string-for-filename t)
 '(fci-rule-color "#5B6268")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(objed-cursor-color "#ff6c6b")
 '(org-emphasis-alist
   (quote
    (("*" bold)
     ("/" italic)
     ("_" underline)
     ("" default verbatim)
     ("" default verbatim)
     ("="
      (:foreground "black" :background "orange")
      verbatim)
     ("~" org-code verbatim)
     ("+"
      (:strike-through t)))))
 '(org-journal-date-format "%A, %d %B %Y")
 '(org-journal-date-prefix "#+TITLE: ")
 '(org-journal-dir "~/Documents/03.Resources/org-roam")
 '(org-journal-file-format "%Y-%m-%d.org")
 '(package-selected-packages
   (quote
    (tao-theme transpose-frame org-roam-server org-roam-bibtex org-ref org-pomodoro org-noter-pdftools org-journal cmake-ide)))
 '(pdf-view-midnight-colors (cons "#bbc2cf" "#282c34"))
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
