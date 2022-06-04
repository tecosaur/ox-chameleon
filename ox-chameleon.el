;;; ox-chameleon.el --- Make exports match your theme -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 TEC
;;
;; Author: TEC <https://github.com/tecosaur>
;; Maintainer: TEC <tec@tecosaur.com>
;; Created: August 15, 2021
;; Modified: August 15, 2021
;; Version: 0.0.1
;; Keywords: convenience faces
;; Homepage: https://github.com/tecosaur/ox-chameleon
;; Package-Requires: ((emacs "26.3") (engrave-faces "0.3.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Make exports match your theme
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'rx))

(require 'engrave-faces)

(defvar engrave-faces-current-preset-style)

(defvar ox-chameleon-snap-fgbg-to-bw nil
  "When non-nil, snap bg/fg colours to black/white when they're close.")

(defvar ox-chameleon-engrave-preset nil
  "An engrave-faces preset to use when generating stylings.
When set to nil, the current theme will be used.")

(defvar ox-chameleon--p nil
  "Used to indicate whether the current export is trying to blend in. Set just before being accessed.")

(defun ox-chameleon--install (orig-fun info)
  (setq ox-chameleon--p
        (let ((backend (cl-struct-slot-value 'org-export-backend 'name (plist-get info :back-end))))
          (pcase backend
            ('latex (when (equal (plist-get info :latex-class) "chameleon")
                       (plist-put info :latex-class
                                  (if (plist-get info :beamer-theme)
                                      "beamer" org-latex-default-class))
                       (unless (plist-get info :latex-engraved-theme)
                         (plist-put info :latex-engraved-theme "t"))
                       t))
            ('html (when (equal (plist-get info :html-content-class) "chameleon")
                     (plist-put info :html-content-class (concat "chameleon " (symbol-name (car custom-enabled-themes))))
                      (unless (plist-get info :html-engraved-theme)
                        (plist-put info :html-engraved-theme "t"))
                      t))
            (_ nil))))
  (funcall orig-fun info))
(advice-add 'org-export-install-filters :around #'ox-chameleon--install)

(defun ox-chameleon-org-html-export (orig-fn info)
  (when ox-chameleon--p
    (plist-put info :html-head-extra (ox-chameleon-generate-html-colourings info)))
  (funcall orig-fn info))
(advice-add 'org-html--build-head :around #'ox-chameleon-org-html-export)

(defun ox-chameleon-org-latex-export (orig-fn info &optional template snippet?)
  (if (and ox-chameleon--p (not snippet?))
      (concat (funcall orig-fn info template snippet?)
              (ox-chameleon-generate-latex-colourings info))
    (funcall orig-fn info template snippet?)))
(advice-add 'org-latex-make-preamble :around #'ox-chameleon-org-latex-export)

;; Require modes that provide faces used in `engrave-faces-generate-preset'.
(unless (interactive-p)
  (require 'highlight-numbers nil t)
  (require 'highlight-quoted nil t)
  (require 'rainbow-delimiters nil t))

(defun ox-chameleon-generate-latex-colourings (info)
  (let ((engrave-faces-current-preset-style (or ox-chameleon-engrave-preset
                                                (engrave-faces-get-theme t))))
    (concat
     "\n%% make document follow Emacs theme\n"
     (apply #'format "\n\\definecolor{obg}{HTML}{%s}\n\\definecolor{ofg}{HTML}{%s}\n"
            (ox-chameleon--generate-fgbg-colours))
     (ox-chameleon--generate-latex-heading-colourings)
     (ox-chameleon--generate-latex-text-colourings)
     (if (plist-get info :beamer-theme)
         (if (string-match-p "default$" (plist-get info :beamer-theme))
             (ox-chameleon--generate-beamer-colourings)
           (ox-chameleon--generate-beamer-themed-colourings))
       (concat "\n\\pagecolor{obg}\n\\color{ofg}\n"
               (ox-chameleon--generate-koma-structural-colourings)))
     (ox-chameleon--generate-latex-src-colourings)
     "\n%% end customisations\n\n")))

(defun ox-chameleon--face-attr (face attr &optional no-default)
  (if-let ((spec (cdr (assoc face engrave-faces-current-preset-style)))
           (value (plist-get spec attr)))
      value
    (when (and engrave-faces-current-preset-style
               (not no-default))
      (message "ox-chameleon: %s %s not provided, falling back to current theme."
               face attr)
      (face-attribute face attr nil 'default))))

(defun ox-chameleon--hex-to-srgb (hex)
  (mapcar (lambda (range) (/ (string-to-number (apply #'substring hex range) 16) 255.0))
          '((1 3) (3 5) (5 7))))

(defun ox-chameleon--generate-fgbg-colours ()
  (mapcar (lambda (hex) (substring hex 1))
          (let ((bg (ox-chameleon--face-attr 'default :background))
                (fg (ox-chameleon--face-attr 'default :foreground)))
            (if ox-chameleon-snap-fgbg-to-bw
                (cl-destructuring-bind ((hb sb lb) (hf sf lf))
                    (list (apply #'color-rgb-to-hsl (ox-chameleon--hex-to-srgb bg))
                          (apply #'color-rgb-to-hsl (ox-chameleon--hex-to-srgb fg)))
                  (list (if (and (> lb 0.95) (< (* sb (- 1 lb)) 0.01)) "#ffffff" bg)
                        (if (and (< lf 0.4) (< (* sf lf) 0.1)) "#000000" fg)))
              (list bg fg)))))

(defun ox-chameleon-generate-html-colourings (info)
  "Generate the style tag to be inserted into the html <head>."
  (let ((engrave-faces-current-preset-style (or ox-chameleon-engrave-preset
                                                (engrave-faces-get-theme t))))
    (require 'engrave-faces-html)
    (concat "<style>"
            (ox-chameleon--generate-html-root-style)
            "body { background: var(--bg); color: var(--fg); font-family: var(--variable-pitch-font);}"
            "pre { font-family: var(--fixed-pitch-font);}"
            (ox-chameleon--generate-html-heading-style)
            (ox-chameleon--generate-html-code-style)
            (ox-chameleon--face-to-css 'link "a")
            (ox-chameleon--face-to-css 'link-visited "a:visited")
            (ox-chameleon--face-to-css 'highlight "a:hover")
            "</style>")))

(defun ox-chameleon--generate-html-root-style ()
  (concat
   ":root {"
   (apply #'format "--bg: #%s;\n--fg: #%s;\n" (ox-chameleon--generate-fgbg-colours))
   (format "--variable-pitch-font: '%s';\n--fixed-pitch-font: '%s';\n}"
           (ox-chameleon--face-attr 'variable-pitch :family)
           (ox-chameleon--face-attr 'default :family))))

(defun ox-chameleon--generate-html-heading-style ()
  (concat
   (ox-chameleon--face-to-css 'outline-1 "h1")
   (string-join
    (cl-loop for i from 2 to 5
             collect (ox-chameleon--face-to-css
                       (intern (format "outline-%s" (- i 1)))
                       (format "h%s" (- i 1)))))))

(defun ox-chameleon--generate-html-rainbow-parens ()
  (when (require 'rainbow-delimiters nil t)
    (string-join
     (cl-loop for i from 1 to 9
              collect (ox-chameleon--face-to-css
                       (intern (format "rainbow-delimiters-depth-%s-face" i))
                       (format ".org-rainbow-delimiters-depth-%s" i))))))

(defun ox-chameleon--generate-html-code-style ()
  (concat
   (ox-chameleon--face-to-css 'org-block ".org-src-container")
   (ox-chameleon--face-to-css 'highlight-quoted-symbol ".org-highlight-quoted-symbol")
   (ox-chameleon--face-to-css 'highlight-quoted-quote ".org-highlight-quoted-quote")
   (ox-chameleon--face-to-css 'highlight-numbers-number ".org-highlight-numbers-number")
   (ox-chameleon--generate-html-code-style-font-lock)
   (ox-chameleon--generate-html-rainbow-parens)
   (ox-chameleon--generate-html-block-names)))

(defun ox-chameleon--generate-html-block-names ()
  (string-join
   (mapcar (lambda (lang)
             (if-let ((symbols (with-temp-buffer
                                 (org-mode)
                                 prettify-symbols-alist)))
                 (let ((begin (alist-get "#+begin_src" symbols nil nil #'string=))
                       (end (alist-get "#+end_src" symbols nil nil #'string=)))
                   (format
                    "pre.src-%s::before { content: '%s %s'; display: block; %s }
                     pre.src-%s::after  { content: '%s'; display: block; %s }"
                    (symbol-name (car lang))
                    begin
                    (symbol-name (car lang))
                    (ox-chameleon--face-to-css 'org-block-begin-line)
                    (symbol-name (car lang))
                    end
                    (ox-chameleon--face-to-css 'org-block-end-line)))
               (format
                "pre.src-%s::before { content: '%s'; display: block; %s }"
                (symbol-name (car lang))
                (replace-regexp-in-string "-" " " (capitalize (symbol-name (car lang))))
                (ox-chameleon--face-to-css 'org-block-begin-line))))
           org-babel-load-languages)))

(defun ox-chameleon--generate-html-code-style-font-lock ()
  (string-join
   (mapcar (lambda (face)
             (ox-chameleon--face-to-css
              face
              (format ".org-%s" (substring (symbol-name face) 10 -5))))
           '(font-lock-comment-face
             font-lock-comment-delimiter-face
             font-lock-string-face
             font-lock-doc-face
             font-lock-doc-markup-face
             font-lock-keyword-face
             font-lock-builtin-face
             font-lock-function-name-face
             font-lock-variable-name-face
             font-lock-type-face
             font-lock-constant-face
             font-lock-warning-face
             font-lock-negation-char-face
             font-lock-preprocessor-face))))

(defun ox-chameleon--face-to-css (face &optional selector)
  (let ((pre (if selector (format "%s {" selector) ""))
        (post (if selector "}" "")))
    (concat pre
            (string-join
             (cl-map 'list (lambda (attr)
                             (let ((val (ox-chameleon--face-attr face (car attr) t)))
                               (when (engrave-faces--check-nondefault (car attr) val)
                                 (format "%s: %s;" (cdr attr)
                                         (if (eq :weight (car attr))
                                             (engrave-faces-html--css-weight val)
                                           val)))))
                     '((:foreground . "color")
                       (:background . "background")
                       (:weight . "font-weight")
                       (:family . "font-family"))))
            post)))

(defun ox-chameleon--generate-latex-text-colourings ()
  (apply #'format
         "
%% textual elements

\\definecolor{link}{HTML}{%s}
\\colorlet{url}{link}
\\definecolor{cite}{HTML}{%s}
\\definecolor{itemlabel}{HTML}{%s}
\\definecolor{code}{HTML}{%s}
\\definecolor{verbatim}{HTML}{%s}

\\DeclareTextFontCommand{\\texttt}{\\color{code}\\ttfamily}

\\let\\oldverb\\verb
\\def\\verb{\\bgroup\\color{verbatim}\\oldverb}
\\makeatletter
\\let\\verb@oldegroup\\verb@egroup
\\def\\verb@egroup{\\verb@oldegroup\\egroup}
\\makeatother
"
         (mapcar (lambda (hex) (substring hex 1))
                 (list
                  (ox-chameleon--face-attr 'org-link :foreground)
                  (ox-chameleon--face-attr 'org-cite :foreground)
                  (ox-chameleon--face-attr 'org-list-dt :foreground)
                  (ox-chameleon--face-attr 'org-code :foreground)
                  (ox-chameleon--face-attr 'org-verbatim :foreground)))))

(defun ox-chameleon--generate-latex-src-colourings ()
  (apply #'format
         "
%% code blocks

\\definecolor{codebackground}{HTML}{%s}
\\colorlet{EFD}{ofg}
\\definecolor{codeborder}{HTML}{%s}
"
         (mapcar (lambda (hex) (substring hex 1))
                 (list (ox-chameleon--face-attr 'org-block :background)
                       (if (featurep 'doom-themes)
                           (doom-blend (ox-chameleon--face-attr 'org-block :background)
                                       (ox-chameleon--face-attr 'default :foreground)
                                       0.95)
                         (ox-chameleon--face-attr 'shadow :foreground))))))

(defun ox-chameleon--generate-latex-heading-colourings ()
  (apply #'format
         "
%% heading colours
\\definecolor{documentTitle}{HTML}{%s}
\\definecolor{documentInfo}{HTML}{%s}
\\definecolor{level1}{HTML}{%s}
\\definecolor{level2}{HTML}{%s}
\\definecolor{level3}{HTML}{%s}
\\definecolor{level4}{HTML}{%s}
\\definecolor{level5}{HTML}{%s}
\\definecolor{level6}{HTML}{%s}
\\definecolor{level7}{HTML}{%s}
\\definecolor{level8}{HTML}{%s}
"
         (mapcar (lambda (hex) (substring hex 1))
                 (list
                  (ox-chameleon--face-attr 'org-document-title :foreground)
                  (ox-chameleon--face-attr 'org-document-info :foreground)
                  (ox-chameleon--face-attr 'outline-1 :foreground)
                  (ox-chameleon--face-attr 'outline-2 :foreground)
                  (ox-chameleon--face-attr 'outline-3 :foreground)
                  (ox-chameleon--face-attr 'outline-4 :foreground)
                  (ox-chameleon--face-attr 'outline-5 :foreground)
                  (ox-chameleon--face-attr 'outline-6 :foreground)
                  (ox-chameleon--face-attr 'outline-7 :foreground)
                  (ox-chameleon--face-attr 'outline-8 :foreground)))))

(defun ox-chameleon--generate-beamer-structural-colourings ()
  (format
   "
"))

(defun ox-chameleon--generate-koma-structural-colourings ()
  (format
   "
%% structural elements

\\addtokomafont{title}{\\color{documentTitle}}
\\addtokomafont{author}{\\color{documentInfo}}
\\addtokomafont{date}{\\color{documentInfo}}
\\addtokomafont{section}{\\color{level1}}
\\newkomafont{sectionprefix}{\\color{level1}}
\\addtokomafont{subsection}{\\color{level2}}
\\newkomafont{subsectionprefix}{\\color{level2}}
\\addtokomafont{subsubsection}{\\color{level3}}
\\newkomafont{subsubsectionprefix}{\\color{level3}}
\\addtokomafont{paragraph}{\\color{level4}}
\\newkomafont{paragraphprefix}{\\color{level4}}
\\addtokomafont{subparagraph}{\\color{level5}}
\\newkomafont{subparagraphprefix}{\\color{level5}}

%% list labels

\\renewcommand{\\labelitemi}{\\textcolor{itemlabel}{\\textbullet}}
\\renewcommand{\\labelitemii}{\\textcolor{itemlabel}{\\normalfont\\bfseries \\textendash}}
\\renewcommand{\\labelitemiii}{\\textcolor{itemlabel}{\\textasteriskcentered}}
\\renewcommand{\\labelitemiv}{\\textcolor{itemlabel}{\\textperiodcentered}}

\\renewcommand{\\labelenumi}{\\textcolor{itemlabel}{\\theenumi.}}
\\renewcommand{\\labelenumii}{\\textcolor{itemlabel}{(\\theenumii)}}
\\renewcommand{\\labelenumiii}{\\textcolor{itemlabel}{\\theenumiii.}}
\\renewcommand{\\labelenumiv}{\\textcolor{itemlabel}{\\theenumiv.}}
"
   (substring (ox-chameleon--face-attr 'org-list-dt :foreground) 1)))

(defun ox-chameleon--generate-beamer-themed-colourings ()
  (let ((builtin-fg (substring (ox-chameleon--face-attr 'font-lock-builtin-face :foreground) 1))
        (varname-fg (substring (ox-chameleon--face-attr 'font-lock-variable-name-face :foreground) 1))
        (link-fg (substring (ox-chameleon--face-attr 'org-link :foreground) 1))
        (ol-3-fg (substring (ox-chameleon--face-attr 'outline-3 :foreground) 1))
        (default-fg (substring (ox-chameleon--face-attr 'default :foreground) 1)))
  (format
   "
%% beamer

\\definecolor{builtin}{HTML}{%s}
\\definecolor{varname}{HTML}{%s}

\\NewCommandCopy{\\oldusetheme}{\\usetheme}
\\renewcommand*{\\usetheme}[2][]{\\oldusetheme[#1]{#2}
  \\setbeamercolor{title separator}{fg=documentTitle}
  \\setbeamercolor{progress bar}{fg=documentTitle}
  \\setbeamercolor{progress bar in head/foot}{fg=documentTitle}
  \\setbeamercolor{progress bar in section page}{fg=documentTitle}

  \\setbeamercolor{normal text}{fg=ofg, bg=obg}
  \\setbeamercolor{alerted text}{fg=%s}
  \\setbeamercolor*{item}{fg=itemlabel}
%s
}

\\usepackage{etoolbox}
\\makeatletter
\\patchcmd{\\beamer@section}{%%
  \\edef\\insertsectionhead{\\noexpand\\hyperlink{Navigation\\the\\c@page}{\\unexpanded{#1}}}}{%%
  \\edef\\insertsectionhead{\\begingroup\\noexpand\\hypersetup{hidelinks}\\noexpand\\hyperlink{Navigation\\the\\c@page}{\\unexpanded{#1}}\\endgroup}}
\\makeatother
"
   builtin-fg
   varname-fg
   (if (or (string= builtin-fg ol-3-fg)
           (string= builtin-fg link-fg)
           (string= builtin-fg default-fg))
       "varname" "builtin")
   (pcase org-beamer-theme
     ((rx "metropolis" line-end)
      (format
       "  \\setbeamercolor{block title}{fg=%s, bg=}"
       (if (string= builtin-fg ol-3-fg)
           "level1" "level3")))
     (_ "  \\setbeamercolor{title}{fg=documentTitle, bg=obg}
  \\setbeamercolor{titlelike}{fg=ofg, bg=obg}
  \\setbeamercolor{section title}{fg=level1, bg=obg}
  \\setbeamercolor{frametitle}{fg=level2, bg=ofg!15!obg}
  \\setbeamercolor{block title}{fg=level3, bg=}")))))

(defun ox-chameleon--generate-beamer-colourings ()
  (format
   "
%% beamer

\\definecolor{builtin}{HTML}{%s}

\\setbeamercolor{title}{fg=documentTitle, bg=}
\\setbeamercolor{titlelike}{fg=ofg, bg=}
\\setbeamercolor{section title}{fg=level1, bg=}
\\setbeamercolor{frametitle}{fg=level2, bg=}
\\setbeamercolor{block title}{fg=ofg, bg=}

\\setbeamercolor{title separator}{fg=builtin}
\\setbeamercolor{progress bar}{fg=builtin}

\\setbeamercolor{normal text}{fg=ofg, bg=obg}
\\setbeamercolor{alerted text}{fg=builtin}%s
\\setbeamercolor*{item}{fg=itemlabel}

\\setbeamercolor{navigation symbols}{fg=ofg!50!obg, bg=ofg!30!obg}
"
   (substring (ox-chameleon--face-attr 'font-lock-builtin-face :foreground) 1)
   (if (eq 'italic (ox-chameleon--face-attr 'font-lock-builtin-face :slant))
       "\n\\setbeamerfont{alerted text}{series=\\bfseries}"
     "")))

(provide 'ox-chameleon)
;;; ox-chameleon.el ends here
