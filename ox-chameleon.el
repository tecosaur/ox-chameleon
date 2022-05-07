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
;; Package-Requires: ((emacs "26.3") (engrave-faces "0.1.0"))
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

(defvar engrave-faces-preset-styles)

(defvar ox-chameleon-snap-fgbg-to-bw nil
  "When non-nil, snap bg/fg colours to black/white when they're close.")

(defvar ox-chameleon-engrave-preset nil
  "An engrave-faces preset to use when generating stylings.
When set to nil, the current theme will be used.")

(defvar ox-chameleon--p nil
  "Used to indicate whether the current export is trying to blend in. Set just before being accessed.")

(defun ox-chameleon--install (orig-fun info)
  (setq ox-chameleon--p
        (when (equal (plist-get info :latex-class) "chameleon")
          (plist-put info :latex-class
                     (if (plist-get info :beamer-theme)
                         "beamer" org-latex-default-class))
          (unless (plist-get info :latex-engraved-theme)
            (plist-put info :latex-engraved-theme "t"))
          t))
  (funcall orig-fun info))
(advice-add 'org-export-install-filters :around #'ox-chameleon--install)

(defun ox-chameleon-org-latex-export (orig-fn info &optional template snippet?)
  (if (and ox-chameleon--p (not snippet?))
      (concat (funcall orig-fn info template snippet?)
              (ox-chameleon-generate-colourings info))
    (funcall orig-fn info template snippet?)))
(advice-add 'org-latex-make-preamble :around #'ox-chameleon-org-latex-export)

;; Require modes that provide faces used in `engrave-faces-generate-preset'.
(unless (interactive-p)
  (require 'highlight-numbers nil t)
  (require 'highlight-quoted nil t)
  (require 'rainbow-delimiters nil t))

(defun ox-chameleon-generate-colourings (info)
  (let ((engrave-faces-preset-styles (or ox-chameleon-engrave-preset
                                         (engrave-faces-generate-preset))))
    (concat
     "\n%% make document follow Emacs theme\n"
     (apply #'format "\n\\definecolor{obg}{HTML}{%s}\n\\definecolor{ofg}{HTML}{%s}\n"
            (ox-chameleon--generate-fgbg-colours))
     (ox-chameleon--generate-heading-colourings)
     (ox-chameleon--generate-text-colourings)
     (if (plist-get info :beamer-theme)
         (if (string-match-p "default$" (plist-get info :beamer-theme))
             (ox-chameleon--generate-beamer-colourings)
           (ox-chameleon--generate-beamer-themed-colourings))
       (concat "\n\\pagecolor{obg}\n\\color{ofg}\n"
               (ox-chameleon--generate-koma-structural-colourings)))
     (ox-chameleon--generate-src-colourings)
     "\n%% end customisations\n\n")))

(defun ox-chameleon--face-attr (face attr)
  (if-let ((spec (cdr (assoc face engrave-faces-preset-styles)))
           (value (plist-get spec attr)))
      value
    (when engrave-faces-preset-styles
      (message "ox-chameleon: %s %s not provided, falling back to current theme."
               face attr))
    (face-attribute face attr nil 'default)))

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

(defun ox-chameleon--generate-text-colourings ()
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

(defun ox-chameleon--generate-src-colourings ()
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

(defun ox-chameleon--generate-heading-colourings ()
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
