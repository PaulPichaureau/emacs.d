;; ----------------------------------------------------------------
;; init.el de Paul Pichaureau
;; ----------------------------------------------------------------

;; fait de suite pour un plus joli démarrage

;; Le titre de la fenêtre : nom du buffer (nom du fichier)
(setq frame-title-format '(buffer-file-name "Emacs (%f)" "%b"))
;; vire barre de menu graphique
(tool-bar-mode -1)
;; Surtout pas de son !
(setq visible-bell t)

;; Pas de barre de défilement
;; (setq scroll-bar-mode -1)


;; Démarrage du serveur emacs
(server-start)

;; Qui suis-je ?
(setq user-full-name "Paul Pichaureau")
(setq user-mail-address "paul.pichaureau@alcandre.net")

;;   LOGNAME and USER are expected in many Emacs packages
;;   Check these environment variables.

(if (and (null (getenv "USER"))
         ;; Windows includes variable USERNAME, which is copied to
         ;; LOGNAME and USER respectively.
         (getenv "USERNAME"))
    (setenv "USER" (getenv "USERNAME")))

(if (and (getenv "LOGNAME")
         ;;  Bash shell defines only LOGNAME
         (null (getenv "USER")))
    (setenv "USER" (getenv "LOGNAME")))

(if (and (getenv "USER")
         (null (getenv "LOGNAME")))
    (setenv "LOGNAME" (getenv "USER")))

;; -------------------------------------------------------------------
;; Gestion des chemins
;; -------------------------------------------------------------------

(defvar drive-letter (substring data-directory 0 3))
(defvar home-dir (concat drive-letter "Paul/"))
(defvar user-init-dir "~/.emacs.d/"
  "* Le répertoire racine de tous mes fichiers concernant Emacs.")

(setenv "HOME" home-dir)
(setenv "TEMP" (concat home-dir "tmp/"))
(setenv "TMPDIR" (concat home-dir "tmp/"))

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/elpa//")

(defun cuid (FILENAME)
  "* Tous les fichiers personnels sont stockés relativement au
répertoire d'installation `user-init-dir'.  Utiliser cette
fonction pour définir un répertoire/fichier relatif.  Attention
`user-init-dir' se termine par un /"
  (concat user-init-dir FILENAME))

(setq load-path (append load-path (list (cuid "etc")
                                        (cuid "site-lisp")
                                        )))

;; Pour avoir tous les fichiers de backup dans un seul répertoire
(defun make-backup-file-name (file)
  (concat (cuid "backup/") (file-name-nondirectory file) "~"))

;;
;; (add-to-list 'package-archives
             ;; '("marmalade" . "http://marmalade-repo.org/packages/"))

;; packages installés par ELPA
(package-initialize)

;; -------------------------------------------------------------------
;; Cygwin
;; Permet d'accèder aux binaires de cygwin depuis emacs
;; Remplacer c:/cygwin par le repertoire ou cygwin est installé

;; (setq cygwinpath (concat drive-letter "cygwin/bin"))
;; (setenv "PATH" (concat cygwinpath (getenv "PATH")))
;; (setq exec-path (cons cygwinpath exec-path))

;; (require 'cygwin-mount)
;; (cygwin-mount-activate)

;; (add-to-list 'Info-default-directory-list
;;              (concat drive-letter "cygwin/info"))

;; ;; M-x shell: This change M-x shell permanently

;; ;; Would call Windows command interpreter. Change it.

;; (setq shell-file-name "bash")
;; (setenv "SHELL" shell-file-name)
;; (setq explicit-shell-file-name shell-file-name)

;; Remove C-m (^M) characters that appear in output
(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

;; (setq w32-quote-process-args ?\")


;; -------------------------------------------------------------------
;; Unicode

(require 'mule)
;; Environnement utf-8
(setq default-file-name-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")   ;;  prefer utf-8 for language settings
(set-input-method nil)               ;; no funky input for normal editing;

;; -------------------------------------------------------------------
;; Paramètrage propre à Windows

;; Pour avoir l'affichage des italiques
;; doit être positionné avant toute commande définissant les polices
;; (setq w32-enable-synthesized-fonts t)

;; Utilise la sélection normale des polices de windows
(setq w32-use-w32-font-dialog t)

;; pour gagner deux touches
(setq w32-pass-rwindow-to-system nil)
;; (setq w32-pass-lwindow-to-system nil)

;; *******************************************************************

;; -------------------------------------------------------------------
;; Apparence
;; -------------------------------------------------------------------



;; texte en couleurs
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t
      font-lock-maximum-size nil)

(if (functionp 'global-hi-lock-mode)
    (global-hi-lock-mode 1)
  (hi-lock-mode 1))

(setq scroll-margin 1                    ; do smooth scrolling, ...
      scroll-conservatively 101)

(defun set-frame-size-according-to-resolution ()
  (interactive)
  ;; (if (display-graphic-p)
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    ;; (if (> (x-display-pixel-width) 1280)
    ;;        (add-to-list 'default-frame-alist (cons 'width 80120))
    (add-to-list 'default-frame-alist (cons 'width 88))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist
         (cons 'height (/ (- (x-display-pixel-height) 96)
                             (frame-char-height))))))

;; Couleurs, etc
(add-to-list 'custom-theme-load-path (cuid "themes"))


;; -------------------------------------------------------------------
;; Réglages divers
;; -------------------------------------------------------------------

;; Activer la mise en exergue des espaces superflus ou hétérogènes
;; (require 'whitespace)

;; Supprimer les espaces superflus avant sauvegarde
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

(setq kill-whole-line t ;; Kill EOL too
      ;; Ignorer certaines extensions
      completion-ignored-extensions
      (append completion-ignored-extensions
              '(".ilg" ".ind" ".pdf" ".tui" ".tuo" ".ted" ".tmp"
                ".orig" ".ps" ".mpo" "mpd" ".log" ".aux" ".toc"
                ".toe"  ".blg" ".ilg" ".ist" ".out" ".table" ".bib"
                ".idx" ".ind" ".log" ".synctex" ".toc" ".dvi" ".gnuplot" ".docx"
                ".1" ".2" ".3" ".4" ".5" ".6" ".7" ".8" ".9"))

      ;; Paste at point NOT at cursor
      mouse-yank-at-point t
      ;; vire les anciennes versions des fichiers
      delete-old-versions t
      ;; utiliser la poubelle de windows
      delete-by-moving-to-trash t
      ;; virer les messages d'accueils
      ;; initial-scratch-message nil
      inhibit-startup-message t          ; don't show ...
      ;; inhibit-startup-echo-area-message t
      completion-cycle-threshold 5      ;; la completion cycle parmi
                                        ;; les choix s'il y en a moins
                                        ;; que 5
      redisplay-dont-pause t ;; une amélioration de la performance
      completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      )

(setq-default transient-mark-mode t)
;; Les tabulations sont remplacées par des espaces, important !
(setq-default indent-tabs-mode nil)

;; Pour recharger automatiquement un fichier modifié par un programme
;; extérieur
(global-auto-revert-mode 1)

;; on en a marre d'ecrire yes, ou no en entier. Un y ou un n suffit
(fset 'yes-or-no-p 'y-or-n-p)

;; remplacer le texte selectionné
(delete-selection-mode 1)

;; bookmark
(defvar bookmark-file (cuid ".emacs.bmk"))
(setq bookmark-save-flag 1)

;; Prise en compte des abréviations
;; (setq-default abbrev-mode t)
(setq save-abbrevs t)
(setq abbrev-file-name (convert-standard-filename (cuid ".abbrev_defs")))
(read-abbrev-file)

;; On se met en mode Text pour une grande partie des fichiers.
(setq major-mode 'text-mode)

;; Postscript
(setq auto-mode-alist (cons '("\\.pt1$" . ps-mode)
                            auto-mode-alist))

;; Python
(autoload 'python-mode "python-mode" "Python editing mode." t)
(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.cmd$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
            interpreter-mode-alist))

(font-lock-add-keywords 'python-mode
                        '(("[=<>*+()!-/]" .  'font-lock-variable-name-face)))

(font-lock-add-keywords 'python-mode
                        '(("[0-9\\.]*" .  'font-lock-constant-face)))

(defun my-python-goodies ()
  (show-paren-mode 1)
  (reveal-mode 1)
  ;; add some local hooks
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
  (add-hook 'after-save-hook 'semantic-fetch-tags nil t)
  (unless (or (file-exists-p "makefile")
              (file-exists-p "Makefile"))
    (set (make-local-variable 'compile-command)
         (concat "python " buffer-file-name)))
  )

(add-hook 'python-mode-hook 'my-python-goodies)

;; Pour charger mes scripts pythons
(setenv "PATH" (concat "~/Python/;" (getenv "PATH")))


;; Metapost/metafont
(setq auto-mode-alist
      (cons '("\\.mp$" . metapost-mode) auto-mode-alist))

(setq auto-mode-alist
      (cons '("\\.mf$" . metafont-mode) auto-mode-alist))

;; scilab
(load "~/.emacs.d/site-lisp/scilab.el")
(setq auto-mode-alist (cons '("\\(\\.sci$\\|\\.sce$\\)" . scilab-mode) auto-mode-alist))
(setq scilab-mode-hook '(lambda () (setq fill-column 74)))


;;-------------------------------------------------------------------
;; Différents modes et packages externes

;;ido
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)
;; (setq ido-ignore-extensions t)
;; (setq ido-file-extensions-order '(".tex" ".txt" ".py" ".log" ".aux" ".el" ".ini" ))

;;recentf
(require 'recentf)
(setq recentf-max-saved-items 500
      recentf-max-menu-items 60
      recentf-save-file  (cuid "recentf"))
(recentf-mode 1)

;; show recent open at startup
(add-hook 'emacs-startup-hook 'recentf-open-files)

;; Pour se souvenir des commandes d'une session sur l'autre
(require 'save-history)
(setq save-history-file (cuid ".emacs-histories"))

;; saveplace: save location in file when saving files
(setq save-place-file (cuid "saveplace"))
(setq-default save-place t)            ;; activate it for all buffers
(setq save-place-file (cuid "saveplace.el") )
(require 'saveplace)                   ;; get the package


;;-------------------------------------------------------------------
;; Pour (La)TeX : auctex

(setenv "PATH"
        (concat (concat drive-letter "texmf/miktex/bin;")
                (concat drive-letter "localtexmf/miktex/bin;")
                (concat drive-letter "Programmes/gnuplot/binary;")
                (concat drive-letter "Programmes/gs/gsview;")
                (getenv "PATH")))

(add-to-list 'load-path (cuid "site-lisp/auctex/"))
(add-to-list 'load-path (cuid "site-lisp/site-start.d/"))
(add-to-list 'Info-default-directory-list (cuid "info"))

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(require 'tex-mik)
;; (require 'sumatra-forward)

;; Pour couper les lignes proprement
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

;; Facilite la saisie des maths
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; Correction automatique (pas sûr pour celui-ci)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; (eval-after-load "flyspell"
;; '(define-key flyspell-mode-map [down-mouse-3] 'flyspell-correct-word))


(setq hilit-AmSLaTeX-commands t
      hilit-multilingual-strings t
      LaTeX-math-abbrev-prefix "\"")

(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(setq reftex-plug-into-AUCTeX t)

;; AUCTeX : reglages divers
(setq tex-default-mode 'latex-mode)     ; mode latex par defaut

(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))
(add-to-list 'auto-mode-alist '("\\.mtx\\'" . latex-mode))

(add-hook 'LaTeX-mode-hook 'my-latex-environments)

(defun my-latex-environments ()
  (LaTeX-add-environments
   '("itemized" LaTeX-env-item)
   '("listsujet" LaTeX-env-item)
   '("Exemple" LaTeX-env-item)
   '("Remarque" LaTeX-env-item)))

;; Pour faire les changements \'e <-> é
(require 'texaccents)
(require 'accents)
(require 'genconv)

(setq TeX-command-default "PDFLaTeX")

;; Utilisation du visualiseur pdf Sumatra
(setq TeX-PDF-mode t
      TeX-source-correlate-mode t
      TeX-source-correlate-method 'synctex)

(setq TeX-view-program-list
   '(("Sumatra PDF" ("\"SumatraPDF.exe\" -reuse-instance"
                      (mode-io-correlate " -forward-search %b %n ") " %o"))))

(eval-after-load 'tex
  '(progn
     (assq-delete-all 'output-pdf TeX-view-program-selection)
     (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))))

;; (setq TeX-view-program-list
  ;; '(("Sumatra PDF" ("SumatraPDF.exe -reuse-instance"
                      ;; ;; (mode-io-correlate " -forward-search %b %n ") " %o"))))


;; Commandes du menu de AUC TeX
;; The MikTeX commands.
(setq TeX-command-list
      (list (list "TeX" "tex --shell-escape --src-specials --interaction nonstopmode  %t" 'TeX-run-TeX nil t)
            (list "LaTeX" "latex --shell-escape --src-specials --interaction nonstopmode %t" 'TeX-run-LaTeX nil t)
            (list "PDFLaTeX" "pdflatex -synctex=-1 --shell-escape --src-specials  --interaction nonstopmode %t"
                  'TeX-run-LaTeX nil t)
            (list "latex2wiki" "latex2wiki.cmd %t" 'TeX-run-command nil t)
            (list "Dvips" "dvips %d -o %f" 'TeX-run-command nil t)
            ;; (list "ps2pdf" "ps2pdf  %f" 'TeX-run-command nil t)
            (list "yap" "yap -1 %s" 'TeX-run-discard nil nil)
            (list "View" "SumatraPDF.exe -reuse-instance -forward-search %b %n  %o"  'TeX-run-discard nil nil)
            (list "Sumatra" "SumatraPDF.exe  %o"'TeX-run-discard nil nil)
            ;; (list "Print" "gsview32 -p %s.ps" 'TeX-run-command t nil)
            ;; (list "File" "dvips %d -o %f " 'TeX-run-command t nil)
            (list "BibTeX" "bibtex %s" 'TeX-run-BibTeX nil nil)
            (list "Index" "makeindex %s" 'TeX-run-command nil t)
                        (list "Check" "lacheck %s" 'TeX-run-compile nil t)
            ;;             (list "Other" "" 'TeX-run-command t t)
            ))

;; (assq-delete-all 'output-pdf  TeX-view-program-selection)
;; (add-to-list 'TeX-view-program-selection   '(output-pdf "Sumatra PDF"))


;; Support pour tikz

(add-to-list 'load-path (cuid "site-lisp/auto-overlays/"))
(load "~/.emacs.d/site-lisp/auc-tikz.el")



;; -------------------------------------------------------------------
;; ispell : aspell

(setq-default ispell-program-name "P:/Programmes/aspell/bin/aspell")
(setq ispell-dictionary "francais")
(setq ispell-personal-dictionary "~/.emacs.d/personnal-dic")

(add-hook 'LaTeX-mode-hook '(lambda ()
  (LaTeX-math-mode)

  (setq LaTeX-section-hook;I don't want to put useless labels everywhere
	'(LaTeX-section-heading
	  LaTeX-section-title
	  LaTeX-section-section))

  (setq ispell-tex-skip-alists
        ;; tell ispell to ignore content of this
    (list
     (append
      (car ispell-tex-skip-alists) ; commands
      '(("\\\\cite"            ispell-tex-arg-end)
        ("\\\\nocite"          ispell-tex-arg-end)
        ("\\\\includegraphics" ispell-tex-arg-end)
        ("\\\\bibliography"    ispell-tex-arg-end)
        ("\\\\ref"             ispell-tex-arg-end)
        ("\\\\input"             ispell-tex-arg-end) ;
        ("\\\\code"            ispell-tex-arg-end) ;personal
        ("\\\\label"           ispell-tex-arg-end)))
     (append
      (cadr  ispell-tex-skip-alists) ; environment
      '(("equation\\*" . "\\\\end[ \n]*{[ \n]*equation\\*[ \n]*}")
        ("align\\*" . "\\\\end[ \n]*{[ \n]*align\\*[ \n]*}")
        ("gather\\*" . "\\\\end[ \n]*{[ \n]*gather\\*[ \n]*}")
        ("multline\\*" . "\\\\end[ \n]*{[ \n]*multline\\*[ \n]*}")
        ("[^\\]\\$" . "[^\\]\\$")
        ("tikzpicture" . "\\\\end[ \n]*{[ \n]*tikzpicture[ \n]*}")))))
  ))
;; -------------------------------------------------------------------
;; configure HTML editing
(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . html-mode))

(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level '2)
(add-hook 'php-mode-user-hook 'turn-on-font-lock)

;; display color in css file
(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background
                     (match-string-no-properties 0)))))))

(defun hexcolour-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords nil hexcolour-keywords))

(add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)


;; Pour utiliser Tidy, un bon "parser" HTML
(autoload 'tidy-buffer "tidy" "Run Tidy HTML parser on current buffer" t)
(autoload 'tidy-parse-config-file "tidy" "Parse the `tidy-config-file'" t)
(autoload 'tidy-save-settings "tidy" "Save settings to `tidy-config-file'" t)
(autoload 'tidy-build-menu  "tidy" "Install an options menu for HTML Tidy." t)

;; pour faire n'importe quoi
(require 'lorem-ipsum)


;; -------------------------------------------------------------------
;; Emacs powââââ....


;; (define-skeleton sk-enonce-entete
;;   "Insert LaTeX enonce header"
;;   ""
;;   "%% -------------------------------------------------------------------\n"
;;   "%%CONCOURS      "_"\n"
;;   "%%COMMENTAIRE   \n"
;;   "%%MOTS-CLES     \n"
;;   "%%DIFFICULTE    \n"
;;   "%%LABEL         \n"
;;   "%%ORIGINE       \n"
;;   "%%EPREUVE       \n"
;;   "%%GENRE         \n"
;;   "%%TITRE         \n"
;;   "%%FILIERE       \n"
;;   "%% -------------------------------------------------------------------\n"
;;   "\n\\begin{Exercise}\n\n\\end{Exercise}\n"
;;   "%% -------------------------------------------------------------------\n"
;;   "\n\\begin{Answer}\n\tPas de solution.\n\\end{Answer}\n"
;;   )

;; ----------------------------------------------------------------
;; Fonctions Lisp utiles

;; qd vous recevez un fichier .txt d'une machine dos/windows il est
;; tres enevervant de voir les ^M a chaque fin de ligne, utiliser M-x
;; dos2unix pour vous en debarrasser.
;; dos2unix
(defun dos-unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; l'inverse du precedent.
;; unix2dos
(defun unix-dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))


;; Pour faire toute une ligne de commentaire
(defun comment-full-line ()
  (interactive)
  (beginning-of-line)
  (insert "-------------------------------------------------------------------\n")
  (next-line -1)
  (comment-and-go-down))


(defun comment-full-big-line ()
  (interactive)
  (beginning-of-line)
  (insert "*******************************************************************\n")
  (next-line -1)
  (comment-and-go-down))


(add-hook 'after-save-hook
          (lambda ()
            (highlight-changes-remove-highlight (point-min) (point-max))))


;; -------------------------------------------------------------------
;; Version control system : git
(setq gitpath (concat drive-letter "Programmes/git/bin"))
(setenv "PATH" (concat gitpath ";" (getenv "PATH")))
(setq exec-path (cons gitpath exec-path))

(add-to-list 'load-path (cuid "site-lisp/magit/"))
(require 'magit)
(require 'magit-svn)

;; show git modif un gutter
(eval-after-load "git-gutter" '(load "git-gutter-fringe"))

(defun maybe-use-git-gutter ()
  "Run `git-gutter' if the current file is being tracked by git."
  (when (eq (vc-backend (buffer-file-name)) 'Git)
    (git-gutter)))

(add-hook 'after-save-hook 'maybe-use-git-gutter)
(add-hook 'after-change-major-mode-hook 'maybe-use-git-gutter)
(add-hook 'window-configuration-change-hook 'maybe-use-git-gutter)



;; highlight current line temporary
(require 'pulse)


;; Aspect du eshell
(defun my-buffer-face-mode-courrier ()
    (interactive)
    (setq buffer-face-mode-face '(:foreground "#93a1a1" :background "#002b36"))
    (buffer-face-mode))

'(font-lock-keyword-face ((t (:foreground "Navy" :slant normal :weight bold))))
(add-hook 'eshell-mode-hook 'my-buffer-face-mode-courrier)


;; Abbréviations avec yasnippet
(add-to-list 'load-path
              "~/.emacs.d/site-lisp/yasnippet")
(require 'yasnippet)
(yas/global-mode 1)
(yas/initialize)
(setq yas/root-directory "~/.emacs.d/snippet")
;; Load the snippets
(yas/load-directory yas/root-directory)

;; -------------------------------------------------------------------
;; utilisation d'org-mode

(setq org-agenda-files (list "~/journal.org"))
;; (setq initial-buffer-choice "~/journal.org")
;; Set to the location of your Org files on your local system
(setq org-directory "~/Documents/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/journal.org")
;; Set to <your Dropbox root directory>/MobileOrg.
;; (setq org-mobile-directory "~/Dropbox/MobileOrg")




;; -------------------------------------------------------------------
;; affiche toutes les polices disponibles (à évaluer dans *scratch*)
(dolist (font-family (font-family-list))
  (let ((str font-family))
    (insert (propertize (concat "The quick brown fox jumps over the lazy dog ("
     str ")") 'face `((:family ,font-family))))
    (newline)))

;; Polices intéressantes
;; (Consolas) (le défaut)
;; (Verdana)
;; (Book Antiqua) (défendable, belle italique)
;; (Segoe UI)
;; (Calibri)
;; (Cambria)
;; (Candara) (non, mais pour les chiffres 123456 tb)

;; Plus spécifiques
;; (Berlin Sans FB)
;; (Gill Sans Ultra Bold)
;; (Lithos Pro Regular)
;; (Segoe Print)

;; Police variable pour certains modes
;; (dolist (hook '(erc-mode-hook
                ;; LaTeX-mode-hook
                ;; text-mode-hook
                ;; org-mode-hook
                ;; edit-server-start-hook
                ;; markdown-mode-hook))
  ;; (add-hook hook (lambda () (variable-pitch-mode t))))



(defadvice kill-region (before slick-copy activate compile)
  "When called interactively with no active region, cut the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))


(defun shrink-whitespaces ()
  "Remove white spaces around cursor to just one or none.
If current line does not contain non-white space chars, then remove blank lines to just one.
If current line contains non-white space chars, then shrink any whitespace char surrounding cursor to just one space.
If current line is a single space, remove that space.

Calling this command 3 times will always result in no whitespaces around cursor."
  (interactive)
  (let (
        cursor-point
        line-has-meat-p  ; current line contains non-white space chars
        spaceTabNeighbor-p
        whitespace-begin whitespace-end
        space-or-tab-begin space-or-tab-end
        line-begin-pos line-end-pos
        )
    (save-excursion
      ;; todo: might consider whitespace as defined by syntax table, and also consider whitespace chars in unicode if syntax table doesn't already considered it.
      (setq cursor-point (point))

      (setq spaceTabNeighbor-p (if (or (looking-at " \\|\t") (looking-back " \\|\t")) t nil) )
      (move-beginning-of-line 1) (setq line-begin-pos (point) )
      (move-end-of-line 1) (setq line-end-pos (point) )
      ;;       (re-search-backward "\n$") (setq line-begin-pos (point) )
      ;;       (re-search-forward "\n$") (setq line-end-pos (point) )
      (setq line-has-meat-p (if (< 0 (count-matches "[[:graph:]]" line-begin-pos line-end-pos)) t nil) )
      (goto-char cursor-point)

      (skip-chars-backward "\t ")
      (setq space-or-tab-begin (point))

      (skip-chars-backward "\t \n")
      (setq whitespace-begin (point))

      (goto-char cursor-point)      (skip-chars-forward "\t ")
      (setq space-or-tab-end (point))
      (skip-chars-forward "\t \n")
      (setq whitespace-end (point))
      )

    (if line-has-meat-p
        (let (deleted-text)
          (when spaceTabNeighbor-p
            ;; remove all whitespaces in the range
            (setq deleted-text (delete-and-extract-region space-or-tab-begin space-or-tab-end))
            ;; insert a whitespace only if we have removed something
            ;; different that a simple whitespace
            (if (not (string= deleted-text " "))
                (insert " ") ) ) )

      (progn
        ;; (delete-region whitespace-begin whitespace-end)
        ;; (insert "\n")
        (delete-blank-lines)
        )
      todo: possibly code my own delete-blank-lines here for better efficiency, because delete-blank-lines seems complex.
      )
    )
  )

;; iswitch : Using the arrow keys to select a buffer
(defun iswitchb-local-keys ()
  (mapc (lambda (K)
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
        '(("<right>" . iswitchb-next-match)
          ("<left>"  . iswitchb-prev-match)
          ("<up>"    . ignore             )
          ("<down>"  . ignore             ))))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;; arrête de demander si je veux bien faire ce que je veux faire
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; smart moves

(defun beginning-of-line-or-buffer()
  ;; goto the beginning of line. If already, goto beginning of buffer
  (interactive)
  (if (eq (line-beginning-position) (point))
      (beginning-of-buffer)
   (beginning-of-line)))
(defun end-of-line-or-buffer()
  ;; goto the end of line. If already, goto end of buffer
  (interactive)
  (if (eq (line-end-position) (point))
      (end-of-buffer)
   (end-of-line)))

;; navigate to beg/end of current line, considering indent and
;; comments

(defun point-in-comment ()
  "Determine if the point is inside a comment"
  (interactive)
  (let ((face (plist-get (text-properties-at (point)) 'face)))
    (when (not (listp face)) (setq face (list face)))
    (or (memq 'font-lock-comment-face face)
        (memq 'font-lock-comment-delimiter-face face))))

(defun my-back-to-indentation ()
  (if visual-line-mode
      (flet ((beginning-of-line (arg) (beginning-of-visual-line arg)))
        (back-to-indentation))
    (back-to-indentation)))

(defun my-back-to-indentation-or-beginning (&optional arg)
  "Jump back to indentation of the current line.  If already
there, jump to the beginning of current line.  If visual mode is
enabled, move according to the visual lines."
  (interactive "p")
  (if (or (/= arg 1)
          (= (point) (save-excursion
                       (my-back-to-indentation)
                       (point))))
      (progn
        (if visual-line-mode
            (beginning-of-visual-line arg)
          (move-beginning-of-line arg))
        (when (/= arg 1)
          (my-back-to-indentation)))
    (my-back-to-indentation)))

(defun my-end-of-code-or-line (&optional arg)
  "Move to the end of code.  If already there, move to the end of line,
that is after the possible comment.  If at the end of line, move
to the end of code.

Example:
  (serious |code here)1 ;; useless commend2

In the example, | is the current point, 1 is the position of
point after one invocation of this funciton, 2 is position after
repeated invocation. On subsequent calls the point jumps between
1 and 2.

Comments are recognized in any mode that sets syntax-ppss
properly."
  (interactive "p")
  (flet ((end-of-line-lov () (if visual-line-mode
                                 (end-of-visual-line arg)
                               (move-end-of-line arg)))
         (beg-of-line-lov () (if visual-line-mode
                                 (beginning-of-visual-line arg)
                               (move-beginning-of-line arg))))
    (let ((eoc (save-excursion
                 (end-of-line-lov)
                 (while (and (point-in-comment)
                             (not (bolp)))
                   (backward-char))
                 (skip-syntax-backward " ")
                 ;; if we skipped all the way to the beginning, that
                 ;; means there's only comment on this line, so this
                 ;; should just jump to the end.
                 (if (= (point) (save-excursion
                                  (beg-of-line-lov)
                                  (point)))
                     (progn (end-of-line-lov)
                            (point))
                   (point)))))
      (if (= (point) eoc)
          (end-of-line-lov)
        (goto-char eoc)))))




;; *******************************************************************
;; -------------------------------------------------------------------
;; Mon propre clavier
;; -------------------------------------------------------------------

;; Permet de récupérer C-i et C-m, qui sinon sont confondus avec Tab et...
;; Translate the problematic keys to the function key Hyper:
(define-key input-decode-map (kbd "C-i") (kbd "H-i"))
(define-key input-decode-map (kbd "C-m") (kbd "H-m"))
;; Rebind then accordantly:
(global-set-key (kbd "H-i") 'whatever-you-want)

;; -------------------------------------------------------------------
;; Standard Keyboard Shortcuts

;; Standard shortcut keys
;; CTRL+C: Copy
;; CTRL+X: Cut
;; CTRL+V: Paste
(cua-mode t)
(setq cua-auto-tabify-rectangles nil ;; Don't tabify after rectangle commands
      cua-keep-region-after-copy nil ;; Standard Windows behaviour
      cua-enable-cursor-indications t
      cua-enable-modeline-indications nil
      cua-paste-pop-rotate-temporarily t)

;; undo/redo
;; CTRL+Z: Undo
(global-unset-key "\C-z")
;; Redo 	Ctrl+Shift+z 	redo
;; Redo 	Ctrl+y 	redo

;; open/save/write file
;; CTRL + O	Opens the Open dialog box.
;; CTRL + N	Opens a new blank document.
;; CTRL + F	Opens the Find dialog box.
;; CTRL + P	Opens the Print dialog box.
;; CTRL + S	Saves the document that currently has the input focus.
;; CTRL + A	Select all.
;; (global-set-key "\C-n" 'switch-to-buffer)
;; (global-set-key "\C-p" 'print-buffer)
(global-unset-key "\C-o")
(global-set-key "\C-o" 'find-file)
;;(global-unset-key "\C-w")
;;(global-set-key "\C-w" 'kill-this-buffer)
(global-unset-key "\C-s")
(global-set-key "\C-s" 'save-buffer)
(global-set-key [(control shift s)] 'write-file)
(global-set-key "\C-a" 'mark-whole-buffer)

(global-set-key [(control f)] 'isearch-forward)
(global-set-key [f3] 'isearch-repeat-forward)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)
(add-hook 'isearch-mode-hook
 (lambda ()
 (define-key isearch-mode-map (kbd "C-s") 'save-buffer)
 (define-key isearch-mode-map (kbd "M-S") 'isearch-repeat-backward)
 ;; (define-key isearch-mode-map (kbd "C-x") 'isearch-yank-word-or-char)
 (define-key isearch-mode-map (kbd "C-v") 'isearch-yank-kill)
 ;; (define-key isearch-mode-map (kbd "C-c") 'isearch-yank-pop)
 ))


;; To kill buffers : f4
(global-set-key [C-f4] 'kill-this-buffer) ; standard keybinding

;; CTRL+TAB	Moves to next pane or palette.
;; CTRL+SHIFT+TAB	Moves to previous pane or palette.
;; F6	Moves to next pane or palette (same as CTRL+TAB).
;; SHIFT+F6	Moves to previous pane or palette (same as CTRL+SHIFT+TAB).

(defun next-user-buffer ()
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
      (setq i (1+ i)) (next-buffer))))

(defun previous-user-buffer ()
  "Switch to the previous user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
      (setq i (1+ i)) (previous-buffer) )))

(global-set-key [f6]   'next-user-buffer)
(global-set-key [S-f6]   'previous-user-buffer)
(global-set-key (kbd "<C-tab>") 'next-user-buffer)
(global-set-key (kbd "<C-S-tab>") 'prev-user-buffer)

;; CTRL+F6	Moves to next window in a group of related windows
;; CTRL+SHIFT+F6	Moves to previous window in a group of related windows

;; irrelevant

;; Standard shortcuts for cursor navigation
;; CTRL+RIGHT ARROW Move to the beginning of the next word.
;; CTRL+LEFT ARROW Move to the beginning of previous word.
;; CTRL+DOWN ARROW Move to the beginning of the next paragraph.
;; CTRL+UP ARROW Move to the beginning of the previous paragraph.

;; (global-set-key [C-down]  'forward-paragraph)
;; (global-set-key [C-up]  'backward-paragraph)
(global-set-key [C-left]  'left-word)
(global-set-key [C-right]  'right-word)

;;Home => beginning of line
;; (global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<home>") 'my-back-to-indentation-or-beginning)
;; (global-set-key (kbd "<end>") 'end-of-line)
(global-set-key (kbd "<end>") 'my-end-of-code-or-line)
;; CTRL+HOME	Move cursor to the top of the document.
;; CTRL+END	Move cursor to the end of the document.

;; Holding SHIFT + cursor movement (arrow, HOME, or END keys)	Select or extend the selection.

;; by default emacs>12

;; Holding SHIFT + CTRL + Cursor movement	Select or extend the selection by words or block of text.

;; to be implemented

;; -------------------------------------------------------------------
;; Personal keyboard shortcuts

;; something I do *very* often
(global-set-key [M-left]  'my-back-to-indentation-or-beginning)
(global-set-key [M-right]  'my-end-of-code-or-line)

;; kill the whole current line
;; Pour faire toute une ligne de commentaire
(defun kill-whole-line ()
  (interactive)
  (beginning-of-line)
  (kill-line))

(global-set-key [(meta delete)]  'kill-whole-line)

;; “cut” and “copy” act on the current line if no text is visually selected

(defadvice kill-ring-save (around slick-copy activate)
  "When called interactively with no active region, copy a single
line instead."
  (if (or (use-region-p) (not (called-interactively-p)))
      ad-do-it
    (kill-new (buffer-substring (line-beginning-position)
                                (line-beginning-position 2))
              nil '(yank-line))
    (message "Copied line")))

(defadvice kill-region (around slick-copy activate)
  "When called interactively with no active region, kill a single line instead."
  (if (or (use-region-p) (not (called-interactively-p)))
      ad-do-it
    (kill-new (filter-buffer-substring (line-beginning-position)
                                       (line-beginning-position 2) t)
              nil '(yank-line))))

(defun yank-line (string)
  "Insert STRING above the current line."
  (beginning-of-line)
  (unless (= (elt string (1- (length string))) ?\n)
    (save-excursion (insert "\n")))
  (insert string))



;; indent after return
(global-set-key (kbd "RET") 'newline-and-indent)

(defun insert-empty-line ()
  "Insert an empty line after the current line.
   Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (open-line 1)
  (forward-line 1)
  (indent-according-to-mode))

(global-set-key [(shift return)] 'insert-empty-line)

;; Move between windows
(global-set-key [C-next]  'windmove-down)
(global-set-key [C-prior] 'windmove-up)


;; kill buffers
(global-set-key [f4] 'kill-this-buffer) ; personnal shortcut
(global-set-key [S-f4] 'kill-some-buffers)

;; Pour fixer un point et y revenir : C-(&) et C-(")
(global-set-key [(control &)] '(lambda () (interactive) (point-to-register 38 nil)))
(global-set-key [(control \")] '(lambda () (interactive) (jump-to-register 38 nil)))

;; Find file and open in other window
(global-set-key (kbd "C-x C-g") 'find-file-other-window)
;; Find file at point
(global-set-key (kbd "C-x f") 'ffap)

;; Kill characters backward until encountering the beginning of a word.
(global-set-key [(control backspace)] 'backward-kill-word)

;; I comment often...
;; I comment stuff often, let's be efficient
(defun comment-line ()
  "Comments the current line"
  (interactive)
  (condition-case nil
      (comment-region (point-at-bol) (point-at-eol)) (error nil))
  )

 (defun comment-and-go-down ()
   "Comments the current line and goes to the next one"
   (interactive)
   (condition-case nil
       (comment-region (point-at-bol) (point-at-eol)) (error nil))
   (next-line 1))

(defun comment-and-go-up ()
  "Comments the current line and goes to the previous one"
  (interactive)
  (condition-case nil
      (comment-region (point-at-bol) (point-at-eol)) (error nil))
  (next-line -1))

 (defun uncomment-and-go-down ()
   "Comments the current line and goes to the next one"
   (interactive)
   (condition-case nil
       (uncomment-region (point-at-bol) (point-at-eol)) (error nil))
   (next-line 1))

(defun uncomment-and-go-up ()
  "Uncomments the current line and goes to the previous one"
  (interactive)
  (condition-case nil
      (uncomment-region (point-at-bol) (point-at-eol)) (error nil))
  (next-line -1))

(define-key global-map [(meta up)] 'uncomment-and-go-up)
(define-key global-map [(meta down)] 'uncomment-and-go-down)
(define-key global-map [(control up)] 'comment-and-go-up)
(define-key global-map [(control down)] 'comment-and-go-down)

;; select text in quote
;; (define-key global-map [(meta =)] 'select-text-in-quote )

;; copy the current line
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (message "Current line is copied.")
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))

;; (defun copy-line (arg)
  ;; "Copy lines (as many as prefix argument) in the kill ring"
  ;; (interactive "p")
  ;; (kill-ring-save (line-beginning-position)
                  ;; (line-beginning-position (+ 1 arg)))
      ;; (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(global-set-key "\M-k" 'kill-whole-line)

(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion
      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))
      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )
      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let
  ;; put the point in the lowest line and return
  (next-line arg))

(global-set-key "\M-\C-k" 'duplicate-line)

(defun mark-whole-line ()
  (interactive)
  (beginning-of-line)
  (cua-set-mark)
  (end-of-line))

(global-set-key (kbd "C-M-SPC") 'mark-whole-line)

;; very convenient: Goto last change, after a movement
(require 'goto-last-change)
(global-set-key [(control *)] 'goto-last-change)


(global-set-key [(control \,)] 'goto-line)
;; (global-set-key [(control \.)] 'call-last-kbd-macro)
;; (global-set-key [(control j)] 'join-line)

;; ;; (define-key global-map (kbd "M-SPC")  'shrink-whitespaces)

(global-set-key [(control ù)] 'next-error)

;; -------------------------------------------------------------------
;; Touches de fonction
(global-set-key [f1] 'iswitchb-buffer)
;; Les bookmarks (indispensables !): f1
(global-set-key [M-f1] 'bookmark-jump)
(global-set-key [C-f1] 'bookmark-set)
(global-set-key [S-f1] 'recentf-open-file)

;; Ouvre un fichier trouvé grâce à kpsewhich
(defun kpse-open (file)
  "Find a command in the tex arborescence and open it"
  (interactive "sFilename: ")
  (let ((result (shell-command-to-string (format "kpsewhich %s " file))))
    (if (string-match "\\([^\n]*\\)" result)
        (progn
          (find-file (match-string 1 result))
          )
      )
    )
  )
(global-set-key [S-f1] 'kpse-open)

;; Les commentaires : f2
(global-set-key [f2] 'comment-dwim)
(global-set-key [C-f2] 'uncomment-region)
(global-set-key [S-f2] 'comment-full-line)
(global-set-key [C-S-f2] 'comment-full-big-line)

;; Les greps : f3
(global-set-key [f3] 'grep)
(global-set-key [C-f3] 'grep-find)

;; Les remplacements : f5
(global-set-key [f5] 'replace-string)
(global-set-key [C-f5] 'replace-regexp)
(global-set-key [M-f5] 'occur)
(global-set-key [S-f5] 'query-replace)


;; Les dates : f7

;; Pour insérer le nom du fichier courant
(defun insert-buffer-file-name-nond-sansext ()
  "Insère le nom du fichier courant"
  (interactive)
  (insert (file-name-sans-extension (file-name-nondirectory buffer-file-name))))

;; Pour insérer la date du jour
(defun insert-date ()
  "Insère la date complète"
  (interactive)
  (insert (format-time-string "%A %e %B %Y - %k:%M")))

;; La même en format court
(defun insert-short-date ()
  "Insère la date abrégée"
  (interactive)
  (insert (format-time-string "%e %B %Y")))

;; La même en format très court
(defun insert-very-short-date ()
  "Insère la date abrégée"
  (interactive)
  (insert (format-time-string "%Y/%m/%d")))

;; La même en format très court pour les fichiers
(defun insert-very-short-date-file ()
  "Insère la date abrégée"
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun pp-sign ()
  "spit out my name, email and the current time"
  (interactive)
  (insert "Paul Pichaureau (paul.pichaureau@alcandre.net)")
  (insert-very-short-date))

(global-set-key [f7]   'insert-buffer-file-name-nond-sansext)
(global-set-key [S-f7] 'insert-date)
(global-set-key [C-f7] 'insert-very-short-date)
(global-set-key [M-f7] 'insert-very-short-date-file)

;; utilisation de occur

(eval-when-compile
  (require 'cl))

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

;; global key for `multi-occur-in-this-mode'
(global-set-key (kbd "C-S-<f5>") 'multi-occur-in-this-mode)

;; Pour diviser la fenêtre en deux ou n'en mettre qu'une : f8 C-f8
(global-set-key [f8] 'split-window-vertically)
(global-set-key [C-f8] 'delete-other-windows)

;; Allez à la ligne n° : f9
(global-set-key [S-f9] 'goto-line)

;; ;; ispell/flyspell : f10
(global-set-key [f10] 'ispell-buffer)
(global-set-key [C-f10] 'ispell-region)
(global-set-key [S-f10] 'flyspell-buffer)

;; (global-set-key [f12] 'indent-region)

;; Environnement
(global-set-key [C-f12] 'eshell)
(global-set-key [S-f12] 'recentf-open-files)
(global-set-key [M-f12] 'magit-status)

;; Enfin la souris : les copiers/coller
(global-set-key [S-mouse-2] 'yank-pop)
(global-set-key [mouse-2] 'yank)


;;

(global-set-key [M-:] 'hippie-expand)


(defun dont-kill-emacs ()
  (interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))

(global-set-key "\C-x\C-c" 'dont-kill-emacs)


(defun cmd-current-word-or-region (cmd)
  "Applique la commande au mot sous ou précédant le curseur"
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
      (progn (forward-char)
        (backward-word)
        ;; (forward-word 1)
        (kill-word 1)))
  (insert cmd)
  (backward-char)
  (yank))

(defun emph-current-word-or-region ()
  (interactive)
  (cmd-current-word-or-region "\\emph{}"))

(defun textbf-current-word-or-region ()
  (interactive)
  (cmd-current-word-or-region "\\textbf{}"))


(defun index-current-word ()
  "Indexe le mot sous ou précédant le curseur"
  (interactive)
  (forward-char)
  (backward-word 1)
  (forward-word 1)
  (kill-new (current-word))
  (insert "\\index{}")
  (backward-char)
  (yank))

(defun guill-current-region ()
  "Met des guillements autour de la région."
  (interactive)
  (kill-region (region-beginning) (region-end))
  (insert "«»")
  (backward-char)
  (yank))


(defun brace-current-region ()
  "Met des accolades autour de la région"
  (interactive)
  (command-current-region nil))

(defun query-command-current-region ()
  "Met des accolades autour de la région, et la commande passée en
argument devant"
  (interactive)
  (let ((commande (read-string "Quelle commande : "))
	)
    (command-current-region commande)))

(defun command-current-region (commande)
  "Met des accolades autour de la région, et la commande passée en
argument devant"
  (interactive)
  (kill-region (region-beginning) (region-end))
  (insert (concat commande "{}"))
  (backward-char)
  (yank))


(defun replace-matching (delim1 delim2 redelim1 redelim2)
  "When the cursor is immediately after delim2, replace
the closing delim2 with redelim2 and the matching delim1 with
redelim1"

  ()
  (save-excursion
    (let ((end-point (point)))
      (backward-list)
      (let ((start-point (point)))
        (goto-char end-point)
        (re-search-backward delim2 nil t)
        (replace-match redelim2 nil nil)

        (goto-char start-point)
        (re-search-forward delim1 nil t)
        (replace-match redelim1 nil nil)))))

(defun replace-matching-paren ()
  "Remplace (...) par \pa{...} Nécessite de se placer après la parenthèses fermantes"
  (interactive)
  (replace-matching "(" ")" "\\\\pa{" "}"))

(defun replace-dollar-equation ()
  "Remplace $...$ par \begin{equation*} ... \end{equation*} "
  (interactive)
  (save-excursion
    (re-search-backward "\\$" nil t)
    (replace-match "\n\\\\end{equation*}\n" nil nil)
    (re-search-backward "\\$" nil t)
    (replace-match "\n\\\\begin{equation*}\n" nil nil)))

;; Redefinitions de touches valables uniquement dans le mode LaTeX
(add-hook 'LaTeX-mode-hook 'my-latex-keybindings)
(defun my-latex-keybindings ()
  (interactive)
  (progn
    ;; (local-set-key (quote [C-tab]) 'TeX-complete-symbol)
    ;; é <-> \'e etc. Merci Walter !
    ;; (local-set-key [f11] 'latex2iso)
    ;; (local-set-key [S-f11] 'quotedprintable2iso)
    ;; (local-set-key [M-f11] 'iso2latex)
    ;; Permet de mettre une région en paramètre d'une commande
    (local-set-key [(control =)] 'query-command-current-region)
    (local-set-key [apps] 'TeX-command-master)
    (local-set-key [(control ?ù)] 'TeX-next-error)
    (local-set-key [C-S-f11]   'latex-math-mode) ; Il est cool celui-là !

    (local-set-key [f11]   '(lambda () (interactive) (LaTeX-modify-environment "align*")))
    (local-set-key [C-f11]   '(lambda () (interactive) (LaTeX-modify-environment "multline*")))

    (local-set-key [C-:]   '(lambda () (interactive) (insert "\\:")))
    (local-set-key [(control ?\,)]   '(lambda () (interactive) (insert "\\,")))
   (local-set-key  [(control ?\;)]   '(lambda () (interactive) (insert "\\;")))
   (local-set-key [C-!]   '(lambda () (interactive) (insert "\\!")))

   (local-set-key [(control ?\))]   'replace-matching-paren)
   (local-set-key [(control $)]   'replace-dollar-equation)

    ;; (local-set-key [tab]    'hippie-expand)
    (local-set-key [?\H-i]  'emph-current-word-or-region)
    (local-set-key (kbd "C-b") 'textbf-current-word-or-region)
    (local-set-key [f9] 'sumatra-jump-to-line)
    )
  )


;; navigation aisée
(add-hook 'outline-minor-mode-hook
          (lambda ()
            (require 'outline-magic)
            (define-key outline-minor-mode-map [(f12)] 'outline-cycle)))
(outline-minor-mode t)
(global-set-key [M-next]  'outline-next-visible-heading)
(global-set-key [M-prior]  'outline-previous-visible-heading)
(global-set-key [f12]  'outline-cycle)

;; highlight current line temporary
(require 'pulse)
(setq pulse-flag nil) ;; don't pulse, just highlight
(pulse-toggle-integration-advice t)
(defadvice  outline-next-visible-heading (after pulse-advice activate)
  "After going to next visible heading, pulse the line the cursor lands on."
  (when (and pulse-command-advice-flag (cedet-called-interactively-p))
    (pulse-momentary-highlight-one-line (point))))
(defadvice  outline-previous-visible-heading (after pulse-advice activate)
  "After going to next visible heading, pulse the line the cursor lands on."
  (when (and pulse-command-advice-flag (cedet-called-interactively-p))
    (pulse-momentary-highlight-one-line (point))))
(defadvice  windmove-up (after pulse-advice activate)
  "After windmove, pulse the line the cursor lands on."
  (when (and pulse-command-advice-flag (cedet-called-interactively-p))
    (pulse-momentary-highlight-one-line (point))))
(defadvice  windmove-down (after pulse-advice activate)
  "After windmove, pulse the line the cursor lands on."
  (when (and pulse-command-advice-flag (cedet-called-interactively-p))
    (pulse-momentary-highlight-one-line (point))))
(defadvice  cua-scroll-up (after pulse-advice activate)
  "After scrolling, pulse the line the cursor lands on."
  (when (and pulse-command-advice-flag (cedet-called-interactively-p))
    (pulse-momentary-highlight-one-line (point))))
(defadvice  cua-scroll-down (after pulse-advice activate)
  "After scrolling, pulse the line the cursor lands on."
  (when (and pulse-command-advice-flag (cedet-called-interactively-p))
    (pulse-momentary-highlight-one-line (point))))


(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; -------------------------------------------------------------------
;; Customization

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-default-environment "equation*")
 '(LaTeX-file-extensions (quote ("tex" "sty" "cls" "ltx" "texi" "texinfo" "mtx" "etx" "dtx")))
 '(LaTeX-indent-environment-list (quote (("verbatim" current-indentation) ("verbatim*" current-indentation) ("alltt") ("array") ("displaymath") ("eqnarray") ("eqnarray*") ("equation") ("equation*") ("picture") ("tabbing") ("table") ("table*") ("tabular") ("tabular*") ("align") ("align*"))))
 '(LaTeX-indent-level 4)
 '(LaTeX-item-indent 4)
 '(LaTeX-item-regexp "\\(bib\\)?item\\b")
 '(LaTeX-left-comment-regexp "%")
 '(LaTeX-math-list (quote ((38 "&" "" nil) (34 "card" "Log-like" nil))))
 '(TeX-auto-save nil)
 '(TeX-brace-indent-level 4)
 '(TeX-check-path (quote ("./" "P:/texmf/tex/")))
 '(TeX-close-quote "\\fg")
 '(TeX-file-extensions (quote ("tex" "sty" "cls" "ltx" "texi" "texinfo" "dtx" "mtx" "etx")))
 '(TeX-macro-global (quote ("P:/texmf/tex/")))
 '(TeX-macro-private (quote ("P:/localtexmf/tex/")))
 '(TeX-output-view-style (quote (("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "dvips %d -o && start \"\" %f") ("^dvi$" "." "yap -1 %dS %d") ("^pdf$" "." "SumatraPDF.exe %o") ("^html?$" "." "start \"\" %o"))))
 '(TeX-parse-self t)
 '(ansi-color-names-vector ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(ansi-term-color-vector [unspecified "#110F13" "#b13120" "#719f34" "#ceae3e" "#7c9fc9" "#7868b5" "#009090" "#F4EAD5"])
 '(archive-zip-expunge (quote ("zip" "-d")))
 '(archive-zip-extract (quote ("unzip" "-e" "-o-")))
 '(archive-zip-update (quote ("zip" "-u")))
 '(archive-zip-update-case (quote ("zip" "-u" "-P")))
 '(background-color "#002b36")
 '(background-mode dark)
 '(calendar-date-style (quote european))
 '(calendar-day-name-array ["Dimanche" "Lundi" "Mardi" "Mercredi" "Jeudi" "Vendredi" "Samedi"])
 '(calendar-european-date-display-form (quote ((if dayname (concat dayname "  ")) day " " monthname " " year)))
 '(calendar-month-abbrev-array ["Jan" "Fév" "Mar" "Avr" "Mai" "Jui" "Jul" "Aou" "Sep" "Oct" "Nov" "Dec"])
 '(calendar-month-name-array ["Janvier" "Février" "Mars" "Avril" "Mai" "Juin" "Juillet" "Août" "Septembre" "Octobre" "Novembre" "Decembre"])
 '(calendar-week-start-day 1)
 '(case-fold-search t)
 '(cursor-color "#93a1a1")
 '(cursor-in-non-selected-windows nil)
 '(custom-enabled-themes (quote (Alcandre)))
 '(custom-safe-themes (quote ("e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "49b4eaf0df35beaec59da10e651e2e42ace57550096eac6b789756f96e7992d1" "9f443833deb3412a34d2d2c912247349d4bd1b09e0f5eaba11a3ea7872892000" "21d9280256d9d3cf79cbcf62c3e7f3f243209e6251b215aede5026e0c5ad853f" "787574e2eb71953390ed2fb65c3831849a195fd32dfdd94b8b623c04c7f753f0" "936e5cac238333f251a8d76a2ed96c8191b1e755782c99ea1d7b8c215e66d11e" "68769179097d800e415631967544f8b2001dae07972939446e21438b1010748c" "526abfd99b7c0184a79dfeda39a591c78a4633cf092cc7f273bf1dba4f6b1c3a" "5e726125e49978bd8abe2388045c69151b5831ce9ab3ee97070bca7e226d5899" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "54e312857acc912122baa9dd49712fb99c690c72a1236cffbcc6831aa9128965" "ccd24002ff5125265d21c198e467b5ff8a78048a61f9a8f594d877b55b88af84" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" default)))
 '(default-frame-alist (quote ((menu-bar-lines . 1) (top . 0) (left . 0))))
 '(desktop-save-mode nil nil (desktop))
 '(european-calendar-style t)
 '(fci-rule-character-color "#202020")
 '(fci-rule-color "#383838")
 '(file-cache-find-command-posix-flag t)
 '(flyspell-use-meta-tab nil)
 '(font-latex-fontify-script t)
 '(font-latex-fontify-sectioning 1)
 '(font-latex-match-textual-keywords (quote ("item" "part" "chapter" "section" "subsection" "subsubsection" "paragraph" "subparagraph" "subsubparagraph" "title" "author" "date" "thanks" "address" "caption" "Exercise" "Question" "subQuestion" "subsubQuestion")))
 '(font-latex-quotes (quote french))
 '(font-latex-title-fontity (quote color))
 '(font-latex-verb-like-commands (quote ("verb" "verb*")))
 '(foreground-color "#93a1a1")
 '(fringe-mode 0 nil (fringe))
 '(global-font-lock-mode t nil (font-lock))
 '(global-linum-mode t)
 '(grep-command nil)
 '(grep-window-height nil)
 '(imenu-auto-rescan t)
 '(ispell-extra-args (quote ("-t --sug-mode=ultra")))
 '(ispell-silently-savep t)
 '(ispell-use-framepop-p t)
 '(iswitchb-buffer-ignore (quote ("^ " "^\\*.*\\*")))
 '(iswitchb-mode t)
 '(italic ((t (:slant italic))))
 '(line-number-mode nil)
 '(line-spacing 0)
 '(linum-format "%7d")
 '(lunar-phase-names (quote ("Nouvelle lune" "Premier quartier" "Pleine lune" "Dernier quartier")))
 '(main-line-color1 "#1e1e1e")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(minibuffer-frame-alist (quote ((width . 80) (height . 2))))
 '(minibuffer-prompt-properties (quote (read-only t face minibuffer-prompt)))
 '(mouse-drag-copy-region t)
 '(mouse-wheel-scroll-amount (quote (0.01)))
 '(mouse-yank-at-point t)
 '(org-hierarchical-checkbox-statistics nil)
 '(org-hierarchical-todo-statistics nil)
 '(org-log-done (quote time))
 '(org-mobile-force-id-on-agenda-items nil)
 '(org-provide-todo-statistics (quote (quote all-headlines)))
 '(org-startup-folded nil)
 '(org-time-stamp-custom-formats (quote ("<%a %d/%m/%y>" . "<%a %d/%m/%y %H:%M>")))
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(pdf-view-command "sumatraPDF.exe")
 '(php-manual-url "http://www.php.net/manual/fr/manual.php")
 '(powerline-color1 "#1e1e1e")
 '(powerline-color2 "#111111")
 '(preview-gs-command "P:\\texmf\\miktex\\bing\\mgs.exe")
 '(preview-image-type (quote pnm))
 '(preview-scale-function 1.75)
 '(ps-view-command "gsview32.exe")
 '(py-honor-comment-indentation nil)
 '(py-imenu-show-method-args-p t)
 '(read-buffer-function nil)
 '(recentf-menu-filter (quote recentf-show-basenames))
 '(reftex-label-alist (quote ((nil 115 "%f:" nil nil nil))))
 '(same-window-buffer-names (quote ("*eshell*" "*mail*" "*info*" "*inferior-lisp*" "*ielm*" "*scheme*")))
 '(save-abbrevs (quote silently))
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-paren-ring-bell-on-mismatch t)
 '(show-paren-style (quote mixed))
 '(size-indication-mode t)
 '(solar-n-hemi-seasons (quote ("Équinoxe de printemps" "Solstice d'été" "Équinoxe d'automne" "Solstice d'hiver")))
 '(solarized-contrast (quote high))
 '(tags-case-fold-search t)
 '(tex-close-quote " \\fg")
 '(tex-open-quote "\\og ")
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tidy-shell-command "c:/gnu/bin/tidy")
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(uniquify-separator nil)
 '(vc-annotate-background "#2b2b2b")
 '(vc-annotate-color-map (quote ((20 . "#bc8383") (40 . "#cc9393") (60 . "#dfaf8f") (80 . "#d0bf8f") (100 . "#e0cf9f") (120 . "#f0dfaf") (140 . "#5f7f5f") (160 . "#7f9f7f") (180 . "#8fb28f") (200 . "#9fc59f") (220 . "#afd8af") (240 . "#bfebbf") (260 . "#93e0e3") (280 . "#6ca0a3") (300 . "#7cb8bb") (320 . "#8cd0d3") (340 . "#94bff3") (360 . "#dc8cc3"))))
 '(vc-annotate-very-old-color "#dc8cc3"))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:foreground "dark green"))))
 '(font-latex-bold-face ((t (:inherit bold :foreground "black"))))
 '(font-latex-sectioning-1-face ((t (:inherit font-latex-sectioning-2-face :slant normal :weight bold :height 1.5))))
 '(font-latex-sectioning-2-face ((t (:inherit font-latex-sectioning-3-face :weight bold))))
 '(font-latex-sectioning-3-face ((t (:inherit font-latex-sectioning-4-face :slant italic :height 1.1))))
 '(font-latex-sectioning-4-face ((t (:inherit (font-latex-sectioning-5-face variable-pitch)))))
 '(font-latex-sectioning-5-face ((t (:foreground "dark green"))))
 '(font-latex-sedate-face ((t (:foreground "dark violet" :slant italic))))
 '(font-lock-keyword-face ((t (:foreground "Dark Blue" :slant normal :weight bold))))
 '(linum ((t (:inherit (shadow default) :family "Corbel"))))
 '(variable-pitch ((t (:height 1.1 :family "Constantia")))))


;; :background "gray97" :foreground "SystemWindowText
 ;; '(font-latex-sedate-face ((t (:foreground "#b37400"))))
 ;; '(font-latex-verbatim-face ((((class color) (background light)) (:foreground "SaddleBrown" :inherit fixed-pitch))))
 ;; '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 ;; '(font-lock-comment-face ((t (:foreground "firebrick"))))
 ;; '(font-lock-keyword-face ((t (:foreground "Navy" :weight bold))))
 ;; '(font-lock-type-face ((((class color) (min-colors 88) (background light)) (:foreground "ForestGreen"))))
 ;; '(font-lock-variable-name-face ((t (:foreground "magenta"))))
 ;; '(highlight ((t (:background "khaki"))))
 ;; '(highlight-changes ((t (:underline "gray40"))))
 ;; '(highlight-changes-delete ((t (:foreground "red1" :strike-through t))))
 ;; '(minibuffer-prompt ((t (:foreground "medium blue"))))
 ;; '(region ((t (:background "khaki"))))
 ;; '(show-paren-match ((((class color) (background light)) (:background "pale turquoise"))))
 ;; '(variable-pitch ((t (:family "Segoe UI"))))
 ;; '(widget-documentation ((t (:foreground "dark green")))))

 ;; '(background-color "#002b36")
 ;; '(background-mode light)
;; (foreground-color . "SystemWindowText") (background-color . "SystemWindow")

; at the end
(set-frame-size-according-to-resolution)

 ;; '(preview-gs-command "P:\\Programmes\\gs\\gs8.64\\bin\\GSWIN32C.EXE")
