;; ----------------------------------------------------------------
;; init.el de Paul Pichaureau
;; ----------------------------------------------------------------            
;; Démarrage du serveur emacs
(server-start)

;; Qui suis-je ?
(setq user-full-name "Paul Pichaureau")
(setq user-mail-address "paul.pichaureau@alcandre.net")


;; -------------------------------------------------------------------
;; Gestion des chemins

(defvar usb-drive-letter (substring data-directory 0 3))
(defvar usb-home-dir (concat usb-drive-letter "Paul/"))

(setenv "HOME" usb-home-dir)

(setenv "TEMP" "c:\Windows\TEMP")
(setenv "TMPDIR" "c:\Windows\TEMP")

(add-to-list 'load-path "~/.emacs.d/site-lisp")

(defvar user-init-dir "~/.emacs.d/"
  "* Le répertoire racine de tous mes fichiers concernant Emacs.")

(defun cuid (FILENAME)
  "* Tous les fichiers personnels sont stockés relativement
au répertoire d'installation `user-init-dir'.
Utiliser cette fonction pour définir un répertoire/fichier relatif.
Attention `user-init-dir' se termine par un /"
  (concat user-init-dir FILENAME))

(setq load-path (append load-path (list (cuid "etc")
                                        (cuid "site-lisp")
                                        )))

;; Pour avoir tous les fichiers de backup dans un seul répertoire
(defun make-backup-file-name (file)
  (concat (cuid "backup/") (file-name-nondirectory file) "~"))

;; -------------------------------------------------------------------
;; Apparence
(add-to-list 'load-path (cuid "site-lisp/color-theme-alcandre/"))
(require 'color-theme-alcandre)
(color-theme-alcandre-light)

;; Le titre de la fenêtre : nom du buffer (nom du fichier)
(setq frame-title-format '(buffer-file-name "%b (%f)" "%b"))

;; Surtout pas de son !
(setq visible-bell t)

;; vire barre de menu graphique
(tool-bar-mode -1)

;; texte en couleurs
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t
      font-lock-maximum-size nil)

(if (functionp 'global-hi-lock-mode)
    (global-hi-lock-mode 1)
  (hi-lock-mode 1))

;;
;; Allow the cursor color to toggle depending on overwrite mode
;;
(defvar cursor-color nil "Last cursor color used (string)")
(defun ins-cursor-set () "set cursor color according to ins mode"
  (let ((sv-crs-str cursor-color))
    (if overwrite-mode
        (setq cursor-color "black")     ;overwrite cursor color
      (setq cursor-color "red"))        ;insert mode
    (or (string= sv-crs-str cursor-color)
        (set-cursor-color cursor-color)))) ;X terminal cursor color

(add-hook 'post-command-hook 'ins-cursor-set)

(setq scroll-margin 1                    ; do smooth scrolling, ...
       scroll-conservatively 101)        


;; ;; -------------------------------------------------------------------
;; ;; Cygwin
;; ;; Permet d'accèder aux binaires de cygwin depuis emacs
;; ;; Remplacer c:/cygwin par le repertoire ou cygwin est installé

;; (setq cygwinpath (concat usb-drive-letter "cygwin/bin"))
;; (setenv "PATH" (concat cygwinpath (getenv "PATH")))
;; (setq exec-path (cons cygwinpath exec-path))

;; (require 'cygwin-mount)
;; (cygwin-mount-activate)

(add-to-list 'Info-default-directory-list (concat usb-drive-letter "cygwin/info"))
;; (add-hook 'comint-output-filter-functions
;; 'shell-strip-ctrl-m nil t)
;; (add-hook 'comint-output-filter-functions
;; 'comint-watch-for-password-prompt nil t)
;; (setq explicit-shell-file-name "bash.exe")
;; ;; For subprocesses invoked via the shell
;; ;; (e.g., "shell -c command")
;; (setq shell-file-name explicit-shell-file-name)

;; (require 'shell)

;; ;; De plus la commande shell branche sur cygwin
;; (defun shell-cygwin ()
;; (interactive)
;; (let ((bufname "*shell-cygwin*")
;; (bufobject))

;; (setq bufobject (get-buffer bufname))

;; (cond
;; ((and bufobject (get-buffer-process bufobject))
;; (pop-to-buffer bufname)
;; )
;; (t
;; (progn
;; (set-buffer
;; (apply 'make-comint-in-buffer
;; "shell"
;; bufname
;; explicit-shell-file-name
;; nil
;; '("--rcfile" "~/.profile" "-i")
;; ))
;; (shell-mode)
;; (pop-to-buffer (current-buffer))
;; )
;; )
;; )
;; )
;; )

;; let there be a marker on every empty line on the left fringe
;; (setq default-indicate-empty-lines t)

;; -------------------------------------------------------------------
;; Unicode

(require 'mule)
;; Environnement utf-8
(setq default-file-name-coding-system 'utf-8)
;; (setq locale-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")       ; prefer utf-8 for language settings
(set-input-method nil)                   ; no funky input for normal editing;

;; -------------------------------------------------------------------
;; Paramètrage propre à Windows

;; Pour avoir l'affichage des italiques
;; doit être positionné avant toute commande définissant les polices
(setq w32-enable-synthesized-fonts t)
;; (setq w32-enable-italics t)
;; Utilise la sélection normale des polices de windows
(setq w32-use-w32-font-dialog t)

;; pour gagner deux touches
(setq w32-pass-rwindow-to-system nil)
;; (setq w32-pass-lwindow-to-system nil)

;; mark current line:
;; (global-hl-line-mode 1)
;; color for current line:
;; (set-face-background 'hl-line "#ceff84")


;; -------------------------------------------------------------------
;; Réglages divers

;; Mode activant l'affichage des numéros de ligne dans la marge gauche de la
;; fenêtre
(require 'linum)
(add-hook 'after-change-major-mode-hook 'linum-on) 

;; Activer la mise en exergue des espaces superflus ou hétérogènes
;; (require 'whitespace)

(setq kill-whole-line t ;; Kill EOL too
      ;; Ignorer certaines extensions
      completion-ignored-extensions
      (append completion-ignored-extensions
              '(".ilg" ".ind" ".pdf" ".tui" ".tuo" ".ted" ".tmp"
                ".orig" ".ps" ".mpo" "mpd" ".log" ".aux" ".toc"
                ".toe"
                ".1" ".2" ".3" ".4" ".5" ".6" ".7" ".8" ".9"))
      
      ;; Paste at point NOT at cursor
      mouse-yank-at-point t
      ;; vire les anciennes versions des fichiers
      delete-old-versions t
      ;; utiliser la poubelle de windows
      delete-by-moving-to-trash t
      ;; virer les messages d'accueils
      ;; initial-scratch-message nil
      ;;       inhibit-startup-message t          ; don't show ...    
      ;;       inhibit-startup-echo-area-message t
      
      )
;; la completion cycle parmi les choix s'il y en a moins que 5
(setq completion-cycle-threshold 5)

;; une amélioration de la performance
(setq redisplay-dont-pause t)

;; Affichage du bloc de texte selectionné
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

;; Pour se souvenir des commandes d'une session sur l'autre
(require 'save-history)
(setq save-history-file (cuid ".emacs-histories"))

(defvar bookmark-file (cuid ".emacs.bmk"))
(setq bookmark-save-flag 1)

;; saveplace: save location in file when saving files
(setq save-place-file (cuid "saveplace"))  
(setq-default save-place t)            ;; activate it for all buffers
(require 'saveplace)                   ;; get the package

;;-------------------------------------------------------------------
;; Différents modes 

;; Prise en compte des abréviations
(setq-default abbrev-mode t)
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
;; Pour (La)TeX : auctex 

(setenv "PATH" (concat (concat usb-drive-letter "texmf/miktex/bin;") (getenv "PATH")))

(setenv "PATH" (concat (concat usb-drive-letter "localtexmf/miktex/bin;") (getenv "PATH")))

;; pour gnuplot
(setenv "PATH" (concat (concat usb-drive-letter "Programmes/gnuplot/binary;") (getenv "PATH")))

;; pour gs
(setenv "PATH" (concat (concat usb-drive-letter "Programmes/gs/gsview;") (getenv "PATH")))

(add-to-list 'load-path (cuid "site-lisp/auctex/"))
(add-to-list 'load-path (cuid "site-lisp/site-start.d/"))
(add-to-list 'Info-default-directory-list (cuid "info"))

(load "auctex.el" nil t t)
(require 'tex-mik)
(require 'sumatra-forward)

;; Pour couper les lignes proprement
(add-hook 'latex-mode-hook 'turn-on-auto-fill)

;; Facilite la saisie des maths 
(add-hook 'latex-mode-hook 'LaTeX-math-mode)

(setq hilit-AmSLaTeX-commands t)
(setq hilit-multilingual-strings t)
(setq LaTeX-math-abbrev-prefix "\"")

(load "preview-latex.el" nil t t)
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

;; Commandes du menu de AUC TeX
;; The MikTeX commands.
(setq TeX-command-list
      (list (list "TeX" "tex --shell-escape --src-specials --interaction nonstopmode  %t" 'TeX-run-TeX nil t)
            (list "LaTeX" "latex --shell-escape --src-specials --interaction nonstopmode %t" 'TeX-run-LaTeX nil t)
            (list "PDFLaTeX" "pdflatex -synctex=-1 --shell-escape --src-specials --interaction nonstopmode %t"
                  'TeX-run-LaTeX nil t)
            (list "latex2wiki" "latex2wiki.cmd %t" 'TeX-run-command nil t)
            (list "Dvips" "dvips %d -o %f" 'TeX-run-command nil t)
            ;; (list "ps2pdf" "ps2pdf  %f" 'TeX-run-command nil t)
            (list "yap" "yap -1 %s" 'TeX-run-discard nil nil)
            (list "View" "SumatraPDF.exe %o"  'TeX-run-discard nil nil)
            (list "Sumatra" "SumatraPDF.exe -inverse-search \"P:\Programmes\emacs\bin\emacsclientw.exe +%l \"%f\"\" %o"'TeX-run-discard nil nil)
            ;; (list "Print" "gsview32 -p %s.ps" 'TeX-run-command t nil)
            ;; (list "File" "dvips %d -o %f " 'TeX-run-command t nil)
            (list "BibTeX" "bibtex %s" 'TeX-run-BibTeX nil nil)
            (list "Index" "makeindex %s" 'TeX-run-command nil t)
            ;;             (list "Check" "lacheck %s" 'TeX-run-compile nil t)
            ;;             (list "Other" "" 'TeX-run-command t t)
            ))

;; -------------------------------------------------------------------
;; ispell : aspell

(setenv "PATH" (concat (concat usb-drive-letter "Programmes/hunspell;") (getenv "PATH")))

(setq-default ispell-program-name "P:/Programmes/aspell/bin/aspell")
(setq ispell-dictionary "francais")

(setq ispell-personal-dictionary "~/.emacs.d/personnal-dic")



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


;; -------------------------------------------------------------------
;; Librairies annexes
;; -------------------------------------------------------------------

;; Pour utiliser Tidy, un bon "parser" HTML
;; (autoload 'tidy-buffer "tidy" "Run Tidy HTML parser on current buffer" t)
;; (autoload 'tidy-parse-config-file "tidy" "Parse the `tidy-config-file'" t)
(autoload 'tidy-save-settings "tidy" "Save settings to `tidy-config-file'" t)
(autoload 'tidy-build-menu  "tidy" "Install an options menu for HTML Tidy." t)

;; -------------------------------------------------------------------
;; Emacs powââââ....


;; Un template pour définir quelques abréviation d'environnements
;; Avec Skeleton & abbrev
(define-skeleton sk-align
  "Insert LaTeX align environment"
  "Commentaire: "
  "\\begin{align*}\n"
  "   " _ "\n"
  "\\end{align*}\n")

(define-abbrev text-mode-abbrev-table "xal" 
  "" 'sk-align)

(define-skeleton sk-multline
  "Insert LaTeX align environment"
  "Commentaire: "
  "\\begin{multline*}\n"
  "   " _ "\n"
  "\\end{multline*}\n")

(define-abbrev text-mode-abbrev-table "xmu" 
  "" 'sk-multline)

(define-skeleton sk-equation
  "Insert LaTeX equation environment"
  "Commentaire: "
  "\\begin{equation*}\n"
  "   " _ "\n"
  "\\end{equation*}\n")

(define-abbrev text-mode-abbrev-table "xeq" 
  "" 'sk-equation)

(define-skeleton sk-finale-star
  "Insert LaTeX finale environment"
  "Commentaire: "
  "\\begin{finale*}[" str | "" "]\n"
  "    " _ "\n"
  "\\end{finale*}\n")

(define-abbrev text-mode-abbrev-table "xff" 
  "" 'sk-finale-star)

(define-skeleton sk-finale
  "Insert LaTeX finale environment"
  ""
  "\\begin{finale} "_"  \\end{finale}")

(define-abbrev text-mode-abbrev-table "xf" 
  "" 'sk-finale)

(define-skeleton sk-enonce-entete
  "Insert LaTeX enonce header"
  ""
  "%% -------------------------------------------------------------------\n"
  "%%CONCOURS      "_"\n"
  "%%COMMENTAIRE   \n"
  "%%MOTS-CLES     \n"
  "%%DIFFICULTE    \n"
  "%%LABEL         \n"
  "%%ORIGINE       \n"
  "%%EPREUVE       \n"
  "%%GENRE         \n"
  "%%TITRE         \n"
  "%%FILIERE       \n"
  "%% -------------------------------------------------------------------\n"
  "\n\\begin{Exercise}\n\n\\end{Exercise}\n"
  "%% -------------------------------------------------------------------\n"
  "\n\\begin{Answer}\n\tPas de solution.\n\\end{Answer}\n"
  )

(define-skeleton sk-leftcenterright
  "Insert H&K leftcenterright command"
  ""
  "\\leftcentersright{}{$\n" _ "\n$}{}\n") 

(define-abbrev text-mode-abbrev-table "xlc" 
  "" 'sk-leftcenterright)

(define-skeleton sk-python-cmd
  "@setlocal enableextensions & python -x %~f0 %* & goto :EOF\n"
  "# -*- coding: utf8 -*-\n\n"
  )


(define-skeleton sk-vfpp
  "Insert LaTeX align environment"
  ""
  "\\VFPP{"
  "" _ ""
  "}")

(define-abbrev text-mode-abbrev-table "vp" 
  "" 'sk-vfpp)

(define-skeleton sk-dtpp
  "Insert LaTeX align environment"
  ""
  "\\DTPP{"
  "" _ ""
  "}")

(define-abbrev text-mode-abbrev-table "dp" 
  "" 'sk-dtpp)


(define-skeleton sk-thm
  "Insert theoreme environment"
  "Commentaire: "
  "\\begin{theoreme}[" _ "]\n"
  "   \n"
  "\\end{theoreme}\n")

(define-abbrev text-mode-abbrev-table "xthm" 
  "" 'sk-thm)

(define-skeleton sk-definition
  "Insert definition environment"
  "Commentaire: "
  "\\begin{definition}[" _ "]\n"
  "   \n"
  "\\end{definition}\n")

(define-abbrev text-mode-abbrev-table "xdef" 
  "" 'sk-definition)

;; ----------------------------------------------------------------
;; Fonctions Lisp utiles

;; pour faire n'importe quoi
(require 'lorem-ipsum)

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

(defun kill-line-append-save ()
  "Yank and paste current line"
  (interactive)
  (kill-line)
  (yank)
  )

;; Pour faire toute une ligne de commentaire
(defun comment-full-line ()
  (interactive)
  (beginning-of-line)
  (insert "-------------------------------------------------------------------\n")
  (next-line -1)
  (comment-and-go-down))

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

(define-key global-map [(shift down)] 'comment-and-go-down)
(define-key global-map [(shift up)] 'comment-and-go-up)
(define-key global-map [(meta down)] 'uncomment-and-go-down)
(define-key global-map [(meta up)] 'uncomment-and-go-up)

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

(defun brace-current-region ()
  "Met des accolades autour de la région"
  (interactive)
  (command-current-region nil))

(defun query-command-current-region ()
  "Met des accolades autour de la région, et la commande passée en
argument devant"
  (interactive )
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

(add-hook 'after-save-hook
          (lambda ()
            (highlight-changes-remove-highlight (point-min) (point-max))))


;; (defun conditionnal-concat (before during after)
  ;; ;; "Concatène les trois chaines, sauf si la chaine during est vide, auquel cas on ne met rien."
  ;; ((if (eq during "")
       ;; ()
     ;; (concat before during after)
;; )))


;; ----------------------------------------------------------------
;; Mon propre clavier

(global-set-key (kbd "RET") 'newline-and-indent)

;; key binding similar to other Windows applications
(global-set-key [(control f)] 'isearch-forward)
(global-set-key [(control s)] 'save-buffer)
;; (global-set-key [f3] 'isearch-repeat-forward)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)

;; Comme M-tab n'est pas disponible avec windows :-(
;; (define-key function-key-map [(control tab)] [?\M-\t])

;; pour faire comme sous windows : C-a C-v C-z M-f4
(global-set-key "\C-a" 'mark-whole-buffer)
(global-unset-key "\C-z")
(global-set-key "\C-z" 'undo)


(global-set-key "\M-k" 'kill-line-append-save )

;; Pour fixer un point et y revenir : C-(&) et C-(")
(global-set-key [(control &)] '(lambda () (interactive) (point-to-register 38 nil)))
(global-set-key [(control \")] '(lambda () (interactive) (jump-to-register 38 nil)))

;; Find file and open in other window
(global-set-key (kbd "C-x C-g") 'find-file-other-window)
;; Find file at point
(global-set-key (kbd "C-x f") 'ffap)

;; Ma manière de me déplacer 
;; (global-set-key [(meta delete)] 'kill-sexp)
(global-set-key [(control backspace)] 'backward-kill-word)
(global-set-key [C-next] 'windmove-down)
(global-set-key [C-prior] 'windmove-up)
(global-set-key [C-left]  'beginning-of-line)
(global-set-key [C-right]   'end-of-line)
(global-set-key [C-up] 'scroll-down-line)
(global-set-key [C-down] 'scroll-up-line)

(global-set-key [(control \,)] 'goto-line)
;; (global-set-key [(control \.)] 'call-last-kbd-macro) 
(global-set-key [(control j)] 'join-line)

(setq mouse-wheel-scroll-amount '(1))


(defun duplicate-line()
  (interactive)
  ;; (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )

(global-set-key [(control \;)] 'duplicate-line)


;; Touches de fonction
(global-set-key [f1] 'iswitchb-buffer)
;; Les bookmarks (indispensables !): f1
(global-set-key [M-f1] 'bookmark-jump)
(global-set-key [C-f1] 'bookmark-set)
(global-set-key [S-f1] 'kpse-open)

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


;; Les commentaires : f2
(global-set-key [f2] 'comment-dwim)
(global-set-key [C-f2] 'uncomment-region)
(global-set-key [S-f2] 'comment-full-line)

;; Les greps : f3
(global-set-key [f3] 'grep)
(global-set-key [C-f3] 'grep-find)

;; Les dates : f4
(global-set-key [f4]    'insert-buffer-file-name-nond-sansext)
(global-set-key [S-f4] 'insert-date)
(global-set-key [C-f4] 'insert-very-short-date)
(global-set-key [M-f4] 'insert-very-short-date-file)

;; Les remplacements : f5
(global-set-key [f5] 'replace-string)
(global-set-key [C-f5] 'replace-regexp)
(global-set-key [M-f5] 'occur)
(global-set-key [S-f5] 'query-replace)

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

;; global key for `multi-occur-in-this-mode' - you should change this.
(global-set-key (kbd "C-S-<f5>") 'multi-occur-in-this-mode)



;; Pour tuer les buffers : f6
(global-set-key [f6] 'kill-this-buffer)
(global-set-key [S-f6] 'kill-some-buffers)

;; Quelques modes utilisés : f7
(global-set-key [S-f7]   'flyspell-mode)
(global-set-key [M-f7]   'metafont-mode)
(global-set-key [C-f7]   'wikipedia-mode)

;; Pour diviser la fenêtre en deux ou n'en mettre qu'une : f8 C-f8
(global-set-key [f8] 'split-window-vertically)
(global-set-key [C-f8] 'delete-other-windows)

(global-set-key [(control *)] 'next-error)

;; Allez à la ligne n° : f9
(global-set-key [S-f9] 'goto-line)
;;; Set F8 as a shortcut key to perform forward search
(global-set-key [f9] 'sumatra-jump-to-line)

;; ;; ispell/flyspell : f10
(global-set-key [f10] 'ispell-buffer)
(global-set-key [C-f10] 'ispell-region)
(global-set-key [S-f10] 'flyspell-buffer)

;; Pour rendre le buffer plus joli : f12 
;; refill-mode automatise le 'refilling', mais c'est assez pénible
;; pour la saisie courante
(global-set-key [S-f12] 'refill-mode)  
(global-set-key [C-f12] 'indent-region)

(global-set-key [f12] 'ecb-activate) ;; ecb
(global-set-key [M-f12] 'highlight-changes-mode)



;; Enfin la souris : les copiers/coller
(global-set-key [S-mouse-2] 'yank-pop)
(global-set-key [mouse-2] 'yank)

;; Redefinitions de touches valables uniquement dans le mode LaTeX
(add-hook 'LaTeX-mode-hook 'my-latex-keybindings)
(defun my-latex-keybindings ()
  (progn
    ;; (local-set-key (quote [C-tab]) 'TeX-complete-symbol)
    ;; é <-> \'e etc. Merci Walter !
    (local-set-key [f11] 'latex2iso)
    (local-set-key [S-f11] 'quotedprintable2iso)
    (local-set-key [C-f11] 'iso2latex)
    ;; Permet de mettre une région en paramètre d'une commande
    (local-set-key [(control =)] 'query-command-current-region)
    (local-set-key [apps] 'TeX-command-master)
    (local-set-key [(control *)] 'TeX-next-error)
    (local-set-key [f7]     'latex-mode)     
    (local-set-key [C-f7]   'latex-math-mode) ; Il est cool celui-là !
    ;; (local-set-key [tab]    'hippie-expand)     
    )
  )

;; -------------------------------------------------------------------

(defun next-user-buffer ()
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer) )))

(global-set-key (kbd "<C-tab>") 'next-user-buffer) 

;; -------------------------------------------------------------------
;; Version control system
(setq gitpath (concat usb-drive-letter "Programmes/git/bin"))
(setenv "PATH" (concat gitpath ";" (getenv "PATH")))
(setq exec-path (cons gitpath exec-path))

(add-to-list 'load-path (cuid "site-lisp/magit/"))
(require 'magit)

;; ;; ;; (require 'git)
;; ;; ;; (require 'git-mswin)

(require 'tabbar)

(add-to-list 'load-path "~/.emacs.d/site-lisp/ecb")
(require 'ecb-autoloads)
(setq stack-trace-on-error t)
(setq ecb-activate-hook '(lambda ()  (set-frame-size (selected-frame) 140 66)))
(defun my-buffer-face-mode-courrier ()
    (interactive)
    (setq buffer-face-mode-face '(:foreground "#93a1a1" :background "#002b36"))
    (buffer-face-mode))

;; '(font-lock-keyword-face ((t (:foreground "Navy" :slant normal :weight bold))))
(add-hook 'eshell-mode-hook 'my-buffer-face-mode-courrier)


(add-to-list 'load-path
              "~/.emacs.d/elpa/yasnippet-0.6.1")
(require 'yasnippet)
;; (require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(setq yas/root-directory "~/.emacs.d/snippets")
;; Load the snippets
(yas/load-directory yas/root-directory)

(setq yas/prompt-functions '( yas/ido-prompt
                              yas/completing-prompt))

(require 'popup)
;; add some shotcuts in popup menu mode
(define-key popup-menu-keymap (kbd "M-n") 'popup-next)
(define-key popup-menu-keymap (kbd "TAB") 'popup-next)
(define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
(define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
(define-key popup-menu-keymap (kbd "M-p") 'popup-previous)

(defun yas/popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))

;; (setq yas/prompt-functions '(yas/popup-isearch-prompt yas/no-prompt))

;; -------------------------------------------------------------------
;; utilisation d'org-mode

(setq org-agenda-files (list "~/journal.org"))
(setq initial-buffer-choice "~/journal.org")


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
 '(LaTeX-math-list (quote ((38 "&" "" nil))))
 '(TeX-auto-save nil)
 '(TeX-brace-indent-level 4)
 '(TeX-byte-compile nil)
 '(TeX-check-path (quote ("./" "P:/texmf/tex/")))
 '(TeX-close-quote "\\fg")
 '(TeX-file-extensions (quote ("tex" "sty" "cls" "ltx" "texi" "texinfo" "dtx" "mtx" "etx")))
 '(TeX-macro-global (quote ("P:/texmf/tex/")))
 '(TeX-macro-private (quote ("P:/localtexmf/tex/")))
 '(TeX-output-view-style (quote (("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "dvips %d -o && start \"\" %f") ("^dvi$" "." "yap -1 %dS %d") ("^pdf$" "." "SumatraPDF.exe %o") ("^html?$" "." "start \"\" %o"))))
 '(TeX-parse-self t)
 '(ansi-color-names-vector ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(archive-zip-expunge (quote ("zip" "-d")))
 '(archive-zip-extract (quote ("unzip" "-e" "-o-")))
 '(archive-zip-update (quote ("zip" "-u")))
 '(archive-zip-update-case (quote ("zip" "-u" "-P")))
 '(background-color "#fdf6e3")
 '(background-mode light)
 '(calendar-date-style (quote european))
 '(calendar-day-name-array ["Dimanche" "Lundi" "Mardi" "Mercredi" "Jeudi" "Vendredi" "Samedi"])
 '(calendar-european-date-display-form (quote ((if dayname (concat dayname "  ")) day " " monthname " " year)))
 '(calendar-month-abbrev-array ["Jan" "Fév" "Mar" "Avr" "Mai" "Jui" "Jul" "Aou" "Sep" "Oct" "Nov" "Dec"])
 '(calendar-month-name-array ["Janvier" "Février" "Mars" "Avril" "Mai" "Juin" "Juillet" "Août" "Septembre" "Octobre" "Novembre" "Decembre"])
 '(calendar-week-start-day 1)
 '(case-fold-search t)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(cursor-in-non-selected-windows nil)
 '(custom-safe-themes (quote ("526abfd99b7c0184a79dfeda39a591c78a4633cf092cc7f273bf1dba4f6b1c3a" "1d4a0b079319f925dd5605dabc423a1539d3f7484e852253827cc710cd1f1f94" "713e49ca8c53a3af0a0270ff64df41039ba82148b0fae49878af27a67a9fc078" "6d34be815075c7aa18c676a66568ee6bd70a4e6e75a2a88ce0690ea380de8153" "ccd24002ff5125265d21c198e467b5ff8a78048a61f9a8f594d877b55b88af84" "34c8118dc62283ce538e5343571faf564b9c0161464fdaef6c2db919bf71e83b" "03fddc40283d16f04ca7367f5f1a016b6db667135b8bdb8f30e7c5d35ef7cf7a" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "13fef26f56ee55aad1b752d22a2cfce236ab8cc38d4f24e63f56f9ebabe0bbcf" "1f419ef5dc40b10bab8be95a888e27998bd8058ae3d752e7b2b698f9c048f85a" "98be73f3572bbba8f9fd12f2b0be2b4fd806146229417bd436e02d846af1c586" "8c9f149ab3de2757baf60a15b4e5d2eeba2906a74a2ae4ea991878b37ad6fd89" "d59b38abf7a784bde1f59f4b9c00b73318978bc0a8f59e4f86040f1f7ade7784" "a26f8279803728f486e165f372f4f6a5c99d3c2690365a73a6a573a112550342" "eacd07c67b857be7d208650cad3effdd8f9a7192b69eb9a2400904a5ddeba840" "bc7ab2310b0b7cdc30917ed6299b9338eefe40523f7b64a65e3f5fe0eaad0429" "d58351530dee2e383d41ff7cd54317b848da016b34396c0b91dd0281f0b2a7ac" "dbd637d5895f2a6f7bcf0f6f67c8f6e8d9f0cfec105d12817dee6b913c6c712e" "b8d1250d4c289a8645add08d688b026188a98fe47c70814d56e11d0e45c627dd" "065829b6be5b3086456a59aa0bb68c313aad1b318a79d1f1ea88375b811ee275" "9b980c223f3c8f583cc4ee562d806768a1b6f5bc62097244d48092822efd77da" "c1b64838c7fcf4945785ad83c34337b21dd24f35b359ea2403533748301ce904" default)))
 '(default-frame-alist (quote ((menu-bar-lines . 1) (foreground-color . "SystemWindowText") (background-color . "SystemWindow") (top . 0) (left . 0) (width . 83) (height . 65))))
 '(desktop-save-mode nil nil (desktop))
 '(ecb-auto-activate t)
 '(ecb-compile-window-height nil)
 '(ecb-layout-name "Alcandre4")
 '(ecb-layout-window-sizes (quote (("Alcandre4" (ecb-methods-buffer-name 0.2 . 0.49230769230769234) (ecb-history-buffer-name 0.2 . 0.49230769230769234) (ecb-directories-buffer-name 0.21379310344827587 . 0.49230769230769234) (ecb-sources-buffer-name 0.21379310344827587 . 0.49230769230769234)))))
 '(ecb-minor-mode-text "")
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-source-file-regexps (quote ((".*" ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\|log\\|toc\\|aux\\|out\\|table\\|synctex\\|bbl\\|blg\\|ilg\\|ind\\|idx\\|gnuplot\\)$\\)\\)") ("^\\.\\(emacs\\|gnus\\)$")))))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.2)
 '(european-calendar-style t)
 '(fci-rule-color "#383838")
 '(file-cache-find-command-posix-flag t)
 '(font-latex-fontify-script t)
 '(font-latex-fontify-sectioning 1)
 '(font-latex-match-textual-keywords (quote ("item" "part" "chapter" "section" "subsection" "subsubsection" "paragraph" "subparagraph" "subsubparagraph" "title" "author" "date" "thanks" "address" "caption" "Exercise" "Question" "subQuestion" "subsubQuestion")))
 '(font-latex-quotes (quote french))
 '(font-latex-title-fontity (quote color))
 '(font-latex-verb-like-commands (quote ("verb" "verb*")))
 '(foreground-color "#657b83")
 '(fringe-mode 0 nil (fringe))
 '(global-font-lock-mode t nil (font-lock))
 '(global-linum-mode t)
 '(grep-window-height nil)
 '(imenu-auto-rescan t)
 '(indicate-buffer-boundaries (quote left))
 '(indicate-empty-lines t)
 '(ispell-silently-savep t)
 '(ispell-use-framepop-p t)
 '(iswitchb-buffer-ignore (quote ("^ " "^\\*.*\\*")))
 '(iswitchb-mode t)
 '(italic ((t (:slant italic))))
 '(line-number-mode nil)
 '(lunar-phase-names (quote ("Nouvelle lune" "Premier quartie" "Pleine lune" "Dernier quartier")))
 '(mouse-drag-copy-region t)
 '(mouse-yank-at-point t)
 '(org-hierarchical-checkbox-statistics nil)
 '(org-hierarchical-todo-statistics nil)
 '(org-log-done (quote time))
 '(org-startup-folded nil)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(pdf-view-command "gsview32.exe")
 '(php-manual-url "http://www.php.net/manual/fr/manual.php")
 '(preview-gs-command "P:\\Programmes\\gs\\gs8.64\\bin\\GSWIN32C.EXE")
 '(preview-image-type (quote pnm))
 '(preview-scale-function 1.75)
 '(ps-view-command "gsview32.exe")
 '(py-honor-comment-indentation nil)
 '(py-imenu-show-method-args-p t)
 '(read-buffer-function nil)
 '(recentf-menu-filter (quote recentf-show-basenames))
 '(same-window-buffer-names (quote ("*eshell*" "*mail*" "*info*" "*inferior-lisp*" "*ielm*" "*scheme*")))
 '(save-abbrevs (quote silently))
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-paren-ring-bell-on-mismatch t)
 '(show-paren-style (quote mixed))
 '(size-indication-mode t)
 '(solar-n-hemi-seasons (quote ("Equinoxe de printemps" "Solstice d'été" "Équinoxe d'Automne" "Solstice d'hiver")))
 '(tabbar-mode t nil (tabbar))
 '(tabbar-mwheel-mode t nil (tabbar))
 '(tags-case-fold-search t)
 '(template-default-directories (quote ("P:/Paul/.emacs.d/templates/")))
 '(tex-close-quote " \\fg")
 '(tex-open-quote "\\og ")
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tidy-shell-command "c:/gnu/bin/tidy")
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(uniquify-separator nil))


(put 'narrow-to-region 'disabled nil)

(put 'narrow-to-page 'disabled nil)

(put 'downcase-region 'disabled nil)




(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#fdf6e3" :foreground "SystemWindowText" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :family "Consolas"))))
 '(tabbar-default ((t (:inherit variable-pitch :background "gray50" :foreground "gray75" :box nil :height 0.9))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "black" :foreground "#fdf6e3" :box (:line-width 1 :color "white" :style pressed-button)))))
 '(variable-pitch ((t (:family "Segoe UI")))))


