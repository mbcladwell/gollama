(setq user-emacs-directory "~/.emacs.d/")
(tool-bar-mode -1)
(setq debug-on-error t)
(add-to-list 'load-path ".")
;;(add-to-list 'load-path "/usr/share/emacs/site-lisp/ess")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(load-theme 'zenburn t)
(show-paren-mode t)
(electric-pair-mode t)
(transient-mark-mode t)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(column-number-mode)
(global-display-line-numbers-mode t)
(global-auto-revert-mode 1)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq visible-bell t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;;next added to subvert bug
(setq package-check-signature 'nil)
(save-place-mode 1)
(setq history-length 25)
(savehist-mode 1)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
;; For all buffers
(global-font-lock-mode 1)
(global-set-key (kbd "<f12>") 'other-window)
(require 'diff-mode)
(require 'arei)
(global-set-key (kbd "<f5>") 'arei-evaluate-last-sexp)
(global-set-key (kbd "<f6>") 'arei-evaluate-sexp)
(global-set-key (kbd "<f7>") 'arei-evaluate-buffer)
(global-set-key (kbd "<f8>") 'arei-evaluate-region)
 (global-set-key (kbd "<f9>") 'arei-evaluate-defun)

(defun load-and-run-fave-file()
  (interactive)
;;   (find-file "~/projects/babweb/babweb/test/run.scm")
   (find-file "~/projects/gollama/test/test.scm")
   (find-file "~/projects/gollama/gollama/utilities.scm")
   (find-file "~/projects/gollama/gollama/ollama.scm")
   (find-file "~/projects/gollama/gollama/db.scm")
   (find-file "~/projects/gollama/gollama/menus.scm")
  
  ; (find-file "~/projects/bookmunger/bookmunger.scm")
  (sesman-start)
  )
  
(global-set-key (kbd "<f4>") 'load-and-run-fave-file)



;;(setq geiser-mode-auto-p nil) 
;; Initialize package sources
(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(require 'orderless) ;;search in any order; space separated words; works with vertico
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))


(use-package marginalia
  :after vertico
  :ensure t
  :custom
(marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode t)
  )
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
 ;; (evil-collection-define-key 'normal 'dired-mode-map
 ;;   "h" 'dired-single-up-directory
 ;;   "l" 'dired-single-buffer)
  )

(use-package dired-single
  :commands (dired dired-jump))

(load-and-run-fave-file)
