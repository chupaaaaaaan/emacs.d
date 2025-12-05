;;; package --- Summary

;;; Commentary:

;; chupaaaaaaan's Emacs settings.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-and-compile
  ;; Consts
  (defconst chpn/dir-jars      "~/.local/jar/")
  (defconst chpn/dir-pkg-local "~/.local/elisp/")

  ;; Functions
  (defun chpn/open-file (fname)
    "Open FNAME and switch to the buffer non-interactively."
    (switch-to-buffer (find-file-noselect fname)))

  (defun chpn/copy-directory-recursively (src dest)
    "Copy files that only exist in SRC directory to DEST recursively."
    (unless (file-directory-p src)
      (error "chpn/copy-directory-recursively: SRC must be a directory: %s" src))
    (dolist (file (directory-files src nil "^[^.].*"))
      (let* ((from-path (expand-file-name file src))
             (to-path (expand-file-name file dest)))
        (if (file-directory-p from-path)
            (when (or (not (file-exists-p to-path)) (file-directory-p to-path))
              (chpn/copy-directory-recursively from-path to-path))
          (chpn/copy-file-if-not-exist from-path to-path)))))

  (defun chpn/copy-file-if-not-exist (src dest)
    "Copy SRC file to DEST.
- When DEST exists and is a directory,
  if no file with the same name as SRC exists in DEST, copy SRC to DEST.
- When DEST does not exist and its name represents a directory,
  make DEST directory and copy SRC to DEST.
- When DEST does not exist and its name represents a file, copy SRC to DEST.
- In cases other than the above, no copy will occur."
    (unless (file-regular-p src)
      (error "chpn/copy-file-if-not-exist: SRC must be a file: %s" src))
    (if (file-exists-p dest)
        (when (file-directory-p dest)
          (let* ((file (file-name-nondirectory src))
                 (dest-name (expand-file-name file dest)))
            (unless (file-exists-p dest-name)
              (copy-file src dest-name))))
      (let ((parent-dir (file-name-directory dest)))
        (unless (file-directory-p parent-dir)
          (make-directory parent-dir t))
        (copy-file src dest))))

  (defun chpn/from-dir-jars (jar-file) (expand-file-name jar-file chpn/dir-jars))

  ;; Directory setup
  (dolist (dir (list chpn/dir-jars chpn/dir-pkg-local))
    (unless (file-directory-p dir)
      (make-directory dir t)))

  ;; Local settings
  ;; "my:"から始まる設定はローカルで実施する設定
  (chpn/copy-file-if-not-exist (concat user-emacs-directory "local-setting.el") chpn/dir-pkg-local)
  (add-to-list 'load-path chpn/dir-pkg-local)
  (require 'local-setting nil t))

(eval-and-compile
  ;; HTTP_PROXY環境変数が存在している場合のみ、「scheme://」を削った文字列をプロキシとして設定する
  ;; HTTP_PROXYおよびHTTPS_PROXYは同じになる想定
  (let ((http-proxy (getenv "HTTP_PROXY")))
    (when http-proxy
      (let ((proxy-host (replace-regexp-in-string ".*//" "" http-proxy)))
        (customize-set-variable 'url-proxy-services `(("http" . ,proxy-host) ("https" . ,proxy-host))))))

  (customize-set-variable
   'package-archives '(("org"          . "https://orgmode.org/elpa/")
                       ("melpa"        . "https://melpa.org/packages/")
                       ("melpa-stable" . "https://stable.melpa.org/packages/")
                       ("gnu"          . "https://elpa.gnu.org/packages/")))
  (customize-set-variable 'gnutls-algorithm-priority  "normal:-vers-tls1.3")

  (package-initialize)

  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf blackout :ensure t)
    :config
    (leaf-keywords-init)))

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree :ensure t))

(leaf macrostep :ensure t
  :bind
  ("C-c e" . macrostep-expand))

(leaf transient-dwim :ensure t
  :bind
  ("M-=" . transient-dwim-dispatch))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; define prefix-key
(define-prefix-command 'chpn-function-prefix)
(define-key global-map (kbd "M-i") 'chpn-function-prefix)

(define-prefix-command 'chpn-toggle-prefix)
(define-key global-map (kbd "M-t") 'chpn-toggle-prefix)

(global-unset-key (kbd "C-x C-c"))
(defalias 'exit 'save-buffers-kill-emacs)

;; Settings that do not depend on some major modes or minor modes
(global-set-key (kbd "C-h")   'delete-backward-char)
(global-set-key [f7] (lambda () (interactive) (chpn/open-file (concat user-emacs-directory "init.el"))))
(global-set-key [f8] (lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key (kbd "C-+") (lambda () (interactive) (insert (format-time-string "%F" (current-time)))))
(global-set-key (kbd "C-*") (lambda () (interactive) (insert (format-time-string "%F %T" (current-time)))))

(leaf cus-start
  :custom
  (menu-bar-mode . nil)
  (tool-bar-mode . nil)
  (indent-tabs-mode . nil)
  (transient-mark-mode . t)
  (scroll-conservatively . 101)
  (scroll-margin . 10)
  (history-delete-duplicates . t)
  (history-length . 1000)
  (message-log-max . 10000)
  (enable-recursive-minibuffers . t)
  (auto-save-timeout . 20)
  (auto-save-interval . 60)
  (bidi-paragraph-direction . 'left-to-right)
  (display-line-numbers . t)
  `(gc-cons-threshold . ,(* 10 gc-cons-threshold))
  :hook
  (after-change-major-mode-hook . disable-line-numbers-for-specific-modes)
  ;; (buffer-list-update-hook . disable-line-numbers-for-buffer-name-patterns)
  :preface
  (defun disable-line-numbers-for-specific-modes ()
    "Disable line numbers for specific modes."
    (when (derived-mode-p 'eshell-mode
                          'shell-mode
                          'vterm-mode
                          'dired-mode
                          'org-agenda-mode
                          'treemacs-mode)
      (display-line-numbers-mode 0)))
  (defun disable-line-numbers-for-buffer-name-patterns ()
    "Disable line numbers for buffers with names which match some patterns."
    (dolist (pattern '("^\*.*\*$" "^CAPTURE-.*\.org$"))
      (when (string-match-p pattern (buffer-name))
        (display-line-numbers-mode 0)))))

(leaf cus-edit
  :custom
  `(custom-file . ,(concat user-emacs-directory "customize.el")))

(leaf startup
  :custom
  (inhibit-startup-screen . t))

(leaf scroll-bar
  :custom
  (scroll-bar-mode . nil))

(leaf savehist
  :custom
  (savehist-mode . t))

(leaf mouse
  :custom
  (mouse-yank-at-point . t))

(leaf files
  :custom
  (backup-directory-alist . `((".*" . ,(concat user-emacs-directory ".cache/"))))
  (make-backup-files . t)
  (auto-save-default . t))

(leaf autorevert
  :custom
  (global-auto-revert-mode . t)
  (auto-revert-interval . 0.5))

(leaf simple
  :bind
  ("C-," . previous-error)
  ("C-." . next-error)
  (chpn-toggle-prefix
   :package init
   ("l" . toggle-truncate-lines))
  :config
  (line-number-mode 1)
  (column-number-mode 1))

(leaf mb-depth
  :custom
  (minibuffer-depth-indicate-mode . t))

(leaf elec-pair
  :bind
  (chpn-toggle-prefix
   :package init
   ("e" . electric-pair-local-mode))
  :custom
  (electric-pair-mode . t))

(leaf hungry-delete :ensure t
  :blackout t
  :bind
  (chpn-toggle-prefix
   :package init
   ("d" . global-hungry-delete-mode))
  :custom
  (global-hungry-delete-mode . t)
  (hungry-delete-join-reluctantly . t))

(leaf shut-up :ensure t)

(leaf uniquify
  ;; :require t
  :custom
  (uniquify-buffer-name-style . 'post-forward)
  (uniquify-separator . "|"))

(leaf browse-url
  :custom
  (browse-url-generic-program . "wslview")
  (browse-url-browser-function . 'browse-url-generic))

(leaf centaur-tabs :ensure t
  :disabled t
  :defun (centaur-tabs-headline-match centaur-tabs-get-group-name)
  :bind
  ("M-[" . centaur-tabs-backward)
  ("M-]" . centaur-tabs-forward)
  ("C-c t b" . centaur-tabs-switch-group)
  ("C-c t p" . centaur-tabs-group-by-projectile-project)
  ("C-c t g" . centaur-tabs-group-buffer-groups)
  :hook
  (term-mode-hook . centaur-tabs-local-mode)
  (calendar-mode-hook . centaur-tabs-local-mode)
  (helpful-mode-hook . centaur-tabs-local-mode)
  :custom
  (centaur-tabs-style . "bar")
  (centaur-tabs-height . 20)
  (centaur-tabs-set-icons . t)
  (centaur-tabs-set-bar . 'over)
  (centaur-tabs-cycle-scope . 'tabs)
  ;; (centaur-tabs-set-modified-marker . t)
  (centaur-tabs-set-close-button . nil)
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  ;; (centaur-tabs-enable-buffer-reordering)
  (defun centaur-tabs-buffer-groups ()
    "Control buffers' group rules.
This function overwrite default function in centaur-tabs-functions.el.
Original function is from
https://github.com/ema2159/centaur-tabs#my-personal-configuration"
    (list
     (cond
      ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
      ;; "Remote")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode)))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer)))))))

;;TODO: 雑多な設定を整理する
;; maximize frame
(toggle-frame-maximized)

;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;; Increase the amount of data which Emacs reads from the process.
;; Again the emacs default is too low 4k considering that the some of the language server responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024))

(leaf nerd-icons :ensure t :require t)

(leaf font-setting
  :defun chpn/set-font
  :if (display-graphic-p)
  :bind
  (chpn-function-prefix
   :package init
   ("f" . chpn/choice-font))
  :pre-setq
  (use-default-font-for-symbols . nil)
  (inhibit-compacting-font-caches . t)
  :init
  (defun chpn/set-font (size)
    "set font size."
    (set-face-attribute 'default nil :family "Ricty ShinDiminished" :height size)
    (set-fontset-font nil 'unicode (font-spec :family "Ricty ShinDiminished") nil 'append)
    (set-fontset-font nil 'unicode (font-spec :family "Symbols Nerd Font Mono") nil 'append))
  (defun chpn/choice-font ()
    (interactive)
    (let* ((candidates '(12 15 18 21 24 27))
           (selection (completing-read "Font size: " (mapcar 'number-to-string candidates)))
           (h (round (* (string-to-number selection) 10))))
      (chpn/set-font h)))
  (chpn/set-font 180))

(leaf mule
  :custom
  ;; (default-input-method . "japanese-mozc")
  (current-language-environment . "Japanese"))

;; input method
(leaf mozc :ensure t
  :disabled
  :if (eq system-type 'gnu/linux)
  :bind*
  ("<henkan>" . (lambda () (interactive) (unless current-input-method (toggle-input-method))))
  ("<muhenkan>" . (lambda () (interactive) (when current-input-method (toggle-input-method))))
  :config
  (leaf mozc-posframe
    :vc (;; original repository: derui/mozc-posframe
         ;; 最新版ではうまく動かない（原因は調べていない）ため、forkしたうえで動作するリビジョンを取得
         ;; なお`:branch'プロパティにはブランチ名・タグ名のみ指定可能 (内部的に `git clone --branch' を実行しているため)
         :url "https://github.com/chupaaaaaaan/mozc-posframe.git"
         :branch "version-my-using")
    :require t
    :custom
    (mozc-candidate-style . 'posframe)))

(leaf exec-path-from-shell :ensure t
  :if (or (eq system-type 'darwin)
          (eq system-type 'gnu/linux))
  :init
  (exec-path-from-shell-initialize))

(leaf keyfreq :ensure t
  :custom
  (keyfreq-mode . 1)
  (keyfreq-autosave-mode . 1)
  (keyfreq-buffer . "*KeyFrequency*"))

(leaf which-key :ensure t
  :blackout which-key-mode
  :custom
  (which-key-mode . t))

(leaf golden-ratio :ensure t
  :blackout t
  :bind
  (chpn-toggle-prefix
   :package init
   ("g" . golden-ratio-mode))
  :custom
  (golden-ratio-mode . t)
  (golden-ratio-extra-commands . '(ace-window
                                   projectile-vc
                                   persp-list-buffers
                                   quit-window
                                   xref-goto-xref
                                   undo-tree-visualizer-quit
                                   magit-mode-bury-buffer))
  (golden-ratio-exclude-modes . '(treemacs-mode
                                  imenu-list-major-mode)))

(leaf ace-window :ensure t
  :bind
  ("M-o" . ace-window)
  :custom
  (aw-dispatch-always . t))

(leaf follow
  :defvar (follow-mode)
  :bind
  (chpn-toggle-prefix
   :package init
   ("f" . toggle-follow-mode))
  :preface
  (defun toggle-follow-mode ()
    "Toggle follow mode."
    (interactive)
    (follow-mode (if follow-mode -1 1))
    (message "Follow mode %s" (if follow-mode "enabled" "disabled"))))

;; (leaf perspective
;;   :ensure t
;;   :leaf-defer nil
;;   :custom
;;   `((persp-state-default-file . ,(concat chpn/dir-cache "persp-state-file"))
;;     (persp-modestring-short . t))
;;   :bind
;;   ;; ("C-x b"   . persp-switch-to-buffer*)
;;   ("C-x k"   . persp-kill-buffer*)
;;   ("C-x C-b" . persp-bs-show)
;;   :hook
;;   (kill-emacs-hook . persp-state-save)
;;   :config
;;   (persp-mode))

(leaf nyan-mode :ensure t
  :custom
  (nyan-cat-face-number . 4)
  (nyan-animate-nyancat . t)
  (nyan-mode . t))

(leaf time
  :custom
  (display-time-interval . 60)
  (display-time-format . " %F %R ")
  (display-time-mode . t))

(leaf moody :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

;; (leaf hide-mode-line :ensure t
;;   :hook
;;   (treemacs-mode-hook . hide-mode-line-mode))

(leaf hl-line
  :custom
  (global-hl-line-mode . nil)
  :bind
  (chpn-toggle-prefix
   :package init
   ("h" . global-hl-line-mode)))

(leaf paren
  :require t
  :custom-face
  ;; (show-paren-match ((nil (:underline "#ff5555"))))
  (show-paren-match . '((nil (:background "#44475a" :foreground "#f1fa8c"))))
  :custom
  (show-paren-mode . t)
  (show-paren-delay . 0.3)
  (show-paren-style . 'mixed)
  (show-paren-when-point-inside-paren . t)
  (show-paren-when-point-in-periphery . t)
  :bind
  (chpn-toggle-prefix
   :package init
   ("p" . toggle-show-paren))
  :preface
  (defun toggle-show-paren ()
    "Toggle show paren."
    (interactive)
    (show-paren-mode (if show-paren-mode -1 1))
    (message "Show paren %s" (if show-paren-mode "enabled" "disabled"))))

(leaf rainbow-delimiters :ensure t
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

(leaf highlight-indent-guides :ensure t
  :blackout t
  :defvar highlight-indent-guides-mode
  :bind
  (chpn-toggle-prefix
   :package init
   ("i" . toggle-highlight-indent-guides))
  :hook
  ((prog-mode-hook yaml-mode-hook) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-character . 124)
  (highlight-indent-guides-auto-enabled . t)
  (highlight-indent-guides-responsive . 'stack)
  (highlight-indent-guides-method . 'column) ;; 'fill, 'column, 'character, 'bitmap
  ;; modus-vivendi-tinted テーマ向けに値を調整
  (highlight-indent-guides-auto-odd-face-perc . 50)
  (highlight-indent-guides-auto-even-face-perc . 100)
  (highlight-indent-guides-auto-character-face-perc . 100)
  (highlight-indent-guides-auto-top-odd-face-perc . 500)
  (highlight-indent-guides-auto-top-even-face-perc . 600)
  (highlight-indent-guides-auto-top-character-face-perc . 600)
  (highlight-indent-guides-auto-stack-odd-face-perc . 225)
  (highlight-indent-guides-auto-stack-even-face-perc . 300)
  (highlight-indent-guides-auto-stack-character-face-perc . 300)
  :preface
  (defun toggle-highlight-indent-guides ()
    "Toggle highlight indent guides."
    (interactive)
    (highlight-indent-guides-mode (if highlight-indent-guides-mode -1 1))
    (message "Highlight indent guides %s" (if highlight-indent-guides-mode "enabled" "disabled"))))

;; volatile-highlights
(leaf volatile-highlights :ensure t
  :require t
  :blackout t
  :defun (vhl/define-extension vhl/install-extension)
  :custom
  (volatile-highlights-mode . t)
  :config
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree))


(leaf modus-themes :ensure t
  :require t
  :custom
  (modus-themes-italic-constructs . t)
  (modus-themes-bold-constructs . nil)
  (modus-themes-hl-line . '(underline accented))
  (modus-themes-region . '(bg-only no-extend))
  (modus-themes-to-toggle . '(modus-operandi-tinted modus-vivendi-tinted))
  :bind
  ("<f5>" . modus-themes-toggle)
  :config
  (load-theme 'modus-vivendi-tinted t))

(leaf dashboard :ensure t
  :require t
  :custom
  (dashboard-startup-banner . 'logo)
  (dashboard-center-content . t)
  (dashboard-icon-type . 'nerd-icons)
  (dashboard-set-heading-icons . t)
  (dashboard-set-file-icons . t)
  (dashboard-items . '((recents   . 7)
                       (agenda    . 7)
                       (bookmarks . 15)))
  :bind
  ("<f6>" . dashboard-open)
  :config
  (dashboard-setup-startup-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf saveplace
  :custom
  (save-place-mode . t))

;; Recent files
(leaf recentf
  :defvar recentf-exclude
  :custom
  (recentf-mode . t)
  (recentf-max-saved-items . 20000000)
  (recentf-auto-cleanup . 'never)
  :hook
  (auto-save-hook . recentf-save-list)
  :config
  (dolist (pattern `(,user-emacs-directory "/tmp/"))
    (add-to-list 'recentf-exclude pattern)))

;; scroll-lock
(leaf scroll-lock
  :require t
  :bind
  (chpn-toggle-prefix
   :package init
   ("m" . scroll-lock-mode)))

(leaf undo-tree :ensure t
  :blackout t
  :custom
  (global-undo-tree-mode . t)
  (undo-tree-history-directory-alist . `((".*" . ,(concat user-emacs-directory ".cache/")))))

(leaf amx :ensure t)

(leaf vertico :ensure t
  :bind
  (vertico-map
   :package vertico
   ("M-RET" . minibuffer-force-complete-and-exit)
   ("M-TAB" . minibuffer-complete))
  :custom
  (vertico-mode . t)
  (read-extended-command-predicate . #'command-completion-default-include-p))

(leaf consult :ensure t
  :defvar (consult-xref)
  :custom
  (xref-show-xrefs-function . #'consult-xref)
  (xref-show-definitions-function . #'consult-xref)
  :bind
  ;;; Virtual Buffers
  ("C-x b" . consult-buffer)
  ;; ("C-x 4 b" . consult-buffer-other-window)
  ;; ("C-x 5 b" . consult-buffer-other-frame)
  ("C-x r b" . consult-bookmark)

  ;;; Editting
  ;; ("C-y" . consult-yank-from-kill-ring)
  ("M-y"   . consult-yank-pop)
  ("C-c k" . consult-kmacro)

  ;;; Register
  ("M-'"   . consult-register-store)
  ("M-#"   . consult-register-load)
  ("C-M-#" . consult-register)

  ;;; Navigation
  ("M-g g" . consult-goto-line)
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-g o" . consult-outline)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)

  ;;; Search
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)

  ;;; Grep and Find
  ("M-s g" . consult-ripgrep)
  ("M-s G" . consult-git-grep)
  ("M-s d" . consult-find)

  ;;; Compilation
  ("M-g e" . consult-compile-error)

  ;;; Histories
  ("C-x M-:" . consult-complex-command)
  ("C-c h" . consult-history)
  ("M-e"   . consult-isearch-history)

  ;;; Modes
  ("C-c m" . consult-mode-command)

  (minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history))
  (isearch-mode-map
   ("M-e" . consult-isearch-history))
  :hook
  (completion-list-mode-hook . consult-preview-at-point-mode))

(leaf isearch-mb :ensure t
  :bind
  ("C-s" . isearch-forward-regexp)
  ("C-r" . isearch-backward-regexp)
  :custom
  (isearch-mb-mode . t))

(leaf consult-ghq :ensure t
  :bind
  ;; ("M-s c s" . consult-ghq-switch-project)
  ("M-s c f" . consult-ghq-find)
  ("M-s c g" . consult-ghq-grep))

(leaf consult-projectile :ensure t
  :bind
  (projectile-command-map
   :package projectile
   ("b" . consult-projectile)
   ("p" . consult-projectile-switch-project)
   ("f" . consult-projectile-find-file)
   ("d" . consult-projectile-find-dir)
   ("e" . consult-projectile-recentf)))

(leaf consult-flycheck :ensure t
  :bind
  ("M-s e" . consult-flycheck))

(leaf consult-lsp :ensure t
  :bind
  ("M-s E" . consult-lsp-diagnostics)
  ("M-s s" . consult-lsp-file-symbols)
  ("M-s S" . consult-lsp-symbols))

(leaf orderless :ensure t
  :custom
  (completion-styles . '(orderless substring basic))
  (complefftion-category-overrides . '((file (styles orderless partial-completion substring basic)))))

(leaf marginalia :ensure t
  :custom
  (marginalia-mode . t)
  :bind
  (minibuffer-local-map
   ("M-A" . marginalia-cycle)))

(leaf anzu :ensure t
  :blackout t
  :bind
  ("M-r" . anzu-query-replace-regexp)
  :custom
  (global-anzu-mode . t)
  (anzu-deactivate-region . t)
  (anzu-search-threshold . 1000))

(leaf go-translate :ensure t
  :defvar (gt-default-translator
           gt-prompt-map
           gt-posframe-pop-render-timeout)
  :defun (gt-translator
          gt-taker
          gt-google-engine
          gt-posframe-pop-render
          gt-prompt-next-target)
  :custom
  (gt-langs . '(en ja))
  :bind
  (chpn-function-prefix
   :package init
   ("t" . gt-do-translate))
  :config
  (setq gt-posframe-pop-render-timeout nil
        gt-default-translator          (gt-translator
                                        :taker (gt-taker :prompt t)
                                        :engines (gt-google-engine)
                                        :render (gt-posframe-pop-render :backcolor "#333333" :forecolor "#ffffff"))
        gt-prompt-map                  (let ((map (make-sparse-keymap)))
                                         (set-keymap-parent map minibuffer-local-map)
                                         (define-key map "\C-g" #'top-level)
                                         (define-key map "\C-n" #'next-line-or-history-element)
                                         (define-key map "\C-p" #'previous-line-or-history-element)
                                         (define-key map "\M-n" #'gt-prompt-next-target)
                                         (define-key map "\M-p" (lambda () (interactive) (gt-prompt-next-target t)))
                                         (define-key map "\C-l" #'delete-minibuffer-contents)
                                         map)))

(leaf plz :ensure t)
(leaf emacsql :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf org :ensure t
  :defvar (org-directory
           org-mode-line-string
           org-agenda-files
           org-default-notes-file
           org-clock-effort
           org-modules
           recentf-exclude)
  :defun (org-back-to-heading
          org-clock-get-clocked-time
          org-clock-out
          org-clocking-p
          org-duration-to-minutes
          org-end-of-subtree
          org-entry-delete
          org-entry-end-position
          org-entry-get
          org-entry-put
          org-get-tags
          org-get-todo-state
          org-read-date
          org-todo
          org-update-statistics-cookies
          chpn/org-agenda-today-timestamp-until
          ladicle/task-clocked-time)
  :custom
  ;; files and directories
  `(org-directory . ,my:org-dir)
  (org-default-notes-file . "notes.org")
  (org-agenda-files . `(,(concat org-directory "agenda/") "notes.org"))

  ;; view style
  (org-startup-indented . t)
  (org-indent-indentation-per-level . 2)
  (org-startup-with-inline-images . t)
  (org-startup-folded . 'content)

  ;; agenda
  (org-agenda-span . 'day)
  (org-agenda-include-diary . nil)
  (org-agenda-dim-blocked-tasks . t)
  (org-agenda-window-setup . 'current-window)
  (org-agenda-log-mode-items . '(clock))
  (org-agenda-tags-todo-honor-ignore-options . t)
  (org-agenda-clockreport-parameter-plist . '(:maxlevel 2 :fileskip0 t :link t :tags t))
  (org-agenda-start-on-weekday . 0)
  (org-agenda-skip-deadline-if-done . t)
  (org-agenda-skip-scheduled-if-done . t)
  (org-agenda-skip-scheduled-if-deadline-is-shown . 'not-today)
  (org-agenda-skip-scheduled-delay-if-deadline . 'post-deadline)
  (org-agenda-use-time-grid . nil)
  (org-agenda-breadcrumbs-separator . " » ")
  (org-agenda-format-date
   . (lambda (date) (format-time-string "%x（%a）" (encode-time `(0 0 0 ,(nth 1 date) ,(nth 0 date) ,(nth 2 date))))))
  (org-habit-graph-column . 80)
  (org-extend-today-until . chpn/today-cutoff-hour) ;; 翌日午前`chpn/today-cutoff-hour'時までは当日とみなす
  (org-use-effective-time . t) ;; habitの一貫性グラフを正しく表示するために必要
  (org-agenda-remove-tags . t)
  (org-agenda-block-separator . ?·)
  (org-agenda-custom-commands . '(("i" "Agenda"
                                   ((agenda "" ((org-agenda-span 'day)
                                                (org-agenda-overriding-header "Main Agenda: Daily Tasks and Schedules")
                                                (org-habit-show-habits nil)
                                                (org-agenda-prefix-format " %i %-12:c%-12t% s [%4e] %.48b")))

                                    (agenda "" ((org-agenda-overriding-header "Habits")
                                                (org-agenda-skip-function
                                                 '(or (chpn/org-agenda-skip-if-noprop "STYLE" "habit")
                                                      (chpn/org-agenda-skip-if-tags '("START" "FINISH") t)))))

                                    (agenda "" ((org-agenda-overriding-header "Tasks to Perform at Start of Day")
                                                (org-agenda-skip-function
                                                 '(or (chpn/org-agenda-skip-if-noprop "STYLE" "habit")
                                                      (chpn/org-agenda-skip-if-notags '("START") t)))))

                                    (agenda "" ((org-agenda-overriding-header "Tasks to Perform at Finish of Day")
                                                (org-agenda-skip-function
                                                 '(or (chpn/org-agenda-skip-if-noprop "STYLE" "habit")
                                                      (chpn/org-agenda-skip-if-notags '("FINISH") t)))))

                                    (tags (format "CLOSED>=\"<%s>\"|LAST_REPEAT>=\"<%s>\"|TODO=\"DOING\""
                                                  (chpn/org-agenda-today-timestamp-until chpn/today-cutoff-hour)
                                                  (chpn/org-agenda-today-timestamp-until chpn/today-cutoff-hour)) ;; 翌日午前`chpn/today-cutoff-hour'時までは当日とみなす
                                          ((org-agenda-overriding-header "Doing and Today's Done (including habits)")
                                           (org-agenda-prefix-format " %i %-12:c %-48.48b")))

                                    (tags-todo "-INBOX-START-FINISH-PROJECT-BOOK/-DOING-DONE-CANCELED"
                                               ((org-agenda-overriding-header "Tasks")
                                                (org-agenda-prefix-format " %i %-12:c %-48.48b")
                                                (org-agenda-todo-ignore-scheduled 'all)
                                                (org-agenda-sorting-strategy '(category-keep priority-down))))

                                    (tags-todo "+INBOX"
                                               ((org-agenda-overriding-header "Inbox")
                                                (org-agenda-todo-ignore-scheduled nil)
                                                (org-tags-match-list-sublevels nil)))

                                    (tags-todo "-INBOX+PROJECT/-DONE-CANCELED"
                                               ((org-agenda-overriding-header "Projects")
                                                (org-agenda-prefix-format " %i %-12:c %-48.48b")
                                                (org-agenda-sorting-strategy '(category-keep))))

                                    (tags-todo "-INBOX+BOOK/-DONE-CANCELED"
                                               ((org-agenda-overriding-header "Books")
                                                (org-agenda-prefix-format " %i %-12:c")
                                                (org-agenda-todo-ignore-scheduled 'all)
                                                (org-agenda-sorting-strategy '(category-keep))))) nil)))

  ;; refile
  (org-refile-use-outline-path . 'file)
  (org-outline-path-complete-in-steps . nil)
  (org-refile-targets . '((org-agenda-files . (:tag  . "PROJECT"))))

  ;; log
  (org-log-done . 'time)
  (org-log-done-with-time . t)
  (org-log-into-drawer . t)
  (org-log-redeadline . 'time)
  (org-log-reschedule . 'time)
  
  ;; clock/timer
  (org-clock-out-remove-zero-time-clocks . t)
  (org-clock-clocktable-default-properties . '(:maxlevel 2 :scope agenda :wstart 2 :fileskip0 t :link t :tags t :block thismonth))
  (org-clock-clocked-in-display . 'mode-line) ;; 'frame-title
  (org-timer-default-timer . 30)

  ;; todo
  (org-todo-keywords . '((sequence "TODO(t)" "DOING(p)" "WAIT(w@)" "|" "DONE(d)" "CANCELED(c@)")))
  (org-enforce-todo-dependencies . t)
  (org-enforce-todo-checkbox-dependencies . t)
  (org-track-ordered-property-with-tag . t)
  ;; (org-priority-start-cycle-with-default . nil)

  ;; capture
  (org-capture-templates . `(("p" "project: 新規プロジェクト" entry (file "agenda/inbox.org")
                              ,(concat "%[" user-emacs-directory "templates/capture/project.org]")
                              :empty-lines 1 :jump-to-captured nil)
                             ("t" "task: 新規タスク" entry (file "agenda/inbox.org")
                              ,(concat "%[" user-emacs-directory "templates/capture/task.org]")
                              :empty-lines 1 :jump-to-captured nil)
                             ("s" "schedule: 新規予定" entry (file "agenda/inbox.org")
                              ,(concat "%[" user-emacs-directory "templates/capture/schedule.org]")
                              :empty-lines 1)
                             ("m" "memo: 新規メモ" plain (file chpn/today-memo-string-with-mkdir)
                              ,(concat "%[" user-emacs-directory "templates/capture/memo.org]")
                              :empty-lines 1 :jump-to-captured 1 :unnarrowed nil)
                             ("l" "link: リンクを追加" item (clock)
                              "%A\n"
                              :immediate-finish 1 :prepend nil)))

  ;; tags
  (org-tag-persistent-alist . '((:startgroup) ("PROJECT" . ?p) ("BOOK" . ?b) ("START" . ?s) ("FINISH" . ?f) (:endgroup)))
  (org-tag-alist . '((:startgroup . nil) ("comment" . ?c) (:endgroup . nil)))
  (org-tags-exclude-from-inheritance . '("PROJECT" "BOOK" "START" "FINISH"))

  ;; property
  (org-global-properties . '(("Effort_ALL" . "0:05 0:15 0:30 1:00 1:30 2:00 2:30 3:00 4:00")))

  ;; columns
  (org-columns-default-format . "%80ITEM %TODO %25SCHEDULED %DEADLINE %EFFORT{:} %CLOCKSUM_T %CLOCKSUM %TAGS")

  ;; ;; archive
  (org-archive-location . "archive/archive_%s::")

  ;; source code
  ;; org-src-lang-modes は、言語名とメジャーモード名が一致していれば設定不要っぽい
  (org-src-tab-acts-natively . t)

  ;; babel
  (org-babel-load-languages . '((plantuml . t)
                                (sql . t)
                                (java . t)
                                (shell . t)
                                (haskell . t)))
  `(org-plantuml-jar-path . ,(chpn/from-dir-jars "plantuml.jar"))
  (org-babel-haskell-compiler . "ghc -package-db $(stack path --snapshot-pkg-db)")

  :bind
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  ("C-c l" . org-store-link)
  ("M-q"   . chpn-org-prefix)
  (chpn-org-prefix
   ("m"   . (lambda () (interactive) (chpn/open-file (consult-find (concat org-directory "memo/") "\.org#")))))
  (org-mode-map
   ("C-c r"   . org-clock-report)
   ("C-c t c" . org-table-create)
   ("C-c t -" . org-table-insert-row)
   ("C-c t |" . org-table-insert-column)
   ("C-c t =" . org-table-insert-hline)
   ("C-c n" . org-narrow-to-subtree)
   ("C-c b" . org-narrow-to-block)
   ("C-c w" . widen)
   ("C-c e" . org-set-effort))
  (org-agenda-mode-map
   :package org-agenda
   ("C" . org-agenda-columns)
   ("w" . org-agenda-refile)
   ("d" . org-agenda-set-property)
   ("W" . org-agenda-week-view)
   ("D" . org-agenda-day-view))

  :hook
  (kill-emacs-hook . ladicle/org-clock-out-and-save-when-exit)
  (org-clock-in-hook . (lambda ()
                         (when (org-clocking-p) (org-todo "DOING"))
                         (setq org-mode-line-string (ladicle/task-clocked-time))
                         (run-at-time 0 60 (lambda ()
                                             (setq org-mode-line-string (ladicle/task-clocked-time))
                                             (force-mode-line-update)))
                         (force-mode-line-update)))
  (org-mode-hook . (lambda ()
                     (dolist (key '("C-'" "C-," "C-."))
                       (unbind-key key org-mode-map))))
  (auto-save-hook . org-save-all-org-buffers)
  (org-capture-before-finalize-hook . (lambda () (org-update-statistics-cookies "ALL")))
  (org-after-todo-statistics-hook . chpn/org-summary-todo)
  (org-after-tags-change-hook . chpn/org-cookie-data-by-project)

  :preface
  (define-prefix-command 'chpn-org-prefix)
  (defconst chpn/today-cutoff-hour 6)
  (defun chpn/org-cookie-data-by-project ()
    "TODOエントリにPROJECTタグがついている場合は、COOKIE_DATA プロパティに \"todo\" を設定する。
TODOエントリにPROJECTタグがついていない場合は、COOKIE_DATA プロパティに \"checkbox recursive\" を設定する。
TODOエントリ以外は無視される。
この関数は、`org-after-tags-change-hook'に設定して使用することを想定している。"
    (save-excursion
      (org-back-to-heading t)
      (when (org-get-todo-state)
        (let* ((tags (org-get-tags nil t))
               (has-project (member "PROJECT" tags))
               (target (if has-project "todo" "checkbox recursive")))
          (org-entry-delete nil "COOKIE_DATA")
          (org-entry-put nil "COOKIE_DATA" target)))))
  (defun chpn/org-agenda-today-timestamp-until (offset)
    "現在時刻が OFFSET 時より前なら \"昨日 OFFSET:00\"、
そうでなければ \"今日 OFFSET:00\" を \"YYYY-MM-DD HH:00\" 形式で返す。
OFFSET は 0‒23 の整数を想定する。"
    (unless (and (integerp offset) (<= 0 offset 23))
      (user-error "OFFSET must be an integer between 0 and 23"))
    (let* ((now          (current-time))
           (current-hour (nth 2 (decode-time now)))
           (base-time    (if (< current-hour offset) (time-subtract now (days-to-time 1)) now))
           (fmt          (format "%%Y-%%m-%%d %02d:00" offset)))
      (format-time-string fmt base-time)))
  (defun chpn/org-agenda-skip-if-noprop (key value)
    "値がVALUEであるKEYプロパティを持たないエントリの収集をスキップする。"
    (org-back-to-heading t)
    (let* ((end (org-entry-end-position)))
      (unless (string= (org-entry-get (point) key) value) end)))
  (defun chpn/org-agenda-skip-if-tags (tags &optional local)
    "エントリが TAGS リスト内のいずれかのタグを持つ場合、agendaでの収集をスキップする。
LOCAL が非nilの場合は、エンティティに直接指定されたタグのみ検査する。
この関数は、`org-agenda-skip-function'の引数として使用することを想定している。"
    (org-back-to-heading t)
    (let* ((end (org-entry-end-position))
           (tags-at-point (org-get-tags nil local)))
      (when (cl-intersection tags tags-at-point :test #'string=) end)))
  (defun chpn/org-agenda-skip-if-notags (tags &optional local)
    "エントリが TAGS リスト内のいずれのタグも持たない場合、agendaでの収集をスキップする。
LOCAL の意味は`chpn/org-agenda-skip-if-tags'と同じである。
この関数は、`org-agenda-skip-function'の引数として使用することを想定している。"
    (org-back-to-heading t)
    (let* ((end (org-entry-end-position))
           (tags-at-point (org-get-tags nil local)))
      (unless (cl-intersection tags tags-at-point :test #'string=) end)))
  (defun chpn/org-summary-todo (n-done n-not-done)
    "すべてのサブツリーが終了したらDONEに切り替える。その他の場合はTODOにする。"
    (let (org-log-done org-log-states)
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (defun chpn/today-memo-string-with-mkdir ()
    (let* ((title (read-string "memo title: "))
           (dn (concat org-directory "memo/" (format-time-string "%F_" (current-time)) title))
           (kr (cons title kill-ring)))
      (setq kill-ring (delete-dups kr))
      (setq kill-ring-yank-pointer kill-ring)
      (unless (file-directory-p dn)
        (make-directory dn))
      (concat dn "/index.org")))
  (defun ladicle/task-clocked-time ()
    "Return a string with the clocked time and effort, if any"
    (interactive)
    (let* ((clocked-time (org-clock-get-clocked-time))
           (h (truncate clocked-time 60))
           (m (mod clocked-time 60))
           (work-done-str (format "%d:%02d" h m)))
      (if org-clock-effort
          (let* ((effort-in-minutes
                  (org-duration-to-minutes org-clock-effort))
                 (effort-h (truncate effort-in-minutes 60))
                 (effort-m (truncate (mod effort-in-minutes 60)))
                 (effort-str (format "%d:%02d" effort-h effort-m)))
            (format " %s/%s" work-done-str effort-str))
        (format " %s" work-done-str))))
  (defun ladicle/org-clock-out-and-save-when-exit ()
    "Save buffers and stop clocking when kill emacs."
    (ignore-errors (org-clock-out) t)
    (save-some-buffers t))

  :init
  (chpn/copy-directory-recursively (concat user-emacs-directory "templates/agenda") (concat org-directory "agenda/"))
  :config
  (dolist (pattern org-agenda-files)
    (add-to-list 'recentf-exclude pattern))
  (add-to-list 'org-modules 'org-habit)
  (leaf ob-async :ensure t :require t)

  (leaf org-modern :ensure t
    :custom
    (global-org-modern-mode    . t)
    (org-modern-progress       . nil)
    (org-modern-todo           . nil)
    (org-modern-block          . nil)
    (org-modern-table-vertical . 1)
    (org-modern-timestamp      . t)
    (org-modern-list           . nil)
    (org-modern-replace-stars  . '("󰇈" "󰎥" "󰎨" "󰎫" "󰎲" "󰎯" "󰎴" "󰎷" "󰎺" "󰎽"))
    (org-modern-star           . 'replace)
    (org-priority-highest      . ?A)
    (org-priority-lowest       . ?D)
    (org-priority-default      . ?C)
    (org-modern-priority       . '((?A . "")   ;; 重要度高・緊急度高
                                   (?B . "")   ;; 重要度低・緊急度高
                                   (?C . "")   ;; 重要度高・緊急度低
                                   (?D . ""))) ;; 重要度低・緊急度低
    (org-modern-checkbox       . '((?X . "󰄵") (?- . "󰡖") (?\s . "󰄱"))))

  (leaf org-roam :ensure t
    :defvar (org-directory
             org-roam-directory)
    :custom
    (org-roam-directory . roam-dir)
    (org-roam-db-autosync-mode . t)
    (org-roam-completion-everywhere . t)
    (org-roam-capture-templates . '(("f" "Fleeting" plain "%?"
                                     :target (file+head "fleet/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                     :unnarrowed t)
                                    ("k" "Competitive Programming" plain "%?"
                                     :target (file+head "kyopro/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                     :jump-to-captured t
                                     :unnarrowed t)
                                    ("b" "Bibliography" plain "%?"
                                     :target (file+head "biblio/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                     :jump-to-captured t
                                     :unnarrowed t)
                                    ("p" "Permanent" plain "%?"
                                     :target (file+head "permanent/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                     :jump-to-captured t
                                     :unnarrowed t)
                                    ("s" "Structure" plain "%?"
                                     :target (file+head "permanent/structure-%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                     :jump-to-captured t
                                     :unnarrowed t)))
    :bind
    ("C-c o" . chpn-roam-prefix)
    (chpn-roam-prefix
     ("a" . org-roam-alias-add)
     ("b" . org-roam-buffer-toggle)
     ("c" . org-roam-capture)
     ("f" . org-roam-node-find)
     ("i" . org-roam-node-insert)
     ("r" . org-roam-ref-add)
     ("w" . org-roam-refile))
    :preface
    (define-prefix-command 'chpn-roam-prefix)
    (defconst roam-dir (concat org-directory "roam/"))
    (unless (file-directory-p roam-dir) (make-directory roam-dir t))
    :config
    (dolist (dir (mapcar (lambda (x) (concat org-roam-directory x)) '("fleet" "kyopro" "biblio" "fleet" "permanent")))
      (unless (file-directory-p dir) (make-directory dir t))))

  (leaf org-journal :ensure t
    :defvar (org-journal-file-type
             org-capture-templates
             chpn/org-journal-insert-template)
    :bind
    ("C-c j" . org-journal-new-entry)
    :custom
    (org-journal-dir . journal-dir)
    (org-journal-date-format . date-format-jp)
    (org-journal-search-result-date-format . date-format-jp)
    (org-journal-time-prefix . "** ")
    :hook
    (org-journal-after-header-create-hook . chpn/org-journal-insert-template)
    :preface
    (defconst journal-dir (concat org-directory "journal/"))
    (defconst date-format-jp "%x（%a）")
    (unless (file-directory-p journal-dir) (make-directory journal-dir t))
    (defun chpn/org-journal-insert-template ()
      (insert "\n")
      (goto-char (point-max))
      (let ((template-file (concat user-emacs-directory "templates/journal/template.org")))
        (if (file-exists-p template-file)
            (insert-file-contents template-file)
          (user-error "Journal template file `%s' not found" template-file)))
      (goto-char (point-max))))

  (leaf org-re-reveal :ensure t)
  (leaf company-org-block :ensure t))

(leaf calendar
  :defvar (calendar-holidays
           japanese-holidays)
  :defun (calendar-cursor-to-date
          calendar-date-string
          calendar-check-holidays)
  :require japanese-holidays
  :bind
  ("M-2" . calendar)
  :custom
  (calendar-mark-holidays-flag . t)
  (calendar-day-header-array . dow-array-jp)
  (calendar-day-name-array . dow-array-jp)
  (calendar-date-display-form . '((format "%s年 %s月 %s日（%s）" year month day dayname)))
  (calendar-month-header . '(propertize (format "%d年 %s月" year month)
                                        'font-lock-face 'calendar-month-header))
  :hook
  (calendar-move-hook . chpn/japanese-holiday-show)
  (calendar-today-visible-hook . japanese-holiday-mark-weekend)
  (calendar-today-visible-hook . calendar-mark-today)
  (calendar-today-invisible-hook . japanese-holiday-mark-weekend)
  :preface
  (defconst dow-array-jp ["日" "月" "火" "水" "木" "金" "土"])
  (defun chpn/japanese-holiday-show (&rest _args)
    (let* ((date (calendar-cursor-to-date t))
           (date-string (calendar-date-string date))
           (holiday-list (calendar-check-holidays date)))
      (when holiday-list
        (message "%s: %s" date-string (mapconcat #'identity holiday-list "; ")))))
  :init
  (leaf japanese-holidays :ensure t)
  :config
  (setq calendar-holidays
        (append japanese-holidays holiday-local-holidays holiday-other-holidays)))

;; latex
(leaf ox-latex
  :after org
  :defvar org-format-latex-options
  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.5))
  :custom
  (org-latex-default-class . "bxjsarticle")
  (org-latex-pdf-process . '("latexmk -e '$latex=q/uplatex %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -e '$dvipdf=q/dvipdfmx -o %D %S/' -norc -gg -pdfdvi %f"))
  ;; (org-latex-pdf-process . '("latexmk -e '$lualatex=q/lualatex %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -norc -gg -pdflua %f"))
  ;; (org-export-in-background . t)
  (org-file-apps . '(("pdf" . "evince %s")))
  (org-latex-classes . '(("article" "\\documentclass[11pt]{article}"
                          ("\\section{%s}" . "\\section*{%s}")
                          ("\\subsection{%s}" . "\\subsection*{%s}")
                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                          ("\\paragraph{%s}" . "\\paragraph*{%s}")
                          ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                         ("report" "\\documentclass[11pt]{report}"
                          ("\\part{%s}" . "\\part*{%s}")
                          ("\\chapter{%s}" . "\\chapter*{%s}")
                          ("\\section{%s}" . "\\section*{%s}")
                          ("\\subsection{%s}" . "\\subsection*{%s}")
                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                         ("book" "\\documentclass[11pt]{book}"
                          ("\\part{%s}" . "\\part*{%s}")
                          ("\\chapter{%s}" . "\\chapter*{%s}")
                          ("\\section{%s}" . "\\section*{%s}")
                          ("\\subsection{%s}" . "\\subsection*{%s}")
                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                         ("bxjsarticle" "\\documentclass[autodetect-engine,dvi=dvipdfmx,11pt,a4paper,ja=standard]{bxjsarticle}
[NO-DEFAULT-PACKAGES]
\\usepackage{amsmath}
\\usepackage{newtxtext,newtxmath}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\ifdefined\\kanjiskip
  \\usepackage{pxjahyper}
  \\hypersetup{colorlinks=true}
\\else
  \\ifdefined\\XeTeXversion
      \\hypersetup{colorlinks=true}
  \\else
    \\ifdefined\\directlua
      \\hypersetup{pdfencoding=auto,colorlinks=true}
    \\else
      \\hypersetup{unicode,colorlinks=true}
    \\fi
  \\fi
\\fi"
                          ("\\section{%s}" . "\\section*{%s}")
                          ("\\subsection{%s}" . "\\subsection*{%s}")
                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                          ("\\paragraph{%s}" . "\\paragraph*{%s}")
                          ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

                         ("jlreq" "\\documentclass[11pt,paper=a4]{jlreq}
[NO-DEFAULT-PACKAGES]
\\usepackage{amsmath}
\\usepackage{newtxtext,newtxmath}
\\ifdefined\\kanjiskip
  \\usepackage[dvipdfmx]{graphicx}
  \\usepackage[dvipdfmx]{hyperref}
  \\usepackage{pxjahyper}
  \\hypersetup{colorlinks=true}
\\else
  \\usepackage{graphicx}
  \\usepackage{hyperref}
  \\hypersetup{pdfencoding=auto,colorlinks=true}
\\fi"
                          ("\\section{%s}" . "\\section*{%s}")
                          ("\\subsection{%s}" . "\\subsection*{%s}")
                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                          ("\\paragraph{%s}" . "\\paragraph*{%s}")
                          ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

                         ("jlreq-tate" "\\documentclass[tate,11pt,paper=a4]{jlreq}
[NO-DEFAULT-PACKAGES]
\\usepackage{amsmath}
\\usepackage{newtxtext,newtxmath}
\\ifdefined\\kanjiskip
  \\usepackage[dvipdfmx]{graphicx}
  \\usepackage[dvipdfmx]{hyperref}
  \\usepackage{pxjahyper}
  \\hypersetup{colorlinks=true}
\\else
  \\usepackage{graphicx}
  \\usepackage{hyperref}
  \\hypersetup{pdfencoding=auto,colorlinks=true}
\\fi"
                          ("\\section{%s}" . "\\section*{%s}")
                          ("\\subsection{%s}" . "\\subsection*{%s}")
                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                          ("\\paragraph{%s}" . "\\paragraph*{%s}")
                          ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf multiple-cursors :ensure t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-S-l" . mc/skip-to-next-like-this)
   ("C-S-k" . mc/skip-to-previous-like-this)
   ("C-M->" . mc/unmark-next-like-this)
   ("C-M-<" . mc/unmark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Develop Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf *git
  :config
  (leaf git-modes :ensure t)
  (leaf git-timemachine :ensure t
    :bind
    (chpn-function-prefix
     :package init
     ("q" . git-timemachine-toggle)))
  (leaf magit :ensure t
    :custom
    (magit-auto-revert-mode . nil)
    :config
    (leaf magit-section :ensure t))
  (leaf git-gutter :ensure t
    :blackout t
    :bind
    ("M-g s" . git-gutter:stage-hunk)
    ("M-g r" . git-gutter:revert-hunk)
    :custom
    (global-git-gutter-mode . t)
    (git-gutter:modified-sign . "=")
    (git-gutter:added-sign . "+")
    (git-gutter:deleted-sign . "-")
    :custom-face
    (git-gutter:modified . '((t (:foreground "#f1fa8c" :background "#f1fa8c"))))
    (git-gutter:added . '((t (:foreground "#50fa7b" :background "#50fa7b"))))
    (git-gutter:deleted . '((t (:foreground "#ff79c6" :background "#ff79c6"))))))

(leaf yasnippet :ensure t
  :blackout yas-minor-mode
  :custom
  (yas-global-mode . t)
  :config
  (leaf yasnippet-snippets :ensure t)
  (leaf haskell-snippets :ensure t)
  (leaf yasnippet-capf :ensure t)
  (leaf awk-yasnippets :ensure t)
  (leaf elm-yasnippets :ensure t))

(leaf company :ensure t
  :blackout t
  :hook (prog-mode-hook
         text-mode-hook
         emacs-lisp-mode-hook)
  :custom
  (company-idle-delay . 0)
  (company-echo-delay . 0)
  (company-minimum-prefix-length . 2)
  (company-selection-wrap-around . t)
  :bind
  ("C-c y" . company-yasnippet)
  (company-active-map
   ("<tab>" . company-complete)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("C-s" . company-filter-candidates)
   ("C-h" . nil)
   ("M-n" . nil)
   ("M-p" . nil))
  (company-search-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("C-s" . company-search-repeat-forward)
   ("C-r" . company-search-repeat-backward)
   ("C-h" . company-search-delete-char)
   ("M-n" . nil)
   ("M-p" . nil))
  :config
  (leaf company-box :ensure t
    :blackout t
    :hook
    (company-mode-hook . company-box-mode)
    :custom
    (company-box-icons-alist . 'company-box-icons-images)
    (company-box-show-single-candidate . nil)))

(leaf editorconfig :ensure t)

(leaf jsonrpc :ensure t)

(leaf copilot :ensure t
  :disabled t
  :defvar (copilot-mode)
  :hook (prog-mode-hook
         text-mode-hook
         emacs-lisp-mode-hook)
  :bind
  (copilot-completion-map
   ("<tab>"     . copilot-accept-completion) ;; Tab
   ("C-<tab>"   . copilot-accept-completion-by-line) ;; Ctrl-Tab
   ("<backtab>" . copilot-accept-completion-by-word) ;; Shift-Tab
   ("M-n"       . copilot-next-completion)
   ("M-p"       . copilot-previous-completion))
  (chpn-toggle-prefix
   :package init
   ("c" . toggle-copilot-mode))
  :custom
  (copilot-indent-offset-warning-disable . t)
  :init
  (defun toggle-copilot-mode ()
    "Toggle GitHub Copilot mode."
    (interactive)
    (copilot-mode (if copilot-mode -1 1))
    (message "Copilot mode %s" (if copilot-mode "enabled" "disabled"))))

(leaf copilot-chat :ensure t
  :disabled t
  :hook
  (git-commit-setup-hook . copilot-chat-insert-commit-message)
  :custom
  (copilot-chat-frontend . 'org))

;; projectile
(leaf projectile :ensure t
  :blackout t
  :bind
  (projectile-mode-map
   ("C-c p" . projectile-command-map))
  :custom
  (projectile-enable-caching . t)
  (projectile-require-project-root . t)
  (projectile-dirconfig-comment-prefix . "#")
  (projectile-mode . t))

(leaf treemacs :ensure t
  :defun (treemacs-load-theme
          chpn/treemacs-record-previous-window)
  :bind
  ("M-1" . treemacs-select-window)
  (treemacs-mode-map
   ("M-1" . chpn/treemacs-restore-previous-window))
  :custom
  (treemacs-is-never-other-window . t)
  (treemacs-no-delete-other-windows . t)
  ;; (treemacs-width 20)
  (treemacs-follow-mode . t)
  (treemacs-filewatch-mode . t)
  (treemacs-fringe-indicator-mode . t)
  (treemacs-git-mode . 'simple)
  (treemacs-project-follow-cleanup . t)
  :preface
  (defvar chpn/treemacs-previous-window nil)
  (defun chpn/treemacs-record-previous-window (&rest _)
    "Treemacs ウインドウに切り替える前のウインドウを記録する。"
    (setq chpn/treemacs-previous-window (selected-window)))
  (advice-add 'treemacs-select-window :before #'chpn/treemacs-record-previous-window)
  (defun chpn/treemacs-restore-previous-window ()
    "記録しておいたウインドウ（Treemacs 移動前）にフォーカスを戻す。"
    (interactive)
    (if (window-live-p chpn/treemacs-previous-window)
        (select-window chpn/treemacs-previous-window)
      (message "Previous window not found.")))
  :config
  (leaf treemacs-projectile :ensure t
    :bind
    (treemacs-project-map
     :package treemacs
     ("p" . treemacs-projectile)))
  (leaf treemacs-icons-dired :ensure t
    :config
    (treemacs-icons-dired-mode))
  (leaf treemacs-magit :ensure t :require t)
  (leaf treemacs-nerd-icons :ensure t
    :config
    (treemacs-load-theme "nerd-icons")))

(leaf flycheck :ensure t
  :blackout t
  :hook (prog-mode-hook emacs-lisp-mode-hook)
  :config
  (leaf flycheck-posframe :ensure t
    :custom
    (flycheck-posframe-position . 'window-bottom-right-corner)
    (flycheck-posframe-border-width . 2)
    :hook flycheck-mode-hook
    :config
    (flycheck-posframe-configure-pretty-defaults)))

;; lsp
(leaf lsp-mode :ensure t
  :commands (lsp lsp-deferred)
  :custom
  (lsp-diagnostics-provider . :auto)
  (lsp-completion-provider . :capf)
  (lsp-lens-enable . t)
  (lsp-semantic-tokens-enable . t)
  (lsp-semantic-tokens-honor-refresh-requests . t)
  (lsp-enable-links . t)
  ;; (lsp-log-io t)
  ;; (lsp-document-sync-method 'lsp--sync-incremental)
  (lsp-keymap-prefix . "M-l")
  :hook
  (lsp-mode-hook . lsp-enable-which-key-integration)
  (lsp-mode-hook . lsp-ui-mode)
  (elm-mode-hook           . lsp-deferred)
  (java-mode-hook          . lsp-deferred)
  (haskell-mode-hook       . lsp-deferred)
  (js-mode-hook            . lsp-deferred)
  (typescript-ts-mode-hook . lsp-deferred)
  (tsx-ts-mode-hook        . lsp-deferred)
  (terraform-mode-hook     . lsp-deferred)
  (sh-mode-hook            . lsp-deferred)
  (python-base-mode-hook   . lsp-deferred)
  :config
  (leaf lsp-ui :ensure t
    ;; :custom-face
    ;; (lsp-ui-doc-background ((nil (:background "black"))))
    :bind
    (lsp-mode-map
     ("C-c d" . lsp-ui-doc-glance))
    :custom
    (lsp-ui-doc-enable . nil)
    (lsp-ui-doc-header . t)
    (lsp-ui-doc-include-signature . t)
    (lsp-ui-doc-position . 'at-point)
    (lsp-ui-doc-max-width . 150)
    (lsp-ui-doc-max-height . 30)
    (lsp-ui-doc-show-with-mouse . t)
    (lsp-ui-doc-show-with-cursor . t)
    (lsp-ui-doc-use-childframe . t)
    (lsp-ui-doc-use-webkit . nil)
    ;; (lsp-ui-flycheck-list-position . 'right)
    (lsp-ui-imenu-enable . nil)
    ;; (lsp-ui-imenu-auto-refresh . t)
    ;; (lsp-ui-imenu-kind-position . 'top)
    ;; (lsp-ui-imenu-window-width . 0)
    (lsp-ui-peek-enable . nil)
    (lsp-ui-peek-peek-height . 50)
    (lsp-ui-peek-list-width . 50)
    (lsp-ui-peek-fontify . 'on-demand)
    (lsp-ui-peek-show-directory . t)
    (lsp-ui-sideline-enable . nil)
    (lsp-ui-sideline-show-symbol . t)
    (lsp-ui-sideline-show-hover . t)
    (lsp-ui-sideline-show-diagnostics . nil)
    (lsp-ui-sideline-show-code-actions . t)))

(leaf lsp-haskell :ensure t
  :custom
  (lsp-haskell-server-args . '("-d"))
  (lsp-haskell-formatting-provider . "fourmolu"))

(leaf haskell-mode :ensure t
  :custom
  (haskell-indentation-layout-offset . 4)
  (haskell-indentation-left-offset . 4)
  (haskell-indentation-starter-offset . 4)
  (haskell-indentation-where-post-offset . 4)
  (haskell-indentation-where-pre-offset . 4)
  :bind
  (haskell-mode-map
   ("C-c C-h" . haskell-compile)
   ("C-c ?" . consult-hoogle))
  :config
  (leaf consult-hoogle :ensure t))

(leaf cabal-mode :ensure t)

(leaf restclient :ensure t
  :config
  (leaf ob-restclient :ensure t))

(leaf urlenc :ensure t)

(leaf lsp-terraform
  :custom
  (lsp-terraform-ls-enable-show-reference . t)
  :bind
  (terraform-mode-map
   ("C-c C-i" . lsp-terraform-ls-init)
   ("C-c C-v" . lsp-terraform-ls-validate)))

(leaf terraform-mode :ensure t
  :custom
  (terraform-indent-level . 2)
  :config
  (leaf company-terraform :ensure t
    :config
    (company-terraform-init)))

(leaf yaml-mode :ensure t)

(leaf dockerfile-mode :ensure t)

(leaf docker-compose-mode :ensure t
  :require yaml-mode)

(leaf markdown-mode :ensure t)

(leaf js
  :custom
  (js-indent-level . 2)
  (js-jsx-indent-level . 2))

(leaf treesit
  :custom
  (treesit-font-lock-level . 4)
  :config
  (leaf treesit-auto :ensure t
    :defun (global-treesit-auto-mode
            treesit-auto-add-to-auto-mode-alist)
    :require t
    :custom
    (treesit-auto-install . 'prompt)
    (treesit-auto-langs . '(python tsx typescript json))
    :config
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode)))

(leaf elm-mode :ensure t
  :custom
  (elm-package-json . "elm.json")
  :hook
  (elm-mode-hook . elm-format-on-save-mode))

(leaf lsp-pyright :ensure t
  :custom
  (lsp-pyright-type-checking-mode . "strict")
  (lsp-pyright-python-executable-cmd . "python3"))

(leaf python
  :custom
  (python-shell-interpreter . "python3")
  (python-indent-guess-indent-offset-verbose . nil)
  (python-indent-offset))

(leaf apache-mode :ensure t)

(leaf nginx-mode :ensure t
  :mode ("/nginx/sites-\\(?:available\\|enabled\\)/")
  :config
  (leaf company-nginx :ensure t
    :hook
    (nginx-mode-hook . company-nginx-keywords)))

(leaf plantuml-mode :ensure t
  :mode ("\\.puml\\'")
  :custom
  `(plantuml-jar-path . ,(chpn/from-dir-jars "plantuml.jar"))
  (plantuml-default-exec-mode . 'jar)
  (plantuml-output-type . "png"))

(leaf nxml-mode
  :mode ("\.xml$" "\.xsl$" "\.xhtml$" "\.page$")
  :custom
  (nxml-child-indent . 2)
  (nxml-attribute-indent . 2)
  (nxml-slash-auto-complete-flag . t))

(leaf sqlformat :ensure t
  :custom
  (sqlformat-command . 'pgformatter)
  (sqlformat-args . '("-s2" "-L"))
  :bind
  (sql-mode-map
   ("<tab>" . sqlformat-buffer)))

(leaf vterm :ensure t vterm-toggle
  :defvar (vterm-keymap-exceptions)
  :custom
  (vterm-keymap-exceptions . '("C-c" "C-x" "C-u" "C-g" "C-h" "C-l" "M-x" "M-o" "C-y" "M-y" ;; default setting
                               ;; "M-i" "M-t" "<f1>" "M-[" "M-["
                               "<f1>" "<f2>" "<f3>" "<f4>" "<f5>" "<f6>"
                               "<f7>" "<f8>" "<f9>" "<f10>" "<f11>" "<f12>"
                               "M-<f1>" "M-<f2>" "M-<f3>" "M-<f4>" "M-<f5>" "M-<f6>"
                               "M-<f7>" "M-<f8>" "M-<f9>" "M-<f10>" "M-<f11>" "M-<f12>"
                               "M-1" "M-2" "M-3" "M-4" "M-5" "M-6" "M-7" "M-8" "M-9" "M-0"
                               "M-!" "M-\"" "M-#" "M-$" "M-%" "M-&" "M-'" "M--" "M-=" "M-^" "M-~" "M-|"
                               "M-{" "M-}" "M-;" "M-:" "M-," "M-." "M-<" "M->" "M-_"
                               "M-a" "M-b" "M-c" "M-d" "M-e" "M-f" "M-g" "M-h" "M-i" "M-j" "M-k" "M-l"
                               "M-m" "M-n" "M-p" "M-q" "M-r" "M-s" "M-t" "M-u" "M-v" "M-w" "M-z"
                               ))
  :bind
  (vterm-mode-map
   ("C-h" . (lambda () (interactive) (vterm-send-key "h" nil nil t)))
   ("C-g" . (lambda () (interactive) (vterm-send-key "g" nil nil t)))
   ("M-[" . vterm-toggle-backward)
   ("M-]" . vterm-toggle-forward))
  (chpn-function-prefix
   :package init
   ("v" . chpn/vterm))
  :preface
  (defun chpn/vterm (&optional arg)
    "補完ミニバッファ上で、既存のvtermバッファを選択するか、新しいvtermバッファを開く。
候補:
- `New vterm'  名前を指定して新規作成
- 既存の vterm バッファ
C-u を付けると選んだ候補を *別ウィンドウ* で開く。"
    (interactive "P")
    (let* ((existing (mapcar (lambda (buf)
                               (cons (buffer-name buf) buf))
                             (seq-filter (lambda (buf)
                                           (with-current-buffer buf (derived-mode-p 'vterm-mode)))
                                         (buffer-list))))
           (special  '(("New vterm" . :new-named)))
           (cands    (append existing special))
           (completion-extra-properties
            (list :annotation-function
                  (lambda (cand)
                    (pcase (cdr (assoc cand cands))
                      (:new-named "  create (prompt)")
                      (buf (with-current-buffer buf
                             (concat "  " default-directory)))))))
           (choice (completing-read "vterm: " (mapcar #'car cands) nil t)))
      (pcase (cdr (assoc choice cands))
        (:new-named
         (let* ((default (generate-new-buffer-name "*vterm*"))
                (name (read-string "Buffer name: " default))
                (vterm-buffer-name name))
           (if arg
               (vterm-other-window)
             (vterm))))
        (buf (if arg
                 (pop-to-buffer buf)
               (switch-to-buffer buf)))))))

(leaf web-mode :ensure t
  :mode ("\.html$")
  :custom
  (web-mode-markup-indent-offset . 4)
  (web-mode-css-indent-offset . 4)
  (web-mode-code-indent-offset . 4)
  (web-mode-enable-auto-pairing . t)
  (web-mode-enable-auto-closing . t)
  (web-mode-auto-close-style . 2)
  :custom-face
  (web-mode-doctype-face . '((nil (:foreground "Pink3"))))
  (web-mode-html-tag-face . '((nil (:foreground "Green"))))
  (web-mode-html-attr-value-face . '((nil (:foreground "Yellow"))))
  (web-mode-html-attr-name-face . '((nil (:foreground "#0FF")))))

(leaf web-beautify :ensure t)

(provide 'init)
;;; init.el ends here
