(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

;; https://github.com/Mstrodl/.emacs.d/blob/master/config.org
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      (require package)
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;; Evil
(setq evil-want-keybinding nil)
(require-package 'evil)
(evil-mode 1)
(require-package 'undo-tree)
(global-undo-tree-mode)
(with-eval-after-load 'undo-tree (defun undo-tree-overridden-undo-bindings-p () nil)) ;; because I overrode C-/ haha
(evil-set-undo-system 'undo-tree)
(setq undo-tree-history-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/undo"))))
(require-package 'evil-collection)
(evil-collection-init)


;; Indents
(setq-default indent-tabs-mode nil)
(setq-default standard-indent 2)
(setq-default c-basic-offset 2)
(setq-default tab-width standard-indent)

(setq-default rust-ts-mode-indent-offset 4)
(setq-default typescript-indent-level 2)
(setq-default js-indent-level 4)
(setq-default lua-indent-level 4)
(setq-default c-default-style "linux")

;; Treemacs
(require-package 'treemacs-evil)
(setq treemacs-default-visit-action 'treemacs-visit-node-close-treemacs)
(setq treemacs-follow-after-init nil)


;; Helm
(require-package 'helm)
(require 'helm-command) ;; loads helm-M-x-map, so helm-buffers-list works before helm-M-x is run
(global-set-key (kbd "M-x") 'helm-M-x)
(add-hook 'helm-after-initialize-hook
          (lambda()
            ;; (define-key helm-buffer-map (kbd "M-m") 'helm-toggle-all-marks)
            ;; (define-key helm-buffer-map (kbd "M-d") 'helm-buffer-run-kill-buffers)
            (define-key helm-buffer-map (kbd "ESC") 'helm-keyboard-quit)
            (define-key helm-M-x-map (kbd "ESC") 'helm-keyboard-quit)
            (define-key helm-map (kbd "ESC") 'helm-keyboard-quit)
            ))


;; Colorful Mode
(require-package 'colorful-mode)
(dolist
    (mode '(fundamental-mode
            conf-xdefaults-mode
            text-mode
            vterm-mode
            conf-space-mode
            conf-toml-mode
            help-mode
            markdown-mode))
  (add-to-list 'global-colorful-modes mode))
(global-colorful-mode)

;; Misc
(global-auto-revert-mode t)
(require-package 'vterm)
(setq vterm-shell 'zsh)
(setq evil-symbol-word-search t)
(setq inhibit-startup-screen t)
(setq visible-bell nil
      ring-bell-function
      (lambda()
        (invert-face 'mode-line)
        (run-with-timer 0.1 nil #'invert-face 'mode-line)))
(setq line-number-mode t)
(setq-default doc-view-resolution 200)
(setq-default doc-view-continuous t)
(prefer-coding-system 'utf-8)
(define-coding-system-alias 'UTF-8 'utf-8)
;; Put all auto saves here
(setq auto-save-file-name-transforms
  `((".*" "~/.emacs.d/auto-saves/" t)))
;; Colors in compilaiton window
(require-package 'ansi-color)
(setq ansi-color-for-compilation-mode t)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
;; Misc modes
(require-package 'ron-mode)
(require-package 'sudo-edit)


;; Shell-pop
(setq shell-pop-shell-type (quote ("vterm" "*vterm*" (lambda nil (vterm shell-pop-term-shell)))))
(setq shell-pop-term-shell "/usr/bin/zsh")
(setq shell-pop-full-span t)
(setq shell-pop-window-size 20)
(setq shell-pop-window-position "bottom")
(setq shell-pop-autocd-to-working-dir t)
(setq shell-pop-cleanup-buffer-at-process-exit t)
(require-package 'shell-pop)


;; Comment Do What I Mean Line
(defun comment-dwim-line (&optional arg)
"Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at the end of the line,
then comment current line.
Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
    (interactive "*P")
    (comment-normalize-vars)
    (if (not (region-active-p))
        (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))


;; Keybinds
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key evil-motion-state-map (kbd "SPC") nil)
(define-key evil-normal-state-map (kbd "SPC r c") (lambda() (interactive) (load-file "~/.emacs.d/init.el")))

;; treemacs
(define-key treemacs-mode-map (kbd "SPC 0") 'treemacs)
(define-key treemacs-mode-map (kbd "SPC l") 'treemacs-next-workspace)
(define-key treemacs-mode-map (kbd "SPC s") 'treemacs-switch-workspace)
(define-key treemacs-mode-map (kbd "SPC e") 'treemacs-edit-workspaces)
(define-key treemacs-mode-map (kbd "SPC '") 'shell-pop)
(define-key evil-normal-state-map (kbd "SPC 0") 'treemacs)
(define-key evil-normal-state-map (kbd "SPC SPC") 'helm-M-x)
;; window manipulaiton
(define-key evil-normal-state-map (kbd "SPC w d") 'evil-window-delete)
;; split windows left, right, up, down
(define-key evil-normal-state-map (kbd "SPC w h") 'split-window-right)
(define-key evil-normal-state-map (kbd "SPC w l") (lambda()
                                                    (interactive)
                                                    (split-window-right)
                                                    (evil-window-right 1)))
(define-key evil-normal-state-map (kbd "SPC w k") 'split-window-below)
(define-key evil-normal-state-map (kbd "SPC w j") (lambda()
                                                    (interactive)
                                                    (split-window-below)
                                                    (evil-window-down 1)))
;; buffers
(define-key evil-normal-state-map (kbd "SPC b b") 'helm-buffers-list)
(define-key evil-normal-state-map (kbd "SPC b d") 'evil-delete-buffer)
(define-key evil-normal-state-map (kbd "SPC b h") 'previous-buffer)
(define-key evil-normal-state-map (kbd "SPC b l") 'next-buffer)
(define-key evil-normal-state-map (kbd "SPC b r") 'rename-buffer)
;; code
(define-key evil-normal-state-map (kbd "SPC c r") 'recompile)
(define-key evil-normal-state-map (kbd "SPC c k") 'kill-compilation)
(define-key evil-normal-state-map (kbd "C-/") 'comment-dwim-line)
(define-key evil-normal-state-map (kbd "C-SPC") 'lsp-execute-code-action)
(define-key evil-normal-state-map (kbd "SPC c c") (lambda()
                                                    (interactive)
                                                    (call-interactively 'compile)))
(define-key evil-normal-state-map (kbd "SPC c i") (lambda()
                                                    (interactive)
                                                    (indent-region (point-min) (point-max) nil)))

(define-key evil-normal-state-map (kbd "?") 'lsp-ui-doc-glance)
;; flycheck
(define-key evil-normal-state-map (kbd "SPC f l") 'flycheck-list-errors)
(define-key evil-normal-state-map (kbd "SPC f n") 'flycheck-next-error)
(define-key evil-normal-state-map (kbd "SPC f p") 'flycheck-previous-error)
;; minibuffer
;; gotta unbind them first first
(define-key evil-ex-completion-map (kbd "C-S-v") 'evil-paste-after)
(define-key evil-ex-completion-map (kbd "C-v") 'evil-paste-after)
(define-key minibuffer-local-map (kbd "C-S-v") 'evil-paste-after)
(define-key minibuffer-local-map (kbd "C-v") 'evil-paste-after)
(define-key minibuffer-local-must-match-map (kbd "C-n") 'next-line-or-history-element)
(define-key minibuffer-local-must-match-map (kbd "C-p") 'previous-line-or-history-element)
;; misc
(define-key evil-normal-state-map (kbd "SPC f a") 'flyspell-auto-correct-word)
(define-key evil-normal-state-map (kbd "SPC e r") 'eval-region)
(define-key evil-normal-state-map (kbd "SPC '") 'shell-pop)
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
(define-key evil-normal-state-map (kbd "(") 'treesit-beginning-of-defun)
(define-key evil-normal-state-map (kbd ")") 'treesit-end-of-defun)
(define-key evil-visual-state-map (kbd "(") 'treesit-beginning-of-defun)
(define-key evil-visual-state-map (kbd ")") 'treesit-end-of-defun)


;; Winum
(setq winum-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "M-1") 'winum-select-window-1)
        (define-key map (kbd "M-2") 'winum-select-window-2)
        (define-key map (kbd "M-3") 'winum-select-window-3)
        (define-key map (kbd "M-4") 'winum-select-window-4)
        (define-key map (kbd "M-5") 'winum-select-window-5)
        (define-key map (kbd "M-6") 'winum-select-window-6)
        (define-key map (kbd "M-7") 'winum-select-window-7)
        (define-key map (kbd "M-8") 'winum-select-window-8)
        map))
(require-package 'winum)
(winum-mode)


;; LSP & Company
(setq company-idle-delay 1
      company-minimum-prefix-length 0)
;; (setq lsp-keymap-prefix "SPC l")
(require-package 'lsp-mode)
;; (define-key lsp-mode-map (kbd "SPC l") lsp-command-map)
(require-package 'lsp-ui)
(require-package 'flycheck)
(require-package 'company)
(require-package 'dap-mode)
(require-package 'yasnippet)
(global-flycheck-mode)
(yas-global-mode)
(evil-define-key 'normal lsp-mode-map (kbd "SPC l") lsp-command-map)
(evil-define-minor-mode-key 'normal lsp-mode (kbd "SPC l") lsp-command-map)
(add-hook 'rust-ts-mode-hook #'lsp)
(add-hook 'elpy-mode-hook #'lsp)
(add-hook 'java-ts-mode-hook #'lsp)
(add-hook 'js2-mode-hook #'lsp)
(add-hook 'prolog-mode-hook #'lsp)
(add-hook 'web-mode-hook #'lsp)
(add-hook 'csharp-mode-hook #'lsp)
(add-hook 'c-ts-mode-hook #'lsp)
(add-hook 'lua-mode-hook #'lsp)
(add-hook 'c++-ts-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'go-ts-mode-hook #'lsp)
(add-hook 'markdown-mode-hook #'flyspell-mode)


;; TRAMP
(setq tramp-use-connection-share 'suppress)
(setq shell-file-name "/bin/bash")


;; Rust
(require-package 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(setq lsp-rust-analyzer-server-display-inlay-hints t
      lsp-rust-analyzer-completion-auto-import-enable t
      lsp-completion-enable-additional-text-edit t)
(add-hook 'before-save-hook (lambda()
                              (when (eq 'rust-ts-mode major-mode)
                                (lsp-format-buffer))))
(add-hook 'rust-ts-mode-hook (lambda()
                               (local-set-key (kbd "SPC c l")
                                              'rust-run-clippy)
                               (local-set-key (kbd "SPC c r")
                                              'rust-run)
                               (local-set-key (kbd "SPC c R")
                                              'rust-run-release)
                               ))

;; C :)
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-ts-mode))
(add-hook 'before-save-hook (lambda()
                              (when (eq 'c-ts-mode major-mode)
                                (lsp-format-buffer))))

;; C++
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
(add-hook 'before-save-hook (lambda()
                              (when (eq 'c++-ts-mode major-mode)
                                (lsp-format-buffer))))

;; C# :(
(add-hook 'before-save-hook (lambda()
                              (when (eq 'csharp-mode major-mode)
                                (lsp-format-buffer))))

;; JSON
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))

;; YAML
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))

;; Terraform
(require-package 'terraform-mode)
(add-hook 'terraform-mode 'terraform-format-on-save-mode)

;; Python
;; (require-package 'elpy)
;; (require-package 'auto-virtualenv)
;; (require-package 'py-isort)
;; (require-package 'python-black)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
;; (eval-after-load "company"
;; '(add-to-list 'company-backends 'company-anaconda))
(add-hook 'before-save-hook (lambda ()
                              (when (eq 'python-ts-mode major-mode)
                                (lsp-format-buffer))))
(add-hook 'python-ts-mode-hook
          (lambda ()
            (setq-default tab-width 4)))
(add-to-list 'lsp-disabled-clients 'pylsp)
;; ruff over TRAMP
;; (add-to-list 'lsp-disabled-clients 'ruff)
;; (add-to-list 'lsp-disabled-clients 'ruff-tramp)
;; (add-to-list 'lsp-disabled-clients 'ty-ls)
;; (add-to-list 'lsp-disabled-clients 'ty-ls-tramp)
(lsp-register-client
 (make-lsp-client :new-connection (lsp-tramp-connection
                                   (lambda () (append lsp-ruff-server-command lsp-ruff-ruff-args)))
                  :major-modes '(python-ts-mode)
                  :multi-root t
                  :remote? t
                  :add-on? t
                  :uri->path-fn (lambda (x)
                                  (nth 2 (split-string x ":")))
                  :server-id 'ruff-tramp))
;; ty over TRAMP
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () lsp-python-ty-clients-server-command))
                  :major-modes '(python-ts-mode)
                  :multi-root t
                  :remote? t
                  :add-on? t
                  :server-id 'ty-ls-tramp))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () lsp-python-ty-clients-server-command))
                  :activation-fn (lsp-activate-on "python")
                  :priority -1
                  :add-on? t
                  :server-id 'ty-ls))
(add-hook 'python-ts-mode-hook #'lsp)
(add-hook 'lsp-mode-hook
          (lambda ()
            (when (eq 'python-ts-mode major-mode)
              (setq lsp-diagnostics-provider :none))))
;; (add-hook 'python-ts-mode-hook 'auto-virtualenv-set-virtualenv)

;; Java
(require-package 'lsp-java)
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
(add-hook 'before-save-hook (lambda()
                              (when (eq 'java-ts-mode major-mode)
                                (lsp-format-buffer))))

;; Lua
(require-package 'lua-mode)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

;; Prolog (fuck you perl)
(lsp-register-client
  (make-lsp-client
   :new-connection
   (lsp-stdio-connection (list "swipl"
                               "-g" "use_module(library(lsp_server))."
                               "-g" "lsp_server:main"
                               "-t" "halt"
                               "--" "stdio"))
   :major-modes '(prolog-mode)
   :priority 1
   :multi-root t
   :server-id 'prolog-ls))
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;; SQL formatting
(require-package 'sqlformat)
(setq sqlformat-command 'sql-formatter)
(add-hook 'sql-mode-hook 'sqlformat-on-save-mode)
(setq sqlformat-args '("-l" "postgresql" "-c" "{\"tabWidth\": 4}"))

;; Auto-insert mode
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/auto-insert/")
(setq auto-insert-query nil)
(define-auto-insert "\\.tex$" "template.tex")
(define-auto-insert "\\.py$" "template.py")
(define-auto-insert "^\\.flake8$" ".flake8")
(define-auto-insert "^pyproject\\.toml$" "pyproject.toml")
(define-auto-insert "\\.c$" "template.c")

;; JavaScript
;; (require-package 'js2-mode)
(require-package 'rjsx-mode)
(require-package 'prettier-js)
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
;; (add-hook 'js-mode-hook 'prettier-js-mode)

;; React
(require-package 'typescript-mode)
(require-package 'web-mode)
(require-package 'tide)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (tide-hl-identifier-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-attr-value-indent-offset 2)
  (company-mode +1))
(setq company-tooltip-align-annotations t)
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'rjsx-mode-hook 'tide-setup-hook)
(add-hook 'web-mode-hook (lambda()
                           (pcase (file-name-extension buffer-file-name)
                             ("tsx" ('tide-setup-hook))
                             (_ ()))))
(flycheck-add-mode 'typescript-tslint 'web-mode)
(add-hook 'web-mode-hook 'company-mode)
;; (add-hook 'web-mode-hook 'prettier-js-mode)
;; (add-hook 'web-mode-hook #'turn-on-smartparens-mode t)

;; HTML formatting
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; LaTeX
(add-hook 'latex-mode-hook #'flyspell-mode)
(add-hook 'latex-mode-hook #'word-wrap-whitespace-mode)
(add-hook 'TeX-mode-hook (lambda()
                           (local-set-key (kbd "SPC c r")
                                          'TeX-command-run-all)))

;; Matlab use octave mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
(add-hook 'octave-mode-hook (lambda()
                              (setq comment-start "%")
                              (setq evil-shift-width 2)
                              ))

;; Golang
(require-package 'go-mode)
(add-to-list 'exec-path (expand-file-name "~/go/bin"))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

;; Livedown (live markdown preview)
(if (file-directory-p (expand-file-name "~/.emacs.d/emacs-livedown"))
    (progn
        (add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
        (require 'livedown)
        (setq livedown-autostart 1)))

;; shell-script-mode
(add-to-list 'auto-mode-alist '("\\.zsh-theme\\'" . shell-script-mode))

;; Vimish Fold
(require-package 'vimish-fold)
(define-key evil-normal-state-map (kbd "z f") 'vimish-fold)
(define-key evil-normal-state-map (kbd "z r") 'vimish-fold-refold)
(define-key evil-normal-state-map (kbd "z u") 'vimish-fold-unfold)
(define-key evil-normal-state-map (kbd "z d") 'vimish-fold-delete)
(define-key evil-normal-state-map (kbd "z z") 'vimish-fold-toggle)

;; Indent highlight guides
(require-package 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

(setq backup-directory-alist
        `(("." . ,(concat user-emacs-directory "autosaves"))))

;; Treesitter
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (java "https://github.com/tree-sitter/tree-sitter-java")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(require-package 'doom-themes)
(setq doom-themes-enable-bold t)
(setq doom-themes-enable-italic t)
(load-theme 'doom-pixel t)

;; Semantic
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(require-package 'semantic)
(semantic-mode 1)
(require-package 'stickyfunc-enhance)

;; Projectile
(require-package 'projectile)
(setq projectile-globally-ignored-file-suffixes '(".png" ".gif" ".pdf"  "*.class"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("e34c6ff1b5a1b78ef6a49fba8a98cf20c3a56ad825aa517354e43c281e4cd646"
     "5938d2792c97b146b55b4cae8f45c3642aedb955133cb3d1f81e13d80750263a"
     "c7de972928215fc1a25fe87f98f2ea2215e0666f032b24e3c2e6e4f601e35cc4"
     "6ad331c9e610068f0d0a17eb4d4d21d9662e2cbd63e042cc69f7b976a23b3129"
     "fcff3bd64e1bcede13d527baae1ee4ce7b3dae1346495ff30fd17ee7d4781134"
     "9dd5c7baf655ff549949a761ccec016091b2a4141d7c9105b05cf21f3d873f6a"
     "94172571752196ec37617e043eda3546a552a71bbe31c6eea2af19013ea14383"
     "14100afaf6ad421e6f7b7bda74fc58d52f5e8241272dd1621f8b77d18965fb47"
     "6ec0fb8b90690eb602eaafb53b9fee71dc56de01d5fb40b44d835fb54f062f84"
     "76e2758f007aaca39f02e37fe061d1f14029cc15f456d68ad5541dbfa7322938"
     "4c6cfb98f58ddddfc810aac6e27f1c79f4d9500d3dad26b428967ce924003e58"
     "197105c4eb4a90bf72b108a35af850d9772756d9162d69b5d7553b5d16d8b541"
     "ece2542cfe228e30f487edddd4b8199a53cd7e5389062005266219cc06138417"
     "2e3c04d7f77bc7f571d9cd1fcc3388cc5284284776a93a1301e790509dea42d4"
     "18ca0e7214673e3a7fce3bd6811f58b956ae827df4da51edb5764441dba43874"
     "f676dea960effe9835256297def3c6e52058263c143fd90a15a50eca248ac6bb"
     "3a5a08fd785c8a6c340b6e71205f0189926bfd7b9c90dee6eb29ef448ba1796e"
     "0f82869eb28c97c2502d735c1c5e08ce70ca0b321a4f39aff32e1cde42ccf1cb"
     "6731c4ce0944cc2b35bcdfb0fe3e192fb4fbf002845d76e33975db1c709eb6bb"
     "7b8fa16385cc547dc9678c29389babfd449916d2b13269b2b89f86e2d97436ff"
     "1fc6b7661b1821b63b0fd38c1adb4c69344501e65a7fd49be9321c1792ee6099"
     "6143cac999431ab6e94e21ac83c9cfea3e95efbaeb52e4045a493b7650c2c4e7"
     "b25aa10e4bcf36a6ccf2169f3cc277f487f8b214c8d55c84811d1494e44ef8f2"
     "66878c3ac27a29b713fc462f624627bda8b2bdaef4181ed82620694a30d2bc87"
     "c39e75c161ab0a40ae8d57f6081c2ddb157615a9fe1d30afa3df249cf9d57de1"
     "b137666681231f13f11513d23e141a5dc7e81b3d2f7919f42a71e59be5e3ec64"
     "558d5798432b98cd74f1230b033222960f0dc4e13df661323a81820d992ae93d"
     "5df7c6fcf5eccc1f37ca8f0cac60b8863300d444eac3760c21d36d9ea9b5f9ba"
     "96922e829a1e887928d8586c9f00aea89b1e844755dfe7144ddb3522fc23cb42"
     "d00cf47ed0adcee416624d505967d492c29dc19de0062b70459c989ad1a6510b"
     "90bb09f60d53935ad58e65e86f36080ca527e3a9e3851d74108963843db1326f"
     "5bb66ba3b98499b0da06c23116c9f86d77843bffdd1cd010c32c7d34ba67e597"
     "959879e742b16a6c291815d35bef4a3aafbd0db529ce260d4a1f3dc86c154eef"
     "c640f23d88432f93914ff9e60289c70e9e4fd9ee6de55c5ea8a083e8980b1e1a"
     "3a561040ac81bd71dd6301fa934a9f485047caa27ac149a2078c7b5ce9efb09b"
     "494f95c67aa0a1e6f63fe7ca3ba04406ed927934a26ac6394334ced98ef214d1"
     "1c5700a92ec3940be150d976e319eb747ef08929ebed2656b25f387794a153e2"
     "dbedce6fa449ac9518245a0e41ab028abd0a70f4c54541f30f184e4369c3471a"
     "670868b5ac3474e61ac0d57517ae1912a7a305bc283649063a662db4efdda17b"
     "f703483ca7234fea9b761ea2c406bcda7135d89df1593fe14fe6882e80c1ada6"
     "a09ae0566c7293b363b0c8c042081eab2c4127ee96c04ec7011b9627cf7f1c04"
     "e4c75d03ba72f5686965a7b164dc0b6187f994d548451bef1068ef03ad43e510"
     "ec7153fb7b49006884eb649c5ff299e518922dad1d6da657c09861a71afeb1e8"
     "b9643487d54c5b6f83b3fa1fb64dd419433e6ca255a2f51b9fa562325145ccb2"
     "891c69fa725840ca0336a15faa2f84fe2b02fab6c87eb8266223dab83e2572f6"
     "ec0f046cddd6e0309d0420c2649f793294eef0bb3c7c9a4939287ae27e8efe3e"
     "587f2ac20f383e0769b40eb8e2384b0f4155157f0ba5cd816bd44edbaa1816e9"
     "a773d1be23ed92ced6ba63f261e0f8256bdac531ca2835c3301c50b33490e895"
     "ea7e80076c3d256912ab1fd825a551a97e0d36a4ee069d83c61fb9b186b6cf16"
     "02b63ebf992317c8f7baa696c797622289cb3f0824f603690dfdcebd28c87fe3"
     "afc643a5537104f90a1675ba4518826c25005dd955c82a1253deddbda2283109"
     "406df210526f6e86c7cb82a08d8db42cb5703de356e8d8ea15307d5b1bdc13f9"
     "a713860237327e1877a00ae4d86fc2d72970f33b92180b52476432d186b9a1e3"
     "914859deb77b5928a534e4839aec3d5e64764344c944b2e0219ff63092f4fc85"
     "00026dabe7a174d3ea4c7e6d2f3c1d2d93098a4f48d7392e7d2fd56996eb7382"
     "c2fe8298464ed2f7c54db5f50e5c02d1b186810342e0abb5dc56fcc438d34c8b"
     "3978f735426f54d1dff97fbc2cd755a6bc19b62796613388ced06d414c25bdcd"
     "7fa3f029214c2f8f993c68ba70c9e49fb9a4d82f4c1be76e4b4a1814c8441151"
     "fdde57022127bd493abf479e6c12e13462973d5be9f1b8df152cc6bdd41e0438"
     "c5c6d641767329992eb1b86f87ef45ca63141190f571692447bd9cc88d2e9f62"
     "840364b105f4f6024d2a838b873ba833e49d058d4559135a22b4351d9934bcfe"
     "8c4d9efce21d20194c8778b414115f1e8663236ca22b0cdf145d32fe563e4be4"
     "5def3bbd06c9246980c4b4975e56febc187967ff39ce829ea63b3da1add47624"
     "9922bca5f20a360696a00cf97c608847ef7c5eaaced85143077cce98969ac75b"
     "80c80cd5ae8ca357c306a9e24392055893ca7f90cc71bef6cd69b1bc4b21f015"
     "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8"
     "e5494adf200eeff1505839672150dde6053e086869189c381b1ce9b792dda3a8"
     default))
 '(helm-minibuffer-history-key "M-p")
 '(highlight-indent-guides-method 'character)
 '(js-indent-level 2)
 '(lsp-headerline-breadcrumb-segments '(project file symbols))
 '(package-selected-packages
   '(auctex auctex-latexmk auctex-lua auto-virtualenv colorful-mode
            company-anaconda csv-mode cuda-mode dap-mode
            dockerfile-mode dotenv-mode elpy evil evil-collection
            evil-surround go-mode helm highlight-indent-guides
            ido-vertical-mode js2-mode lsp-java lsp-mode lua-mode
            magit mips-mode mode-line-bell pdf-tools php-mode
            prettier-js projectile prolog-mode puppet-mode
            python-black pyvenv racket-mode ron-mode rust-mode
            shell-pop sudo-edit terraform-mode treemacs treemacs-evil
            vimish-fold web-mode winum yaml-mode))
 '(prettier-js-args '("--tab-width 4"))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-warning ((t (:underline (:color "yellow" :style wave))))))

(find-file "~/notes/notes.md")
