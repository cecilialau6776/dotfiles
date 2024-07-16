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
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
(define-coding-system-alias 'UTF-8 'utf-8)
;; Colors in compilaiton window
(require-package 'ansi-color)
(setq ansi-color-for-compilation-mode t)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
;; Misc modes
(require-package 'ron-mode)


;; Shell-pop
(setq shell-pop-shell-type (quote ("vterm" "*vterm*" (lambda nil (vterm shell-pop-term-shell)))))
(setq shell-pop-term-shell "/usr/bin/zsh")
(setq shell-pop-full-span t)
(setq shell-pop-window-size 20)
(setq shell-pop-window-position "bottom")
(setq shell-pop-autocd-to-working-dir t)
(setq shell-pop-cleanup-buffer-at-process-exit t)
(require-package 'shell-pop)


;; Keybinds
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key evil-motion-state-map (kbd "SPC") nil)
(define-key evil-normal-state-map (kbd "SPC r c") (lambda() (interactive) (load-file "~/.emacs.d/init.el")))
(define-key treemacs-mode-map (kbd "SPC 0") 'treemacs)
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
;; code
(define-key evil-normal-state-map (kbd "SPC c r") 'recompile)
(define-key evil-normal-state-map (kbd "SPC c k") 'kill-compilation)
(define-key evil-normal-state-map (kbd "C-/") 'comment-line)
(define-key evil-normal-state-map (kbd "C-SPC") 'lsp-execute-code-action)
(define-key evil-normal-state-map (kbd "SPC c c") (lambda()
                                                    (interactive)
                                                    (call-interactively 'compile)))
(define-key evil-normal-state-map (kbd "SPC c i") (lambda()
                                                    (interactive)
                                                    (indent-region (point-min) (point-max) nil)))
;; flycheck
(define-key evil-normal-state-map (kbd "SPC f l") 'flycheck-list-errors)
(define-key evil-normal-state-map (kbd "SPC f n") 'flycheck-next-error)
(define-key evil-normal-state-map (kbd "SPC f p") 'flycheck-previous-error)
;; misc
(define-key evil-normal-state-map (kbd "SPC f a") 'flyspell-auto-correct-word)
(define-key evil-normal-state-map (kbd "SPC e r") 'eval-region)
(define-key evil-normal-state-map (kbd "SPC '") 'shell-pop)
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
(require-package 'lsp-mode)
(require-package 'lsp-ui)
(require-package 'flycheck)
(require-package 'company)
(require-package 'dap-mode)
(require-package 'yasnippet)
(global-flycheck-mode)
(yas-global-mode)
;; LSP over TRAMP
(lsp-register-client
    (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
                     :major-modes '(python-ts-mode)
                     :remote? t
                     :server-id 'pylsp-remote))
;; (evil-define-key 'normal lsp-mode-map (kbd "SPC l") lsp-command-map)
(evil-define-minor-mode-key 'normal lsp-mode (kbd "SPC l") lsp-command-map)
(add-hook 'rust-ts-mode-hook #'lsp)
(add-hook 'elpy-mode-hook #'lsp)
(add-hook 'java-mode-hook #'lsp)
(add-hook 'js2-mode-hook #'lsp)
(add-hook 'prolog-mode-hook #'lsp)
(add-hook 'web-mode-hook #'lsp)
(add-hook 'csharp-mode-hook #'lsp)
(add-hook 'c-ts-mode-hook #'lsp)
(add-hook 'lua-mode-hook #'lsp)
(add-hook 'c++-ts-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; Rust
(require-package 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(setq lsp-rust-analyzer-server-display-inlay-hints t
      lsp-rust-analyzer-completion-auto-import-enable t
      lsp-completion-enable-additional-text-edit t)
(add-hook 'before-save-hook (lambda()
                              (when (eq 'rust-ts-mode major-mode)
                                (lsp-format-buffer))))

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
(require-package 'elpy)
(require-package 'auto-virtualenv)
(require-package 'py-isort)
(require-package 'python-black)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(elpy-enable)
(add-hook 'elpy-mode-hook (lambda ()
                            (add-hook 'before-save-hook
                                      'python-black-buffer nil t)))
(add-hook 'python-ts-mode-hook #'elpy-mode)
(add-hook 'python-ts-mode-hook 'auto-virtualenv-set-virtualenv)
(add-hook 'before-save-hook (lambda()
                              (when (eq 'python-ts-mode major-mode)
                                (py-isort-buffer))))

;; Java
(require-package 'lsp-java)

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
(add-hook 'js-mode-hook 'prettier-js-mode)

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
(add-hook 'web-mode-hook 'tide-setup-hook
          (lambda () (pcase (file-name-extension buffer-file-name)
                  ("tsx" ('tide-setup-hook))
                  (_ (my-web-mode-hook)))))
(flycheck-add-mode 'typescript-tslint 'web-mode)
(add-hook 'web-mode-hook 'company-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook #'turn-on-smartparens-mode t)

;; HTML formatting
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; Livedown (live markdown preview)
(if (file-directory-p (expand-file-name "~/.emacs.d/emacs-livedown"))
    (progn
        (add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
        (require 'livedown)
        (setq livedown-autostart 1)))

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
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; ;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(require-package 'doom-themes)
(setq doom-themes-enable-bold t)
(setq doom-themes-enable-italic t)
(load-theme 'doom-pixel t)





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("587f2ac20f383e0769b40eb8e2384b0f4155157f0ba5cd816bd44edbaa1816e9" "a773d1be23ed92ced6ba63f261e0f8256bdac531ca2835c3301c50b33490e895" "ea7e80076c3d256912ab1fd825a551a97e0d36a4ee069d83c61fb9b186b6cf16" "02b63ebf992317c8f7baa696c797622289cb3f0824f603690dfdcebd28c87fe3" "afc643a5537104f90a1675ba4518826c25005dd955c82a1253deddbda2283109" "406df210526f6e86c7cb82a08d8db42cb5703de356e8d8ea15307d5b1bdc13f9" "a713860237327e1877a00ae4d86fc2d72970f33b92180b52476432d186b9a1e3" "914859deb77b5928a534e4839aec3d5e64764344c944b2e0219ff63092f4fc85" "00026dabe7a174d3ea4c7e6d2f3c1d2d93098a4f48d7392e7d2fd56996eb7382" "c2fe8298464ed2f7c54db5f50e5c02d1b186810342e0abb5dc56fcc438d34c8b" "3978f735426f54d1dff97fbc2cd755a6bc19b62796613388ced06d414c25bdcd" "7fa3f029214c2f8f993c68ba70c9e49fb9a4d82f4c1be76e4b4a1814c8441151" "fdde57022127bd493abf479e6c12e13462973d5be9f1b8df152cc6bdd41e0438" "c5c6d641767329992eb1b86f87ef45ca63141190f571692447bd9cc88d2e9f62" "840364b105f4f6024d2a838b873ba833e49d058d4559135a22b4351d9934bcfe" "8c4d9efce21d20194c8778b414115f1e8663236ca22b0cdf145d32fe563e4be4" "5def3bbd06c9246980c4b4975e56febc187967ff39ce829ea63b3da1add47624" "9922bca5f20a360696a00cf97c608847ef7c5eaaced85143077cce98969ac75b" "80c80cd5ae8ca357c306a9e24392055893ca7f90cc71bef6cd69b1bc4b21f015" "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8" "e5494adf200eeff1505839672150dde6053e086869189c381b1ce9b792dda3a8" default))
 '(helm-minibuffer-history-key "M-p")
 '(highlight-indent-guides-method 'character)
 '(js-indent-level 2)
 '(package-selected-packages
   '(ron-mode colorful-mode terraform-mode auto-virtualenv mode-line-bell prolog-mode web-mode csv-mode magit dockerfile-mode racket-mode yaml-mode php-mode cuda-mode elpy python-black projectile pyvenv dotenv-mode evil-collection pdf-tools auctex-latexmk auctex-lua auctex lua-mode evil-surround highlight-indent-guides vimish-fold js2-mode prettier-js dap-mode lsp-java shell-pop mips-mode lsp-mode rust-mode winum treemacs-evil treemacs helm ido-vertical-mode evil))
 '(prettier-js-args '("--tab-width 4"))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-warning ((t (:underline (:color "yellow" :style wave))))))

(find-file "~/notes.md")
