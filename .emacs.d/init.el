(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)


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

(setq-default rust-indent-offset 2)
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


;; Misc
(global-auto-revert-mode t)
(require-package 'vterm)
(setq vterm-shell 'zsh)
(setq inhibit-startup-screen t)
(setq visible-bell nil
      ring-bell-function
      (lambda()
        (invert-face 'mode-line)
        (run-with-timer 0.1 nil #'invert-face 'mode-line)))
(setq line-number-mode t)
(setq-default doc-view-resolution 200)
(setq-default doc-view-continuous t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(prefer-coding-system 'utf-8)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
(define-coding-system-alias 'UTF-8 'utf-8)
;; Colors in compilaiton window
(require-package 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)


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
(define-key evil-normal-state-map (kbd "C-/") 'comment-line)
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
;; flyspell
(define-key evil-normal-state-map (kbd "SPC f a") 'flyspell-auto-correct-word)
;; evaluate elisp
(define-key evil-normal-state-map (kbd "SPC e r") 'eval-region)
;; shell-pop
(define-key evil-normal-state-map (kbd "SPC '") 'shell-pop)
;; vterm
;; (define-key vterm-mode-map (kbd "s-c") #'vterm-send-next-C-c)
(define-key vterm-mode-map (kbd "s-<backspace>")
    (lambda () (interactive) (vterm-send-key (kbd "C-w"))))



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
(add-hook 'rust-mode-hook #'lsp)
(add-hook 'elpy-mode-hook #'lsp)
(add-hook 'java-mode-hook #'lsp)
(add-hook 'js2-mode-hook #'lsp)
(add-hook 'prolog-mode-hook #'lsp)
(add-hook 'web-mode-hook #'lsp)
(add-hook 'csharp-mode-hook #'lsp)
(add-hook 'c-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; Rust
(require-package 'rust-mode)
(setq lsp-rust-analyzer-server-display-inlay-hints t
      lsp-rust-analyzer-completion-auto-import-enable t
      lsp-completion-enable-additional-text-edit t)
(add-hook 'before-save-hook (lambda()
                              (when (eq 'rust-mode major-mode)
                                (lsp-format-buffer))))

;; C :)
(add-hook 'before-save-hook (lambda()
                              (when (eq 'c-mode major-mode)
                                (lsp-format-buffer))))

;; C# :(
(add-hook 'before-save-hook (lambda()
                              (when (eq 'csharp-mode major-mode)
                                (lsp-format-buffer))))

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
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Ephemeral theme
(require-package 'doom-themes)
(setq doom-themes-enable-bold t)
(setq doom-themes-enable-italic t)
(load-theme 'doom-ephemeral t)





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(helm-minibuffer-history-key "M-p")
 '(highlight-indent-guides-method 'character)
 '(js-indent-level 2)
 '(package-selected-packages
   '(auto-virtualenv mode-line-bell prolog-mode web-mode csv-mode magit dockerfile-mode racket-mode yaml-mode php-mode cuda-mode elpy python-black projectile pyvenv dotenv-mode evil-collection pdf-tools auctex-latexmk auctex-lua auctex lua-mode evil-surround highlight-indent-guides vimish-fold js2-mode prettier-js dap-mode lsp-java shell-pop mips-mode lsp-mode rust-mode winum treemacs-evil treemacs helm ido-vertical-mode evil))
 '(prettier-js-args '("--tab-width 4"))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-warning ((t (:underline (:color "yellow" :style wave))))))

(find-file "~/notes.md")
