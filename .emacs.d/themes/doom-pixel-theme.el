;;; doom-pixel-theme.el --- pixel                    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  

;; Author:  <pixel@FrameworkClau>
;; Keywords: faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-pixel-theme nil
  "Options for the `doom-pixel' theme."
  :group 'doom-themes)

(defcustom doom-pixel-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-pixel-theme
  :type 'boolean)

(defcustom doom-pixel-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-pixel-theme
  :type 'boolean)

(defcustom doom-pixel-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to determine the exact padding."
  :group 'doom-pixel-theme
  :type '(choice integer boolean))

(defcustom doom-pixel-region-highlight t
  "Determines the selection highlight style. Can be 'frost, 'snowstorm or t
(default)."
  :group 'doom-pixel-theme
  :type 'symbol)


;;
;;; Theme definition

(def-doom-theme doom-pixel
  "A dark theme inspired by Nord."

  ;; name        default   256       16
  ((bg           '("#4D2245" "#000000" "black"        ))
   (fg           '("#f8f8f2" "#f8f8f2" "white"        ))

   (bg-alt       '("#331B2F" "#000000" "black"        ))
   (fg-alt       '("#fdfdfd" "#fdfdfd" "brightwhite"  ))

   (base0        '("#261022" "black"   "black"        ))
   (base1        '("#2D1328" "#2D1328" "brightblack"  ))
   (base2        '("#391933" "#391933" "brightblack"  ))
   (base3        '("#421D3B" "#421D3B" "brightblack"  ))
   (base4        '("#4D2245" "#4D2245" "brightblack"  ))
   (base5        '("#55254C" "#55254C" "brightblack"  ))
   (base6        '("#70516A" "#70516A" "brightblack"  ))
   (base7        '("#8F778A" "#8F778A" "brightblack"  ))
   (base8        '("#F0EBEF" "#F0EBEF" "white"        ))

   (grey         '("#6F3165" "#6F3165" "grey"         ))
   (red          '("#FF7094" "#FF7094" "red"          ))
   (orange       '("#FF7F70" "#FF7F70" "brightred"    ))
   (green        '("#70FF83" "#70FF83" "green"        ))
   (teal         '("#70FFF6" "#70FFF6" "brightgreen"  ))
   (yellow       '("#FFE570" "#FFE570" "yellow"       ))
   (blue         '("#70BDFF" "#70BDFF" "brightblue"   ))
   (dark-blue    '("#5D9DD4" "#5D9DD4" "blue"         ))
   (magenta      '("#FF70EA" "#FF70EA" "magenta"      ))
   (violet       '("#AF70FF" "#AF70FF" "brightmagenta"))
   (dark-violet  '("#7A4FB3" "#7A4FB3" "brightmagenta"))
   (cyan         '("#6CF6ED" "#6CF6ED" "brightcyan"   ))
   (dark-cyan    '("#5DD4CD" "#5DD4CD" "cyan"         ))

   ;; pixel colours
   (pink         '("#FFA3F1" "#FFA3F1" "grey"         ))
   (light-violet '("#CBA3FF" "#CBA3FF" "grey"         ))
   (light-grey   '("#914084" "#914084" "grey"         ))
   (dark-green   '("#5DD46D" "#5DD46D" "green"        ))
   (dark-magenta '("#E665D2" "#E665D2" "magenta"      ))

   ;; face categories -- required for all themes
   (highlight      cyan)
   (vertical-bar   bg-alt)
   (selection      blue)
   (builtin        yellow)
   (comments       light-grey)
   (doc-comments   light-grey)
   (constants      blue)
   (functions      dark-cyan)
   (keywords       dark-magenta)
   (methods        teal)
   (operators      blue)
   (type           dark-cyan)
   (strings        red)
   (variables      dark-green)
   (numbers        dark-green)
   (region         base1)
   (error          orange)
   (warning        yellow)
   (success        green)
   (vc-modified    teal)
   (vc-added       blue)
   (vc-deleted     orange)

   ;; custom categories
   (-modeline-bright doom-pixel-brighter-modeline)
   (-modeline-pad
    (when doom-pixel-padded-modeline
      (if (integerp doom-pixel-padded-modeline) doom-pixel-padded-modeline 4)))

   (region-fg
    (when (memq doom-pixel-region-highlight '(frost snowstorm))
      bg-alt))

   (modeline-fg            fg)
   (modeline-fg-alt        light-grey)
   (modeline-bg            grey)
   (modeline-bg-l          base2)
   (modeline-bg-inactive   base3)
   (modeline-bg-inactive-l `(,(car base3), (cdr base6))))


  ;;;; Base theme face overrides
  (((line-number &override) :foreground grey)
   ((line-number-current-line &override) :foreground blue)
   (link :foreground (doom-lighten light-grey 0.3) :inherit 'underline)
   ((font-lock-comment-face &override)
    :inherit 'bold :background (if doom-pixel-brighter-comments (doom-lighten bg 0.05) 'unspecified))
   ((font-lock-builtin-face &override) :inherit 'italic)
   ((font-lock-keyword-face &override) :inherit 'bold)
   (header-line :background base5)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))
   ((region &override) :foreground region-fg)
   (shadow :foreground base6)
   ((tooltip &override) :background base1)
   ;;;; term
   (vterm-color-black :background grey :foreground grey)
   (vterm-color-magenta :background magenta :foreground magenta)
   (vterm-color-green :background green :foreground green)
   (vterm-color-yellow :background yellow :foreground yellow)
   (vterm-color-cyan :background cyan :foreground cyan)
   (vterm-color-blue :background violet :foreground violet)

   ;;;; company
   (company-tooltip-common :foreground violet)
   (company-tooltip-selection :background base0 :foreground red)
   ;;;; company-box
   (company-box-background :background base0 :foreground fg)
   ;;;; css-mode <built-in> / scss-mode
   (css-property             :foreground fg)
   (css-proprietary-property :foreground violet)
   (css-selector             :foreground red)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :foreground fg)
   (doom-modeline-buffer-major-mode :foreground teal :weight 'bold)
   (doom-modeline-buffer-modified :foreground violet)
   (doom-modeline-buffer-path :foreground red)
   (doom-modeline-highlight :foreground (doom-lighten base2 0.3))
   (doom-modeline-info :inherit 'bold :foreground cyan)
   (doom-modeline-panel :background base0)
   (doom-modeline-project-dir :foreground teal :inherit 'bold)
   (doom-modeline-urgent :foreground modeline-fg)
   ;;;; ediff <built-in>
   (ediff-fine-diff-A    :background (doom-darken violet 0.4) :weight 'bold)
   (ediff-current-diff-A :background (doom-darken base0 0.25))
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; evil
   (evil-ex-lazy-highlight      :background base1  :foreground fg)
   (evil-ex-search              :background base1  :foreground fg)
   (evil-snipe-first-match-face :background base1  :foreground orange)
   ;;;; haskell-mode
   (haskell-constructor-face :inherit 'bold :foreground cyan)
   (haskell-definition-face :inherit 'bold :foreground functions)
   (haskell-keyword-face :inherit 'italic :foreground blue)
   (haskell-literate-comment-face :foreground doc-comments)
   (haskell-operator-face :foreground pink)
   (haskell-type-face :inherit 'bold :foreground violet)
   ;;;; highlight-symbol
   (highlight-symbol-face :background region :distant-foreground fg-alt)
   ;;;; highlight-thing
   (highlight-thing :background region :distant-foreground fg-alt)
   ;;;; ivy
   (ivy-current-match           :background base0      :distant-foreground nil)
   (ivy-posframe-cursor         :background cyan       :foreground base0)
   (ivy-minibuffer-match-face-2 :foreground red        :weight 'bold)
   ;;;; lsp-mode
   (lsp-headerline-breadcrumb-symbols-face :foreground functions :weight 'bold)
   ;;;; magit
   (magit-diff-hunk-heading           :foreground bg :background (doom-blend magenta bg 0.3) :extend t)
   (magit-diff-hunk-heading-highlight :foreground bg :background magenta :weight 'bold :extend t)
   (magit-section-heading :foreground red)
   (magit-branch-remote   :foreground orange)
   ;;;; markdown-mode
   (markdown-markup-face           :foreground red)
   (markdown-link-face             :foreground teal)
   (markdown-link-title-face       :foreground cyan)
   (markdown-header-face           :foreground red :inherit 'bold)
   (markdown-header-delimiter-face :foreground red :inherit 'bold)
   (markdown-language-keyword-face :foreground pink :inherit 'italic)
   (markdown-markup-face           :foreground blue)
   (markdown-bold-face             :foreground blue)
   (markdown-table-face            :foreground fg :background bg)
   ((markdown-code-face &override) :foreground teal :background base1)
   ;;;; notmuch
   (notmuch-wash-cited-text :foreground base6)
   ;;;; outline <built-in>
   (outline-1 :foreground cyan)
   (outline-2 :foreground violet)
   (outline-3 :foreground blue)
   (outline-4 :foreground red)
   (outline-5 :foreground pink)
   (outline-6 :foreground light-grey)
   (outline-7 :foreground yellow)
   (outline-8 :foreground cyan)
   ;;;; org <built-in>
   (org-agenda-done :foreground teal)
   ((org-block &override) :background base2)
   ((org-block-begin-line &override) :inherit 'bold :background base2 :foreground light-grey)
   (org-document-info-keyword :foreground comments)
   (org-headline-done :foreground red)
   (org-link :inherit 'underline :foreground pink)
   (org-list-dt :foreground light-grey)
   (org-todo :foreground red)
   ;;;; mic-paren
   ((paren-face-match &override) :background base3)
   ((paren-face-mismatch &override) :foreground base3)
   ;;;; rjsx-mode
   (rjsx-tag :foreground magenta)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))
   ;;;; vimish-fold
   ((vimish-fold-overlay &override) :background base3 :foreground base7)
   ((vimish-fold-fringe &override)  :foreground teal)))

;;; doom-pixel-theme.el ends here
