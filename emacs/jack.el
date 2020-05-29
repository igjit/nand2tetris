(define-derived-mode jack-mode java-mode "Jack"
  "Major mode for editing Jack code."
  (setq-local indent-tabs-mode nil))

(add-to-list 'auto-mode-alist '("\\.jack$" . jack-mode))

(flycheck-define-checker jack-checker
  "Check Jack"
  :command ("JackCompiler.sh" source)
  :error-patterns
  ((error line-start (+ not-newline) "(line " line "): " (message) line-end))
  :modes jack-mode)

(add-to-list 'flycheck-checkers 'jack-checker)
