
(use-package isearch
  :ensure nil
  :config
  (setq isearch-lazy-count t
        lazy-count-prefix-format "(%s/%s) "
        lazy-count-suffix-format nil
        search-whitespace-regexp ".*?"))

(provide 'init-search)
